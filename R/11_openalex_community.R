# R/11_openalex_community.R
# Maps the neuroimaging research community from publication output (OpenAlex), as an
# alternative to past OHBM attendees for deciding where hubs should be.
# Community weight of an institution = number of articles published 2020-2024 in core
# neuroimaging journals with at least one author affiliated with it ("full counting").
# Methods and rationale: docs/methods_hub_selection.md (section M1).

# --- 1. Load Libraries and Parameters ---
library(tidyverse)
library(jsonlite)
library(countrycode)
source("R/utils.R")

# Core neuroimaging journals, identified by ISSN-L (unambiguous, unlike names)
community_journals <- c(
  "NeuroImage"            = "1053-8119",
  "Human Brain Mapping"   = "1065-9471",
  "NeuroImage: Clinical"  = "2213-1582",
  "Imaging Neuroscience"  = "2837-6056",
  "Brain Connectivity"    = "2158-0014",
  "Network Neuroscience"  = "2472-1751",
  "Cerebral Cortex"       = "1047-3211",
  "Brain Topography"      = "0896-0267",
  "Neuroinformatics"      = "1539-2791"
)
community_years <- 2020:2024

community_cache_path <- "data/openalex_neuroimaging_institutions.csv"

# API key (free, needed to page through results), read from the environment and never
# written to disk. Only needed when the cache below does not exist yet.
openalex_api_key <- Sys.getenv("OPENALEX_API_KEY")

# --- 2. OpenAlex Helpers ---
openalex_get <- function(endpoint, params) {
  if (nzchar(openalex_api_key)) params$api_key <- openalex_api_key
  query <- paste(names(params), map_chr(params, \(v) URLencode(as.character(v), reserved = TRUE)),
                 sep = "=", collapse = "&")
  url <- paste0("https://api.openalex.org/", endpoint, "?", query)
  Sys.sleep(0.15) # stay well below the API rate limit
  fromJSON(url, simplifyVector = TRUE)
}

works_filter <- paste0(
  "primary_location.source.issn:", paste(community_journals, collapse = "|"),
  ",type:article",
  ",publication_year:", min(community_years), "-", max(community_years)
)

# Number of articles per institution. Grouped results come back 200 groups per page;
# paging through them with a cursor requires an API key.
fetch_institution_counts <- function() {
  n_works <- openalex_get("works", list(filter = works_filter, per_page = 1))$meta$count
  print(paste("Articles matching the journal/year filter:", n_works))

  pages <- list()
  cursor <- "*"
  while (!is.null(cursor) && !is.na(cursor)) {
    res <- openalex_get("works", list(filter = works_filter,
                                      group_by = "authorships.institutions.id",
                                      per_page = 200, cursor = cursor))
    if (length(res$group_by) == 0 || nrow(res$group_by) == 0) break
    pages[[length(pages) + 1]] <- as_tibble(res$group_by)
    cursor <- res$meta$next_cursor
  }
  bind_rows(pages) %>%
    filter(!is.na(key), startsWith(key, "https://openalex.org/I")) %>%
    transmute(institution_id = str_remove(key, "https://openalex.org/"),
              institution = key_display_name, n_works = count)
}

# Location and country of institutions, 50 per request
fetch_institution_geo <- function(ids) {
  batches <- split(ids, ceiling(seq_along(ids) / 50))
  map_dfr(batches, \(batch) {
    res <- openalex_get("institutions", list(filter = paste0("openalex_id:", paste(batch, collapse = "|")),
                                             select = "id,geo,type", per_page = 50))
    tibble(institution_id = str_remove(res$results$id, "https://openalex.org/"),
           institution_type = res$results$type,
           city = res$results$geo$city,
           country_code = res$results$geo$country_code,
           latitude = res$results$geo$latitude,
           longitude = res$results$geo$longitude)
  })
}

# --- 3. Build or Load the Community Table ---
if (file.exists(community_cache_path)) {
  print(paste("Loading cached community data from", community_cache_path))
  community <- read_csv(community_cache_path, show_col_types = FALSE, na = "")
} else {
  print("Querying OpenAlex...")
  if (!nzchar(openalex_api_key)) {
    stop("OPENALEX_API_KEY is not set. Get a free key at https://openalex.org/settings/api ",
         "and add the line OPENALEX_API_KEY=<key> to ~/.Renviron, then restart R.")
  }
  counts <- fetch_institution_counts()
  geo <- fetch_institution_geo(counts$institution_id)
  community <- counts %>%
    left_join(geo, by = "institution_id") %>%
    arrange(desc(n_works))
  print(paste("Institutions found:", nrow(community)))
  write_csv(community, community_cache_path, na = "")
  print(paste("Community data cached to", community_cache_path))
}

missing_geo <- community %>% filter(is.na(latitude) | is.na(longitude))
if (nrow(missing_geo) > 0) {
  warning(sprintf("%d institutions (%d articles) have no coordinates and are dropped.",
                  nrow(missing_geo), sum(missing_geo$n_works)))
}
community <- community %>% filter(!is.na(latitude) & !is.na(longitude))

# --- 4. Regions ---
# UN geoscheme regions; Taiwan and Kosovo are not UN members, so they are set by hand
add_un_regions <- function(df, iso2) {
  df %>% mutate(
    region = countrycode(iso2, "iso2c", "un.region.name",
                         custom_match = c(TW = "Asia", XK = "Europe")),
    subregion = countrycode(iso2, "iso2c", "un.regionsub.name",
                            custom_match = c(TW = "Eastern Asia", XK = "Southern Europe"))
  )
}

# Some institutions have coordinates but no country in OpenAlex: take the country under their
# coordinates, or, for coastal points falling just outside the map polygons, the country of
# the nearest institution that has one.
fill_country_from_coordinates <- function(df) {
  missing <- which(is.na(df$country_code))
  if (length(missing) == 0) return(df)
  country_name <- sub(":.*", "", maps::map.where("world", df$longitude[missing], df$latitude[missing]))
  df$country_code[missing] <- countrycode(country_name, "country.name", "iso2c", warn = FALSE)
  still_missing <- which(is.na(df$country_code))
  known <- which(!is.na(df$country_code))
  for (i in still_missing) {
    d <- geosphere::distHaversine(c(df$longitude[i], df$latitude[i]),
                                  as.matrix(df[known, c("longitude", "latitude")]))
    df$country_code[i] <- df$country_code[known[which.min(d)]]
  }
  print(sprintf("Country filled in from coordinates for %d institutions", length(missing)))
  df
}

community <- community %>%
  fill_country_from_coordinates() %>%
  add_un_regions(.$country_code)

attendees <- readRDS("output/geocoded_attendee_data.rds") %>%
  mutate(country_code = countrycode(country_clean, "country.name", "iso2c",
                                    custom_match = c("Amiens" = "FR", "Elst" = "NL",
                                                     "Serbia And Montenegro" = "RS"))) %>%
  add_un_regions(.$country_code)
if (any(is.na(attendees$subregion))) {
  warning(sprintf("%d attendees could not be assigned to a UN subregion.", sum(is.na(attendees$subregion))))
}

# Cleaned tables used by 12_ and 13_
saveRDS(community, "output/community_institutions.rds")
saveRDS(attendees, "output/attendees_with_regions.rds")

# --- 5. Community vs. Attendees by Region ---
community_vs_attendees <- full_join(
  community %>% count(region, subregion, wt = n_works, name = "community_works"),
  attendees %>% filter(!is.na(subregion)) %>% count(region, subregion, name = "attendees_2023_2025"),
  by = c("region", "subregion")
) %>%
  replace_na(list(community_works = 0, attendees_2023_2025 = 0)) %>%
  mutate(
    community_share = community_works / sum(community_works),
    attendee_share = attendees_2023_2025 / sum(attendees_2023_2025),
    # > 1: the subregion sends more attendees than its share of the community predicts
    representation_ratio = attendee_share / community_share
  ) %>%
  arrange(desc(community_share))

write.csv(community_vs_attendees, "output/reports/community_vs_attendees_by_region.csv", row.names = FALSE)

# --- 6. Figures ---
world_map <- map_data("world")

ggplot() +
  geom_polygon(data = world_map, aes(long, lat, group = group),
               fill = "gray92", color = "white", linewidth = 0.2) +
  geom_point(data = arrange(community, n_works),
             aes(longitude, latitude, size = n_works),
             shape = 21, fill = "#2171b5", color = "white", stroke = 0.2, alpha = 0.7) +
  scale_size_area(max_size = 8, name = "Articles\n2020-2024") +
  coord_quickmap(xlim = c(-180, 180), ylim = c(-56, 75), expand = FALSE) +
  labs(title = "Neuroimaging research community by institution",
       subtitle = sprintf("Articles in %d core neuroimaging journals, %d-%d (OpenAlex), %s institutions",
                          length(community_journals), min(community_years), max(community_years),
                          format(nrow(community), big.mark = ","))) +
  theme_void() +
  theme(plot.title = element_text(face = "bold"),
        plot.background = element_rect(fill = "white", color = NA))
ggsave("output/figures/community_map.png", width = 14, height = 7, dpi = 200)

# Share of the community vs. share of attendees, per subregion
community_vs_attendees %>%
  filter(community_share >= 0.002 | attendee_share >= 0.002) %>%
  mutate(subregion = fct_reorder(subregion, community_share)) %>%
  pivot_longer(c(community_share, attendee_share), names_to = "measure", values_to = "share") %>%
  mutate(measure = recode(measure, community_share = "Share of neuroimaging articles",
                          attendee_share = "Share of OHBM attendees 2023-2025")) %>%
  ggplot(aes(share, subregion)) +
  geom_line(aes(group = subregion), color = "gray70", linewidth = 0.8) +
  geom_point(aes(color = measure), size = 3) +
  scale_x_continuous(labels = scales::percent) +
  scale_color_manual(values = c("Share of neuroimaging articles" = "#2171b5",
                                "Share of OHBM attendees 2023-2025" = "#d94801"),
                     name = NULL) +
  labs(title = "Where the community is vs. who comes to OHBM",
       subtitle = "UN subregions with at least 0.2% of either", x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = "top", plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())
ggsave("output/figures/community_vs_attendees_by_region.png", width = 9, height = 6, dpi = 200)

# ============================================================
# --- Exploration (not part of the saved pipeline outputs) ---
# ============================================================

community %>%
  slice_max(n_works, n = 15) %>%
  select(institution, city, country_code, n_works) %>%
  knitr::kable(caption = "Top 15 institutions by neuroimaging articles") %>%
  print()

community_vs_attendees %>%
  select(subregion, community_works, attendees_2023_2025, community_share, attendee_share,
         representation_ratio) %>%
  knitr::kable(digits = 3, caption = "Community vs. attendees by UN subregion") %>%
  print()

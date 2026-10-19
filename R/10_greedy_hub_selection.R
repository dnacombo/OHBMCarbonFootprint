# R/10_greedy_hub_selection.R
# Greedy search for the lowest-footprint set of conference hubs.
# Step 1: every candidate city above `min_population` is tried as a single host city.
# Step k: the k-1 hubs already chosen are kept, and every remaining candidate city is
#         tried as an additional hub (attendees go to their nearest hub).
# Run on all years pooled, then on each conference year separately. Greedy selection is not guaranteed to find the
# best possible k-hub set, but each step's choice is optimal given the earlier ones.

# --- 1. Load Libraries, Data and Parameters ---
library(tidyverse)
library(geosphere)
library(maps)
source("R/utils.R")

min_population <- 2e5
n_hubs <- 6

# Country names as spelled in maps::world.cities. Russia and Turkey are included;
# Russian cities east of the Urals are dropped below by `europe_max_lon`.
europe_countries <- c(
  "Albania", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria",
  "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France",
  "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Kosovo", "Latvia",
  "Lithuania", "Luxembourg", "Moldova", "Montenegro", "Netherlands", "North Macedonia",
  "Norway", "Poland", "Portugal", "Romania", "Russia", "Serbia", "Slovakia", "Slovenia",
  "Spain", "Sweden", "Switzerland", "Turkey", "UK", "Ukraine"
)
europe_max_lon <- 60

# Candidate pool for each step: element k restricts hub k to those countries.
# NULL means "all countries"; steps beyond the length of the list are world-wide.
# e.g. list(europe_countries) forces hub 1 to be in Europe.
hub_countries <- list(NULL)

geocoded_attendees <- readRDS("output/geocoded_attendee_data.rds")
if (any(is.na(geocoded_attendees$latitude))) {
  warning(sprintf("Some attendee locations (n = %d) could not be geocoded. These will be excluded.",
                  sum(is.na(geocoded_attendees$latitude))))
}
attendees <- geocoded_attendees %>% filter(!is.na(latitude) & !is.na(longitude))
n_attendees <- nrow(attendees)

# Candidate cities come with their own coordinates, so the geocoding cache is not used.
all_cities <- maps::world.cities %>%
  as_tibble() %>%
  filter(pop > min_population) %>%
  transmute(city = name, country = country.etc, population = pop,
            latitude = lat, longitude = long,
            label = paste0(city, " (", country, ")"))

# Indices into `all_cities` of the candidates allowed at step k
candidate_idx <- function(k) {
  countries <- if (k <= length(hub_countries)) hub_countries[[k]] else NULL
  if (is.null(countries)) return(seq_len(nrow(all_cities)))
  which(all_cities$country %in% countries &
          !(all_cities$country == "Russia" & all_cities$longitude > europe_max_lon))
}

# --- 2. Distances (attendees x cities, km) ---
dist_km <- distm(attendees[, c("longitude", "latitude")],
                 all_cities[, c("longitude", "latitude")],
                 fun = distHaversine) / 1000

# Total footprint (tCO2e) given each attendee's distance to their assigned hub
total_footprint_t <- function(distance_km) sum(calculate_footprint(distance_km)) / 1000

# --- 3. Greedy Hub Selection ---
# Runs the greedy search on the attendees selected by `rows` (logical vector over `attendees`)
greedy_hubs <- function(rows) {
  d <- dist_km[rows, , drop = FALSE]
  n <- nrow(d)
  chosen_idx <- integer()
  d_current <- rep(Inf, n) # distance from each attendee to their nearest chosen hub
  previous_total <- NA_real_
  step_results <- list()

  for (k in seq_len(n_hubs)) {
    idx <- setdiff(candidate_idx(k), chosen_idx)

    results <- all_cities[idx, ] %>%
      mutate(
        city_idx = idx,
        step = k,
        previous_hubs = paste(all_cities$label[chosen_idx], collapse = "; "),
        total_footprint_tonCO2e = map_dbl(idx, \(j) total_footprint_t(pmin(d_current, d[, j]))),
        footprint_per_attendee_tonCO2e = total_footprint_tonCO2e / n,
        reduction_vs_previous_pct = 100 * (1 - total_footprint_tonCO2e / previous_total),
        rank = min_rank(total_footprint_tonCO2e)
      ) %>%
      arrange(rank)

    best_j <- results$city_idx[1]
    chosen_idx <- c(chosen_idx, best_j)
    d_current <- pmin(d_current, d[, best_j])
    previous_total <- results$total_footprint_tonCO2e[1]
    step_results[[k]] <- results
  }

  bind_rows(step_results) %>% mutate(n_attendees = n)
}

# Samples: all years pooled, then each conference year on its own
year_labels <- attendees %>%
  distinct(year, conference_city) %>%
  arrange(year) %>%
  mutate(sample = paste0(year, " (", conference_city, ")"))
samples <- c("All years", year_labels$sample)
sample_rows <- c(
  list("All years" = rep(TRUE, n_attendees)),
  setNames(lapply(year_labels$year, \(y) attendees$year == y), year_labels$sample)
)

candidates <- map_dfr(samples, \(s) {
  print(paste("Greedy selection for:", s))
  greedy_hubs(sample_rows[[s]]) %>% mutate(sample = s, .before = 1)
}) %>%
  mutate(sample = factor(sample, levels = samples))

hub_sequence <- candidates %>%
  filter(rank == 1) %>%
  group_by(sample, step) %>%
  slice(1) %>%
  ungroup() %>%
  select(sample, n_attendees, step, city, country, label, population, latitude, longitude,
         total_footprint_tonCO2e, footprint_per_attendee_tonCO2e, reduction_vs_previous_pct)

# --- 4. Save Results ---
file_tag <- function(s) if (s == "All years") "all_years" else str_extract(s, "^\\d{4}")
for (s in samples) {
  write.csv(candidates %>% filter(sample == s),
            sprintf("output/reports/greedy_hub_candidates_%s.csv", file_tag(s)), row.names = FALSE)
}
write.csv(hub_sequence, "output/reports/greedy_hub_sequence.csv", row.names = FALSE)

# --- 5. Maps ---
world_map <- map_data("world")
map_dir <- "output/figures/greedy_hub_maps"
dir.create(map_dir, showWarnings = FALSE, recursive = TRUE)

footprint_map <- function(results, best, previous, sample_attendees, xlim, ylim, title, subtitle) {
  label_offset <- (ylim[2] - ylim[1]) / 25
  ggplot() +
    geom_polygon(data = world_map, aes(long, lat, group = group),
                 fill = "gray92", color = "white", linewidth = 0.2) +
    geom_point(data = sample_attendees, aes(longitude, latitude),
               color = "gray55", size = 0.4, alpha = 0.3) +
    geom_point(data = arrange(results, desc(footprint_per_attendee_tonCO2e)),
               aes(longitude, latitude, fill = footprint_per_attendee_tonCO2e),
               shape = 21, color = "gray30", stroke = 0.2, size = 2.5) +
    geom_point(data = best, aes(longitude, latitude),
               shape = 21, fill = NA, color = "black", stroke = 1.2, size = 6) +
    geom_label(data = best, aes(longitude, latitude, label = city),
               nudge_y = label_offset, size = 3.5, label.size = 0.2) +
    geom_point(data = previous, aes(longitude, latitude),
               shape = 23, fill = "white", color = "black", stroke = 1, size = 4) +
    geom_label(data = previous, aes(longitude, latitude, label = paste0("Hub ", step, ": ", city)),
               nudge_y = -label_offset, size = 3.5, label.size = 0.2) +
    scale_fill_gradient(low = "#deebf7", high = "#08306b",
                        name = "tCO2e per\nattendee") +
    coord_quickmap(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
    theme_void() +
    theme(plot.title = element_text(face = "bold"),
          plot.background = element_rect(fill = "white", color = NA),
          legend.position = "right")
}

for (s in samples) {
  sample_attendees <- attendees[sample_rows[[s]], ]
  sample_sequence <- hub_sequence %>% filter(sample == s)
  attendee_desc <- if (s == "All years") "all 2023-2025 attendees" else paste(str_remove_all(s, "[()]"), "attendees")

  for (k in seq_len(n_hubs)) {
    results <- candidates %>% filter(sample == s, step == k)
    best <- slice(results, 1)
    previous <- sample_sequence %>% filter(step < k)

    # World extent for world-wide steps, otherwise zoom on the candidate cities
    is_world <- length(candidate_idx(k)) == nrow(all_cities)
    xlim <- if (is_world) c(-180, 180) else range(results$longitude) + c(-5, 5)
    ylim <- if (is_world) c(-56, 75) else range(results$latitude) + c(-3, 3)

    title <- if (k == 1) {
      sprintf("Travel footprint if OHBM (%s) met in a single city", attendee_desc)
    } else {
      sprintf("Travel footprint (%s) with %s plus hub %d", attendee_desc,
              paste(previous$city, collapse = ", "), k)
    }
    subtitle <- sprintf(
      "Cities > %s inhabitants. Best: %s, %.2f tCO2e per attendee (%s t total)%s",
      format(min_population, big.mark = ",", scientific = FALSE),
      best$label, best$footprint_per_attendee_tonCO2e,
      format(round(best$total_footprint_tonCO2e), big.mark = ","),
      if (k == 1) "" else sprintf(", -%.0f%% vs. %d hub%s", best$reduction_vs_previous_pct,
                                  k - 1, if (k == 2) "" else "s")
    )

    footprint_map(results, best, previous, sample_attendees, xlim, ylim, title, subtitle)
    ggsave(file.path(map_dir, sprintf("greedy_%s_hub%d_map.png", file_tag(s), k)),
           width = if (is_world) 14 else 10, height = if (is_world) 7 else 8, dpi = 200)
  }
}

# Footprint per attendee as hubs are added, one panel per sample
ggplot(hub_sequence, aes(step, footprint_per_attendee_tonCO2e)) +
  geom_line(color = "#08306b", linewidth = 0.7) +
  geom_point(color = "#08306b", size = 3) +
  geom_text(aes(label = city), vjust = -1.2, size = 3) +
  facet_wrap(~sample, nrow = 1) +
  scale_x_continuous(breaks = seq_len(n_hubs), expand = expansion(add = 0.7)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Travel footprint per attendee as hubs are added (greedy selection)",
       x = "Number of hubs", y = "tCO2e per attendee") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 11))
ggsave("output/figures/greedy_hub_sequence.png", width = 16, height = 5.5, dpi = 200)

# ============================================================
# --- Exploration (not part of the saved pipeline outputs) ---
# ============================================================

# Hubs chosen at each step, one column per sample
hub_sequence %>%
  select(sample, step, city) %>%
  pivot_wider(names_from = sample, values_from = city) %>%
  knitr::kable(caption = "Greedy hub sequence by sample") %>%
  print()

hub_sequence %>%
  select(sample, step, footprint_per_attendee_tonCO2e) %>%
  pivot_wider(names_from = sample, values_from = footprint_per_attendee_tonCO2e) %>%
  knitr::kable(digits = 3, caption = "tCO2e per attendee by sample") %>%
  print()

for (s in samples) {
  candidates %>%
    filter(sample == s, step == 1) %>%
    slice_head(n = 10) %>%
    select(rank, label, population, total_footprint_tonCO2e, footprint_per_attendee_tonCO2e) %>%
    knitr::kable(digits = 3, caption = paste("Top 10 single hubs -", s)) %>%
    print()
}

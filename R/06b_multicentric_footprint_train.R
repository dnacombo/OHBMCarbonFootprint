# R/06b_multicentric_footprint_train.R
# Train-share sensitivity of the multicentric footprint (version of 06_ with a train/plane split),
# and comparison with the monocentric version from 05b_. Attendees are still assigned to the
# nearest hub by distance; rail-eligible trips are then made by train with probability train_share.

library(tidyverse)
library(geosphere)
source("R/utils.R")

geocoded_attendees <- readRDS("output/geocoded_attendee_data.rds")

# --- 1. Hubs and Nearest-Hub Assignment ---
hub_coords <- get_hub_coordinates(c("Glasgow", "Montreal", "Seoul", "Brisbane"))
hub_countries <- hub_coords %>%
  transmute(hub_city, hub_iso3 = attendee_country(longitude, latitude))

attendees <- calculate_multicentric_footprint(geocoded_attendees, hub_coords) %>%
  rename(footprint_kgCO2e_plane_only = footprint_kgCO2e) %>%
  left_join(hub_countries, by = "hub_city") %>%
  mutate(origin_iso3 = attendee_country(longitude, latitude, country_clean))

# --- 2. Footprint over the Sensitivity Grid ---
rail_factors <- get_rail_emission_factors()

grid_footprint <- attendees %>%
  select(ID, year, hub_city, origin_iso3, hub_iso3, distance_km) %>%
  crossing(train_sensitivity_grid) %>%
  mutate(
    rail_eligible = rail_eligible(distance_km, origin_iso3, hub_iso3, max_train_km),
    footprint_kg_co2e = calculate_footprint_train(distance_km, origin_iso3, hub_iso3,
                                                  train_share, max_train_km, ef_scenario,
                                                  rail_factors = rail_factors)
  )

summarise_grid <- function(df, ...) {
  df %>%
    group_by(..., train_share, max_train_km, ef_scenario) %>%
    summarise(
      n_attendees = n(),
      n_rail_eligible = sum(rail_eligible),
      total_footprint_tonCO2e = sum(footprint_kg_co2e) / 1000,
      per_attendee_ton_co2e = mean(footprint_kg_co2e) / 1000,
      .groups = "drop"
    ) %>%
    group_by(...) %>%
    mutate(reduction_vs_plane_only_pct =
             100 * (1 - total_footprint_tonCO2e / total_footprint_tonCO2e[train_share == 0][1])) %>%
    ungroup() %>%
    arrange(..., max_train_km, ef_scenario, train_share)
}

sensitivity_summary <- summarise_grid(grid_footprint, year)
sensitivity_by_hub <- summarise_grid(grid_footprint, year, hub_city)

# --- 3. Save Results ---
central <- train_central_scenario
detailed <- attendees %>%
  mutate(
    rail_eligible = rail_eligible(distance_km, origin_iso3, hub_iso3, central$max_train_km),
    footprint_kgCO2e = calculate_footprint_train(distance_km, origin_iso3, hub_iso3,
                                                 central$train_share, central$max_train_km,
                                                 central$ef_scenario, rail_factors = rail_factors)
  )
saveRDS(detailed, file = "output/multicentric_footprint_train_detailed.rds")
print("Detailed footprint (central train scenario) saved to output/multicentric_footprint_train_detailed.rds")

write.csv(sensitivity_summary, file = "output/reports/multicentric_train_sensitivity.csv", row.names = FALSE)
write.csv(sensitivity_by_hub, file = "output/reports/multicentric_train_sensitivity_by_hub.csv", row.names = FALSE)
print("Sensitivity summaries saved to output/reports/multicentric_train_sensitivity{,_by_hub}.csv")

p <- sensitivity_summary %>%
  plot_train_sensitivity(year, "Multicentric (4 hubs): per-attendee travel emissions vs train share")
ggsave("output/figures/multicentric_train_sensitivity.png", p, width = 11, height = 5, dpi = 150, bg = "white")

# --- 4. Comparison with Monocentric (05b_) ---
mono_file <- "output/reports/monocentric_train_sensitivity.csv"
if (!file.exists(mono_file)) stop("Run R/05b_monocentric_footprint_train.R first.")
mono <- read_csv(mono_file, show_col_types = FALSE) %>%
  mutate(year = factor(year), ef_scenario = factor(ef_scenario, levels = levels(sensitivity_summary$ef_scenario)))

comparison <- sensitivity_summary %>%
  mutate(year = factor(year)) %>%
  inner_join(mono, by = c("year", "train_share", "max_train_km", "ef_scenario"),
             suffix = c("_multicentric", "_monocentric")) %>%
  transmute(year, conference_city, train_share, max_train_km, ef_scenario,
            total_footprint_tonCO2e_monocentric, total_footprint_tonCO2e_multicentric,
            multi_to_mono_ratio = total_footprint_tonCO2e_multicentric / total_footprint_tonCO2e_monocentric,
            savings_pct = 100 * (1 - multi_to_mono_ratio))
write.csv(comparison, file = "output/reports/multi_vs_mono_train_sensitivity.csv", row.names = FALSE)
print("Comparison saved to output/reports/multi_vs_mono_train_sensitivity.csv")

p_cmp <- comparison %>%
  mutate(year = paste0(year, " (mono: ", conference_city, ")"),
         per_attendee_ton_co2e = multi_to_mono_ratio) %>%
  plot_train_sensitivity(year, "Multicentric / monocentric total emissions vs train share") +
  labs(y = "Multicentric / monocentric emissions",
       subtitle = "Same train scenario applied to both formats. Lines: central rail emission factor; bands: low-high. Y-axes differ")
ggsave("output/figures/multi_vs_mono_train_sensitivity.png", p_cmp, width = 11, height = 5, dpi = 150, bg = "white")

print("--- Multicentric train sensitivity: % reduction vs plane-only (central emission factor) ---")
print(knitr::kable(
  sensitivity_summary %>%
    filter(ef_scenario == "central", train_share > 0) %>%
    select(year, max_train_km, n_rail_eligible, train_share, reduction_vs_plane_only_pct) %>%
    pivot_wider(names_from = train_share, values_from = reduction_vs_plane_only_pct, names_prefix = "share_"),
  digits = 1))
print("--- Multicentric savings vs monocentric, % (central emission factor) ---")
print(knitr::kable(
  comparison %>%
    filter(ef_scenario == "central") %>%
    select(year, conference_city, max_train_km, train_share, savings_pct) %>%
    pivot_wider(names_from = train_share, values_from = savings_pct, names_prefix = "share_"),
  digits = 1))

# --- Exploration (not part of the saved pipeline outputs) ---

# Reduction by hub at the central scenario
print(knitr::kable(
  sensitivity_by_hub %>%
    filter(train_share == central$train_share, max_train_km == central$max_train_km,
           ef_scenario == central$ef_scenario) %>%
    select(year, hub_city, n_attendees, n_rail_eligible, per_attendee_ton_co2e, reduction_vs_plane_only_pct),
  digits = 3, caption = "Central train scenario, by hub"))

# Rail-eligible attendees by hub and origin country (max 1500 km, pooled years)
print(knitr::kable(
  attendees %>%
    filter(rail_eligible(distance_km, origin_iso3, hub_iso3, 1500)) %>%
    count(hub_city, origin_iso3, sort = TRUE),
  caption = "Rail-eligible attendees (<= 1500 km), all years"))

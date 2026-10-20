# R/05b_monocentric_footprint_train.R
# Train-share sensitivity of the monocentric footprint (version of 05_ with a train/plane split).
# Rail-eligible trips (non-local, <= max_train_km, same land-connected rail zone) are made by
# train with probability train_share, using country-specific rail emission factors
# (data/rail_emission_factors.csv). Sweeps train_sensitivity_grid from utils.R.

library(tidyverse)
library(geosphere)
source("R/utils.R")

# --- 1. Load Geocoded Data ---
geocoded_data_file <- "output/geocoded_attendee_data.rds"
if (!file.exists(geocoded_data_file)) {
  stop("Geocoded data file not found. Please run script 03_geocode.R first.")
}
attendee_data <- readRDS(geocoded_data_file)

if (any(is.na(attendee_data$latitude))) {
  warning(sprintf("Some attendee locations (n = %d) could not be geocoded. These will be excluded from the footprint calculation.", sum(is.na(attendee_data$latitude))))
  attendee_data <- attendee_data %>% filter(!is.na(latitude))
}

# --- 2. Conference Locations and Countries ---
hub_cities <- c("Glasgow", "Montreal", "Seoul", "Brisbane")
conference_hubs <- get_hub_coordinates(hub_cities) %>%
  mutate(hub_iso3 = attendee_country(longitude, latitude)) %>%
  rename(conference_city = hub_city, conference_lat = latitude, conference_lon = longitude)

# --- 3. Distances and Origin Countries ---
footprint_data <- attendee_data %>%
  left_join(conference_hubs, by = "conference_city")

footprint_data$travel_distance_km <- distHaversine(
  p1 = footprint_data[, c("longitude", "latitude")],
  p2 = footprint_data[, c("conference_lon", "conference_lat")]
) / 1000

footprint_data <- footprint_data %>%
  mutate(origin_iso3 = attendee_country(longitude, latitude, country_clean),
         rail_zone_origin = rail_zone(origin_iso3),
         rail_zone_hub = rail_zone(hub_iso3))

# --- 4. Footprint over the Sensitivity Grid ---
rail_factors <- get_rail_emission_factors()

grid_footprint <- footprint_data %>%
  select(ID, year, conference_city, origin_iso3, hub_iso3, travel_distance_km) %>%
  crossing(train_sensitivity_grid) %>%
  mutate(
    rail_eligible = rail_eligible(travel_distance_km, origin_iso3, hub_iso3, max_train_km),
    footprint_kg_co2e = calculate_footprint_train(travel_distance_km, origin_iso3, hub_iso3,
                                                  train_share, max_train_km, ef_scenario,
                                                  rail_factors = rail_factors)
  )

sensitivity_summary <- grid_footprint %>%
  group_by(year, conference_city, train_share, max_train_km, ef_scenario) %>%
  summarise(
    n_attendees = n(),
    n_rail_eligible = sum(rail_eligible),
    total_footprint_tonCO2e = sum(footprint_kg_co2e) / 1000,
    per_attendee_ton_co2e = mean(footprint_kg_co2e) / 1000,
    .groups = "drop"
  ) %>%
  group_by(year, conference_city) %>%
  mutate(reduction_vs_plane_only_pct =
           100 * (1 - total_footprint_tonCO2e / total_footprint_tonCO2e[train_share == 0][1])) %>%
  ungroup() %>%
  arrange(year, max_train_km, ef_scenario, train_share)

# --- 5. Save Results ---
central <- train_central_scenario
detailed <- footprint_data %>%
  mutate(
    rail_eligible = rail_eligible(travel_distance_km, origin_iso3, hub_iso3, central$max_train_km),
    carbon_footprint_kg_co2e_plane_only = calculate_footprint(travel_distance_km),
    carbon_footprint_kg_co2e = calculate_footprint_train(travel_distance_km, origin_iso3, hub_iso3,
                                                         central$train_share, central$max_train_km,
                                                         central$ef_scenario, rail_factors = rail_factors)
  )
saveRDS(detailed, file = "output/monocentric_footprint_train_detailed.rds")
print("Detailed footprint (central train scenario) saved to output/monocentric_footprint_train_detailed.rds")

summary_file <- "output/reports/monocentric_train_sensitivity.csv"
write.csv(sensitivity_summary, file = summary_file, row.names = FALSE)
print(paste("Sensitivity summary saved to", summary_file))

p <- sensitivity_summary %>%
  arrange(year) %>%
  mutate(conference_city = fct_inorder(paste0(conference_city, " (", year, ")"))) %>%
  plot_train_sensitivity(conference_city, "Monocentric: per-attendee travel emissions vs train share")
ggsave("output/figures/monocentric_train_sensitivity.png", p, width = 11, height = 5, dpi = 150, bg = "white")

print("--- Monocentric train sensitivity: % reduction vs plane-only (central emission factor) ---")
print(knitr::kable(
  sensitivity_summary %>%
    filter(ef_scenario == "central", train_share > 0) %>%
    select(year, conference_city, max_train_km, n_rail_eligible, train_share, reduction_vs_plane_only_pct) %>%
    pivot_wider(names_from = train_share, values_from = reduction_vs_plane_only_pct, names_prefix = "share_"),
  digits = 1))

# --- Exploration (not part of the saved pipeline outputs) ---

# Who is rail-eligible at each cut-off, by origin country and conference city
eligible_by_country <- footprint_data %>%
  crossing(max_train_km = unique(train_sensitivity_grid$max_train_km)) %>%
  filter(rail_eligible(travel_distance_km, origin_iso3, hub_iso3, max_train_km)) %>%
  count(conference_city, origin_iso3, max_train_km) %>%
  pivot_wider(names_from = max_train_km, values_from = n, values_fill = 0, names_prefix = "le_") %>%
  arrange(conference_city, desc(across(last_col())))
print(knitr::kable(eligible_by_country, caption = "Rail-eligible attendees by origin country and max train distance (km)"))

# Short non-eligible trips (e.g. across water): what is left to planes
print(knitr::kable(
  footprint_data %>%
    filter(travel_distance_km >= 100, travel_distance_km <= 1500,
           !rail_eligible(travel_distance_km, origin_iso3, hub_iso3, 1500)) %>%
    count(conference_city, origin_iso3, sort = TRUE),
  caption = "Trips of 100-1500 km that cannot go by rail"))

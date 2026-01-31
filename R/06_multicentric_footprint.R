# --- Multicentric Conference Footprint Calculation ---
# --- 1. Load Libraries and Data ---
library(tidyverse)
library(geosphere) # For distance calculations
library(tidygeocoder)
source("R/utils.R")

# Load geocoded attendee data
geocoded_attendees <- readRDS("output/geocoded_attendee_data.rds")

# --- 2. Define Hubs and Geocode Them ---
hub_cities <- tibble(
  full_address = c("Montreal", "Seoul", "Brisbane")
)

# Get hub coordinates from the cache
hub_coords <- get_hub_coordinates(hub_cities$full_address)

print("Hub locations retrieved:")
print(hub_coords)

# --- 3. Calculate Distances and Find Closest Hub ---
# Process data for each attendee to find the closest hub
# We can nest the hub distances and then unnest the closest one
attendees_with_closest_hub <- geocoded_attendees %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>% # Exclude attendees with no location
  rowwise() %>%
  mutate(
    hub_distances = list(calculate_hub_distances(latitude, longitude, hub_coords))
  ) %>%
  unnest(hub_distances) %>%
  group_by(ID, year) %>%
  slice_min(order_by = distance_km, n = 1, with_ties = FALSE) %>%
  ungroup()

# --- 4. Calculate Carbon Footprint ---
# Use the calculate_footprint function from utils.R
multicentric_footprint <- attendees_with_closest_hub %>%
  mutate(
    footprint_kgCO2e = calculate_footprint(distance_km)
  )

# --- 5. Save Detailed and Summary Results ---
# Save the detailed data
saveRDS(multicentric_footprint, file = "output/multicentric_footprint_detailed.rds")
print("Detailed multicentric footprint data saved to output/multicentric_footprint_detailed.rds")

# Create and save a summary
multicentric_summary <- multicentric_footprint %>%
  group_by(year) %>%
  summarise(
    total_attendees = n(),
    total_footprint_tonCO2e = sum(footprint_kgCO2e, na.rm = TRUE) / 1000
  ) %>%
  arrange(year)

write.csv(multicentric_summary, file = "output/reports/multicentric_footprint_summary.csv", row.names = FALSE)
print("Summary of multicentric footprint saved to output/reports/multicentric_footprint_summary.csv")

print("Multicentric scenario analysis complete.")
print(multicentric_summary)

# Compare with monocentric results
monocentric_summary <- read_csv("output/reports/monocentric_footprint_summary.csv",col_types = 'fid')
comparison <- multicentric_summary %>%
  full_join(monocentric_summary, by = c("year", 'total_attendees'), suffix = c("_multicentric", "_monocentric")) %>%
  select(year, total_attendees, total_footprint_tonCO2e_multicentric, conference_city, total_footprint_tonCO2e_monocentric)

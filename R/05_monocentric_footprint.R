# R/04_monocentric_footprint.R
# This script calculates the carbon footprint for the past three monocentric conferences.

library(tidyverse)
library(geosphere) # For distance calculations
library(tidygeocoder) # For geocoding the hubs
source("R/utils.R")

# --- 1. Load Geocoded Data ---
geocoded_data_file <- "output/geocoded_attendee_data.rds"
if (!file.exists(geocoded_data_file)) {
  stop("Geocoded data file not found. Please run script 03_geocode.R first.")
}
attendee_data <- readRDS(geocoded_data_file)

# --- 2. Define Conference Locations and Emission Factors ---

# Define the required conference hubs as a tibble
hub_cities <- tibble(full_address = c("Montreal", "Seoul", "Brisbane"))

# Now, retrieve the hub coordinates from the up-to-date cache for our calculation
conference_hubs <- get_hub_coordinates(hub_cities$full_address) %>%
  rename(conference_city = hub_city, conference_lat = latitude, conference_lon = longitude)

# --- 3. Calculate Distances and Footprint ---

# First, ensure the attendee data has coordinates. Stop if any are missing.
if (any(is.na(attendee_data$latitude))) {
  warning("Some attendee locations could not be geocoded. These will be excluded from the footprint calculation.")
  attendee_data <- attendee_data %>% filter(!is.na(latitude))
}

# Join the hub coordinates to the attendee data
footprint_data <- attendee_data %>%
  left_join(conference_hubs, by = "conference_city")

# Calculate the distance from each attendee to their conference hub
# distHaversine returns distance in meters
footprint_data$travel_distance_m <- distHaversine(
  p1 = footprint_data[, c("longitude", "latitude")],
  p2 = footprint_data[, c("conference_lon", "conference_lat")]
)

# Calculate the carbon footprint based on the distance
footprint_data <- footprint_data %>%
  mutate(
    travel_distance_km = travel_distance_m / 1000,
    carbon_footprint_kg_co2e = calculate_footprint(travel_distance_km)
  )

# --- 4. Save Detailed Results ---
detailed_results_file <- "output/monocentric_footprint_detailed.rds"
saveRDS(footprint_data, file = detailed_results_file)

print(paste("Detailed footprint calculations saved to", detailed_results_file))

# --- 5. Aggregate and Summarize Results ---

summary_report <- footprint_data %>%
  group_by(year, conference_city) %>%
  summarise(
    total_attendees = n(),
    total_footprint_tonCO2e = sum(carbon_footprint_kg_co2e, na.rm = TRUE) / 1000,
    .groups = 'drop'
  ) %>%
  arrange(year)

print("--- Monocentric Footprint Summary ---")
print(summary_report)

# Save the summary report to a CSV file
summary_file <- "output/reports/monocentric_footprint_summary.csv"
write.csv(summary_report, file = summary_file, row.names = FALSE)

print(paste("Summary report saved to", summary_file))

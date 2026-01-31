# R/03_geocode.R
# This script takes the cleaned data and performs geocoding, using a cache to avoid re-querying.

library(tidyverse)
library(tidygeocoder)
source("R/utils.R") # Load utility functions

# --- 1. Define File Paths and Load Data ---
cleaned_data_file <- "output/cleaned_attendee_data.rds"
geocoded_output_file <- "output/geocoded_attendee_data.rds"

cleaned_attendees <- readRDS(cleaned_data_file)

# --- 2. Implement Caching Logic ---
# Load cache
geocoding_cache <- get_geocoding_cache()

# Identify unique addresses from the source data
unique_addresses <- cleaned_attendees %>% distinct(full_address)

# Determine which addresses are new and need to be geocoded
new_addresses_to_geocode <- unique_addresses %>%
  anti_join(geocoding_cache, by = "full_address")

# --- 3. Geocode Only New Addresses ---
if (nrow(new_addresses_to_geocode) > 0) {
  print(paste("Found", nrow(new_addresses_to_geocode), "new addresses to geocode..."))
  
  # Geocode only the new addresses
  newly_geocoded <- new_addresses_to_geocode %>%
    geocode(address = full_address, method = 'osm', lat = latitude, long = longitude)
  
  print("Geocoding of new addresses complete.")

  # remove NA entries if any
  newly_geocoded <- newly_geocoded %>% filter(!is.na(latitude) & !is.na(longitude))

  # Add the newly geocoded results to the cache and save
  update_geocoding_cache(newly_geocoded)
  
} else {
  print("No new addresses to geocode. All locations found in cache.")
}

# --- 4. Join and Save Final Data ---
# reload cache to make sure it's up to date
geocoding_cache <- get_geocoding_cache()
# Join the full cache with the attendee data
final_geocoded_data <- cleaned_attendees %>%
  left_join(geocoding_cache, by = "full_address") %>%
  mutate(year = as.factor(year))

saveRDS(final_geocoded_data, file = geocoded_output_file)

# --- 5. Summarize ---
print(paste("Final geocoded data saved to", geocoded_output_file))
print(paste("Number of attendees with no geocoded location:", sum(is.na(final_geocoded_data$latitude))))

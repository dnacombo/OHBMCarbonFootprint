# R/utils.R
# This file contains utility functions and project-wide configuration.

# --- Project-wide Configuration ---
geocoding_cache_path <- "output/geocoding_cache.rds"


#' Calculate Carbon Footprint
#'
#' This function calculates the carbon footprint for a given travel distance.
#' It assumes a zero footprint for local travel below a certain threshold.
#'
#' @param distance_km A numeric vector of travel distances in kilometers.
#' @param emission_factor_kg_co2e_per_km The emission factor in kg CO2e per km for a round trip.
#' @param local_travel_threshold_km The distance in km below which travel is considered local (zero footprint).
#'
#' @return A numeric vector of the calculated carbon footprint in kg CO2e.
calculate_footprint <- function(distance_km, 
                                emission_factor_kg_co2e_per_km = 0.15 * 2, 
                                local_travel_threshold_km = 100) {
  
  # Calculate footprint
  footprint <- if_else(
    distance_km <= local_travel_threshold_km,
    0,
    distance_km * emission_factor_kg_co2e_per_km
  )
  
  return(footprint)
}

#' Get Geocoding Cache
#'
#' This function loads the main project geocoding cache. If the cache file
#' doesn't exist, it initializes an empty cache.
#'
#' @return A tibble representing the geocoding cache.
get_geocoding_cache <- function() {
  if (file.exists(geocoding_cache_path)) {
    geocoding_cache <- readRDS(geocoding_cache_path)
  } else {
    geocoding_cache <- tibble(full_address = character(), latitude = double(), longitude = double())
  }
  return(geocoding_cache)
}

#' Update Geocoding Cache
#'
#' This function updates the main project geocoding cache with new data and
#' saves it to the cache file.
#'
#' @param new_data A tibble of newly geocoded data to add to the cache.
update_geocoding_cache <- function(new_data) {
  if (nrow(new_data) > 0) {
    geocoding_cache <- get_geocoding_cache()
    geocoding_cache <- bind_rows(geocoding_cache, new_data)
    saveRDS(geocoding_cache, file = geocoding_cache_path)
    print("Main geocoding cache updated.")
  }
}

#' Get Hub Coordinates
#'
#' This function takes a vector of hub location names, geocodes them using the
#' main project cache, and returns a tibble with their coordinates.
#'
#' @param hub_locations A character vector of location names to be used as hubs.
#' @return A tibble with columns `hub_city`, `hub_lat`, and `hub_lon`.
get_hub_coordinates <- function(hub_locations) {
  
  hub_cities <- tibble(full_address = hub_locations)
  
  # Use the main geocoding cache
  geocoding_cache <- get_geocoding_cache()
  
  # Geocode hubs if they are not in the cache
  hubs_to_geocode <- hub_cities %>%
    anti_join(geocoding_cache, by = "full_address")
  
  if (nrow(hubs_to_geocode) > 0) {
    print(paste("Geocoding", nrow(hubs_to_geocode), "missing conference hubs..."))
    newly_geocoded_hubs <- hubs_to_geocode %>%
      geocode(address = full_address, method = 'osm', lat = latitude, long = longitude)
    
    update_geocoding_cache(newly_geocoded_hubs)
    geocoding_cache <- get_geocoding_cache() # Re-load the cache
    print("Main geocoding cache updated with hub locations.")
  }
  
  # Get hub coordinates from the cache
  hub_coords <- geocoding_cache %>%
    filter(full_address %in% hub_cities$full_address) %>%
    rename(hub_city = full_address, latitude = latitude, longitude = longitude)
  
  return(hub_coords)
}

#' Calculate Distances to Hubs
#'
#' Internal function to calculate distances from one attendee to all hubs.
#'
#' @param attendee_lat Latitude of the attendee.
#' @param attendee_lon Longitude of the attendee.
#' @param all_hubs A data frame of hub locations with coordinates.
#' @return A tibble with distances to each hub.
calculate_hub_distances <- function(attendee_lat, attendee_lon, all_hubs) {
  distances <- tibble()
  for (i in 1:nrow(all_hubs)) {
    dist <- distHaversine(
      c(attendee_lon, attendee_lat),
      c(all_hubs$longitude[i], all_hubs$latitude[i])
    ) / 1000 # Convert to km
    
    distances <- bind_rows(
      distances,
      tibble(hub_city = all_hubs$hub_city[i], distance_km = dist)
    )
  }
  return(distances)
}

#' Calculate Multicentric Footprint
#'
#' Calculates the carbon footprint for a multicentric conference scenario.
#'
#' @param attendees_df A data frame of attendees with geocoded locations.
#' @param hubs_df A data frame of hub locations with coordinates.
#' @return A data frame with the detailed footprint calculation for the scenario.
calculate_multicentric_footprint <- function(attendees_df, hubs_df) {
  
  # Find the closest hub for each attendee
  attendees_with_closest_hub <- attendees_df %>%
    filter(!is.na(latitude) & !is.na(longitude)) %>%
    rowwise() %>%
    mutate(
      hub_distances = list(calculate_hub_distances(latitude, longitude, hubs_df))
    ) %>%
    unnest(hub_distances) %>%
    group_by(ID, year) %>%
    slice_min(order_by = distance_km, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  # Calculate the carbon footprint
  footprint_details <- attendees_with_closest_hub %>%
    mutate(
      footprint_kgCO2e = calculate_footprint(distance_km)
    )
  
  return(footprint_details)
}

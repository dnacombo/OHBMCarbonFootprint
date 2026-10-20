# R/utils.R
# This file contains utility functions and project-wide configuration.

library(tidyverse)
library(geosphere)
library(maps)

# --- Project-wide Configuration ---
geocoding_cache_path <- "output/geocoding_cache.rds"
radius_km <- 4500
# Define conference hubs as a tibble
conference_hubs <- tibble(
  city = c("Montreal", "Seoul", "Brisbane"),
  year = c(2023, 2024, 2025)
)
# add Glasgow in 2022
conference_hubs <- conference_hubs %>%
  add_row(city = "Glasgow", year = 2022)


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
                                emission_factor_short_haul = 0.2586 * 2, 
                                emission_factor_medium_haul = 0.1875 * 2, 
                                emission_factor_long_haul = 0.152 * 2, 
                                local_travel_threshold_km = 100,
                                short_to_medium_haul_threshold_km = 1000,
                                medium_to_long_haul_threshold_km = 3500) {
  
  # Calculate footprint
  footprint <- case_when(
    distance_km < local_travel_threshold_km ~ 0,
    distance_km < short_to_medium_haul_threshold_km ~ distance_km * emission_factor_short_haul,
    distance_km < medium_to_long_haul_threshold_km ~ distance_km * emission_factor_medium_haul,
    TRUE ~ distance_km * emission_factor_long_haul
  )
  
  return(footprint)
}

# --- Train/plane split (used by 05b_ and 06b_) ---

# Sensitivity grid: share of rail-eligible trips made by train, maximum (great-circle)
# distance for which train is considered, and level of the rail emission factor.
train_sensitivity_grid <- crossing(
  train_share = c(0, 0.25, 0.5, 0.75, 1),
  max_train_km = c(500, 1000, 1500),
  ef_scenario = factor(c("low", "central", "high"), levels = c("low", "central", "high"))
)
# Default point reported in the detailed per-attendee outputs
train_central_scenario <- list(train_share = 0.5, max_train_km = 1000, ef_scenario = "central")

# Land-connected rail zones (ISO3 codes). Countries not listed form their own zone.
# Great Britain is linked to the continent through the Channel Tunnel; Ireland and
# Northern Ireland ("GBR_NI") are not.
rail_zones <- list(
  europe = c("ALB", "AUT", "BEL", "BGR", "BIH", "BLR", "CHE", "CZE", "DEU", "DNK", "ESP",
             "EST", "FIN", "FRA", "GBR", "GRC", "HRV", "HUN", "ITA", "LIE", "LTU", "LUX",
             "LVA", "MDA", "MKD", "MNE", "NLD", "NOR", "POL", "PRT", "ROU", "SRB", "SVK",
             "SVN", "SWE", "TUR", "UKR"),
  north_america = c("USA", "CAN"),
  china = c("CHN", "HKG", "MAC"),
  ireland = c("IRL", "GBR_NI")
)

#' Country (ISO3) of a location, from its coordinates
#'
#' Uses the `maps` world polygons, falling back to `countrycode` on a free-text
#' country name for points that fall outside the polygons (coasts, small territories).
#' Northern Ireland is coded "GBR_NI" so it can be kept apart from Great Britain.
attendee_country <- function(lon, lat, country_fallback = NA_character_) {
  where <- maps::map.where("world", lon, lat)
  iso <- suppressWarnings(countrycode::countrycode(sub(":.*", "", where), "country.name", "iso3c",
                                                   custom_match = c(UK = "GBR")))
  iso[!is.na(where) & where == "UK:Northern Ireland"] <- "GBR_NI"
  fallback <- suppressWarnings(countrycode::countrycode(rep_len(country_fallback, length(lon)),
                                                        "country.name", "iso3c"))
  coalesce(iso, fallback)
}

rail_zone <- function(iso3) {
  zone <- iso3
  for (z in names(rail_zones)) zone[iso3 %in% rail_zones[[z]]] <- z
  zone
}

get_rail_emission_factors <- function(path = "data/rail_emission_factors.csv") {
  read_csv(path, show_col_types = FALSE)
}

#' Is a trip rail-eligible? Non-local, within `max_train_km` and within one rail zone.
rail_eligible <- function(distance_km, origin_iso3, hub_iso3, max_train_km,
                          local_travel_threshold_km = 100) {
  !is.na(origin_iso3) & !is.na(hub_iso3) &
    distance_km >= local_travel_threshold_km & distance_km <= max_train_km &
    rail_zone(origin_iso3) == rail_zone(hub_iso3)
}

#' Expected round-trip footprint with a train/plane split
#'
#' Rail-eligible trips are made by train with probability `train_share` and flown
#' otherwise (expected value, no randomness); other trips use `calculate_footprint()`.
#' Train emissions use the origin country's factor from `data/rail_emission_factors.csv`
#' (column `ef_<ef_scenario>`), applied to the great-circle distance times
#' `rail_detour_factor`, round trip. All arguments except `rail_factors` are vectorised.
calculate_footprint_train <- function(distance_km, origin_iso3, hub_iso3, train_share,
                                      max_train_km, ef_scenario = "central",
                                      rail_detour_factor = 1.2,
                                      rail_factors = get_rail_emission_factors()) {
  plane <- calculate_footprint(distance_km)
  row <- match(sub("_NI$", "", origin_iso3), rail_factors$iso3)
  row[is.na(row)] <- match("default", rail_factors$iso3)
  ef_scenario <- rep_len(as.character(ef_scenario), length(distance_km))
  ef <- case_when(
    ef_scenario == "low" ~ rail_factors$ef_low[row],
    ef_scenario == "central" ~ rail_factors$ef_central[row],
    ef_scenario == "high" ~ rail_factors$ef_high[row]
  )
  train <- distance_km * rail_detour_factor * ef * 2
  if_else(rail_eligible(distance_km, origin_iso3, hub_iso3, max_train_km),
          train_share * train + (1 - train_share) * plane,
          plane)
}

#' Plot a train-share sensitivity summary: per-attendee footprint vs train share,
#' one line per max train distance (central EF), ribbon spanning low-high EF.
plot_train_sensitivity <- function(summary_df, facet_var, title) {
  wide <- summary_df %>%
    select({{ facet_var }}, train_share, max_train_km, ef_scenario, per_attendee_ton_co2e) %>%
    pivot_wider(names_from = ef_scenario, values_from = per_attendee_ton_co2e) %>%
    arrange(max_train_km) %>%
    mutate(max_train_km = fct_inorder(paste0("≤ ", scales::comma(max_train_km), " km")))
  ggplot(wide, aes(x = train_share, colour = max_train_km, fill = max_train_km)) +
    geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.15, colour = NA) +
    geom_line(aes(y = central), linewidth = 0.7) +
    geom_point(aes(y = central), size = 1.8) +
    facet_wrap(vars({{ facet_var }}), nrow = 1, scales = "free_y") +
    scale_x_continuous(labels = scales::percent) +
    # one hue, light -> dark for the ordered distance cut-off (lightest Blues steps skipped for contrast)
    scale_colour_manual(values = c("#6BAED6", "#2171B5", "#08306B"), aesthetics = c("colour", "fill")) +
    labs(title = title,
         subtitle = "Lines: central rail emission factor; bands: low-high factor range. Y-axes differ between panels",
         x = "Share of rail-eligible trips made by train",
         y = "Per-attendee emissions (t CO2e)",
         colour = "Max train distance", fill = "Max train distance") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
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
    # re-read just before writing, and write to a temp file then rename (atomic), so that
    # scripts run in parallel by run_all.R never leave a half-written or duplicated cache
    geocoding_cache <- get_geocoding_cache()
    geocoding_cache <- bind_rows(geocoding_cache, new_data) %>%
      distinct(full_address, .keep_all = TRUE)
    tmp_path <- tempfile(tmpdir = dirname(geocoding_cache_path), fileext = ".rds")
    saveRDS(geocoding_cache, file = tmp_path)
    file.rename(tmp_path, geocoding_cache_path)
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


#' Add Hub Distances
#'
#' Computes distances from each attendee to all conference hubs and identifies
#' which hubs are within a specified radius. Creates new columns for distance
#' to each hub and a boolean indicating proximity to each hub.
#'
#' @param geocoded_attendees A data frame of attendees with `latitude` and `longitude` columns.
#' @param conference_hubs A data frame of hubs with `city`, `latitude`, and `longitude` columns.
#' @param radius_km The radius in kilometers to determine hub proximity.
#'
#' @return A data frame with added columns: `distance_to_*` for each hub's distance,
#'   `within_*` for proximity indicators, and `closest_hub` for the nearest hub.

add_hub_distances <- function(geocoded_attendees, conference_hubs, radius_km) {
#for each year, compute distance to each possible hub in new columns
# also compute if within radius_km of each hub in new columns
for (i in 1:nrow(conference_hubs)) {
  hub <- conference_hubs[i, ]
  geocoded_attendees <- geocoded_attendees %>%
    rowwise() %>%
    mutate(
      !!paste0("distance_to_", hub$city) := distHaversine(
        c(longitude, latitude),
        c(hub$longitude, hub$latitude)
      ) / 1000, # Convert to km
      # add a column indicating if within radius 
      !!paste0("within_", hub$city) := ifelse(!!sym(paste0("distance_to_", hub$city)) <= radius_km, TRUE, FALSE))
}

# compute closest hub for each attendee
cols <- paste0("distance_to_", conference_hubs$city)
geocoded_attendees <- geocoded_attendees %>%
  mutate(
    closest_hub = apply(
      pick(all_of(cols)),
      1,
      \(row) conference_hubs$city[which.min(row)]
    )
  )
}


#' Distance-Decay Transform
#'
#' Transform of travel distance used by the participation model (12_) and the hub
#' search (13_): relative participation at distance d is exp(beta * distance_decay_g(d)).
#' See docs/methods_hub_selection.md, section M2.
#'
#' @param distance_km Numeric vector of distances in km.
#' @param form "power" for log(1 + d/1000) (participation ~ (1 + d/1000)^beta), or
#'   "exponential" for d/1000 (participation ~ exp(beta * d/1000)).
#' @return Numeric vector, 0 at d = 0.
distance_decay_g <- function(distance_km, form = c("power", "exponential")) {
  form <- match.arg(form)
  switch(form,
         power = log1p(distance_km / 1000),
         exponential = distance_km / 1000)
}

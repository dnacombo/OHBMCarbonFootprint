# R/08_attraction_power.R
# This script estimates the attraction power of conference cities.
# It compares the number of participants within a radius_km radius for each of the last three conference cities.

# --- 1. Load Libraries and Data ---
library(tidyverse)
library(geosphere)
library(maps)

# Load geocoded attendee data
geocoded_attendees <- readRDS("output/geocoded_attendee_data.rds")

# Load utility functions
source("R/utils.R")

# Define conference hubs as a tibble
conference_hubs <- tibble(
  city = c("Montreal", "Seoul", "Brisbane"),
  year = c(2023, 2024, 2025)
)

# Get coordinates for our conference cities of interest and add them to the tibble
hub_coords <- get_hub_coordinates(conference_hubs$city)
conference_hubs <- conference_hubs %>%
  left_join(hub_coords, by = c("city" = "hub_city"))

radius_km <- 4500

# For each attendee, compute distance to that year's conference city
geocoded_attendees <- geocoded_attendees %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  rowwise() %>%
  mutate(
    conference_city_longitude = conference_hubs$longitude[conference_hubs$year == year],
    conference_city_latitude = conference_hubs$latitude[conference_hubs$year == year],
    distance_to_conference_city = distHaversine(
      c(longitude, latitude),
      c(conference_city_longitude, conference_city_latitude)
    ) / 1000  )


attendees_count <- list()
# for each conference city
for (i in 1:nrow(conference_hubs)) {
  hub <- conference_hubs[i, ]
  
  # calculate the number of attendees within the radius for each year
  attendees_count[[i]] <- geocoded_attendees %>%
    filter(!is.na(latitude) & !is.na(longitude)) %>%
    rowwise() %>%
    mutate(
      distance_to_hub = distHaversine(
        c(longitude, latitude),
        c(hub$longitude, hub$latitude)
      ) / 1000 # Convert to km
    ) %>%
    group_by(year) %>%
    mutate(N = n()) %>%
    filter(distance_to_hub <= radius_km) %>%
    summarise(close_to_city = hub$city,
              conference_city = first(conference_city),
              n = n(),
              N = first(N),
              IDs = list(ID)) %>%
    ungroup()
}
# combine results into a single data frame
attendees_count <- bind_rows(attendees_count) %>%
  mutate(nN = n/N, # proportion of attendees within radius
         year = factor(year, levels = sort(unique(conference_hubs$year))),
         close_to_city = factor(close_to_city, levels = conference_hubs$city))

# find attendees not within radius_km of any conference city
all_IDs <- attendees_count %>%
  unnest(IDs) %>%
  select(IDs)
missing_attendees <- filter(geocoded_attendees, ID %in% setdiff(geocoded_attendees$ID, all_IDs$IDs))

# join count of attendees out of radius to attendees_count
attendees_count <- attendees_count %>%
  filter(!is.na(close_to_city)) %>%
  bind_rows(
    missing_attendees %>%
      group_by(year, conference_city) %>%
      summarise(n = n(), .groups = 'drop_last')%>%
      mutate(close_to_city = NA) %>%
      # add N column
      left_join(
        attendees_count %>%
          group_by(year, conference_city) %>%
          summarise(N = first(N), .groups = 'drop_last'),
        by = join_by(year, conference_city))
  ) %>%
  mutate(close_to_city = factor(close_to_city, levels = c(conference_hubs$city, 'None')))



# display results in bars
attendees_count %>%
  ggplot(aes(x = close_to_city, y = n, fill = close_to_city)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(
    title = paste("Number of attendees living within", radius_km, "km of... for each year"),
    x = "Close to city",
    y = "Number of attendees",
    fill = "Number of Attendees"
  ) +
  # fill brewer palette set2, transform so fourth level is grey
  scale_fill_brewer(palette = "Set2", na.value = "grey80",labels = function(x) ifelse(is.na(x), "None", x)) +
  # change label for NA close_to_city
  scale_x_discrete(labels = function(x) ifelse(is.na(x), "None", x)) +
  facet_wrap(~year+conference_city+N) +
  # white background
  theme(background = element_rect(fill = "white")) +
  theme_minimal()
# Save the plot
ggsave("output/figures/attraction_power_bar_plot.png", width = 8, height = 6)


# now compute attraction power as ratio of maximum to other years
attraction_power <- attendees_count %>%
  group_by(close_to_city) %>%
  # compute ratio of N on conference year to the mean of the other years
  mutate(conference_year = conference_hubs$year[conference_hubs$city == first(close_to_city)]) %>%
  summarise(
    attraction_power = n[year == conference_year] / mean(n[year != conference_year]),
    .groups = 'drop') %>%
  filter(!is.na(close_to_city))

# show map of attendees not within radius km of any conference city
world_map <- map_data("world")
ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(x = long, y = lat, map_id = region),
           fill = "lightgray", color = "white", linewidth = .1) +
  geom_point(data = missing_attendees,
             aes(x = longitude, y = latitude, color = year), alpha = 0.3, size = 1) +
  scale_color_brewer(palette = "Set2") +
  labs(title = sprintf("Attendees Not Within %d km of Any Conference City (2023-2025)", radius_km),
       x = "Longitude", y = "Latitude") +
  facet_wrap(~year, ncol = 2) +
  theme_minimal()
ggsave("output/figures/attendees_out_of_radius_map.png", width = 10, height = 6)

ggplot(attraction_power, aes(x = close_to_city, y = attraction_power, fill = close_to_city)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = round(attraction_power, 2)), vjust = -0.5) +
  labs(
    title = "Attraction Power of Conference Cities",
    x = "Conference City",
    y = "Attraction Power (N on Conference Year / Mean of Other Years)"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 14))
ggsave("output/figures/attraction_power_bar_plot_ratio.png", width = 6, height = 4)


# now for each year, compute distance to each possible hub in new columns
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
geocoded_attendees <- geocoded_attendees %>%
  mutate(closest_hub = case_when(
    distance_to_Montreal <= distance_to_Seoul & distance_to_Montreal <= distance_to_Brisbane ~ "Montreal",
    distance_to_Seoul <= distance_to_Montreal & distance_to_Seoul <= distance_to_Brisbane ~ "Seoul",
    distance_to_Brisbane <= distance_to_Montreal & distance_to_Brisbane <= distance_to_Seoul ~ "Brisbane",
    TRUE ~ "None"
  )
  )


# sanity check: count number of attendees within each city for each year
attendees_count_again <- geocoded_attendees %>%
  group_by(year) %>%
  # count number of attendees "within_<city>" for each city
  summarise(
    n_Montreal = sum(within_Montreal, na.rm = TRUE),
    n_Seoul = sum(within_Seoul, na.rm = TRUE),
    n_Brisbane = sum(within_Brisbane, na.rm = TRUE),
    n_None = sum(!(within_Montreal | within_Seoul | within_Brisbane), na.rm = TRUE),
    N = n()
  ) %>%
  pivot_longer(cols = starts_with("n_"), names_to = "close_to_city", values_to = "n") %>%
  mutate(close_to_city = case_when(
    close_to_city == "n_Montreal" ~ "Montreal",
    close_to_city == "n_Seoul" ~ "Seoul",
    close_to_city == "n_Brisbane" ~ "Brisbane",
    close_to_city == "n_None" ~ NA_character_
  )) %>%
  # check that this matches attendees_count
  left_join(
    attendees_count %>%
      select(year, close_to_city, n, N),
    by = c("year", "close_to_city", "N")
  )

# show a world map with points colored by which hub they are closest to
world_map <- map_data("world")

# compute coordinates of circles of radius around each hub
for (i in hub_coords$hub_city) {
  hub <- hub_coords %>%
    filter(hub_city == i)
  
  circle_coords <- destPoint(p = c(hub$longitude, hub$latitude), b = seq(0, 360, by = 1), d = radius_km * 1000)
  circle_df <- as_tibble(circle_coords) %>%
    mutate(hub_city = i)
  
  if (i == hub_coords$hub_city[1]) {
    circles_all <- circle_df
  } else {
    circles_all <- bind_rows(circles_all, circle_df)
  }
}
ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(x = long, y = lat, map_id = region),
           fill = "lightgray", color = "white", linewidth = .1) +
  geom_point(data = geocoded_attendees,
             aes(x = longitude, y = latitude, color = factor(
               case_when(
                 within_Montreal ~ "Montreal",
                 within_Seoul ~ "Seoul",
                 within_Brisbane ~ "Brisbane",
                 TRUE ~ "None"
               )
             )), alpha = 0.3, size = 1) +
  geom_point(data = conference_hubs,
             aes(x = longitude, y = latitude), color = 'black',
             size = 3, shape = 1) +
  geom_point(data = circles_all,
             aes(x = lon, y = lat, group = hub_city, color = hub_city),
             shape = '.') +
  scale_color_brewer(palette = "Set2", na.value = "grey80") +
  labs(title = "Attendee Proximity to Conference Cities (2023-2025)",
       x = "Longitude", y = "Latitude", color = "Close Hub") +
  theme_minimal()
# save plot
ggsave("output/figures/attendee_proximity_to_conference_cities_map.png", width = 10, height = 6)

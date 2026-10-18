# R/08_attraction_power.R
# This script estimates the attraction power of conference cities.
# It compares the number of participants within a radius_km radius for each of the last three conference cities.

# --- 1. Load Libraries and Data ---
# Load utility functions
source("R/utils.R")

# Load geocoded attendee data
geocoded_attendees <- readRDS("output/geocoded_attendee_data.rds")



# Get coordinates for our conference cities of interest and add them to the tibble
hub_coords <- get_hub_coordinates(conference_hubs$city)
conference_hubs <- conference_hubs %>%
  left_join(hub_coords, by = c("city" = "hub_city"))


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

geocoded_attendees <- add_hub_distances(geocoded_attendees, conference_hubs, radius_km)

attendees_count <- list()
# for each conference city
for (i in 1:nrow(conference_hubs)) {
  hub <- conference_hubs[i, ]
  
  # calculate the number of attendees within the radius for each year
  attendees_count[[i]] <- geocoded_attendees %>%
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
    x = "",
    y = "Number of attendees",
    fill = "Close to city"
  ) +
  # fill brewer palette set2, transform so fourth level is grey
  scale_fill_brewer(palette = "Set2", na.value = "grey80",labels = function(x) ifelse(is.na(x), "None", x)) +
  # change label for NA close_to_city
  scale_x_discrete(labels = function(x) ifelse(is.na(x), "None", x)) +
  facet_wrap(~year+conference_city+N) +
  # white background
  theme(background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  theme_minimal()
# Save the plot
ggsave("output/figures/attraction_power_bar_plot.png", width = 8, height = 6)


# now compute attraction power as ratio of maximum to other years
attraction_power <- attendees_count %>%
  filter(close_to_city != 'Glasgow') %>%
  filter(!is.na(close_to_city)) %>%
  group_by(close_to_city) %>%
  # compute ratio of N on conference year to the mean of the other years
  summarise(
    attraction_power = n[close_to_city == conference_city] / mean(n[close_to_city != conference_city]),
    .groups = 'drop')

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
    title = 'Attraction Power of Conference Cities\nfor "local" participants',
    x = "Conference City",
    y = "Attraction Power" # \n(N on Conference Year / Mean of Other Years)
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(text = element_text(size = 18))
ggsave("output/figures/attraction_power_bar_plot_ratio.png", width = 6, height = 4)


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
             aes(x = longitude, y = latitude, color = closest_hub), 
             alpha = 0.3, size = 1) +
  geom_point(data = conference_hubs,
             aes(x = longitude, y = latitude), color = 'black',
             size = 3, shape = 1) +
  geom_point(data = circles_all,
             aes(x = lon, y = lat, group = hub_city, color = hub_city),
             shape = '.') +
  scale_color_brewer(palette = "Set2", na.value = "grey80") +
  labs(title = "Attendee Proximity to Conference Cities (2023-2025)",
       x = "Longitude", y = "Latitude", color = "Close Hub") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "lightblue", color = NA))
# save plot
ggsave("output/figures/attendee_proximity_to_conference_cities_map.png", width = 10, height = 6)

# ============================================================
# --- Exploration (not part of the saved pipeline outputs) ---
# ============================================================

toplot <- geocoded_attendees %>%
  # add within_none column for attendees not within radius of any city
  mutate(within_None = ifelse(!if_any((paste0("within_", conference_hubs$city))), TRUE, FALSE)) %>%
  # merge witin_Glasgow and within_None into within_None column
  mutate(within_None = ifelse(within_Glasgow == TRUE, TRUE, within_None)) %>%
  select(-within_Glasgow) %>%
  pivot_longer(cols = starts_with("within_"), names_to = "within_city", values_to = "within_radius") %>%
  filter(within_radius == TRUE) %>%
  group_by(ID) %>% slice(1) %>%
  mutate(within_city = str_replace(within_city, "^within_", "")) %>%
  # mutate(within_city = factor(within_city, levels = conference_hubs$city)) %>%
  # mutate(closest_hub = factor(closest_hub, levels = conference_hubs$city)) %>%
  group_by(within_city, year, conference_city) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  rename(from_city = within_city, 
    to_city = conference_city,
    number_of_attendees = n) %>%
  select(to_city, year, from_city, number_of_attendees) %>%
  arrange(year, from_city)

toplot %>%
  ggplot(aes(x = from_city, y = number_of_attendees, fill = year)) +
  geom_col(position = "dodge") +
  labs(title = "Number of Attendees by region and year") +
  theme_minimal()

# print a nice table with toplotn adding totals per year in a row after each year
toplot %>%
  group_by(year) %>%
  bind_rows(toplot %>% group_by(year) %>% summarise(number_of_attendees = sum(number_of_attendees)) %>% mutate(from_city = "Total")) %>%
  # reorder by year and from city (Montreal, Seoul, Brisbane, Other)
  arrange(year, from_city) %>%
  knitr::kable()

geocoded_attendees %>%
  group_by(year, conference_city) %>%
  summarise(total_attendees = n()) %>%
  ungroup()%>%
  # order conference_city to put Montreal_Seoul_Brisbane last
  # NOTE: this row is a manually-entered illustrative value for a hypothetical combined
  # three-hub 2027 total, not computed from the pipeline data above. Confirm/document its
  # exact derivation before reusing this figure.
  add_row(year = "2027", conference_city = "Montreal_Seoul_Brisbane", total_attendees = 4184) %>%
  mutate(conference_city = factor(conference_city, levels = c(conference_hubs$city, "Montreal_Seoul_Brisbane"))) %>%
  mutate(year = factor(year, levels = c(2023, 2024, 2025, 2027)))%>%
  # add a three hubs row with the sum of attendees close to each conference city
  ggplot(aes(x = conference_city, y = total_attendees, fill = year)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = total_attendees), vjust = -0.5) +
  labs(title = "Total Attendees by Year") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") 


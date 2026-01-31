# R/04_all_attendees_maps.R
# This script generates a world map of all attendee locations across all years.

# --- 1. Load Libraries and Data ---
library(tidyverse)
library(ggplot2)
library(maps)
source("R/utils.R") # Load utility functions

# Load geocoded attendee data
geocoded_attendees_file <- "output/geocoded_attendee_data.rds"
if (!file.exists(geocoded_attendees_file)) {
  stop("Geocoded attendee data not found. Please run script 03_geocode.R first.")
}
attendees <- readRDS(geocoded_attendees_file)

# --- 2. Prepare Data ---
# Get world map data
world_map <- map_data("world")

# Filter out attendees with no geocoded location
attendees_with_coords <- attendees %>%
  filter(!is.na(latitude) & !is.na(longitude))

# --- 3. Geocode Hubs and Prepare for Plotting ---
# Define hub cities and their corresponding years
hubs_with_years <- tibble(
  year = c(2023, 2024, 2025),
  hub_city = c("Montreal", "Seoul", "Brisbane")
)

# Use the main geocoding cache to get hub coordinates
hub_coords <- get_hub_coordinates(hubs_with_years$hub_city) %>%
  left_join(hubs_with_years, by = c("hub_city"))

# --- 4. Create and Save Maps for Each Year ---
color_scale_max <- 1

# Get unique years from the data
years <- unique(attendees_with_coords$year)

# Ensure the output directory exists
if (!dir.exists("output/figures")) {
  dir.create("output/figures", recursive = TRUE)
}

# Loop through each year to create and save a plot
for (current_year in years) {
  
  # Filter data for the current year
  year_data <- attendees_with_coords %>%
    filter(year == current_year)
  
  # Get the hub for the current year
  current_hub <- hub_coords %>%
    filter(year == current_year)
  
  # compute longitude and latitude of 500 circle points around the hub
  current_circle <- expand_grid(theta = seq(0,2*pi,length.out=100))
  
  # Create the plot for the current year with density layer
  density_map_plot <- ggplot() +
    # Use geom_map for the base layer
    geom_map(
      data = world_map, map = world_map,
      aes(map_id = region),
      fill = "gray80", color = "white", linewidth = .1
    ) +
    expand_limits(x = world_map$long, y = world_map$lat) +

    # Add 2D density layer
    stat_density_2d(
      data = year_data,
      aes(x = longitude, y = latitude, fill = after_stat(level)),
      geom = "polygon",
      contour_var = 'ndensity',
      show.legend = FALSE,
      alpha = 0.5
    ) +
    scale_fill_viridis_c() + #limits = c(0, color_scale_max)) +
    
    # Attendee points
    geom_point(
      data = year_data,
      aes(x = longitude, y = latitude),
      color = "red",
      size = 0.5,
      alpha = 0.3
    ) +
    
    # Add the hub location marker
    geom_point(
      data = current_hub,
      aes(x = longitude, y = latitude),
      color = "blue",
      shape = 18, # Diamond shape
      size = 5,
      alpha = 1.0
    ) +
    # add a circle at 5000 km radius around the hub
    
    # Add titles and theme
    labs(
      title = paste("Attendee Distribution and Conference Hub -", current_year),
      subtitle = "Density of attendee locations, with the conference hub marked in blue.",
      fill = "Attendee Count\nin area"
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  # Define the output filename for the current year
  output_figure_file <- paste0("output/figures/attendee_world_heatmap_", current_year, ".png")
  
  # Save the plot
  ggsave(output_figure_file, plot = density_map_plot, width = 12, height = 7, dpi = 300)
  
  print(paste("Map for", current_year, "saved to", output_figure_file))
  
  # Display the plot
  print(density_map_plot)
}

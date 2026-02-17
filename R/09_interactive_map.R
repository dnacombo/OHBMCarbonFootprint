# R/09_interactive_map.R
# This script creates an interactive map of all attendees for each conference year.

# --- 1. Load Libraries ---
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(crosstalk)

# --- 2. Load Data ---
message("Loading footprint data...")
# This file contains the necessary geocoded data plus calculated distances and footprints.
footprint_data_file <- "output/monocentric_footprint_detailed.rds"
if (!file.exists(footprint_data_file)) {
  stop("Error: Monocentric footprint data not found. Please run 'R/05_monocentric_footprint.R' first.")
}
footprint_data <- readRDS(footprint_data_file)
message(paste("  Loaded", nrow(footprint_data), "records"))

# --- 3. Prepare Data for Mapping ---
message("Preparing data for mapping...")
# Create a formatted tooltip for each attendee
# Round distance and CO2 for cleaner display
map_data <- footprint_data %>%
  mutate(
    tooltip_text = paste(
      "<b>Location:</b>", full_address, "<br>",
      "<b>Distance to Hub:</b>", round(travel_distance_km, 0), "km<br>",
      "<b>Est. CO2 Footprint:</b>", round(carbon_footprint_kg_co2e, 2), "tonnes"
    )
  )

# Split data by year to create separate layers
data_2023 <- map_data %>% filter(year == 2023)
data_2024 <- map_data %>% filter(year == 2024)
data_2025 <- map_data %>% filter(year == 2025)

# --- 4. Create the Interactive Map ---
# Define a color palette for the years
year_colors <- c("2023" = "blue", "2024" = "green", "2025" = "purple")

# Create a unique group for each attendee for highlighting
map_data <- map_data %>% mutate(group = paste(ID, year, sep = "-"))

# Create path data: one row per path segment endpoint
message("Creating path data (this may take a moment)...")
path_data <- map_data %>%
  select(group, year, longitude, latitude, conference_lon, conference_lat) %>%
  rowwise() %>%

  reframe(
    group = group,
    year = year,
    lon = c(longitude, conference_lon),
    lat = c(latitude, conference_lat)
  )
message(paste("  Created", nrow(path_data) / 2, "paths"))

# Create shared data objects for crosstalk
message("Creating shared data objects...")
shared_markers <- SharedData$new(map_data, ~group)
shared_paths <- SharedData$new(path_data, ~group)

# Initialize the map
message("Building the interactive map...")
p <- plot_geo() %>%
  add_paths(
    data = shared_paths,
    x = ~lon, y = ~lat,
    split = ~group,
    color = I("gray"),
    hoverinfo = "none",
    showlegend = FALSE,
    line = list(width = 2)
  ) %>%
  add_markers(
    data = shared_markers,
    x = ~longitude, y = ~latitude,
    text = ~tooltip_text,
    color = ~factor(year),
    colors = year_colors,
    hoverinfo = "text",
    marker = list(size = 5, opacity = 0.6),
    showlegend = FALSE
  ) %>%
  add_markers(
    data = data.frame(
        latitude = c(45.5017, 37.5665, -27.4698),
        longitude = c(-73.5673, 127.0080, 153.0251),
        year = c("2023", "2024", "2025"),
        tooltip_text = c("Montreal (2023)", "Seoul (2024)", "Brisbane (2025)")
    ),
    x = ~longitude, y = ~latitude,
    text = ~tooltip_text,
    color = ~factor(year),
    colors = year_colors,
    hoverinfo = "text",
    marker = list(size = 25, opacity = 0.8, symbol = "star"),
    name = ~year,
    legendgroup = ~year
  ) %>%
  style(hoverlabel = list(bgcolor = "white")) %>%
  layout(
    title = "Conference Attendees by Year",
    geo = list(
      scope = "world",
      projection = list(type = "natural earth"),
      showland = TRUE,
      landcolor = toRGB("gray95"),
      subunitcolor = toRGB("gray85"),
      countrycolor = toRGB("gray85"),
      countrywidth = 0.5,
      subunitwidth = 0.5
    ),
    legend = list(title = list(text = "<b>Conference Year</b>"))
  ) %>%
  highlight(
    on = "plotly_hover",
    off = "plotly_relayout",
    opacityDim = 0.05
  )

# --- 5. Save the Map ---
message("Saving the map (this may take a while for large datasets)...")
output_file <- "output/interactive_attendee_map.html"
saveWidget(p, file = output_file, selfcontained = TRUE)

message(paste("Interactive map has been saved to:", output_file))
message("You can open this HTML file in any web browser.")

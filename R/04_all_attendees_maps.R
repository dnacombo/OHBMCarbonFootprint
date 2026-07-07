# R/04_all_attendees_maps.R
# This script generates a world map of all attendee locations across all years.

# --- 1. Load Libraries and Data ---
source("R/utils.R") # Load utility functions
library(maps)
library(ggbeeswarm)


# Load geocoded attendee data
geocoded_attendees <- readRDS("output/geocoded_attendee_data.rds")

transformed_geocoded_attendees <- geocoded_attendees  |> 
  filter(!is.na(latitude) & !is.na(longitude)) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  sf::st_transform(crs = "+proj=eqearth")

# --- 2. Prepare Data ---
# Get world map data
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  sf::st_transform(crs = "+proj=eqearth")

world_map <- map_data("world")


# Use the main geocoding cache to get hub coordinates
hub_coords <- get_hub_coordinates(conference_hubs$city)
conference_hubs <- conference_hubs |>
  left_join(hub_coords, by = c("city" = "hub_city"))

conference_hubs_sf <- conference_hubs |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  sf::st_transform(crs = "+proj=eqearth")

# --- 4. Create and Save Maps for Each Year ---

# Get unique years from the data
years <- unique(geocoded_attendees$year)

# Ensure the output directory exists
if (!dir.exists("output/figures")) {
  dir.create("output/figures", recursive = TRUE)
}

# Loop through each year to create and save a plot
for (current_year in years) {
  
  # Filter data for the current year
  year_data <- geocoded_attendees |>
    filter(year == current_year)
  
  # Get the hub for the current year
  current_hub <- conference_hubs |>
    filter(year == current_year)
  year_data_sf <- transformed_geocoded_attendees |> filter(year == current_year)
  current_hub_sf <- conference_hubs_sf |> filter(year == current_year)

  # compute longitude and latitude of 500 circle points around the hub
  current_circle <- expand_grid(theta = seq(0,2*pi,length.out=100))
  
  # Create the plot for the current year with density layer
 density_map_plot <- ggplot() +
  geom_sf(data = world, fill = "gray80", color = "white", linewidth = 0.1) +
  geom_sf(data = year_data_sf, color = "red", size = 2, alpha = 0.3) +
  geom_sf(data = current_hub_sf, color = "blue", shape = 1, size = 5) +
  geom_sf(data = current_hub_sf, color = "blue", shape = 1, size = 6) +
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

# compute coordinates of circles of radius around each hub
for (i in hub_coords$hub_city) {
  hub <- hub_coords |>
    filter(hub_city == i)
  
  circle_coords <- destPoint(p = c(hub$longitude, hub$latitude), b = seq(0, 360, by = .1), d = radius_km * 1000)
  circle_df <- as_tibble(circle_coords) |>
    mutate(hub_city = i)
  
  if (i == hub_coords$hub_city[1]) {
    circles_all <- circle_df
  } else {
    circles_all <- bind_rows(circles_all, circle_df)
  }
}
# for each point in circles_all, add columns with the distance to each hub
for (i in hub_coords$hub_city) {
  hub <- hub_coords |>
    filter(hub_city == i)
  circles_all <- circles_all |>
    rowwise() |>
    mutate(!!paste0("distance_to_", i) := distHaversine(c(lon, lat), c(hub$longitude, hub$latitude)) / 1000)
}
# if the minimum distance to any hub is less than distande to hub_city,
# move lon and lat to the midpoint between the concerned hubs
circles_all <- circles_all |> ungroup()

for (i in hub_coords$hub_city) {
  hub_i <- hub_coords |> filter(hub_city == i)
  idx   <- circles_all$hub_city == i
  pts   <- circles_all[idx, ]
  
  orig_dx <- pts$lon - hub_i$longitude
  orig_dx <- if_else(abs(orig_dx) > 180, orig_dx - sign(orig_dx) * 360, orig_dx)
  orig_dy <- pts$lat - hub_i$latitude
  t_min   <- rep(1.0, nrow(pts))
  
  for (j in hub_coords$hub_city) {
    if (i == j) next
    hub_j <- hub_coords |> filter(hub_city == j)
    
    dist_col_i <- paste0("distance_to_", i)
    dist_col_j <- paste0("distance_to_", j)
    
    abx <- hub_j$longitude - hub_i$longitude
    if (abs(abx) > 180) abx <- abx - sign(abx) * 360
    aby <- hub_j$latitude - hub_i$latitude
    
    numer   <- (abx^2 + aby^2) / 2
    denom   <- orig_dx * abx + orig_dy * aby
    overlap <- pts[[dist_col_j]] < pts[[dist_col_i]]
    
    t_val <- case_when(
      overlap & denom > 0  ~ pmin(numer / denom, 1.0),
      overlap & denom <= 0 ~ 0.0,
      TRUE                 ~ 1.0
    )
    t_min <- pmin(t_min, t_val)
  }
  
  new_lon <- hub_i$longitude + t_min * orig_dx
  # ---- fix: normalize output longitude back to [-180, 180] ----
  new_lon <- case_when(
    new_lon >  180 ~ new_lon - 360,
    new_lon < -180 ~ new_lon + 360,
    TRUE           ~ new_lon
  )
  circles_all[idx, "lon"] <- new_lon
  circles_all[idx, "lat"] <- hub_i$latitude + t_min * orig_dy
}

# now same thing but with all years together, colored by year
combined_map_plot <- ggplot() +
  geom_sf(data = world, fill = "gray80", color = "white", linewidth = 0.1) +
  geom_sf(
    data = transformed_geocoded_attendees |> slice_sample(prop = 1),
    aes(color = factor(year)),
    shape = 16, size = 3, alpha = 0.2, show.legend = FALSE
  ) +
  geom_sf(
    data = conference_hubs_sf |> filter(city != "Glasgow"),
    color = "black", shape = 1, size = 6, show.legend = FALSE
  ) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Participant Distribution (2023-2025)",
    subtitle = "Participant locations and regional participant counts colored by year.",
    color = "Year"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )
print(combined_map_plot)

  geocoded_attendees <- add_hub_distances(geocoded_attendees, conference_hubs, radius_km)
toplot <- geocoded_attendees |>
  ungroup() |>
    pivot_longer(cols = starts_with("within_"), names_to = "within_city", values_to = "within_radius") |>
    filter(within_radius == TRUE) |> select(-within_radius) |>
    mutate(within_city = str_replace(within_city, "^within_", "")) |>
    mutate(within_city = factor(within_city, levels = conference_hubs$city)) |>
    mutate(closest_hub = factor(closest_hub, levels = conference_hubs$city))
# create one plot per within_city variable, showing the number of attendees within each city for each year

library(grid)
{
plot_coords <- tibble(
  within_city = conference_hubs$city,
  x = c(-55, 160, 120, -5),
  y = c(30, 30, -50, 70)
) |>
  sf::st_as_sf(coords = c("x", "y"), crs = 4326) |>
  sf::st_transform(crs = "+proj=eqearth")

coords_matrix <- sf::st_coordinates(plot_coords)
plot_coords <- plot_coords |>
  mutate(
    x_proj = coords_matrix[, 1],
    y_proj = coords_matrix[, 2]
  ) |>
  sf::st_drop_geometry()
  
map_and_bar_plots <- combined_map_plot
for (city in conference_hubs$city) {
  city_plot <- toplot |> 
    filter(within_city == .env$city) |>
    group_by(within_city, year) |>
    summarise(n = n(), .groups = "drop") |>
    ggplot(aes(x = year, y = n, fill = year)) +
    geom_col(position = "dodge",
              show.legend = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.4))) +
    geom_text(aes(label = n), size = 3, position = position_dodge(width = 0.9), vjust = -0.25) +
    # labs(title = paste("Number of Attendees within", city, "by Year"), x = "City", y = "Number of Attendees") +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(size = 7, vjust = 1.5),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_blank())
  map_and_bar_plots <- map_and_bar_plots +
    annotation_custom(
      ggplotGrob(city_plot),
      xmin = plot_coords$x_proj[plot_coords$within_city == city] - 1.5e6,
      xmax = plot_coords$x_proj[plot_coords$within_city == city] + 1.5e6,
      ymin = plot_coords$y_proj[plot_coords$within_city == city] - 1.3e6,
      ymax = plot_coords$y_proj[plot_coords$within_city == city] + 1.3e6
    )
}
# insert the city plots into the combined map plot as insets

print(map_and_bar_plots)
ggsave("output/figures/attendee_world_map_with_inset_barplots.png", width = 12, height = 7, dpi = 300)
}

# now print table with the same data
# year, within_city, n, total numbers of participants for that year, percentage of participants within city for that year
table_data <- toplot  |>
  sf::st_drop_geometry() |>
  group_by(year, within_city) |>
  summarise(n = n(), .groups = "drop") |>
  # rename Montreal to North America, Seoul to Asia, Brisbane to Oceania, Glasgow to Europe
  mutate(continent = case_when(
    within_city == "Montreal" ~ "North America",
    within_city == "Seoul" ~ "Asia",
    within_city == "Brisbane" ~ "Oceania",
    within_city == "Glasgow" ~ "Europe",
    TRUE ~ within_city
  )) |>
  group_by(year) |>
  mutate(total_year = sum(n), 
        percentage_year = n / total_year * 100) |>
  ungroup() |>
  group_by(continent) |>
  mutate(total_city = sum(n),
         percentage_city = n / total_city * 100) |>
  select(year, continent, n, total_year, percentage_year, total_city, percentage_city) |>
  arrange(year, continent)
# print the table in nice format, with percentage rounded to 1 decimal place

library(kableExtra)
table_data |>
  select(1:5) |> 
  mutate(percentage_year = round(percentage_year, 1)) |>
  knitr::kable(col.names = c("Year", "Region", "Number of Attendees", "Total Attendees per Year", "Percentage Region within Year"), align = "c") |>
  kableExtra::row_spec(0, bold = TRUE, background = "lightgray") |>
  kableExtra::kable_styling(full_width = FALSE, position = "center") |>
  kableExtra::column_spec(c(1,2,3,4,5), background = kableExtra::spec_color(as.numeric(factor(table_data$year)), palette = c('#66C2A5', '#FC8D62', '#8DA0CB')))|>
  kableExtra::kable_styling(full_width = FALSE, position = "center")


(tmp <- table_data |> 
  select(1:3,6,7) |> 
  arrange(continent) |> 
  mutate(percentage_city = round(percentage_city, 1))) |>
  knitr::kable(col.names = c("Year", "Number of Attendees", "Region", "Total Attendees per Region", "Percentage Year within Region"), align = "c") |>
  # color cell background by year in the Percetage Region within Year column
  kableExtra::row_spec(0, bold = TRUE, background = "lightgray") |>
  kableExtra::kable_styling(full_width = FALSE, position = "center") |>
  kableExtra::kable_styling(full_width = FALSE, position = "center")
  
# now plot percentage_city against percentage_year
ggplot(table_data, aes(x=year, y=percentage_city, col=continent))+
         geom_col(position = "dodge")
       
# now plot percentage_city against percentage_year
ggplot(table_data, aes(x=year, y=percentage_year, size = percentage_city, col=continent))+
  geom_point()
# create a pie chart of percentage_city facet_wrapped against year
year_labels <- table_data |>
  ungroup() |>
  distinct(year, total_year) |>
  mutate(label = paste0(year, "\nn = ", total_year)) |>
  select(year, label) |>
  deframe()

ggplot(table_data, aes(x = "", y = percentage_city, fill = continent)) +
  geom_col(width = 1, color = "white") +  # width=1 removes the center hole; color adds clean borders
  facet_wrap(~year, scale = 'free_y', labeller = as_labeller(year_labels)) +
  coord_polar(theta = "y") +              # Essential: maps the y-axis (percentages) to the angle
  theme_void()                            # Highly recommended: removes ugly background grid lines

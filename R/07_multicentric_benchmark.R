# R/07_multicentric_benchmark.R
# This script benchmarks various multicentric conference scenarios.

# --- 1. Load Libraries and Data ---
library(tidyverse)
library(geosphere)
library(tidygeocoder)

# Load geocoded attendee data
geocoded_attendees <- readRDS("output/geocoded_attendee_data.rds")

# Load utility functions
source("R/utils.R")

# --- 2. Define Scenarios ---
# A list of scenarios to test, where each element is a named vector of hub locations.
scenarios <- list(
  # One_Location_Glasgow = 'Glasgow',
  One_Location_Montreal = 'Montreal',
  One_Location_Seoul = 'Seoul',
  One_Location_Brisbane = 'Brisbane',
  # all possible two-location combinations
  Two_Locations_Montreal_Seoul = c("Montreal", "Seoul"),
  Two_Locations_Montreal_Brisbane = c("Montreal", "Brisbane"),
  Two_Locations_Seoul_Brisbane = c("Seoul", "Brisbane"),
  # Two_Locations_Montreal_Glasgow = c("Montreal", "Glasgow"),
  # Two_Locations_Seoul_Glasgow = c("Seoul", "Glasgow"),
  # Two_Locations_Brisbane_Glasgow = c("Brisbane", "Glasgow"),
  # three-location combination
  Past_Three_Locations = c("Montreal", "Seoul", "Brisbane"),
  # four location combination
  Past_Four_Locations = c("Glasgow", "Montreal", "Seoul", "Brisbane"),
  # six location combination
  Six_Hubs_Global = c("Glasgow", "Montreal", "Sao Paulo", "Seoul", "Johannesburg", "Brisbane")
)

# --- 3. Run Benchmark ---
# This list will store the summary results for each scenario
all_scenario_summaries <- list()

print("Starting multicentric scenario benchmark...")

for (scenario_name in names(scenarios)) {
  
  hub_locations <- scenarios[[scenario_name]]
  print(paste("--- Running Scenario:", scenario_name, "---"))
  print(paste("Hubs:", paste(hub_locations, collapse = ", ")))
  
  # Get hub coordinates using the utility function
  hub_coords <- get_hub_coordinates(hub_locations)
  
  # Calculate the detailed footprint for the current scenario
  scenario_footprint_details <- calculate_multicentric_footprint(geocoded_attendees, hub_coords)
  
  # Summarize the results for the current scenario
  scenario_summary <- scenario_footprint_details %>%
    group_by(year, conference_city) %>%
    summarise(
      total_attendees = n(),
      total_footprint_tonCO2e = sum(footprint_kgCO2e, na.rm = TRUE) / 1000,
      mean_footprint_tonCO2e = mean(footprint_kgCO2e, na.rm = TRUE) / 1000,
    ) %>%
    mutate(scenario = scenario_name, num_hubs = length(hub_locations)) %>%
    select(scenario, num_hubs, year, everything())
  
  # Add the summary to our list of results
  all_scenario_summaries[[scenario_name]] <- scenario_summary
  
  print(paste("Scenario", scenario_name, "complete."))
  print(scenario_summary)
}

# --- 4. Consolidate and Save Results ---
# Combine all scenario summaries into a single data frame
final_benchmark_summary <- bind_rows(all_scenario_summaries)

# factor number of attendees
final_benchmark_summary <- final_benchmark_summary %>% 
  mutate(total_footprint_per_attendee_tonCO2 = total_footprint_tonCO2e / total_attendees)

# make sure the scenarios appear in a consistent order
final_benchmark_summary$scenario <- factor(final_benchmark_summary$scenario, levels = names(scenarios))

# Save the final summary to a CSV file
output_file <- "output/reports/multicentric_benchmark_summary.csv"
write.csv(final_benchmark_summary, file = output_file, row.names = FALSE)

print("--- Benchmark Complete ---")
print(paste("Benchmark summary saved to", output_file))
print(final_benchmark_summary)

ggplot(data=final_benchmark_summary, aes(x=scenario, y=total_footprint_tonCO2e, fill=scenario)) +
  geom_col() +
  geom_text(aes(label=round(total_footprint_tonCO2e)), vjust=-0.5) +
  theme(axis.text.x=element_text(angle = 90, hjust=1)) + 
  theme(axis.text.x=element_blank()) + 
  ylab('') + xlab('') +
  facet_wrap(~year+conference_city+total_attendees) +
  ggtitle('Total OHBM participant transportation footprint (tons CO2e)')
ggsave('output/figures/multicentric_benchmark_total_footprint.png', width=10, height=6)

ggplot(data=final_benchmark_summary, aes(x=scenario, y=total_footprint_per_attendee_tonCO2, fill=scenario)) +
  geom_col() +
  geom_text(aes(label=round(total_footprint_per_attendee_tonCO2,2)), vjust=-0.5) +
  theme(axis.text.x=element_text(angle = 90, hjust=1)) + 
  theme(axis.text.x=element_blank()) + 
  ylab('') + xlab('') +
  facet_wrap(~year+conference_city) +
  ggtitle('Average transportation footprint per participant (tons CO2e)')
ggsave('output/figures/multicentric_benchmark_per_attendee_footprint.png', width=10, height=6)

ggplot(data=final_benchmark_summary, aes(x=scenario, y=total_footprint_per_attendee_tonCO2, fill=factor(num_hubs))) +
  geom_col() +
  # geom_col(aes(color=ifelse(mapply(function(s, c) str_detect(s, c), scenario, conference_city), 'black', NA))) + 
  geom_text(aes(label=round(total_footprint_per_attendee_tonCO2,2)), vjust=-0.5) +
  theme(axis.text.x=element_text(angle = 75, hjust=1)) + 
  # scale_color_manual(values = c("black" = "black"), na.value = NA) +
  # guides(color = "none") +
  # remove "One_Location_" "Two_Locations_" from x axis labels
  scale_x_discrete(labels = function(x) str_replace(x, "^(One|Two)_Location(s?)_", "")) +
  # theme(axis.text.x=element_blank()) + 
  ylab('') + xlab('') +
  # legend label for num_hubs is "Number of Hubs"
  scale_fill_brewer(palette = 'Pastel1', name = "Number of Hubs") +
  facet_grid(~year+conference_city, scales = "free_x") +
  ggtitle('Average transportation footprint per participant (tons CO2e)')
ggsave('output/figures/scenario_benchmark_per_attendee_footprint.png', width=10, height=6)

# Same as above, but showing average across years for each scenario
# fill is number of hubs
# bars are average across years
# points are each year (colored by city name)
ggplot(data=final_benchmark_summary, aes(x=scenario, y=total_footprint_per_attendee_tonCO2, fill=factor(num_hubs))) +
  geom_col(data=final_benchmark_summary %>% group_by(scenario, num_hubs) %>% summarize(total_footprint_per_attendee_tonCO2 = mean(total_footprint_per_attendee_tonCO2)), 
    aes(x=scenario, y=total_footprint_per_attendee_tonCO2, fill=factor(num_hubs)),
    position=position_dodge(width=0.9), show.legend = FALSE) +
  geom_text(aes(label=round(total_footprint_per_attendee_tonCO2,2)), vjust=-0.5) +
  theme(axis.text.x=element_text(angle = 75, hjust=1)) + 
  # remove "One_Location_" "Two_Locations_" from x axis labels
  scale_x_discrete(labels = function(x) str_replace(x, "^(One|Two)_Location(s?)_", "")) +
  # theme(axis.text.x=element_blank()) + 
  ylab('') + xlab('') +
  # legend label for num_hubs is "Number of Hubs"
  scale_fill_brewer(palette = 'Pastel1', name = "Number of Hubs") +
  geom_point(data=final_benchmark_summary, 
    aes(x=scenario, y=total_footprint_per_attendee_tonCO2, color=conference_city), 
    position=position_jitter(width=0.1), size=2) +
  scale_color_brewer(palette = 'Set1', name = "Conference City") +
  facet_wrap(~num_hubs, scales = "free_x") +
  ggtitle('Average transportation footprint per participant (tons CO2e) by number of hubs')
ggsave('output/figures/scenario_benchmark_per_attendee_footprint_by_num_hubs.png', width=10, height=6)

# ============================================================
# --- Exploration (not part of the saved pipeline outputs) ---
# ============================================================

# same but focusing on
# montreal, montreal_seoul, and three locations for 2023
# seoul, montreal_seoul, and three locations for 2024
# brisbane and monbreal_brisbane, and three locations for 2025
ggplot(data=final_benchmark_summary %>% filter((year==2023 & conference_city=='Montreal' & scenario %in% c('One_Location_Montreal', 'Two_Locations_Montreal_Seoul', 'Past_Three_Locations')) |
                                           (year==2024 & conference_city=='Seoul' & scenario %in% c('One_Location_Seoul', 'Two_Locations_Montreal_Seoul', 'Past_Three_Locations')) |
                                           (year==2025 & conference_city=='Brisbane' & scenario %in% c('One_Location_Brisbane', 'Two_Locations_Montreal_Brisbane', 'Past_Three_Locations'))),
       aes(x=scenario, y=total_footprint_per_attendee_tonCO2, fill=factor(num_hubs))) +
  geom_col() +
  geom_text(aes(label=round(total_footprint_per_attendee_tonCO2,2)), vjust=-0.5) +
  theme(axis.text.x=element_text(angle = 75, hjust=1)) +
  # remove "One_Location_" "Two_Locations_" from x axis labels
  scale_x_discrete(labels = function(x) str_replace(x, "^(One|Two)_Location(s?)_", "")) +
  # theme(axis.text.x=element_blank()) +
  ylab('') + xlab('') +
  # legend label for num_hubs is "Number of Hubs"
  scale_fill_brewer(palette = 'Pastel1', name = "Number of Hubs") +
  facet_wrap(~year+conference_city, scales = "free_x") +
  ggtitle('Average transportation footprint per participant (tons CO2e)') +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 18),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 12))

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
  One_Location_Montreal = 'Montreal',
  One_Location_Seoul = 'Seoul',
  One_Location_Brisbane = 'Brisbane',
  Past_Three_Locations = c("Montreal", "Seoul", "Brisbane"),
  Past_Four_Locations = c("Glasgow", "Montreal", "Seoul", "Brisbane"),
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
  theme(axis.text.x=element_blank()) + 
  ylab('') + xlab('') +
  facet_wrap(~year+conference_city+total_attendees) +
  ggtitle('Total OHBM participant transportation footprint (tons CO2e)')
ggsave('output/figures/multicentric_benchmark_total_footprint.png', width=10, height=6)

ggplot(data=final_benchmark_summary, aes(x=scenario, y=total_footprint_per_attendee_tonCO2, fill=scenario)) +
  geom_col() +
  geom_text(aes(label=round(total_footprint_per_attendee_tonCO2,2)), vjust=-0.5) +
  theme(axis.text.x=element_blank()) + 
  ylab('') + xlab('') +
  facet_wrap(~year+conference_city) +
  ggtitle('Average transporation footprint per participant (tons CO2e)')
ggsave('output/figures/multicentric_benchmark_per_attendee_footprint.png', width=10, height=6)


  
  
  
  

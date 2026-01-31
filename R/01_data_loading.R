# 01_data_loading.R

# Load necessary libraries
library(tidyverse)
library(readxl)

# Define file paths
data_file <- "data/12182025 3 Year Attendee.xlsx"
# The output is now the "raw" data before cleaning
output_file <- "output/raw_attendee_data.rds" 

# Load data from each sheet and add conference info
attendee_2025 <- read_excel(data_file, sheet = "2025") %>%
  mutate(year = 2025, conference_city = "Brisbane")
attendee_2024 <- read_excel(data_file, sheet = "2024") %>%
  mutate(year = 2024, conference_city = "Seoul")
attendee_2023 <- read_excel(data_file, sheet = "2023") %>%
  mutate(year = 2023, conference_city = "Montreal")

# Combine into a single data frame and add a unique ID
all_attendees <- bind_rows(attendee_2025, attendee_2024, attendee_2023) %>%
  mutate(ID = row_number())

# Save the raw combined data
saveRDS(all_attendees, file = output_file)

print("Raw data loading complete. Data saved to output/raw_attendee_data.rds")
print(paste("Total rows:", nrow(all_attendees)))

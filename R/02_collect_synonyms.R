# R/02_collect_synonyms.R
# This script cleans location data and outputs a list of unique locations for review.

library(tidyverse)
library(stringr)

# --- 1. Load Data ---
raw_data_file <- "output/raw_attendee_data.rds"
cleaned_output_file <- "output/cleaned_attendee_data.rds"
review_output_file <- "output/unique_locations_for_review.txt"
all_attendees <- readRDS(raw_data_file)

# --- 2. Cleaning and Standardization ---
synonyms_file <- "data/location_synonyms.csv"
if (file.exists(synonyms_file)) {
  synonyms <- read.csv(synonyms_file)
  synonym_vec <- setNames(synonyms$canonical_name, synonyms$synonym)
} else {
  synonym_vec <- c()
}

clean_locations <- function(df, replacements) {
  df %>%
    # 1. First, do basic component cleaning
    mutate(
      across(c(city, state, country), ~ str_trim(str_to_title(.))),
      city_clean = na_if(city, "None"),
      state_clean = na_if(state, "None"),
      country_clean = na_if(country, "None")
    ) %>%
    # then replace synonyms in individual components
    mutate(
      city_clean = str_replace_all(city_clean, fixed(replacements)),
      state_clean = str_replace_all(state_clean, fixed(replacements)),
      country_clean = str_replace_all(country_clean, fixed(replacements))
    ) %>%
    # 2. Combine into a full address
    unite(full_address, c(city_clean, state_clean, country_clean), 
          sep = ", ", na.rm = TRUE, remove = FALSE) %>%
    # avoid any double commas or trailing commas
    mutate(
      full_address = str_replace_all(full_address, ", ?, ?", ", "),
      full_address = str_replace_all(full_address, ", $", "")
    ) %>%
    # 3. apply all replacements to the full_address string
    mutate(
      full_address = str_replace_all(full_address, fixed(replacements)),
      # after cleaning, reparse to city, state, country
      # if only one component, assume it's the country
      # if two components, assume city and country
      # if three components, city, state, country
      city_clean = case_when(
        str_count(full_address, ",") == 0 ~ NA_character_,
        str_count(full_address, ",") == 1 ~ str_trim(str_split_fixed(full_address, ",", 2)[,1]),
        str_count(full_address, ",") >= 2 ~ str_trim(str_split_fixed(full_address, ",", 3)[,1])
      ),
      state_clean = case_when(
        str_count(full_address, ",") < 2 ~ NA_character_,
        str_count(full_address, ",") >= 2 ~ str_trim(str_split_fixed(full_address, ",", 3)[,2])
      ),
      country_clean = case_when(
        str_count(full_address, ",") == 0 ~ str_trim(full_address),
        str_count(full_address, ",") == 1 ~ str_trim(str_split_fixed(full_address, ",", 2)[,2]),
        str_count(full_address, ",") >= 2 ~ str_trim(str_split_fixed(full_address, ",", 3)[,3])
      )
      
    )
}

cleaned_attendees <- clean_locations(all_attendees, synonym_vec)

# --- 3. Save Cleaned Data and Review File ---
# Save the full dataframe with the cleaned 'full_address' column
saveRDS(cleaned_attendees, file = cleaned_output_file)
print(paste("Cleaned data saved to", cleaned_output_file))

# Extract unique locations and save them to a text file for easy review
unique_addresses <- cleaned_attendees %>%
  distinct(full_address) %>%
  arrange(full_address)

write.table(unique_addresses, file = review_output_file, 
            row.names = FALSE, col.names = FALSE, quote = FALSE)
print(paste("Unique locations for review have been saved to", review_output_file))
print(paste("Found", nrow(unique_addresses), "unique locations after cleaning."))

# --- 4. ADVANCED: Find Potential Synonyms using String Distance ---
print("--- Searching for potential synonyms using string distance ---")

library(stringdist)
library(reshape2)

# Re-read the unique addresses we just saved to ensure consistency
unique_addresses <- cleaned_attendees %>%
  distinct(full_address) %>%
  arrange(full_address)

# Calculate the Levenshtein distance matrix
dist_matrix <- stringdistmatrix(unique_addresses$full_address, unique_addresses$full_address, method = "lv")

# Set names for easier reading
rownames(dist_matrix) <- unique_addresses$full_address
colnames(dist_matrix) <- unique_addresses$full_address

# Melt the matrix into a list of pairs
dist_pairs <- melt(dist_matrix, varnames = c("Location1", "Location2"), value.name = "Distance")

# Filter for suspicious pairs (close but not identical)
suspicious_pairs <- dist_pairs %>%
  filter(
    Distance > 0,
    Distance <= 2
  ) %>%
  filter(as.character(Location1) < as.character(Location2))

# --- NEW: Add structural filter to remove false positives like state mismatches ---
# Function to check for state/province mismatches
is_state_mismatch <- function(loc1, loc2) {
  parts1 <- str_split(loc1, ", ")[[1]]
  parts2 <- str_split(loc2, ", ")[[1]]
  
  # Check if both seem to be City, State, Country format
  if (length(parts1) == 3 && length(parts2) == 3) {
    # Check if city and country are the same, but state is different
    if (parts1[1] == parts2[1] && parts1[3] == parts2[3] && parts1[2] != parts2[2]) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Apply the filter
# We use rowwise() to apply our function to each pair of locations
intelligent_pairs <- suspicious_pairs %>%
  rowwise() %>%
  filter(!is_state_mismatch(Location1, Location2)) %>%
  ungroup() %>%
  arrange(Distance, Location1)

if (nrow(intelligent_pairs) > 0) {
  suspicious_pairs_file <- "output/suspicious_pairs_for_review.csv"
  write.csv(intelligent_pairs, suspicious_pairs_file, row.names = FALSE)
  
  print("--- Found Potential Synonyms (Structurally Filtered) ---")
  print(paste("A list of intelligent suggestions has been saved to:", suspicious_pairs_file))
  print("Review this file and add any true synonyms to data/location_synonyms.csv")
  
} else {
  print("No potential new synonyms found with string distance analysis.")
}


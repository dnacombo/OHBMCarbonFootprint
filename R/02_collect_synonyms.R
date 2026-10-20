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
# every synonym says what kind of correction it is (used for the counts in section 2b)
correction_types <- c("misspelling", "diacritics", "variant", "country_name", "format", "wrong_country")
if (file.exists(synonyms_file)) {
  synonyms <- read.csv(synonyms_file)
  synonym_vec <- setNames(synonyms$canonical_name, synonyms$synonym)
  bad_type <- !synonyms$correction_type %in% correction_types
  if (any(bad_type)) stop("Missing or unknown correction_type in ", synonyms_file, " for: ",
                          paste(synonyms$synonym[bad_type], collapse = " | "))
} else {
  synonym_vec <- c()
}

# Same result as str_replace_all(x, fixed(replacements)) (patterns applied in order, each on the
# output of the previous one), but also records which synonyms fired on each element.
# Returns list(value, hits, partial): hits = fired synonyms joined by " >> " (NA if none),
# partial = TRUE if a synonym matched only part of the string rather than the whole of it.
replace_synonyms_logged <- function(x, replacements) {
  hits <- vector("list", length(x))
  partial <- rep(FALSE, length(x))
  for (i in seq_along(replacements)) {
    pattern <- names(replacements)[i]
    hit <- which(str_detect(x, fixed(pattern)))
    if (length(hit) == 0) next
    partial[hit] <- partial[hit] | x[hit] != pattern
    hits[hit] <- lapply(hits[hit], c, pattern)
    x[hit] <- str_replace_all(x[hit], fixed(pattern), replacements[[i]])
  }
  list(value = x,
       hits = map_chr(hits, \(h) if (is.null(h)) NA_character_ else paste(h, collapse = " >> ")),
       partial = partial)
}

clean_locations <- function(df, replacements) {
  df <- df %>%
    # 1. First, do basic component cleaning
    mutate(
      across(c(city, state, country), ~ str_trim(str_to_title(.))),
      city_clean = na_if(city, "None"),
      state_clean = na_if(state, "None"),
      country_clean = na_if(country, "None")
    ) %>%
    # address as it would be without any synonym replacement (for the synonym audit)
    unite(full_address_raw, c(city_clean, state_clean, country_clean),
          sep = ", ", na.rm = TRUE, remove = FALSE) %>%
    mutate(
      full_address_raw = str_replace_all(full_address_raw, ", ?, ?", ", "),
      full_address_raw = str_replace_all(full_address_raw, ", $", "")
    )
  # then replace synonyms in individual components
  component_hits <- list()
  partial_hit <- rep(FALSE, nrow(df))
  for (col in c("city_clean", "state_clean", "country_clean")) {
    res <- replace_synonyms_logged(df[[col]], replacements)
    df[[col]] <- res$value
    component_hits[[col]] <- if_else(is.na(res$hits), NA_character_,
                                     paste0(str_remove(col, "_clean"), ": ", res$hits))
    partial_hit <- partial_hit | res$partial
  }
  df %>%
    mutate(synonyms_component = pmap_chr(component_hits, \(...) {
      h <- na.omit(c(...))
      if (length(h) == 0) NA_character_ else paste(h, collapse = "; ")
    })) %>%
    # 2. Combine into a full address
    unite(full_address, c(city_clean, state_clean, country_clean), 
          sep = ", ", na.rm = TRUE, remove = FALSE) %>%
    # avoid any double commas or trailing commas
    mutate(
      full_address = str_replace_all(full_address, ", ?, ?", ", "),
      full_address = str_replace_all(full_address, ", $", "")
    ) -> df
  # 3. apply all replacements to the full_address string
  res <- replace_synonyms_logged(df$full_address, replacements)
  df %>%
    mutate(
      full_address = res$value,
      synonyms_full_address = res$hits,
      synonym_partial_match = partial_hit | res$partial,
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

# --- 2b. Synonym audit: how many entries were modified, and which ones are ambiguous ---
# Ambiguous = the result depends on the order of rows in location_synonyms.csv:
#  - conflicting: a synonym that fired has several different canonical names in the table
#    (only the first listed is effectively used);
#  - chained: several synonyms fired one after the other on the same string (A -> B -> C).
# Partial matches (a synonym replaced only part of a string, e.g. "Wales" in "New South Wales")
# are reported separately: often intended ("Pq, Canada"), but worth checking.
synonym_audit_file <- "output/synonym_changes_for_review.csv"
conflicting_synonyms <- if (exists("synonyms")) {
  synonyms %>% distinct() %>% count(synonym) %>% filter(n > 1) %>% pull(synonym)
} else character(0)

fired_synonyms <- function(hits) str_split(str_remove_all(hits, "(city|state|country): "), " >> |; ")
# for a synonym listed twice, the first row is the one str_replace_all effectively uses
synonym_type <- if (exists("synonyms")) {
  synonyms %>% distinct(synonym, .keep_all = TRUE) %>% with(setNames(correction_type, synonym))
} else character(0)
synonym_audit <- cleaned_attendees %>%
  mutate(
    synonym_applied = !is.na(synonyms_component) | !is.na(synonyms_full_address),
    modified = full_address != full_address_raw,
    fired = fired_synonyms(paste(synonyms_component, synonyms_full_address, sep = "; ")),
    correction_types = map_chr(fired, \(s) paste(sort(unique(na.omit(synonym_type[s]))), collapse = "; ")),
    conflicting = map_lgl(fired, \(s) any(s %in% conflicting_synonyms)),
    chained = str_detect(coalesce(synonyms_component, ""), " >> ") |
      str_detect(coalesce(synonyms_full_address, ""), " >> "),
    ambiguous = conflicting | chained
  )

print("--- Synonym replacement summary ---")
print(paste("Synonym table:", length(synonym_vec), "rows,",
            n_distinct(names(synonym_vec)), "distinct synonyms,",
            length(conflicting_synonyms), "with conflicting canonical names"))
print(paste("Entries with at least one synonym applied:", sum(synonym_audit$synonym_applied),
            "of", nrow(synonym_audit)))
print(paste("  in city/state/country components:", sum(!is.na(synonym_audit$synonyms_component)),
            "| in full address:", sum(!is.na(synonym_audit$synonyms_full_address))))
print(paste("Entries whose full address was modified:", sum(synonym_audit$modified),
            "(", n_distinct(synonym_audit$full_address_raw[synonym_audit$modified]), "distinct raw addresses )"))
print(paste("Ambiguous entries:", sum(synonym_audit$ambiguous),
            "| conflicting synonym:", sum(synonym_audit$conflicting),
            "| chained replacements:", sum(synonym_audit$chained)))
print(paste("Entries with a partial-string match:", sum(synonym_audit$synonym_partial_match)))

# entries corrected per correction type (an entry can have corrections of several types)
synonym_type_summary <- tibble(correction_type = correction_types) %>%
  mutate(
    n_entries = map_int(correction_type, \(t) sum(str_detect(synonym_audit$correction_types, fixed(t)))),
    pct_of_entries = 100 * n_entries / nrow(synonym_audit)
  ) %>%
  bind_rows(tibble(correction_type = "any", n_entries = sum(synonym_audit$synonym_applied),
                   pct_of_entries = 100 * mean(synonym_audit$synonym_applied))) %>%
  mutate(total_entries = nrow(synonym_audit))
write.csv(synonym_type_summary, "output/reports/synonym_corrections_summary.csv", row.names = FALSE)
misspelled <- synonym_type_summary %>% filter(correction_type == "misspelling")
print(sprintf("Misspelled entries corrected: %d of %d (%.1f%%)",
              misspelled$n_entries, nrow(synonym_audit), misspelled$pct_of_entries))
print(as.data.frame(synonym_type_summary %>% mutate(pct_of_entries = round(pct_of_entries, 2))))

synonym_changes <- synonym_audit %>%
  filter(synonym_applied) %>%
  count(full_address_raw, full_address, synonyms_component, synonyms_full_address, correction_types,
        conflicting, chained, partial = synonym_partial_match, name = "n_entries") %>%
  arrange(desc(conflicting | chained), desc(partial), desc(n_entries))
write.csv(synonym_changes, synonym_audit_file, row.names = FALSE)
print(paste("Per-address synonym changes saved to", synonym_audit_file))

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


# R/13b_equitable_hub_selection_pop250k.R
# Sensitivity of 13_ to the host-city size threshold: any city with at least 250,000 inhabitants
# can be a hub (1,565 cities in maps::world.cities), instead of 1,000,000 (313 cities).
# Runs 13_ unchanged apart from that threshold (13_ is sourced in a private environment, so its
# variables do not leak into the session); outputs are 13_'s file names with the suffix "_pop250k":
#   output/reports/equitable_hub_{pareto,benchmarks,search_checks,bootstrap}_pop250k.csv,
#   output/figures/equitable_hub_*_pop250k.png, output/equitable_hub_results_pop250k.rds.
# With 5x as many candidate cities, expect several hours (13_ takes about 50 minutes).

local({
  hub_selection_overrides <- list(eligible_min_pop = 2.5e5, output_tag = "_pop250k")
  source("R/13_equitable_hub_selection.R", local = TRUE)
})

# ============================================================
# --- Exploration (not part of the saved pipeline outputs) ---
# ============================================================

# Knee solutions with >= 250k vs >= 1M inhabitants (needs 13_'s output)
if (file.exists("output/reports/equitable_hub_pareto.csv")) {
  read_knees <- function(file, threshold) {
    readr::read_csv(file, show_col_types = FALSE) |>
      dplyr::filter(is_knee) |>
      dplyr::transmute(scenario, n_hubs, threshold, hubs = gsub(" \\([^)]*\\)", "", hubs),
                       footprint_t, attendance, coverage, footprint_per_attendee_t)
  }
  knee_comparison <- dplyr::bind_rows(
    read_knees("output/reports/equitable_hub_pareto.csv", ">= 1M"),
    read_knees("output/reports/equitable_hub_pareto_pop250k.csv", ">= 250k")
  ) |>
    dplyr::arrange(scenario, n_hubs, threshold)
  print(knitr::kable(knee_comparison, digits = 3,
                     caption = "Knee solutions: host cities >= 1M vs >= 250k inhabitants"))
}

# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

This is an R data-analysis project (not a package/app) that estimates the travel carbon footprint of
OHBM annual meetings and compares it against hypothetical "multicentric" formats (running the conference
simultaneously in several regional hubs instead of one city). It works from three years of real attendee
location data (2023 Montreal, 2024 Seoul, 2025 Brisbane) plus a synthetic Glasgow 2022 hub, geocodes
attendee cities, computes travel distances and emissions, and produces summary CSVs and figures.

## Running the analysis

There is no build system, package structure, or test suite — this is a pipeline of numbered R scripts
under `R/` meant to be run in order, either from the R console (`source("R/01_data_loading.R")`, etc.)
or via `Rscript R/01_data_loading.R` from the project root. Each script reads its inputs from `output/*.rds`
(or `data/`) and writes new `.rds`/`.csv`/`.png` files back into `output/`, so **scripts must be run in
numerical order** on a clean run, and later scripts assume earlier scripts' output files already exist.
`run_all.R` (project root) runs the whole pipeline, each script in its own `Rscript` process, in parallel
(fork, `--jobs N`) following a dependency list declared at its top — update that list when a script starts
reading another script's output. Logs go to `output/logs/`.

1. `R/01_data_loading.R` — reads `data/12182025 3 Year Attendee.xlsx` (one sheet per year: "2025", "2024",
   "2023"), tags each row with `year`/`conference_city`, assigns a row `ID` → `output/raw_attendee_data.rds`.
2. `R/02_collect_synonyms.R` — cleans/standardizes city/state/country text, applies
   `data/location_synonyms.csv` (synonym → canonical name map), builds a single `full_address` string per
   attendee → `output/cleaned_attendee_data.rds`. Also emits `output/unique_locations_for_review.txt` and,
   via Levenshtein-distance matching, `output/suspicious_pairs_for_review.csv` — a human reviews these and
   adds confirmed synonyms to `data/location_synonyms.csv` before re-running. Synonyms are fixed-string
   replacements applied in table order (components, then `full_address`); the script logs which fired
   (`synonyms_component`, `synonyms_full_address`, `full_address_raw` columns), prints how many entries
   were modified / ambiguous (synonym with several canonical names, or chained replacements) and writes
   `output/synonym_changes_for_review.csv`.
3. `R/03_geocode.R` — geocodes each unique `full_address` via `tidygeocoder` (OSM/Nominatim), caching
   results in `output/geocoding_cache.rds` keyed by `full_address` so re-runs only geocode new addresses
   → `output/geocoded_attendee_data.rds`.
4. `R/04_all_attendees_maps.R` — world maps/heatmaps of attendee distribution per year and combined, plus
   inset bar plots and regional-percentage tables.
5. `R/05_monocentric_footprint.R` — footprint if everyone travels to that year's single real conference
   city → `output/monocentric_footprint_detailed.rds` and `output/reports/monocentric_footprint_summary.csv`.
6. `R/06_multicentric_footprint.R` — footprint if each attendee instead travels to their nearest of the
   four hubs (Glasgow/Montreal/Seoul/Brisbane) → `output/multicentric_footprint_detailed.rds` /
   `output/reports/multicentric_footprint_summary.csv`.
   - `R/05b_monocentric_footprint_train.R` / `R/06b_multicentric_footprint_train.R` — train-share sensitivity
     versions of `05_`/`06_` (originals untouched). Sweep `train_sensitivity_grid` (train share × max train km ×
     low/central/high rail emission factor) with `calculate_footprint_train()` →
     `output/reports/{mono,multi}centric_train_sensitivity.csv`, `*_train_detailed.rds` (central scenario),
     `output/figures/*_train_sensitivity.png`. `06b_` also writes `multi_vs_mono_train_sensitivity.csv` and
     needs `05b_`'s CSV, so run `05b_` first. Share = 0 reproduces the plane-only `05_`/`06_` totals exactly.
7. `R/07_multicentric_benchmark.R` — runs `calculate_multicentric_footprint()` across a matrix of
   hypothetical hub-set **scenarios** (1, 2, 3, 4, 6 hubs, defined in a `scenarios` list) to compare total
   and per-attendee footprint as a function of number/placement of hubs →
   `output/reports/multicentric_benchmark_summary.csv` + several figures under `output/figures/`.
8. `R/08_attraction_power.R` — for each conference city, counts attendees within `radius_km` and computes
   an "attraction power" ratio (attendance in the year that city hosted vs. mean attendance in other years)
   to quantify how much host-city proximity inflates attendance.
9. `R/09_interactive_map.R` — builds a self-contained interactive plotly HTML map (attendee → hub travel
   lines with hover tooltips) from the monocentric footprint data → `output/interactive_attendee_map.html`.
10. `R/10_greedy_hub_selection.R` — greedy hub search over `maps::world.cities` (pop > `min_population`,
   coordinates taken from that dataset, not the geocoding cache). `greedy_hubs()` is run on all years
   pooled and then on each conference year separately (`sample` column). Greedy selection of `n_hubs` hubs:
   at step k the k-1 hubs already chosen are kept and every candidate city (pool restricted per step by `hub_countries[[k]]`; NULL = world) is tried as an extra hub, assigning
   each attendee to the nearest hub. Distances are computed once as a vectorized `geosphere::distm` matrix,
   not with `calculate_multicentric_footprint()`, which would be too slow for ~2000 cities.
   → `output/reports/greedy_hub_candidates_<all_years|year>.csv`, `output/reports/greedy_hub_sequence.csv`,
   `output/figures/greedy_hub_maps/greedy_<sample>_hub<k>_map.png`, `output/figures/greedy_hub_sequence.png`.
11. `R/11_openalex_community.R` — maps the neuroimaging community: research articles 2020–2024 in 9 core
   journals (by ISSN-L) counted per institution via the OpenAlex API (needs `OPENALEX_API_KEY` in
   `~/.Renviron` for cursor paging), cached in `data/openalex_neuroimaging_institutions.csv` (delete it to
   re-fetch). Assigns countries/UN regions with `countrycode` → `output/community_institutions.rds`,
   `output/attendees_with_regions.rds`, community-vs-attendee shares by subregion.
12. `R/12_participation_model.R` — fits how attendance per article decays with distance to the host,
   using country × year counts for 2023–25 (expected count = year level × Σ_i C_i·exp(β·g(d_iy))). Compares
   offset vs. country-fixed-effects models, power vs. exponential decay, quasi-Poisson vs. NB, and a
   leave-one-year-out check → `output/participation_model.rds` (primary β from fixed effects, sensitivity β
   from the offset model).
13. `R/13_equitable_hub_selection.R` — for k = 1..6 hubs among cities ≥ 1M inhabitants, searches hub sets
   (greedy + Teitz–Bart swaps, vectorized over candidates) trading expected footprint (with induced demand)
   against mean regional coverage (each of 6 world regions' community share within 3,500 km, regions weighted
   equally), with min-catchment (150, or 50 in under-represented regions) and attendance-floor constraints;
   ε-dominance (0.5 pt) on the front;
   Pareto front, knee solutions, benchmarks, exhaustive k = 2 check, bootstrap stability. Slow (~1 h+).
   → `output/reports/equitable_hub_*.csv`, `output/figures/equitable_hub_*.png`, `output/equitable_hub_results.rds`.
   - `R/13b_equitable_hub_selection_pop250k.R` — city-size sensitivity of `13_`: hosts ≥ 250k inhabitants
     (1,565 cities instead of 313). It does not copy `13_`: it sets `hub_selection_overrides`
     (`eligible_min_pop`, `output_tag`) and sources `13_` inside `local()`; `13_` reads those through
     `get0()` and names every output with `output_path()`, so outputs get the `_pop250k` suffix. Several hours.
     Any new output in `13_` must go through `output_path()` so 13b doesn't overwrite it.

Methods, formulas, decisions and references for 11–13 are in `docs/methods_hub_selection.md` — keep it in
sync when changing those scripts. `README.md` has the user-facing pipeline table and headline results; update
it when adding a script or when results change.

`R/utils.R` is sourced (`source("R/utils.R")`) by every downstream script and holds all shared state and
logic — **read it first** when touching more than one script:
- `radius_km` (4500) and `conference_hubs` (the canonical Glasgow/Montreal/Seoul/Brisbane × year tibble)
  are defined here as project-wide constants, not passed as parameters.
- `calculate_footprint()` — the core emissions model: zero for trips under `local_travel_threshold_km`
  (100 km), then a distinct kg CO2e/km round-trip factor for short/medium/long haul bands (thresholds at
  1000 km and 3500 km). Changing emission factors or thresholds here affects every script downstream.
- Train/plane split (`05b_`/`06b_` only): `attendee_country()` gives the ISO3 code from coordinates
  (`maps::map.where`, falling back to `countrycode` on `country_clean`; Northern Ireland = `"GBR_NI"`).
  `rail_zone()` and the `rail_zones` list define land-connected zones: Europe incl. GB, USA+CAN, CHN+HKG+MAC,
  IRL+NI; every other country is its own zone. `rail_eligible()` requires 100 km ≤ d ≤ `max_train_km` and the
  same zone. `calculate_footprint_train()` returns the expected footprint: share × rail
  (d × 1.2 detour × origin-country factor × 2) + (1 − share) × `calculate_footprint()`.
  `plot_train_sensitivity()` draws the shared figure style.
- `get_geocoding_cache()` / `update_geocoding_cache()` — the geocoding cache is the source of truth for
  coordinates of both attendees and hubs; it is additive-only (never pruned) and keyed on `full_address`.
- `get_hub_coordinates()` — geocodes/caches hub city names on demand; used whenever a script needs
  coordinates for a *named set* of hubs (as opposed to attendee addresses).
- `calculate_hub_distances()` / `calculate_multicentric_footprint()` — given a set of hub coordinates,
  assigns each attendee to their nearest hub and applies `calculate_footprint()`. This is the core routine
  reused by `06_`, `07_`, and (for the "closest hub" columns) `04_`.
- `add_hub_distances()` — for a *fixed* hub list, adds `distance_to_<city>` and `within_<city>` boolean
  columns for every hub (not just the nearest), plus a `closest_hub` column. Used by `04_` and `08_` when
  the question is "which/how many hubs is this attendee near," not just "which hub are they assigned to."
- `distance_decay_g()` — distance transform of the participation model (power: log(1 + d/1000);
  exponential: d/1000), shared by `12_` and `13_`.

## Data flow and conventions

- Everything is keyed on `full_address` (attendee) or a hub name string — there's no persistent geocoding
  service call at analysis time, only cache lookups, so **never delete `output/geocoding_cache.rds`**
  without expecting a slow full re-geocode of every unique address in the dataset.
  `output/unique_locations_for_review.txt` / `output/suspicious_pairs_for_review.csv` are manual QA aids,
  not consumed by later scripts directly — their output feeds back into `data/location_synonyms.csv`.
- `data/location_synonyms.csv` is hand-edited: `synonym,canonical_name,correction_type`. Each synonym appears
  once, and `correction_type` must be one of `misspelling`, `diacritics`, `variant` (other language,
  romanization, spacing/hyphens, abbreviations), `country_name`, `format` (districts, postcodes, wrong or
  duplicated state/country parts) or `wrong_country`; `02_` stops on a missing or unknown type and counts
  corrected entries per type → `output/reports/synonym_corrections_summary.csv`.
- `data/rail_emission_factors.csv` is a hand-edited input (ISO3 → low/central/high kg CO2e per passenger-km,
  one way, plus a `default` row). Each row's `source` field names its origin, basis and any
  derived/proxy/placeholder values; every cell must be filled (no NAs). Countries without a row use `default`.
- `year` is numeric through geocoding, then coerced to a `factor` in `03_geocode.R`'s output; some later
  scripts re-coerce it locally for plotting — check a given script's own handling rather than assuming.
- New conference hubs are added by extending the `conference_hubs` tibble in `utils.R` (`add_row(...)`) —
  this is the one place hub membership is declared; per-script `hub_cities`/`hub_locations` vectors in
  `05_`/`06_`/`07_`/`08_` are separate, sometimes overlapping lists used for specific scenario analyses.
- Figures go to `output/figures/`, tabular summaries to `output/reports/`, intermediate/full datasets as
  `.rds` directly under `output/`. Follow this split for any new output.
- This project uses `renv` (`renv.lock`, `renv/`, R 4.5) for dependency management — restore with
  `renv::restore()` rather than ad hoc `install.packages()`; after adding a package, `renv::snapshot()`.
- `04_`, `05b_`, `06b_`, `07_`, `08_` and `10_`–`13_` each end with a block banner-commented
  `# --- Exploration (not part of the saved pipeline outputs) ---`: everything above the banner feeds a
  `ggsave()`/`write.csv()`/`saveRDS()` call and is the actual pipeline output; everything below is
  console-only (`print()`/`knitr::kable()`) analysis kept for reference. Keep new throwaway
  plots/tables below the banner so the saved-output pipeline stays easy to scan.

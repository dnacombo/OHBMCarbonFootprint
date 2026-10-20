# OHBM meeting carbon footprint analysis

This project estimates the travel carbon footprint of the OHBM annual meeting and compares it with
**multi-hub formats**, where the meeting runs simultaneously in several regional hubs instead of a single city.

It uses attendee locations from three meetings: 2023 Montreal, 2024 Seoul and 2025 Brisbane. It works through
three questions:

1. **What do past meetings emit, and how much would a few fixed hubs save?** Scripts `05_`–`08_`.
2. **Where should hubs be to minimise the travel of past attendees?** Script `10_`, a greedy search over world cities.
3. **Where should hubs be to serve the whole neuroimaging community, balancing footprint against fair
   access for every world region?** Scripts `11_`–`13_`. This step:
   - measures the community from publications;
   - models how attendance falls with distance;
   - traces the trade-off between footprint and regional coverage.

   The full methods, decisions, results and references are in
   [docs/methods_hub_selection.md](docs/methods_hub_selection.md).

## Pipeline

Run the scripts in `R/` in numerical order. Each script reads the outputs of the earlier ones.

| Script | What it does | Main outputs |
|---|---|---|
| `01_data_loading.R` | Reads the attendee spreadsheet (one sheet per year) | `output/raw_attendee_data.rds` |
| `02_collect_synonyms.R` | Cleans and standardises locations using `data/location_synonyms.csv`; lists suspicious names for manual review | `output/cleaned_attendee_data.rds` |
| `03_geocode.R` | Geocodes locations with OpenStreetMap, with a cache so only new addresses are queried | `output/geocoded_attendee_data.rds` |
| `04_all_attendees_maps.R` | Maps of where attendees come from, per year | `output/figures/attendee_world_*` |
| `05_monocentric_footprint.R` | Footprint of each real meeting (everyone travels to the host) | `output/reports/monocentric_footprint_summary.csv` |
| `06_multicentric_footprint.R` | Footprint if attendees go to the nearest of Glasgow, Montreal, Seoul and Brisbane | `output/reports/multicentric_footprint_summary.csv` |
| `07_multicentric_benchmark.R` | Compares hand-picked hub scenarios (1–6 hubs) | `output/reports/multicentric_benchmark_summary.csv` |
| `08_attraction_power.R` | How much a host city boosts local attendance | `output/figures/attraction_power_*` |
| `09_interactive_map.R` | Interactive, self-contained HTML map of attendee travel | `output/interactive_attendee_map.html` |
| `10_greedy_hub_selection.R` | Adds hubs one at a time to minimise past attendees' footprint, pooled and per year | `output/reports/greedy_hub_*`, `output/figures/greedy_hub_*` |
| `11_openalex_community.R` | Maps the neuroimaging community: articles 2020–2024 in 9 core journals per institution (OpenAlex) | `data/openalex_neuroimaging_institutions.csv`, `output/figures/community_*` |
| `12_participation_model.R` | Fits how attendance per article decays with distance to the meeting | `output/participation_model.rds`, `output/figures/participation_*` |
| `13_equitable_hub_selection.R` | Searches 1–6 hub sets trading footprint against regional coverage; Pareto fronts, robustness checks | `output/reports/equitable_hub_*`, `output/figures/equitable_hub_*` |

Shared settings and functions are in `R/utils.R`: the emission factors, hub list and distance functions.

## How to run

1. **R and packages.** The project uses R 4.5 and [renv](https://rstudio.github.io/renv/). Open
   `CarbonFootprint.Rproj` (or start R in the project folder), then run `renv::restore()` to install the
   recorded package versions.
2. **OpenAlex API key** (only for `11_`, only the first time). The community data is cached in
   `data/openalex_neuroimaging_institutions.csv`, so `11_` works offline as long as that file exists. To
   re-download it, get a free key at <https://openalex.org/settings/api> and add the line
   `OPENALEX_API_KEY=<your key>` to `~/.Renviron`. Never commit the key.
3. **Run the scripts in order**, e.g. `source("R/01_data_loading.R")`, or `Rscript R/01_data_loading.R`
   from the project folder.
   - `03_` contacts OpenStreetMap only for addresses not yet in `output/geocoding_cache.rds`. Keep that file.
   - `13_` takes about 50 minutes, because it searches hub sets for two decay-model variants and runs a
     100-replicate bootstrap.

## Repository layout

- `R/`: the analysis scripts, run in numerical order.
- `data/`: inputs:
  - the attendee spreadsheet;
  - the location synonym list;
  - the cached OpenAlex community table.
- `output/`: everything the scripts produce:
  - `figures/`: plots and maps;
  - `reports/`: summary tables (CSV);
  - intermediate datasets (`.rds`) directly under `output/`.
- `docs/methods_hub_selection.md`: methods, decisions, results and references for scripts `11_`–`13_`.
- `grateful-report.html`, `grateful-refs.bib`: citations for the R packages used.
- `renv.lock`, `renv/`: package versions.
- `CLAUDE.md`: notes for the Claude Code assistant on the code structure and conventions.

## Main results so far

- **Past meetings.** Average travel footprint per attendee was 1.25 t CO2e (Montreal 2023), 1.94 t (Seoul 2024)
  and 3.01 t (Brisbane 2025). Source: `output/reports/monocentric_footprint_summary.csv`.
- **Where the community is vs. who comes.** Relative to their share of neuroimaging articles, the host regions
  were over-represented among 2023–25 attendees:

  | Region | Over/under-representation |
  |---|---|
  | Australia/NZ | 2.3× |
  | East Asia | 1.4× |
  | North America | 1.3× |
  | European subregions | 0.33–0.62× |

- **Participation falls with distance.** A lab 10,000 km from the meeting sends about a quarter as many
  attendees per paper as a lab next to the venue (decay slope −0.57; −0.85 in the sensitivity analysis).
- **Multi-hub formats.** Predicted for the whole community, i.e. counting the extra attendees who come because a
  hub is close. Going from one city to 3–4 hubs:
  - roughly halves the expected footprint, e.g. about 2.2 kt CO2e for Brussels, Chicago, Melbourne and
    Nanchang, against 4.6–7.3 kt for a single past host;
  - while expected attendance roughly doubles.
- **Robust regions, flexible cities.** A Western European hub, one or two North American hubs, an East Asian hub
  and a South-East Australian hub are chosen consistently. The exact city within a region is often a near-tie.
- **Inclusion.** A Latin American hub costs little footprint but would be small (about 50 expected attendees).
  Africa cannot be covered from the publishing community alone. Including it is a policy decision rather than
  an optimisation result.

The assumptions and limitations behind these results are discussed in
[docs/methods_hub_selection.md](docs/methods_hub_selection.md).

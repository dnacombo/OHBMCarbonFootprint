# R/13_equitable_hub_selection.R
# Chooses sets of k conference hubs for the neuroimaging community (11_), predicting who would
# attend with the distance-decay model (12_), and traces the trade-off between
#   - expected travel footprint F (to minimise), and
#   - coverage Q = mean over world regions of the share of each region's community within
#     `coverage_km` of a hub (to maximise; every region counts equally),
# subject to: eligible host cities only, a minimum expected attendance per hub (lower in
# under-represented regions), and total expected attendance at least today's. Full explanation: docs/methods_hub_selection.md (M3-M7).

# --- 1. Load Libraries, Data and Parameters ---
library(tidyverse)
library(geosphere)
library(countrycode)
source("R/utils.R")

eligible_min_pop <- 1e6    # host cities must have at least this population
min_hub_attendees <- 150   # each hub must attract at least this many expected attendees...
min_hub_attendees_low <- 50 # ...except hubs in these under-represented regions/subregions:
low_floor_regions <- c("Africa", "Latin America & Caribbean", "Southern Asia", "South-eastern Asia")
coverage_tolerance <- 0.005 # coverage differences below this are treated as ties on the front
coverage_km <- 3500        # "covered" = within this distance of a hub (medium/long-haul boundary)
coverage_km_extra <- c(1000, 2000) # also reported, not optimised
max_hubs <- 6
lambdas <- c(0, 0.05, 0.1, 0.2, 0.3, 0.5, 0.75, 1, 1.5, 2, 3, 5) # weight of coverage vs footprint
epsilons <- seq(0.30, 0.95, by = 0.05) # coverage targets for the epsilon-constraint cross-check
n_random_starts <- 5
n_boot <- 100
set.seed(2025)

community <- readRDS("output/community_institutions.rds")
participation <- readRDS("output/participation_model.rds")
N <- nrow(community)

# Equity regions: UN M49 regions, with the Americas split into Northern and Latin America
to_equity_region <- function(region, subregion) {
  if_else(region == "Americas",
          if_else(subregion == "Northern America", "Northern America", "Latin America & Caribbean"),
          region)
}
community <- community %>% mutate(equity_region = to_equity_region(region, subregion))
equity_regions <- sort(unique(community$equity_region))

# Candidate host cities, plus benchmark cities that may be below the population threshold
city_label <- function(name, country) paste0(name, " (", country, ")")
cities <- maps::world.cities %>%
  as_tibble() %>%
  transmute(city = name, country = country.etc, population = pop,
            latitude = lat, longitude = long, label = city_label(city, country))

benchmark_sets <- list(
  "Montreal (2023 host)" = "Montreal (Canada)",
  "Seoul (2024 host)" = "Seoul (Korea South)",
  "Brisbane (2025 host)" = "Brisbane (Australia)",
  "Glasgow (2022 host)" = "Glasgow (UK)",
  "Past three hosts" = c("Montreal (Canada)", "Seoul (Korea South)", "Brisbane (Australia)"),
  "Past four hosts" = c("Glasgow (UK)", "Montreal (Canada)", "Seoul (Korea South)", "Brisbane (Australia)"),
  "Six hubs (07_)" = c("Glasgow (UK)", "Montreal (Canada)", "Sao Paulo (Brazil)", "Seoul (Korea South)",
                       "Johannesburg (South Africa)", "Brisbane (Australia)")
)
greedy_10 <- read_csv("output/reports/greedy_hub_sequence.csv", show_col_types = FALSE) %>%
  filter(sample == "All years") %>% arrange(step)
for (k in seq_len(nrow(greedy_10))) {
  benchmark_sets[[sprintf("Greedy 10_, %d hub%s", k, if (k > 1) "s" else "")]] <- greedy_10$label[1:k]
}

sites <- cities %>%
  filter(population >= eligible_min_pop | label %in% unlist(benchmark_sets)) %>%
  distinct(label, .keep_all = TRUE) %>%
  mutate(eligible = population >= eligible_min_pop,
         iso2 = countrycode(country, "country.name", "iso2c"),
         region = countrycode(iso2, "iso2c", "un.region.name", custom_match = c(TW = "Asia", XK = "Europe")),
         subregion = countrycode(iso2, "iso2c", "un.regionsub.name",
                                 custom_match = c(TW = "Eastern Asia", XK = "Southern Europe")),
         equity_region = to_equity_region(region, subregion),
         # minimum expected attendance for a hub in this city
         hub_floor = if_else(equity_region %in% low_floor_regions | subregion %in% low_floor_regions,
                             min_hub_attendees_low, min_hub_attendees))
eligible <- which(sites$eligible)
M <- nrow(sites)
print(sprintf("%d institutions, %d eligible host cities", N, length(eligible)))

# Distances institutions x sites (km)
D <- distm(community[, c("longitude", "latitude")], sites[, c("longitude", "latitude")],
           fun = distHaversine) / 1000

# --- 2. Per-Institution, Per-Site Terms ---
# For institution i assigned to site j: expected attendees E_ij = C_i * kappa * exp(beta * g(d_ij)),
# their footprint E_ij * phi(d_ij) (t CO2e), and coverage weight C_i * [d_ij <= coverage_km].
make_terms <- function(rows, beta, kappa) {
  Dr <- D[rows, , drop = FALSE]
  C <- community$n_works[rows]
  E <- C * kappa * exp(beta * distance_decay_g(Dr, participation$form))
  footprint_kg <- matrix(calculate_footprint(as.vector(Dr)), nrow = nrow(Dr))
  region_onehot <- t(sapply(equity_regions, \(r) as.numeric(community$equity_region[rows] == r)))
  list(D = Dr, E = E, Fp = E * footprint_kg / 1000, Cov = C * (Dr <= coverage_km),
       C = C, Ctot = sum(C), Rg = region_onehot, C_region = as.vector(region_onehot %*% C),
       n = length(rows))
}

# kappa that reproduces the observed mean attendance at the three real hosts
calibrate_kappa <- function(rows, beta) {
  host_sites <- match(c("Montreal (Canada)", "Seoul (Korea South)", "Brisbane (Australia)"), sites$label)
  per_host <- colSums(community$n_works[rows] *
                        exp(beta * distance_decay_g(D[rows, host_sites], participation$form)))
  participation$mean_attendance / mean(per_host)
}

# --- 3. Evaluating Hub Sets ---
# State of a hub set H: each institution's nearest hub and its terms
state_of <- function(Tm, H) {
  if (length(H) == 0) {
    return(list(H = H, d = rep(Inf, Tm$n), a = rep(NA_integer_, Tm$n),
                E = rep(0, Tm$n), Fp = rep(0, Tm$n), Cov = rep(0, Tm$n)))
  }
  a <- max.col(-Tm$D[, H, drop = FALSE], ties.method = "first")
  idx <- cbind(seq_len(Tm$n), H[a])
  list(H = H, d = Tm$D[idx], a = a, E = Tm$E[idx], Fp = Tm$Fp[idx], Cov = Tm$Cov[idx])
}

# Metrics of a single hub set
evaluate_set <- function(Tm, H) {
  S <- state_of(Tm, H)
  covered_extra <- sapply(coverage_km_extra, \(x) sum(Tm$C[S$d <= x]) / Tm$Ctot)
  region_cov <- as.vector(Tm$Rg %*% S$Cov) / Tm$C_region
  hub_att <- sapply(seq_along(H), \(h) sum(S$E[S$a == h]))
  tibble(n_hubs = length(H), hubs = paste(sites$label[H], collapse = "; "),
         footprint_t = sum(S$Fp), attendance = sum(S$E),
         footprint_per_attendee_t = sum(S$Fp) / sum(S$E),
         coverage = mean(region_cov),
         community_coverage = sum(S$Cov) / Tm$Ctot,
         community_coverage_1000km = covered_extra[1], community_coverage_2000km = covered_extra[2],
         worst_region = equity_regions[which.min(region_cov)],
         worst_region_coverage = min(region_cov),
         min_hub_attendance = min(hub_att),
         min_hub_slack = min(hub_att - sites$hub_floor[H]))
}

# All one-hub extensions of a base hub set at once: for every candidate j, the metrics of
# base + {j}. Institution i moves to j only if j is closer than its current hub.
neighbourhood <- function(Tm, S, cand) {
  closer <- Tm$D[, cand, drop = FALSE] < S$d
  dE <- (Tm$E[, cand, drop = FALSE] - S$E) * closer
  dCov <- (Tm$Cov[, cand, drop = FALSE] - S$Cov) * closer
  # slack = expected attendance minus the hub's floor; feasible if every hub has slack >= 0
  min_slack <- colSums(Tm$E[, cand, drop = FALSE] * closer) - sites$hub_floor[cand]
  if (length(S$H) > 0) {
    onehot <- outer(S$a, seq_along(S$H), `==`) * S$E
    kept_att <- crossprod(onehot, !closer) # attendance left at each existing hub
    min_slack <- pmin(min_slack, apply(kept_att - sites$hub_floor[S$H], 2, min))
  }
  region_cov <- (as.vector(Tm$Rg %*% S$Cov) + Tm$Rg %*% dCov) / Tm$C_region
  list(cand = cand,
       F = sum(S$Fp) + colSums((Tm$Fp[, cand, drop = FALSE] - S$Fp) * closer),
       Q = colMeans(region_cov),
       att = sum(S$E) + colSums(dE),
       min_slack = min_slack,
       worst_region_cov = apply(region_cov, 2, min))
}

# --- 4. Objectives and the Pareto Archive ---
# Weighted sum: F / F_ref - lambda * Q.  Epsilon-constraint: F / F_ref, with a large penalty
# when Q < epsilon. F_ref = footprint of the best single hub, so the scores are unit-free.
score <- function(nb, obj, F_ref) {
  if (obj$type == "weighted") nb$F / F_ref - obj$lambda * nb$Q
  else nb$F / F_ref + 100 * pmax(0, obj$epsilon - nb$Q)
}

is_feasible <- function(nb, final) {
  nb$min_slack >= 0 & (!final | nb$att >= participation$mean_attendance)
}

# Non-dominated rows: lower footprint and higher coverage are better
pareto_mask <- function(F, Q) {
  o <- order(F, -Q)
  keep <- logical(length(F))
  best_q <- -Inf
  for (i in o) if (Q[i] > best_q + 1e-12) { keep[i] <- TRUE; best_q <- Q[i] }
  keep
}

archive <- new.env()
archive_reset <- function() archive$rows <- list()
archive_add <- function(base_H, nb, ok) {
  if (!any(ok)) return(invisible())
  keep <- which(ok)[pareto_mask(nb$F[ok], nb$Q[ok])]
  archive$rows[[length(archive$rows) + 1]] <- tibble(
    n_hubs = length(base_H) + 1,
    key = map_chr(nb$cand[keep], \(j) paste(sort(c(base_H, j)), collapse = "-")),
    F = nb$F[keep], Q = nb$Q[keep])
}
# Front with epsilon-dominance (Laumanns et al. 2002): coverage is binned in steps of
# `coverage_tolerance`; within a bin only the lowest footprint is kept, so sets that buy a
# negligible coverage gain with a large footprint increase do not appear on the front.
archive_front <- function() {
  bind_rows(archive$rows) %>%
    distinct(n_hubs, key, .keep_all = TRUE) %>%
    mutate(Q_bin = floor(Q / coverage_tolerance)) %>%
    group_by(n_hubs, Q_bin) %>%
    slice_min(F, n = 1, with_ties = FALSE) %>%
    group_by(n_hubs) %>%
    filter(pareto_mask(F, Q_bin)) %>%
    ungroup() %>%
    select(-Q_bin)
}

# --- 5. Search: Greedy Construction + Teitz-Bart Swaps ---
# Greedy: add the best feasible hub one at a time. Teitz-Bart: repeatedly apply the best single
# swap (remove one hub, add one candidate) until no swap improves the score: a local optimum.
search_hubs <- function(Tm, k, obj, F_ref, start = NULL, record = TRUE) {
  H <- start
  if (is.null(H)) {
    H <- integer()
    for (s in seq_len(k)) {
      nb <- neighbourhood(Tm, state_of(Tm, H), setdiff(eligible, H))
      ok <- is_feasible(nb, final = s == k)
      if (record) archive_add(H, nb, ok)
      if (!any(ok)) return(NULL)
      sc <- score(nb, obj, F_ref)
      H <- c(H, nb$cand[ok][which.min(sc[ok])])
    }
  }
  current <- evaluate_set(Tm, H)
  current_score <- score(list(F = current$footprint_t, Q = current$coverage), obj, F_ref)
  repeat {
    best <- list(score = current_score - 1e-10)
    for (r in seq_along(H)) {
      base <- H[-r]
      nb <- neighbourhood(Tm, state_of(Tm, base), setdiff(eligible, H))
      ok <- is_feasible(nb, final = TRUE)
      if (record) archive_add(base, nb, ok)
      if (!any(ok)) next
      sc <- score(nb, obj, F_ref)
      i <- which(ok)[which.min(sc[ok])]
      if (sc[i] < best$score) best <- list(score = sc[i], H = c(base, nb$cand[i]))
    }
    if (is.null(best$H)) break
    H <- best$H
    current_score <- best$score
  }
  list(H = sort(H), score = current_score)
}

random_feasible_start <- function(Tm, k, tries = 200) {
  for (t in seq_len(tries)) {
    H <- sample(eligible, k)
    m <- evaluate_set(Tm, H)
    if (m$min_hub_slack >= 0 && m$attendance >= participation$mean_attendance) return(H)
  }
  NULL
}

# Full search for one participation scenario
run_scenario <- function(scenario) {
  pm <- participation[[scenario]]
  Tm <- make_terms(seq_len(N), pm$beta, pm$kappa)
  singles <- neighbourhood(Tm, state_of(Tm, integer()), eligible)
  F_ref <- min(singles$F[is_feasible(singles, final = TRUE)])
  archive_reset()
  runs <- list()
  for (k in seq_len(max_hubs)) {
    for (lambda in lambdas) {
      obj <- list(type = "weighted", lambda = lambda)
      res <- search_hubs(Tm, k, obj, F_ref)
      if (is.null(res)) next
      restarts <- map_dbl(seq_len(n_random_starts), \(s) {
        start <- random_feasible_start(Tm, k)
        if (is.null(start)) return(NA_real_)
        search_hubs(Tm, k, obj, F_ref, start = start)$score
      })
      runs[[length(runs) + 1]] <- tibble(
        scenario = scenario, n_hubs = k, objective = "weighted", lambda = lambda, epsilon = NA_real_,
        key = paste(res$H, collapse = "-"), score = res$score,
        restarts_reaching_best = sum(abs(restarts - res$score) < 1e-8, na.rm = TRUE),
        restarts_better = sum(restarts < res$score - 1e-8, na.rm = TRUE),
        restarts_run = sum(!is.na(restarts)))
    }
    for (epsilon in epsilons) {
      res <- search_hubs(Tm, k, list(type = "epsilon", epsilon = epsilon), F_ref)
      if (is.null(res)) next
      runs[[length(runs) + 1]] <- tibble(
        scenario = scenario, n_hubs = k, objective = "epsilon", lambda = NA_real_, epsilon = epsilon,
        key = paste(res$H, collapse = "-"), score = res$score)
    }
    print(sprintf("[%s] k = %d done", scenario, k))
  }
  front <- archive_front()
  list(Tm = Tm, F_ref = F_ref, runs = bind_rows(runs), front = front)
}

key_to_H <- function(key) as.integer(strsplit(key, "-")[[1]])

# Knee of a front: the point farthest from the chord joining its two ends, after scaling
# footprint and coverage to [0, 1]
knee_index <- function(F, Q) {
  if (length(F) < 3) return(which.min(F))
  f <- (F - min(F)) / diff(range(F))
  q <- (Q - min(Q)) / diff(range(Q))
  # ends: lowest footprint (0, q_a) and highest coverage (1, 1) in (f, q) space
  a <- c(f[which.min(F)], q[which.min(F)])
  b <- c(f[which.max(Q)], q[which.max(Q)])
  dist <- abs((b[1] - a[1]) * (a[2] - q) - (a[1] - f) * (b[2] - a[2])) / sqrt(sum((b - a)^2))
  which.max(dist)
}

# --- 6. Run Both Participation Scenarios ---
results <- purrr::map(set_names(c("primary", "sensitivity")), run_scenario)

pareto <- imap_dfr(results, \(res, scenario) {
  res$front %>%
    group_by(n_hubs) %>%
    mutate(is_knee = row_number() == knee_index(F, Q)) %>%
    ungroup() %>%
    mutate(metrics = purrr::map(key, \(k) evaluate_set(res$Tm, key_to_H(k)) %>% select(-n_hubs))) %>%
    unnest(metrics) %>%
    mutate(scenario = scenario, .before = 1) %>%
    select(-F, -Q)
}) %>%
  arrange(scenario, n_hubs, footprint_t)

benchmarks <- imap_dfr(results, \(res, scenario) {
  imap_dfr(benchmark_sets, \(labels, name) {
    evaluate_set(res$Tm, match(labels, sites$label)) %>%
      mutate(scenario = scenario, benchmark = name, .before = 1)
  })
})

# --- 7. Search Checks ---
# (a) exhaustive search over all pairs of eligible cities for k = 2 vs. the heuristic
exhaustive_pairs <- function(res, obj) {
  best <- Inf
  for (j in eligible) {
    nb <- neighbourhood(res$Tm, state_of(res$Tm, j), eligible[eligible > j])
    ok <- is_feasible(nb, final = TRUE)
    if (any(ok)) best <- min(best, min(score(nb, obj, res$F_ref)[ok]))
  }
  best
}
search_checks <- imap_dfr(results, \(res, scenario) {
  map_dfr(c(0, 0.5, 2), \(lambda) {
    run <- res$runs %>% filter(objective == "weighted", n_hubs == 2, lambda == !!lambda)
    tibble(scenario = scenario, check = "k = 2: heuristic vs exhaustive", lambda = lambda,
           heuristic_score = run$score, exhaustive_score = exhaustive_pairs(res, list(type = "weighted", lambda = lambda)))
  }) %>%
    mutate(match = abs(heuristic_score - exhaustive_score) < 1e-8)
})
restart_checks <- bind_rows(purrr::map(results, "runs")) %>%
  filter(objective == "weighted") %>%
  group_by(scenario, n_hubs) %>%
  summarise(searches = n(), restarts = sum(restarts_run),
            restarts_reaching_best = sum(restarts_reaching_best),
            restarts_finding_better = sum(restarts_better), .groups = "drop")

# --- 8. Bootstrap Stability (primary scenario) ---
# Each replicate resamples institutions with replacement and draws beta from its estimated
# sampling distribution, re-calibrates kappa, and re-runs the search at each k with the lambda
# whose solution is closest to the knee. Output: how often each city is selected.
knee_lambda <- results$primary$runs %>%
  filter(objective == "weighted") %>%
  left_join(pareto %>% filter(scenario == "primary", is_knee) %>%
              select(n_hubs, knee_fp = footprint_t, knee_cov = coverage), by = "n_hubs") %>%
  mutate(metrics = purrr::map(key, \(k) evaluate_set(results$primary$Tm, key_to_H(k)))) %>%
  unnest(metrics, names_sep = "_") %>%
  mutate(gap = abs(metrics_footprint_t - knee_fp) / knee_fp + abs(metrics_coverage - knee_cov)) %>%
  group_by(n_hubs) %>%
  slice_min(gap, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(n_hubs, lambda)

boot_selection <- map_dfr(seq_len(n_boot), \(b) {
  rows <- sample(N, N, replace = TRUE)
  beta_b <- rnorm(1, participation$primary$beta, participation$primary$se_beta)
  Tm <- make_terms(rows, beta_b, calibrate_kappa(rows, beta_b))
  singles <- neighbourhood(Tm, state_of(Tm, integer()), eligible)
  F_ref <- min(singles$F[is_feasible(singles, final = TRUE)])
  map_dfr(seq_len(nrow(knee_lambda)), \(r) {
    k <- knee_lambda$n_hubs[r]
    obj <- list(type = "weighted", lambda = knee_lambda$lambda[r])
    # greedy start, plus two random starts from k = 4 on (where local optima appear); keep the best
    candidates <- list(search_hubs(Tm, k, obj, F_ref, record = FALSE))
    if (k >= 4) {
      for (s in 1:2) {
        start <- random_feasible_start(Tm, k)
        if (!is.null(start)) candidates <- c(candidates, list(search_hubs(Tm, k, obj, F_ref, start = start, record = FALSE)))
      }
    }
    candidates <- purrr::compact(candidates)
    if (length(candidates) == 0) return(NULL)
    res <- candidates[[which.min(map_dbl(candidates, "score"))]]
    tibble(replicate = b, n_hubs = knee_lambda$n_hubs[r], hub = sites$label[res$H])
  })
})
boot_frequency <- boot_selection %>%
  count(n_hubs, hub, name = "times_selected") %>%
  mutate(frequency = times_selected / n_boot) %>%
  arrange(n_hubs, desc(frequency))

# --- 9. Save Results ---
write.csv(pareto %>% select(-key), "output/reports/equitable_hub_pareto.csv", row.names = FALSE)
write.csv(benchmarks, "output/reports/equitable_hub_benchmarks.csv", row.names = FALSE)
write.csv(bind_rows(search_checks, restart_checks %>% mutate(check = "random restarts")),
          "output/reports/equitable_hub_search_checks.csv", row.names = FALSE)
write.csv(boot_frequency, "output/reports/equitable_hub_bootstrap.csv", row.names = FALSE)

# --- 10. Figures ---
short_hubs <- function(h) str_replace_all(h, " \\([^)]*\\)", "")

plot_front <- function(scen, title) {
  fr <- pareto %>% filter(scenario == scen)
  bm <- benchmarks %>% filter(scenario == scen, n_hubs <= max_hubs, !str_detect(benchmark, "Greedy"))
  ggplot(fr, aes(coverage, footprint_t / 1000)) +
    geom_line(color = "#2171b5", linewidth = 0.6) +
    geom_point(color = "#2171b5", size = 1.8) +
    geom_point(data = filter(fr, is_knee), shape = 21, size = 4.5, stroke = 1.1, color = "black") +
    geom_label(data = filter(fr, is_knee), aes(label = str_wrap(short_hubs(hubs), 24)),
               size = 2.6, hjust = 1, nudge_x = -0.03, lineheight = 0.9, label.size = 0.15) +
    geom_point(data = bm, shape = 17, size = 2.6, color = "#d94801") +
    geom_text(data = bm, aes(label = benchmark), size = 2.5, color = "#d94801", vjust = -0.9) +
    facet_wrap(~n_hubs, nrow = 2, scales = "free_y",
               labeller = labeller(n_hubs = \(x) paste(x, "hub(s)"))) +
    scale_x_continuous(labels = scales::percent, limits = c(0, 1.02)) +
    scale_y_continuous(expand = expansion(mult = 0.12)) +
    coord_cartesian(clip = "off") +
    labs(title = title,
         subtitle = sprintf(paste0("Blue: Pareto-optimal hub sets (ringed: knee). Orange: past hosts and 07_ scenarios. ",
                                   "Coverage = mean over 6 world regions of the community share within %s km of a hub"),
                            format(coverage_km, big.mark = ",")),
         x = "Mean regional coverage", y = "Expected travel footprint (kt CO2e)") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold"))
}
plot_front("primary", "Footprint vs. coverage of the neuroimaging community (decay slope from fixed effects)")
ggsave("output/figures/equitable_hub_pareto.png", width = 15, height = 9, dpi = 200)
plot_front("sensitivity", "Footprint vs. coverage, sensitivity case (steeper decay slope from the offset model)")
ggsave("output/figures/equitable_hub_pareto_sensitivity.png", width = 15, height = 9, dpi = 200)

# Knee solutions on maps: institutions shaded by distance to their nearest hub
world_map <- map_data("world")
knees <- pareto %>% filter(scenario == "primary", is_knee)
knee_points <- map_dfr(seq_len(nrow(knees)), \(r) {
  S <- state_of(results$primary$Tm, key_to_H(knees$key[r]))
  community %>%
    select(longitude, latitude, n_works) %>%
    mutate(n_hubs = knees$n_hubs[r], distance = S$d)
}) %>%
  mutate(band = cut(distance, c(-Inf, 1000, coverage_km, Inf),
                    labels = c("< 1,000 km", sprintf("1,000-%s km", format(coverage_km, big.mark = ",")),
                               sprintf("> %s km", format(coverage_km, big.mark = ",")))))
knee_hubs <- knees %>%
  mutate(H = purrr::map(key, key_to_H)) %>%
  select(n_hubs, H) %>%
  unnest(H) %>%
  mutate(city = sites$city[H], longitude = sites$longitude[H], latitude = sites$latitude[H])
facet_titles <- knees %>%
  transmute(n_hubs, title = sprintf("%d hub%s: %.0f%% mean regional coverage, %.1f kt CO2e", n_hubs,
                                    if_else(n_hubs > 1, "s", ""), 100 * coverage, footprint_t / 1000))

ggplot() +
  geom_polygon(data = world_map, aes(long, lat, group = group), fill = "gray92", color = "white", linewidth = 0.1) +
  geom_point(data = arrange(knee_points, desc(distance)), aes(longitude, latitude, size = n_works, color = band),
             alpha = 0.7) +
  geom_point(data = knee_hubs, aes(longitude, latitude), shape = 23, fill = "white", color = "black",
             size = 3, stroke = 0.9) +
  geom_text(data = knee_hubs, aes(longitude, latitude, label = city), size = 2.6, vjust = -1.1, fontface = "bold") +
  scale_color_manual(values = c("#9ecae1", "#4292c6", "#08306b"), name = "Distance to nearest hub") +
  scale_size_area(max_size = 3, guide = "none") +
  facet_wrap(~n_hubs, ncol = 2, labeller = labeller(n_hubs = set_names(facet_titles$title, facet_titles$n_hubs))) +
  coord_quickmap(xlim = c(-170, 180), ylim = c(-50, 72), expand = FALSE) +
  labs(title = "Knee solutions: hub sets with the best footprint/coverage compromise for each number of hubs",
       subtitle = "Institutions sized by neuroimaging articles and shaded by distance to their nearest hub") +
  theme_void() +
  theme(plot.title = element_text(face = "bold"), legend.position = "top",
        plot.background = element_rect(fill = "white", color = NA),
        strip.text = element_text(face = "bold", margin = margin(b = 3)))
ggsave("output/figures/equitable_hub_knee_maps.png", width = 15, height = 10.5, dpi = 200)

# Bootstrap selection frequency
boot_frequency %>%
  group_by(n_hubs) %>%
  slice_max(frequency, n = 8, with_ties = FALSE) %>%
  ungroup() %>%
  # order bars within each panel: make labels unique per panel, then strip the suffix
  mutate(hub = fct_reorder(paste0(short_hubs(hub), "___", n_hubs), frequency)) %>%
  ggplot(aes(frequency, hub)) +
  geom_col(fill = "#2171b5", width = 0.7) +
  geom_vline(xintercept = 0.8, linetype = "dashed", color = "gray40") +
  scale_y_discrete(labels = \(x) sub("___.*", "", x)) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
  facet_wrap(~n_hubs, scales = "free_y", nrow = 2, labeller = labeller(n_hubs = \(x) paste(x, "hub(s)"))) +
  labs(title = "How robust are the knee solutions? Selection frequency over bootstrap replicates",
       subtitle = sprintf("%d replicates resampling institutions and the decay slope; dashed line: 80%%", n_boot),
       x = "Share of replicates in which the city is a hub", y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))
ggsave("output/figures/equitable_hub_bootstrap.png", width = 14, height = 8, dpi = 200)

# Results kept for the methods document and further exploration
saveRDS(list(pareto = pareto, benchmarks = benchmarks, runs = bind_rows(purrr::map(results, "runs")),
             search_checks = search_checks, restart_checks = restart_checks,
             knee_lambda = knee_lambda, boot_frequency = boot_frequency,
             parameters = list(eligible_min_pop = eligible_min_pop, min_hub_attendees = min_hub_attendees,
                               min_hub_attendees_low = min_hub_attendees_low, low_floor_regions = low_floor_regions,
                               coverage_tolerance = coverage_tolerance,
                               coverage_km = coverage_km, lambdas = lambdas, epsilons = epsilons,
                               n_random_starts = n_random_starts, n_boot = n_boot)),
        "output/equitable_hub_results.rds")

# ============================================================
# --- Exploration (not part of the saved pipeline outputs) ---
# ============================================================

pareto %>%
  filter(is_knee) %>%
  select(scenario, n_hubs, hubs, footprint_t, attendance, coverage, community_coverage, worst_region,
         worst_region_coverage) %>%
  mutate(hubs = short_hubs(hubs)) %>%
  knitr::kable(digits = 3, caption = "Knee solutions") %>%
  print()
benchmarks %>%
  select(scenario, benchmark, footprint_t, attendance, coverage, community_coverage, worst_region,
         worst_region_coverage, min_hub_attendance) %>%
  knitr::kable(digits = 3, caption = "Benchmarks under the same model") %>%
  print()
print(knitr::kable(search_checks, digits = 6, caption = "k = 2: heuristic vs. exhaustive"))
print(knitr::kable(restart_checks, caption = "Random restarts"))

# R/12_participation_model.R
# Estimates how participation in OHBM decays with travel distance, using the 2023-2025
# meetings (Montreal, Seoul, Brisbane) and the neuroimaging community from 11_.
#
# Model (docs/methods_hub_selection.md, section M2): each institution i sends attendees in
# proportion to its article count C_i, discounted by its distance d_iy to year y's host:
#   A_cy ~ Poisson or negative binomial with mean
#   mu_cy = exp(alpha_y) * sum_{i in country c} C_i * exp(beta * g(d_iy))
# Only country totals A_cy are observed. g() is distance_decay_g() in utils.R.

# --- 1. Load Libraries and Data ---
library(tidyverse)
library(geosphere)
source("R/utils.R")

community <- readRDS("output/community_institutions.rds")
attendees <- readRDS("output/attendees_with_regions.rds") %>%
  mutate(year = as.numeric(as.character(year)))
model_years <- c(2023, 2024, 2025)
decay_forms <- c("power", "exponential")

hosts <- conference_hubs %>%
  filter(year %in% model_years) %>%
  arrange(year) %>%
  left_join(get_hub_coordinates(.$city), by = c("city" = "hub_city"))

# --- 2. Country x Year Data ---
countries <- sort(unique(community$country_code))
country_of_inst <- match(community$country_code, countries)

country_info <- community %>%
  group_by(country_code) %>%
  summarise(C = sum(n_works), region = first(region), subregion = first(subregion)) %>%
  arrange(match(country_code, countries))

# Attendees from countries with no institution in the community data cannot be modelled
excluded <- attendees %>% filter(!country_code %in% countries) %>% count(country_code, year)
if (nrow(excluded) > 0) {
  warning(sprintf("%d attendees from countries with no community institutions are excluded: %s",
                  sum(excluded$n), paste(unique(excluded$country_code), collapse = ", ")))
}

A <- attendees %>%
  filter(country_code %in% countries, year %in% model_years) %>%
  mutate(country_code = factor(country_code, levels = countries),
         year = factor(year, levels = model_years)) %>%
  xtabs(~ country_code + year, data = .) %>%
  unclass()

# Distance from every institution to each year's host (km)
d_km <- distm(community[, c("longitude", "latitude")],
              hosts[, c("longitude", "latitude")], fun = distHaversine) / 1000

# S_cy(beta) = sum over institutions of country c of C_i * exp(beta * g(d_iy))
country_sums <- function(beta, form, years = seq_along(model_years)) {
  sapply(years, \(y) {
    rowsum(community$n_works * exp(beta * distance_decay_g(d_km[, y], form)),
           country_of_inst, reorder = TRUE)[, 1]
  })
}

# --- 3. Model Fitting ---
# Poisson negative log-likelihood (constant terms dropped); par = c(alpha_years, beta)
pois_negll <- function(par, form, years) {
  n_y <- length(years)
  mu <- sweep(country_sums(par[n_y + 1], form, years), 2, exp(par[1:n_y]), `*`)
  sum(mu - A[, years] * log(mu))
}

# Given beta, the Poisson maximum-likelihood year effects have a closed form
profile_alpha <- function(beta, form, years) {
  log(colSums(A[, years, drop = FALSE]) / colSums(country_sums(beta, form, years)))
}

fit_poisson <- function(form, years = seq_along(model_years)) {
  beta_hat <- optimize(\(b) pois_negll(c(profile_alpha(b, form, years), b), form, years),
                       interval = c(-10, 2))$minimum
  par <- c(profile_alpha(beta_hat, form, years), beta_hat)
  vcov <- solve(optimHess(par, pois_negll, form = form, years = years))
  mu <- sweep(country_sums(beta_hat, form, years), 2, exp(par[1:length(years)]), `*`)
  loglik <- sum(dpois(A[, years], mu, log = TRUE))
  n <- length(mu)
  dispersion <- sum((A[, years] - mu)^2 / mu) / (n - length(par))
  list(form = form, family = "Poisson", alpha = par[1:length(years)], beta = beta_hat,
       se_beta = sqrt(vcov[length(par), length(par)]),
       se_beta_quasi = sqrt(vcov[length(par), length(par)] * dispersion),
       vcov = vcov, loglik = loglik, aic = 2 * length(par) - 2 * loglik,
       dispersion = dispersion, theta = NA_real_, mu = mu)
}

# Negative binomial: same mean, variance mu + mu^2 / theta; par = c(alpha_years, beta, log theta)
nb_negll <- function(par, form) {
  mu <- sweep(country_sums(par[4], form), 2, exp(par[1:3]), `*`)
  -sum(dnbinom(A, size = exp(par[5]), mu = mu, log = TRUE))
}

fit_negbin <- function(form, start) {
  opt <- optim(c(start$alpha, start$beta, 0), nb_negll, form = form, method = "BFGS",
               control = list(maxit = 500))
  vcov <- solve(optimHess(opt$par, nb_negll, form = form))
  mu <- sweep(country_sums(opt$par[4], form), 2, exp(opt$par[1:3]), `*`)
  list(form = form, family = "Negative binomial", alpha = opt$par[1:3], beta = opt$par[4],
       se_beta = sqrt(vcov[4, 4]), se_beta_quasi = NA_real_, vcov = vcov[1:4, 1:4],
       loglik = -opt$value, aic = 2 * 5 + 2 * opt$value, dispersion = NA_real_,
       theta = exp(opt$par[5]), mu = mu, converged = opt$convergence == 0)
}

# Country fixed effects instead of the community offset: beta is then identified only from
# how each country's attendance changes as the host moves, so stable country traits (funding,
# OHBM tradition, how well papers measure the community) cannot bias it. Given (alpha, beta),
# each fixed effect has a closed form, exp(gamma_c) = sum_y A_cy / sum_y exp(alpha_y) S_cy,
# so it is profiled out; par = c(alpha for all years but the first, beta).
fe_mu <- function(par, form, years) {
  keep <- rowSums(A[, years, drop = FALSE]) > 0
  n_y <- length(years)
  S <- sweep(country_sums(par[n_y], form, years), 2, exp(c(0, par[seq_len(n_y - 1)])), `*`)[keep, , drop = FALSE]
  list(mu = S * rowSums(A[keep, years, drop = FALSE]) / rowSums(S),
       A = A[keep, years, drop = FALSE], n_fe = sum(keep))
}
fe_negll <- function(par, form, years) {
  m <- fe_mu(par, form, years)
  sum(m$mu - m$A * log(m$mu))
}

fit_fixed_effects <- function(form, start_beta, years = seq_along(model_years)) {
  opt <- optim(c(rep(0, length(years) - 1), start_beta), fe_negll, form = form, years = years,
               method = "BFGS")
  vcov <- solve(optimHess(opt$par, fe_negll, form = form, years = years))
  m <- fe_mu(opt$par, form, years)
  dispersion <- sum((m$A - m$mu)^2 / m$mu) / (length(m$mu) - length(opt$par) - m$n_fe)
  k <- length(opt$par)
  list(form = form, beta = opt$par[k], se_beta = sqrt(vcov[k, k] * dispersion),
       dispersion = dispersion, n_countries = m$n_fe)
}

# Simpler cross-check: ordinary GLM with each country's community-weighted mean distance
fit_aggregate_glm <- function(form) {
  dbar <- sapply(seq_along(model_years), \(y) rowsum(community$n_works * d_km[, y],
                                                    country_of_inst, reorder = TRUE)[, 1]) /
    country_info$C
  df <- tibble(A = as.vector(A), C = rep(country_info$C, 3),
               year = factor(rep(model_years, each = length(countries))),
               g = distance_decay_g(as.vector(dbar), form))
  fit <- glm(A ~ year + g + offset(log(C)), family = quasipoisson, data = df)
  list(form = form, beta = coef(fit)[["g"]], se_beta = sqrt(vcov(fit)["g", "g"]))
}

fits_pois <- purrr::map(set_names(decay_forms), fit_poisson)
fits_nb <- purrr::map(set_names(decay_forms), \(f) fit_negbin(f, fits_pois[[f]]))
fits_fe <- purrr::map(set_names(decay_forms), \(f) fit_fixed_effects(f, fits_pois[[f]]$beta))
fits_glm <- purrr::map(set_names(decay_forms), fit_aggregate_glm)

model_table <- bind_rows(
  map_dfr(fits_pois, \(f) tibble(
    form = f$form, model = "Offset model (quasi-Poisson)", beta = f$beta, se = f$se_beta_quasi,
    loglik = f$loglik, aic = f$aic, dispersion = f$dispersion)),
  map_dfr(fits_fe, \(f) tibble(
    form = f$form, model = "Country fixed effects (quasi-Poisson)", beta = f$beta, se = f$se_beta,
    dispersion = f$dispersion)),
  map_dfr(fits_nb, \(f) tibble(
    form = f$form, model = "Offset model (negative binomial)", beta = f$beta, se = f$se_beta,
    loglik = f$loglik, aic = f$aic, nb_theta = f$theta,
    # NB does not preserve yearly totals; reported for transparency, not used
    fitted_over_observed_total = sum(f$mu) / sum(A))),
  map_dfr(fits_glm, \(f) tibble(
    form = f$form, model = "Aggregate GLM, country-mean distance (quasi-Poisson)",
    beta = f$beta, se = f$se_beta))
) %>%
  mutate(ci_low = beta - 1.96 * se, ci_high = beta + 1.96 * se)

# Decay form: chosen by Poisson AIC (same likelihood, same number of parameters)
chosen_form <- model_table %>% filter(model == "Offset model (quasi-Poisson)") %>%
  slice_min(aic) %>% pull(form)

# Decay slope: the fixed-effects estimate is primary (identified within countries only); the
# offset-model estimate, which also uses differences between countries, is the sensitivity case.
beta_primary <- fits_fe[[chosen_form]]$beta
se_primary <- fits_fe[[chosen_form]]$se_beta
beta_sensitivity <- fits_pois[[chosen_form]]$beta
print(sprintf("Decay form: %s; beta = %.3f (fixed effects, primary), %.3f (offset, sensitivity)",
              chosen_form, beta_primary, beta_sensitivity))

# --- 4. Leave-One-Year-Out Validation ---
# Fit on two meetings, predict how the third meeting's attendees split across subregions.
# Compared with two predictions that ignore the host location: the pooled attendee shares of
# the two training years, and the community shares. Error = total variation distance
# (half the sum of absolute share differences = fraction of attendees put in the wrong subregion).
subregion_shares <- function(x) tapply(x, country_info$subregion, sum) / sum(x)
tvd <- function(p, q) sum(abs(p - q)) / 2

loyo <- map_dfr(seq_along(model_years), \(h) {
  train <- setdiff(seq_along(model_years), h)
  beta_fe <- fit_fixed_effects(chosen_form, beta_primary, train)$beta
  beta_offset <- fit_poisson(chosen_form, train)$beta
  observed <- subregion_shares(A[, h])
  predictions <- list(
    "Decay model, fixed-effects slope" = subregion_shares(country_sums(beta_fe, chosen_form, h)[, 1]),
    "Decay model, offset slope" = subregion_shares(country_sums(beta_offset, chosen_form, h)[, 1]),
    "Past attendance shares" = subregion_shares(rowSums(A[, train])),
    "Community shares" = subregion_shares(country_info$C)
  )
  imap_dfr(predictions, \(p, name) tibble(
    held_out = paste(model_years[h], hosts$city[h]), predictor = name,
    subregion = names(observed), observed = as.numeric(observed), predicted = as.numeric(p),
    tvd = tvd(observed, p),
    beta_train = case_when(str_detect(name, "fixed") ~ beta_fe,
                           str_detect(name, "offset") ~ beta_offset)))
})

loyo_summary <- loyo %>% distinct(held_out, predictor, tvd, beta_train)

# --- 5. Save Results ---
# Prediction for any meeting configuration: institution i is expected to send
#   E_i = C_i * kappa * exp(beta * g(d_i))
# Year levels are re-estimated with beta fixed (closed form), so each year's predicted total
# equals the observed one; kappa is their average.
model_for <- function(beta) {
  alpha <- profile_alpha(beta, chosen_form, seq_along(model_years))
  list(beta = beta, alpha = set_names(alpha, model_years), kappa = mean(exp(alpha)))
}
participation_model <- list(
  form = chosen_form,
  primary = c(model_for(beta_primary), se_beta = se_primary, source = "country fixed effects"),
  sensitivity = c(model_for(beta_sensitivity), se_beta = fits_pois[[chosen_form]]$se_beta_quasi,
                  source = "offset model"),
  years = model_years,
  hosts = hosts$city,
  mean_attendance = mean(colSums(A))
)
saveRDS(participation_model, "output/participation_model.rds")
write.csv(model_table, "output/reports/participation_model_summary.csv", row.names = FALSE)
write.csv(loyo_summary, "output/reports/participation_model_validation.csv", row.names = FALSE)

# --- 6. Figures ---
# Fitted relative participation exp(beta * g(d)) with 95% band, and observed country-year
# rates (relative to each year's level), plotted at the country's community-weighted mean distance
set.seed(1)
d_grid <- seq(0, 19000, by = 100)
decay_curve <- function(beta, se, label) {
  draws <- rnorm(2000, beta, se)
  tibble(model = label, d = d_grid,
         fit = exp(beta * distance_decay_g(d_grid, chosen_form)),
         low = map_dbl(d_grid, \(d) quantile(exp(draws * distance_decay_g(d, chosen_form)), 0.025)),
         high = map_dbl(d_grid, \(d) quantile(exp(draws * distance_decay_g(d, chosen_form)), 0.975)))
}
# primary (fixed-effects) slope only: this is Figure 4 of the paper
curves <- decay_curve(beta_primary, se_primary,
                      sprintf("Country fixed effects: beta = %.2f", beta_primary))
curve_colors <- set_names("#2171b5", unique(curves$model))

dbar <- sapply(seq_along(model_years), \(y) rowsum(community$n_works * d_km[, y],
                                                  country_of_inst, reorder = TRUE)[, 1]) /
  country_info$C
primary_alpha <- participation_model$primary$alpha
rate <- sweep(A, 2, exp(primary_alpha), `/`) / country_info$C
observed_rates <- tibble(
  country_code = rep(countries, 3), year = rep(model_years, each = length(countries)),
  C = rep(country_info$C, 3), attendees = as.vector(A), dbar = as.vector(dbar),
  rate = as.vector(rate)
) %>% filter(attendees > 0)

# points filled by meeting (fill scale), curve by colour scale; the band has a fixed fill
meeting_labels <- set_names(paste(model_years, c("Montreal", "Seoul", "Brisbane")), model_years)
meeting_fills <- set_names(c("#E69F00", "#009E73", "#CC79A7"), meeting_labels)
ggplot() +
  geom_point(data = observed_rates, aes(dbar, rate, size = C, fill = meeting_labels[as.character(year)]),
             color = "transparent", alpha = 0.5, shape = 21) + # not NA: geom_point drops NA-colour rows
  geom_ribbon(data = curves, aes(d, ymin = low, ymax = high), fill = curve_colors, alpha = 0.2) +
  geom_line(data = curves, aes(d, fit, color = model), linewidth = 1) +
  scale_y_log10() +
  scale_color_manual(values = curve_colors, name = NULL) +
  scale_fill_manual(values = meeting_fills, name = "Meeting") +
  scale_size_area(max_size = 6, name = "Country\narticles") +
  labs(title = "Participation in OHBM decays with distance to the meeting",
       subtitle = paste0("Attendees per article ~ (1 + d/1000)^beta; bands: 95% CI.\n",
                         "Points: countries in 2023-2025 at their mean distance to the host, ",
                         "relative to that year's level"),
       x = "Distance to the meeting (km)", y = "Relative participation (1 = at the venue)") +
  theme_minimal() +
  guides(color = guide_legend(order = 1),
         fill = guide_legend(order = 2, override.aes = list(size = 4, alpha = 1)),
         size = guide_legend(order = 3, override.aes = list(fill = "gray50"))) +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())
ggsave("output/figures/participation_decay.png", width = 12, height = 6, dpi = 200)

predictor_colors <- c("Decay model, fixed-effects slope" = "#2171b5",
                      "Decay model, offset slope" = "#c51b7d",
                      "Past attendance shares" = "#d94801",
                      "Community shares" = "#238b45")
predictor_shapes <- c("Decay model, fixed-effects slope" = 16, "Decay model, offset slope" = 1,
                      "Past attendance shares" = 17, "Community shares" = 15)

loyo %>%
  mutate(facet = sprintf("%s held out", held_out)) %>%
  ggplot(aes(observed, predicted, color = predictor, shape = predictor)) +
  geom_abline(color = "gray60", linetype = "dashed") +
  geom_point(size = 2.5, alpha = 0.85) +
  geom_text(data = loyo_summary %>%
              mutate(facet = sprintf("%s held out", held_out),
                     label = sprintf("%.0f%% misassigned", 100 * tvd),
                     y = 0.62 - 0.06 * (match(predictor, names(predictor_colors)) - 1)),
            aes(x = 0.0005, y = y, label = label), hjust = 0, size = 3, show.legend = FALSE) +
  scale_x_sqrt(labels = scales::percent) +
  scale_y_sqrt(labels = scales::percent) +
  scale_color_manual(values = predictor_colors, name = NULL) +
  scale_shape_manual(values = predictor_shapes, name = NULL) +
  guides(color = guide_legend(nrow = 2), shape = guide_legend(nrow = 2)) +
  facet_wrap(~facet) +
  coord_equal(xlim = c(0, 0.62), ylim = c(0, 0.65)) +
  labs(title = "Out-of-sample check: predicting each meeting's attendance by subregion",
       subtitle = "Each point is a UN subregion; the model is fitted on the two other meetings",
       x = "Observed share of attendees", y = "Predicted share") +
  theme_minimal() +
  theme(legend.position = "top", plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())
ggsave("output/figures/participation_validation.png", width = 12, height = 6, dpi = 200)

# ============================================================
# --- Exploration (not part of the saved pipeline outputs) ---
# ============================================================

print(knitr::kable(model_table, digits = 3, caption = "Distance-decay models"))
print(knitr::kable(loyo_summary %>% select(-beta_train) %>%
                     pivot_wider(names_from = predictor, values_from = tvd),
                   digits = 3, caption = "Leave-one-year-out error (fraction misassigned)"))

# Observed vs. fitted attendees by subregion and year (primary slope)
fitted_primary <- sweep(country_sums(beta_primary, chosen_form), 2, exp(primary_alpha), `*`)
tibble(subregion = rep(country_info$subregion, 3), year = rep(model_years, each = length(countries)),
       observed = as.vector(A), fitted = as.vector(fitted_primary)) %>%
  group_by(year, subregion) %>%
  summarise(observed = sum(observed), fitted = round(sum(fitted)), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = c(observed, fitted)) %>%
  knitr::kable(caption = "Observed vs. fitted attendees by subregion (primary slope)") %>%
  print()

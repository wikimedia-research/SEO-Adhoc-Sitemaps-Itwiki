source("modeling_core.R")
source("data.R")

model_data <- list(
  y = itwiki_pvs$desktop,
  N = nrow(itwiki_pvs),
  T = event_days["sitemap deployment & GSC submission"]
)

models <- expand.grid(
  model_name = names(stan_models),
  p = c(1, 2, 3),
  q = c(0, 1, 2, 3),
  stringsAsFactors = FALSE
) %>%
  dplyr::filter(
    (
      (grepl("{q}", model_name, fixed = TRUE) & q > 0) |
        !grepl("{q}", model_name, fixed = TRUE)
    ),
    (
      (grepl("^AR\\(\\{p\\}\\)", model_name) & q == 0) |
        (grepl("^ARMA\\(\\{p\\}\\,\\{q\\}\\)", model_name) & q > 0)
    )
  ) %>%
  dplyr::distinct() %>%
  dplyr::filter(grepl("regressors", model_name), grepl("\\(v[23]{1}\\)", model_name)) %>%
  # dplyr::filter((p > 1) | (p == 1 & q >= 1)) %>%
  dplyr::arrange(model_name, p, q)

models <- models[23, ]

fits <- pmap_dfr(
  models, fit_stan_model, model_data = model_data, fit_dir = "fits",
  creg = itwiki_pvs$month, dates = itwiki_pvs$date,
  xreg = mlr::createDummyFeatures(itwiki_pvs[, "weekday", drop = FALSE], method = "reference"),
  stan_control = list(adapt_delta = 0.999, max_treedepth = 12), stan_iter = 4e3
)

# Run locally:
if (file.exists("fits_index.csv")) fits <- readr::read_csv("fits_index.csv")

source("summarizing.R")

# estimates and confidence intervals across all considered models:
if (!dir.exists(here("results"))) dir.create(here("results"))
if (file.exists(here("results", "delta0_hpd.csv")) && file.exists(here("results", "mse.csv"))) {
  existing_models <- readr::read_csv(here("results", "delta0_hpd.csv"))$model
} else {
  existing_models <- NULL
}
set_names(as.list(fits$fit_path), fits$model) %>%
  .[setdiff(fits$model, existing_models)] %>%
  purrr::map_df(~ load_and_do(.x, hpd_interval, pars = c("delta0")), .id = "model") %>%
  readr::write_csv(
    here("results", "delta0_hpd.csv"),
    append = file.exists(here("results", "delta0_hpd.csv"))
  )
# Compute MSE:
set_names(as.list(fits$fit_path), fits$model) %>%
  .[setdiff(fits$model, existing_models)] %>%
  purrr::map_df(~ load_and_do(.x, calculate_mse, model_data = model_data), .id = "model") %>%
  readr::write_csv(
    here("results", "mse.csv"),
    append = file.exists(here("results", "mse.csv"))
  )

fit_stan_model(
  "ARMA({p},{q}) w/ levelling-off change & regressors (v3)",
  model_data = model_data, p = 2, q = 1, creg = NULL, dates = itwiki_pvs$date,
  xreg = mlr::createDummyFeatures(itwiki_pvs[, "weekday", drop = FALSE], method = "reference"),
  stan_control = list(adapt_delta = 0.9999, max_treedepth = 20), stan_iter = 8e3, n_chains = 6,
  fit_dir = "final", file_name = "arma21r3ev3.rds"
)

main_parameters <- c("delta0", "omega1", "tau", "sigma", "beta", "gamma_mean", "gamma", "phi", "theta")
fit <- readr::read_rds("final/fit_arma21r3ev3.rds")
rstan::traceplot(fit, pars = main_parameters)
View(rstan::summary(fit, pars = c(main_parameters, "alpha"))$summary)

fit_stan_model(
  "ARMA({p},{q}) w/ levelling-off change & regressors (v3)",
  model_data = model_data, p = 2, q = 1, creg = NULL, dates = itwiki_pvs$date,
  xreg = mlr::createDummyFeatures(itwiki_pvs[, "weekday", drop = FALSE], method = "reference"),
  stan_control = list(adapt_delta = 0.9999, max_treedepth = 25), stan_iter = 2e4, n_chains = 4,
  fit_dir = "final", file_name = "arma21r3ev3-longer.rds"
)

fit <- readr::read_rds("final/fit_arma21r3ev3-longer.rds")
rstan::traceplot(fit, pars = main_parameters) +
  ggplot2::scale_x_continuous(limits = c(15000, 20000))
View(rstan::summary(fit, pars = c(main_parameters, "alpha"))$summary)

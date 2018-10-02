source("modeling_core.R")
options(mc.cores = 1)
source("data.R")

model_data <- list(
  y = itwiki_pvs$desktop,
  N = nrow(itwiki_pvs),
  T = event_days["sitemap deployment & GSC submission"]
)

options(mc.cores = 3)
fit_stan_model(
  "ARMA({p},{q}) w/ Gompertz change & regressors (v2)",
  model_data = model_data, p = 3, q = 2, creg = itwiki_pvs$month,
  xreg = mlr::createDummyFeatures(itwiki_pvs[, "weekday", drop = FALSE], method = "reference"),
  stan_control = list(adapt_delta = 0.999), stan_iter = 1e3, n_chains = 3,
  fit_dir = "test", file_name = "arma32r5ev2.rds"
)

fit_stan_model(
  "ARMA({p},{q}) w/ levelling-off change & regressors (v2)",
  model_data = model_data, p = 3, q = 2, creg = itwiki_pvs$month,
  xreg = mlr::createDummyFeatures(itwiki_pvs[, "weekday", drop = FALSE], method = "reference"),
  stan_control = list(adapt_delta = 0.999), stan_iter = 1e3, n_chains = 3,
  fit_dir = "test", file_name = "arma32r3ev2.rds"
)

options(mc.cores = 4)
fit_stan_model(
  "ARMA({p},{q}) w/ levelling-off change & regressors (v3)",
  model_data = model_data, p = 1, q = 1, creg = NULL, dates = itwiki_pvs$date,
  xreg = mlr::createDummyFeatures(itwiki_pvs[, "weekday", drop = FALSE], method = "reference"),
  stan_control = list(adapt_delta = 0.9999, max_treedepth = 15), stan_iter = 4e3, n_chains = 4,
  fit_dir = "test", file_name = "arma11r3ev3.rds"
)
fit_stan_model(
  "ARMA({p},{q}) w/ levelling-off change & regressors (v3)",
  model_data = model_data, p = 3, q = 2, creg = NULL, dates = itwiki_pvs$date,
  xreg = mlr::createDummyFeatures(itwiki_pvs[, "weekday", drop = FALSE], method = "reference"),
  stan_control = list(adapt_delta = 0.999, max_treedepth = 12), stan_iter = 4e3, n_chains = 4,
  fit_dir = "test", file_name = "arma32r3ev3.rds"
)

fits <- list(
  A = readr::read_rds("test/fit_arma32r5ev2.rds"),
  B = readr::read_rds("test/fit_arma32r3ev2.rds"),
  C = readr::read_rds("test/fit_arma32r3ev3.rds"),
  D = readr::read_rds("test/fit_arma11r3ev3.rds")
)
lmls <- list(
  A = readr::read_rds("test/lml_arma32r5ev2.rds"),
  B = readr::read_rds("test/lml_arma32r3ev2.rds"),
  C = readr::read_rds("test/lml_arma32r3ev3.rds"),
  D = readr::read_rds("test/lml_arma11r3ev3.rds")
)
View(summary(fits$A, pars = c("delta0", "lambda", "d", "sigma", "tau", "beta", "alpha"))$summary)
plot(fits$A, pars = "alpha")
# alpha[1:12] = months (Jan-Dec)
View(summary(fits$B, pars = c("delta0", "omega1", "sigma", "tau", "beta", "alpha"))$summary)
plot(fits$B, pars = "alpha")
View(summary(fits$C, pars = c("delta0", "omega1", "sigma", "tau", "beta", "gamma", "alpha"))$summary)
plot(fits$C, pars = "gamma")

if (file.exists("fits_index.csv")) fits <- readr::read_csv("fits_index.csv")
fits %<>%
  dplyr::filter(model %in% c("ARMA(1,1) w/ levelling-off change & regressors (v3)", "ARMA(2,1) w/ levelling-off change & regressors (v3)"))
lmls <- set_names(fits$lml_path, fits$model) %>%
  purrr::map(readr::read_rds)
lmls %>%
  purrr::map_dbl(~ .x$logml) %>%
  bridgesampling::post_prob(model_names = names(lmls))
bridgesampling::bayes_factor(lmls[[2]], lmls[[1]]) # 1.446: some evidence for ARMA(2,1) over ARMA(1,1)

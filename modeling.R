source("data.R")

library(magrittr)
library(glue)
library(purrr)
library(rstan)
library(bridgesampling)

options(mc.cores = parallel::detectCores())
# options(mc.cores = 1)
rstan_options(auto_write = TRUE)

stan_models <- list(
  # stan_model(here("models", "simple.stan"), model_name = "Simple"),
  stan_model(
    here("models", "ar.stan"),
    model_name = "AR({p}) w/ instant change"
  ),
  stan_model(
    here("models", "ar_regression.stan"),
    model_name = "AR({p}) w/ instant change & regressors"
  ),
  stan_model(
    here("models", "arma.stan"),
    model_name = "ARMA({p},{q}) w/ instant change"
  ),
  stan_model(
    here("models", "arma_regression.stan"),
    model_name = "ARMA({p},{q}) w/ instant change & regressors"
  ),
  stan_model(
    here("models", "ar_p3e.stan"),
    model_name = "AR({p}) w/ levelling-off change"
  ),
  stan_model(
    here("models", "ar_regression_p3e.stan"),
    model_name = "AR({p}) w/ levelling-off change & regressors"
  ),
  stan_model(
    here("models", "arma_p3e.stan"),
    model_name = "ARMA({p},{q}) w/ levelling-off change"
  ),
  stan_model(
    here("models", "arma_regression_p3e.stan"),
    model_name = "ARMA({p},{q}) w/ levelling-off change & regressors"
  ),
  stan_model(
    here("models", "ar_p5e.stan"),
    model_name = "AR({p}) w/ Gompertz change"
  ),
  stan_model(
    here("models", "ar_regression_p5e.stan"),
    model_name = "AR({p}) w/ Gompertz change & regressors"
  ),
  stan_model(
    here("models", "arma_p5e.stan"),
    model_name = "ARMA({p},{q}) w/ Gompertz change"
  ),
  stan_model(
    here("models", "arma_regression_p5e.stan"),
    model_name = "ARMA({p},{q}) w/ Gompertz change & regressors"
  )
)
stan_models %<>% set_names(map_chr(stan_models, ~ .x@model_name))

fit_stan_model <- function(model_name, p, q, ...) {
  # Setup:
  args <- list(...)
  has_regression <- grepl("regressors", model_name)
  # Data for Stan:
  model_data <- list(
    N = max(itwiki_pvs$day),
    T = event_days["sitemap deployment"],
    y = itwiki_pvs$both
  )
  if (p > 0) {
    model_data$p <- p
  }
  if (q > 0) {
    model_data$q <- q
  }
  if (has_regression) {
    x <- mlr::createDummyFeatures(itwiki_pvs[, c("month", "weekday", "is_holiday")], method = "reference")
    model_data$K <- ncol(x); model_data$x <- x
  }
  # Initial estimates as starting points for the model:
  if (has_regression) {
    arima_fit <- arima(model_data$y, c(p, 0, q), include.mean = TRUE, xreg = x)
  } else {
    arima_fit <- arima(model_data$y, c(p, 0, q), include.mean = TRUE)
  }
  arima_coefs <- coefficients(arima_fit)
  inits <- list(
    sigma = sqrt(arima_fit$sigma2), mu = unname(arima_coefs["intercept"]),
    phi = unname(arima_coefs[grepl("^ar[0-9]", names(arima_coefs))])
  )
  if (q > 0) {
    inits$theta <- unname(arima_coefs[grepl("^ma[0-9]", names(arima_coefs))])
  }
  if (has_regression) {
    inits$beta <- unname(arima_coefs[(which(names(arima_coefs) == "intercept") + 1):length(arima_coefs)])
  }
  if (grepl("Gompertz", model_name)) {
    inits$lambda <- 3
    inits$d <- 10
  } else if (grepl("levelling-off", model_name, fixed = TRUE)) {
    inits$omega1 <- 0.6
  }
  # Then we apply a little bit of noise for each chain to have slightly different starting points:
  n_chains <- 4
  starting_points <- replicate(n_chains, map(inits, jitter, amount = 1e-2), simplify = FALSE)
  # Draw samples:
  fit <- sampling(
    stan_models[[model_name]], data = model_data,
    control = list(adapt_delta = 0.999), iter = 6e3,
    chains = n_chains, init = starting_points
  )
  # Log Marginal Likelihood via Bridge Sampling for model comparison:
  lml <- bridge_sampler(fit, silent = TRUE)
  # Save samples and log marginal likelihoods:
  if (!dir.exists("fits")) dir.create("fits")
  fit_name <- glue(model_name)
  if ("file_name" %in% names(args)) {
    fit_file <- args$file_name
  } else {
    fit_file <- tempfile("fit_", tmpdir = "fits", fileext = ".rds")
  }
  lml_file <- sub("fit_", "lml_", fit_file, fixed = TRUE)
  readr::write_rds(fit, fit_file, compress = "gz")
  readr::write_rds(lml, lml_file, compress = "gz")
  # Save model name and pointers into a backup CSV, just in case:
  readr::write_csv(
    dplyr::data_frame(model = fit_name, fit_path = fit_file, lml_path = lml_file),
    "fit_index.csv", append = file.exists("fit_index.csv")
  )
  # Cleanup:
  rm(fit, lml)
  gc() # "It can be useful to call gc after a large object has been removed"

  return(dplyr::data_frame(model = fit_name, fit_path = fit_file, lml_path = lml_file))
}

models <- expand.grid(
  model_name = names(stan_models),
  p = 1:3,
  q = 0:3,
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
  dplyr::filter((p > 1) | (p == 1 & q >= 1)) %>%
  dplyr::arrange(model_name, p, q)

fits <- pmap_dfr(models, fit_stan_model)

# Save the model names and pointers to a TSV:
readr::write_tsv(fits, "fit_index.tsv")

# Run locally:
system("scp -r bearloga@bayesian-intervention-analyzer.eqiad.wmflabs:/home/bearloga/itwiki_sitemaps/fit* ./")
if (file.exists("fit_index.tsv")) fits <- readr::read_tsv("fit_index.tsv") else
  if (file.exists("fit_index.csv")) fits <- readr::read_csv("fit_index.csv")

source("summarizing.R")

# estimates and confidence intervals across all considered models:
if (!dir.exists(here("results"))) dir.create(here("results"))
set_names(as.list(fits$fit_path), fits$model) %>%
  purrr::map_df(~ load_and_do(.x, hpd_interval, pars = c("delta0")), .id = "model") %>%
  readr::write_csv(here("results", "delta0_hpd.csv"))
set_names(as.list(fits$fit_path), fits$model) %>%
  .[grepl("levelling-off", names(.), fixed = TRUE)] %>%
  purrr::map_df(~ load_and_do(.x, hpd_interval, pars = c("omega1")), .id = "model") %>%
  readr::write_csv(here("results", "omega1_hpd.csv"))
set_names(as.list(fits$fit_path), fits$model) %>%
  .[grepl("Gompertz", names(.))] %>%
  purrr::map_df(~ load_and_do(.x, hpd_interval, pars = c("lambda", "d")), .id = "model") %>%
  readr::write_csv(here("results", "lambda_hpd.csv"))

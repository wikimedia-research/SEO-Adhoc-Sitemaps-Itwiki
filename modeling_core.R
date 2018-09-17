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
    here("models", "ar_regression_p5e_v2.stan"),
    model_name = "AR({p}) w/ Gompertz change & regressors (v2)"
  ),
  stan_model(
    here("models", "arma_p5e.stan"),
    model_name = "ARMA({p},{q}) w/ Gompertz change"
  ),
  stan_model(
    here("models", "arma_regression_p5e.stan"),
    model_name = "ARMA({p},{q}) w/ Gompertz change & regressors"
  ),
  stan_model(
    here("models", "arma_regression_p5e_v2.stan"),
    model_name = "ARMA({p},{q}) w/ Gompertz change & regressors (v2)"
  )
)
stan_models %<>% set_names(map_chr(stan_models, ~ .x@model_name))

fit_stan_model <- function(model_name, p, q, model_data, fit_dir, xreg = NULL, n_chains = 4, ...) {
  # model_data is a list with components N, T, y
  # Setup:
  args <- list(...)
  has_regression <- grepl("regressors", model_name)
  # Data for Stan:
  if (p > 0) {
    model_data$p <- p
  }
  if (q > 0) {
    model_data$q <- q
  }
  if (has_regression && !is.null(xreg)) {
    x <- mlr::createDummyFeatures(xreg, method = "reference")
    model_data$K <- ncol(x); model_data$x <- x
  }
  # Initial estimates as starting points for the model:
  constrain <- function(x) {
    # constrain to remain in region of stationarity
    return(sign(x) * pmin(abs(x), 0.99))
  }
  if (has_regression) {
    arima_fit <- arima(model_data$y, c(p, 0, q), include.mean = TRUE, xreg = x)
  } else {
    arima_fit <- arima(model_data$y, c(p, 0, q), include.mean = TRUE)
  }
  arima_coefs <- coefficients(arima_fit)
  phi <- array(constrain(unname(arima_coefs[grepl("^ar[1-9]", names(arima_coefs))])), dim = p)
  inits <- list(sigma = sqrt(arima_fit$sigma2), mu = unname(arima_coefs["intercept"]), phi = phi)
  if (q > 0) {
    inits$theta <- array(constrain(unname(arima_coefs[grepl("^ma[1-9]", names(arima_coefs))])), dim = q)
  }
  if (has_regression) {
    inits$beta <- array(unname(arima_coefs[(which(names(arima_coefs) == "intercept") + 1):length(arima_coefs)]), dim = ncol(x))
  }
  initf <- function() {
    inits <- inits
    if (grepl("Gompertz", model_name)) {
      inits$lambda <- runif(1, 2, 3)
      inits$d <- runif(1, 7, 14)
    } else if (grepl("levelling-off", model_name, fixed = TRUE)) {
      inits$omega1 <- runif(1, 0.5, 0.9)
    }
    return(inits)
  }
  # Then we apply a little bit of noise for each chain to have slightly different starting points:
  # starting_points <- replicate(n_chains, map(inits, jitter, amount = 1e-2), simplify = FALSE)
  # Draw samples:
  fit <- sampling(
    stan_models[[model_name]], data = model_data,
    control = list(adapt_delta = 0.999), iter = 6e3,
    chains = n_chains, init = initf
  )
  # Log Marginal Likelihood via Bridge Sampling for model comparison:
  lml <- bridge_sampler(fit, silent = TRUE)
  # Save samples and log marginal likelihoods:
  if (!dir.exists(fit_dir)) dir.create(fit_dir)
  fit_name <- glue(model_name)
  if ("file_name" %in% names(args)) {
    fit_file <- args$file_name
  } else {
    fit_file <- tempfile("fit_", tmpdir = fit_dir, fileext = ".rds")
  }
  lml_file <- sub("fit_", "lml_", fit_file, fixed = TRUE)
  readr::write_rds(fit, fit_file, compress = "gz")
  readr::write_rds(lml, lml_file, compress = "gz")
  # Save model name and pointers into a backup CSV, just in case:
  readr::write_csv(
    dplyr::data_frame(model = fit_name, fit_path = fit_file, lml_path = lml_file),
    glue("{fit_dir}_index.csv"), append = file.exists(glue("{fit_dir}_index.csv"))
  )
  # Cleanup:
  rm(fit, lml)
  gc() # "It can be useful to call gc after a large object has been removed"

  return(dplyr::data_frame(model = fit_name, fit_path = fit_file, lml_path = lml_file))
}

models <- expand.grid(
  model_name = names(stan_models),
  p = c(1, 3, 5, 7),
  q = c(0, 1, 3, 5),
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

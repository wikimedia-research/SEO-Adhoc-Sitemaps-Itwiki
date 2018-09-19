library(magrittr)
library(glue)
library(purrr)
library(rstan)
library(bridgesampling)

options(mc.cores = parallel::detectCores())
# options(mc.cores = 1)
rstan_options(auto_write = TRUE)

stan_models <- list(
  # stan_model("simple.stan", model_name = "Simple",
  stan_model("models/ar.stan", model_name = "AR({p}) w/ instant change"),
  stan_model("models/ar_regression.stan", model_name = "AR({p}) w/ instant change & regressors"),
  stan_model("models/arma.stan", model_name = "ARMA({p},{q}) w/ instant change"),
  stan_model("models/arma_regression.stan", model_name = "ARMA({p},{q}) w/ instant change & regressors"),
  stan_model("models/ar_p3e.stan", model_name = "AR({p}) w/ levelling-off change"),
  stan_model("models/ar_regression_p3e.stan", model_name = "AR({p}) w/ levelling-off change & regressors"),
  stan_model("models/arma_p3e.stan", model_name = "ARMA({p},{q}) w/ levelling-off change"),
  stan_model("models/arma_regression_p3e.stan", model_name = "ARMA({p},{q}) w/ levelling-off change & regressors"),
  stan_model("models/ar_p5e.stan", model_name = "AR({p}) w/ Gompertz change"),
  stan_model("models/ar_regression_p5e.stan", model_name = "AR({p}) w/ Gompertz change & regressors" ),
  stan_model("models/arma_p5e.stan", model_name = "ARMA({p},{q}) w/ Gompertz change"),
  stan_model("models/arma_regression_p5e.stan", model_name = "ARMA({p},{q}) w/ Gompertz change & regressors"),
  stan_model("models/arma_regression_p5e_v2.stan", model_name = "ARMA({p},{q}) w/ Gompertz change & regressors (v2)")
)
stan_models %<>% set_names(map_chr(stan_models, ~ .x@model_name))

prepare_predictors <- function(N, xreg = NULL, creg = NULL) {
  # Prepend continuous predictor for linear trend:
  if (is.null(xreg)) {
    xreg <- matrix(1:N, ncol = 1)
  } else {
    xreg <- cbind(t = 1:N, xreg)
  }
  # Create an intercept if there are no categorical predictors:
  if (is.null(creg)) {
    creg <- matrix(1, nrow = N, ncol = 1)
    J <- array(1, dim = 1)
    J_max <- 1
  } else {
    # Convert every factor into a numeric variable:
    creg <- as.matrix(purrr::map_df(creg, as.integer))
    J <- array(as.integer(apply(creg, 2, max)), dim = ncol(creg))
    J_max <- max(J)
  }
  return(list(x = xreg, c = creg, J = J, J_max = J_max))
}

fit_stan_model <- function(model_name, p, q, model_data, fit_dir, xreg = NULL, creg = NULL,
                           n_chains = 4, stan_control = list(adapt_delta = 0.999), stan_iter = 6e3,
                           ...) {
  # model_data is a list with components N, T, y
  # Setup:
  args <- list(...)
  N <- model_data$N
  # Data for Stan:
  if (p > 0) {
    model_data$p <- p
  }
  if (q > 0) {
    model_data$q <- q
  }
  model_uses_regression <- grepl("regressors", model_name)
  reparameterized_model <- grepl("(v2)", model_name, fixed = TRUE)
  if (model_uses_regression) {
    if (reparameterized_model) {
      regressors <- prepare_predictors(N, xreg, creg)
      model_data$x <- regressors$x
      model_data$K <- ncol(regressors$x)
      model_data$c <- regressors$c
      model_data$M <- ncol(regressors$c)
      model_data$J <- regressors$J
      model_data$J_max <- regressors$J_max
    } else {
      x <- mlr::createDummyFeatures(creg, method = "reference")
      if (!is.null(xreg)) {
        x <- as.matrix(cbind(xreg, x))
      }
      model_data$x <- x
      model_data$K <- ncol(x)
    }
  }
  # Initial estimates as starting points for the model:
  constrain <- function(x) {
    # constrain to remain in region of stationarity
    return(sign(x) * pmin(abs(x), 0.95))
  }
  arima_fit <- arima(model_data$y, c(p, 0, q), include.mean = model_uses_regression && !reparameterized_model)
  arima_coefs <- coefficients(arima_fit)
  phi <- array(constrain(unname(arima_coefs[grepl("^ar[1-9]", names(arima_coefs))])), dim = p)
  inits <- list(sigma = sqrt(arima_fit$sigma2), phi = phi)
  if (q > 0) {
    inits$theta <- array(constrain(unname(arima_coefs[grepl("^ma[1-9]", names(arima_coefs))])), dim = q)
  }
  if (model_uses_regression && !reparameterized_model) inits$mu <- arima_coefs["intercept"]
  initf <- function() {
    inits <- inits
    if (grepl("Gompertz", model_name)) {
      inits$lambda <- runif(1, 0.5, 3)
      inits$d <- runif(1, 5, 14)
    } else if (grepl("levelling-off", model_name, fixed = TRUE)) {
      inits$omega1 <- runif(1, 0.5, 0.9)
    }
    return(inits)
  }
  # Draw samples:
  fit <- sampling(
    stan_models[[model_name]], data = model_data,
    control = stan_control, iter = stan_iter,
    chains = n_chains, init = initf
  )
  # Log Marginal Likelihood via Bridge Sampling for model comparison:
  lml <- bridge_sampler(fit, silent = TRUE)
  # Save samples and log marginal likelihoods:
  if (!dir.exists(fit_dir)) dir.create(fit_dir)
  fit_name <- glue(model_name)
  if ("file_name" %in% names(args)) {
    fit_file <- file.path(fit_dir, args$file_name)
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

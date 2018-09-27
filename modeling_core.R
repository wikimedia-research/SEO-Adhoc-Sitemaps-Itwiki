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
  stan_model("models/arma_regression_p5e_v2.stan", model_name = "ARMA({p},{q}) w/ Gompertz change & regressors (v2)"),
  stan_model("models/arma_regression_p3e_v2.stan", model_name = "ARMA({p},{q}) w/ levelling-off change & regressors (v2)"),
  stan_model("models/arma_regression_p3e_v3.stan", model_name = "ARMA({p},{q}) w/ levelling-off change & regressors (v3)")
)
stan_models %<>% set_names(map_chr(stan_models, ~ .x@model_name))

prepare_predictors <- function(N, xreg = NULL, creg = NULL, version = c(2, 3)) {
  version <- version[1]
  if (!version %in% c(2, 3)) stop("version must be 2 or 3")
  if (version == 2) {
    if (is.null(xreg)) {
      message("Creating a linear trend component")
      xreg <- matrix(1:N, ncol = 1)
    } else {
      message("Prepending a linear trend component")
      xreg <- cbind(t = 1:N, xreg)
    }
    # Create an intercept if there are no categorical predictors:
    if (is.null(creg)) {
      message("Creating an intercept")
      creg <- rep.int(1, N)
      M <- 1
    } else {
      message("Converting factors into an alpha index")
      creg <- as.integer(creg)
      M <- max(creg)
    }
    return(list(x = xreg, c = creg, M = M))
  } else {
    # model version 3 where creg is a vector of N dates
    if (is.null(xreg)) {
      message("Creating a linear trend component")
      xreg <- matrix(1:N, ncol = 1)
    } else {
      message("Prepending a linear trend component")
      xreg <- cbind(t = 1:N, xreg)
    }
    if (is.null(creg)) stop("creg must not be null")
    if (!class(creg) %in% c("Date", "POSIXct")) stop("creg must be Date or POSIXct")
    yd <- as.integer(lubridate::yday(creg))
    # adjust in case of leap years:
    # e.g. 2016-03-01 should have yday of 60 but that's taken by 2016-02-29
    # so to normalize it, we need to take the subset of dates which fall on
    # leap years AND come after Feb 29th and then subtract 1 from their yday
    affected_idx <- lubridate::leap_year(creg) & yd >= 61
    yd[affected_idx] <- yd[affected_idx] - 1
    # resume:
    D <- max(yd)
    M <- max(as.integer(lubridate::month(creg)))
    if (D > 365 || M > 12) stop("maximum 365 days & 12 months per year allowed")
    return(list(x = xreg, D = D, M = M, yd = yd))
  }
}

fit_stan_model <- function(model_name, p, q, model_data, fit_dir, xreg = NULL, creg = NULL, dates = NULL,
                           n_chains = 4, stan_control = list(adapt_delta = 0.99),
                           stan_iter = 2e3, stan_warmup = floor(stan_iter / 2),
                           ...) {
  # model_data is a list with components N, T, y
  if (!all(c("N", "T", "y") %in% names(model_data))) {
    stop(
      "Missing at least one important component of the data: ",
      paste0(setdiff(c("N", "T", "y"), names(model_data)), ", ")
    )
  }
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
  reparameterized_model <- grepl("v[23]", model_name)
  if (model_uses_regression) {
    message("Preparing predictor matrix/matrices")
    if (reparameterized_model) {
      if (grepl("(v2)", model_name, fixed = TRUE)) {
        regressors <- prepare_predictors(N, xreg, creg, version = 2)
        model_data$c <- regressors$c
        model_data$M <- regressors$M
      } else {
        if (is.null(dates)) stop("v3 of the model requires dates")
        regressors <- prepare_predictors(N, xreg, dates, version = 3)
        model_data$yd <- regressors$yd
        model_data$D <- regressors$D
        model_data$M <- regressors$M
      }
      model_data$x <- regressors$x
      model_data$K <- ncol(regressors$x)
    } else {
      message("Creating M-1 dummy variables from M categories")
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
  message("Obtaining estimates to use as initial values")
  arima_fit <- arima(model_data$y, c(p, 0, q), include.mean = model_uses_regression && !reparameterized_model)
  arima_coefs <- coefficients(arima_fit)
  phi <- array(constrain(unname(arima_coefs[grepl("^ar[1-9]", names(arima_coefs))])), dim = p)
  inits <- list(sigma = sqrt(arima_fit$sigma2), phi = phi)
  if (q > 0) {
    inits$theta <- array(constrain(unname(arima_coefs[grepl("^ma[1-9]", names(arima_coefs))])), dim = q)
  }
  if (model_uses_regression && !reparameterized_model) {
    inits$mu <- arima_coefs["intercept"]
  } else if (grepl("(v2)", model_name, fixed = TRUE)) {
    inits$alpha_mean <- mean(model_data$y)
  } else if (grepl("(v3)", model_name, fixed = TRUE)) {
    inits$gamma_mean <- mean(model_data$y)
  }
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
  message("Starting MCMC sampling")
  fit <- sampling(
    stan_models[[model_name]], data = model_data,
    control = stan_control, iter = stan_iter, warmup = stan_warmup,
    chains = n_chains, init = initf
  )
  # Save samples and log marginal likelihoods:
  message("Saving samples")
  if (!dir.exists(fit_dir)) dir.create(fit_dir)
  fit_name <- glue(model_name)
  if ("file_name" %in% names(args)) {
    fit_file <- file.path(fit_dir, paste0("fit_", args$file_name))
  } else {
    fit_file <- tempfile("fit_", tmpdir = fit_dir, fileext = ".rds")
  }
  readr::write_rds(fit, fit_file, compress = "gz")
  # Log Marginal Likelihood via Bridge Sampling for model comparison:
  message("Calculating log marginal likelihood")
  lml_file <- sub("fit_", "lml_", fit_file, fixed = TRUE)
  lml <- try(bridge_sampler(fit, silent = TRUE))
  if ("try-error" %in% class(lml)) {
    warning("Had a problem calculating log marginal likelihood")
    lml_file <- NA
  } else {
    message("Saving log marginal likelihood")
    readr::write_rds(lml, lml_file, compress = "gz")
  }
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

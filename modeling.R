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

fits <- pmap_dfr(
  models, fit_stan_model, model_data = model_data, creg = itwiki_pvs$month, dates = itwiki_pvs$date,
  xreg = mlr::createDummyFeatures(itwiki_pvs[, "weekday", drop = FALSE], method = "reference"),
  fit_dir = "fits", stan_iter = 5e3, stan_control = list(adapt_delta = 0.999)
)

# Run locally:
if (file.exists("fits_index.csv")) fits <- readr::read_csv("fits_index.csv")

source("summarizing.R")

# estimates and confidence intervals across all considered models:
if (!dir.exists(here("results"))) dir.create(here("results"))
if (file.exists(here("results", "delta0_hpd.csv"))) {
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

fit_stan_model(
  "ARMA({p},{q}) w/ levelling-off change & regressors (v3)",
  model_data = model_data, p = 3, q = 2, creg = NULL, dates = itwiki_pvs$date,
  xreg = mlr::createDummyFeatures(itwiki_pvs[, "weekday", drop = FALSE], method = "reference"),
  stan_control = list(adapt_delta = 0.9999, max_treedepth = 15), stan_iter = 3e3,
  fit_dir = "fits2", file_name = "arma32r3ev3.rds"
)

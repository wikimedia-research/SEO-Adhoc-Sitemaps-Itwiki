source("modeling_core.R")
source("data.R")

model_data <- list(
  y = google_traffic$pageviews,
  N = nrow(google_traffic),
  T = submission_day
)

models <- expand.grid(
  model_name = names(stan_models),
  p = c(2, 3),
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
  dplyr::filter(grepl("regressors", model_name), grepl("v2", model_name)) %>%
  dplyr::filter((p > 1) | (p == 1 & q >= 1)) %>%
  dplyr::arrange(model_name, p, q)

fits <- pmap_dfr(
  models, fit_stan_model, model_data = model_data, creg = google_traffic$weekday,
  fit_dir = "fits", stan_iter = 4e3, stan_control = list(adapt_delta = 0.99)
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

fit_stan_model("ARMA({p},{q}) w/ Gompertz change & regressors (v2)", p = 3, q = 2,
               model_data = model_data,
               creg = google_traffic$weekday, # random intercepts
               stan_control = list(adapt_delta = 0.9999, max_treedepth = 15), stan_iter = 1e4,
               fit_dir = "fits2", file_name = "arma32r5ev2.rds")

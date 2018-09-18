source("modeling_core.R")
source("data.R")

model_data <- list(
  y = itwiki_pvs$desktop,
  N = nrow(itwiki_pvs),
  T = event_days["sitemap deployment"]
)
fits <- pmap_dfr(
  models, fit_stan_model, model_data = model_data, fit_dir = "fits",
  xreg = itwiki_pvs[, c("year", "month", "weekday", "is_holiday")]
)

# Save the model names and pointers to a TSV:
readr::write_tsv(fits, "fits_index.tsv")

# Run locally:
if (file.exists("fits_index.tsv")) fits <- readr::read_tsv("fits_index.tsv") else
  if (file.exists("fits_index.csv")) fits <- readr::read_csv("fits_index.csv")

source("summarizing.R")

# estimates and confidence intervals across all considered models:
if (!dir.exists(here("results"))) dir.create(here("results"))
existing_models <- readr::read_csv(here("results", "delta0_hpd.csv"))$model
set_names(as.list(fits$fit_path), fits$model) %>%
  .[setdiff(fits$model, existing_models)] %>%
  purrr::map_df(~ load_and_do(.x, hpd_interval, pars = c("delta0")), .id = "model") %>%
  readr::write_csv(
    here("results", "delta0_hpd.csv"),
    append = file.exists(here("results", "delta0_hpd.csv"))
  )
set_names(as.list(fits$fit_path), fits$model) %>%
  .[setdiff(fits$model, existing_models)] %>%
  .[grepl("levelling-off", names(.), fixed = TRUE)] %>%
  purrr::map_df(~ load_and_do(.x, hpd_interval, pars = c("omega1")), .id = "model") %>%
  readr::write_csv(
    here("results", "omega1_hpd.csv"),
    append = file.exists(here("results", "omega1_hpd.csv"))
  )
set_names(as.list(fits$fit_path), fits$model) %>%
  .[setdiff(fits$model, existing_models)] %>%
  .[grepl("Gompertz", names(.))] %>%
  purrr::map_df(~ load_and_do(.x, hpd_interval, pars = c("lambda", "d")), .id = "model") %>%
  readr::write_csv(
    here("results", "lambda_hpd.csv"),
    append = file.exists(here("results", "lambda_hpd.csv"))
  )

fit_stan_model("ARMA({p},{q}) w/ Gompertz change & regressors (v2)", p = 7, q = 5,
               model_data = model_data, xreg = itwiki_pvs[, c("year", "month", "weekday", "is_holiday")],
               stan_control = list(adapt_delta = 0.9999, max_treedepth = 20), stan_iter = 8e4,
               fit_dir = "fits", file_name = "fit_arma75r5ev2.rds")

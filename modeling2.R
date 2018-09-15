source("modeling_core.R")
source("data.R")

model_data <- list(
  y = google_traffic$pageviews,
  N = nrow(google_traffic),
  T = submission_day
)
fits2 <- pmap_dfr(
  models, fit_stan_model, model_data = model_data, fit_dir = "fits2",
  xreg = google_traffic[, "weekday", drop = FALSE]
)

# Save the model names and pointers to a TSV:
readr::write_tsv(fits, "fits2_index.tsv")

# Run locally:
system("scp -r bearloga@bayesian-intervention-analyzer.eqiad.wmflabs:/home/bearloga/itwiki_sitemaps/fit* ./")
if (file.exists("fits2_index.tsv")) fits <- readr::read_tsv("fits2_index.tsv") else
  if (file.exists("fits2_index.csv")) fits <- readr::read_csv("fits2_index.csv")

source("summarizing.R")

# estimates and confidence intervals across all considered models:
if (!dir.exists(here("results2"))) dir.create(here("results2"))
existing_models <- readr::read_csv(here("results2", "delta0_hpd.csv"))$model
set_names(as.list(fits$fit_path), fits$model) %>%
  .[setdiff(fits$model, existing_models)] %>%
  purrr::map_df(~ load_and_do(.x, hpd_interval, pars = c("delta0")), .id = "model") %>%
  readr::write_csv(
    here("results2", "delta0_hpd.csv"),
    append = file.exists(here("results2", "delta0_hpd.csv"))
  )
set_names(as.list(fits$fit_path), fits$model) %>%
  .[setdiff(fits$model, existing_models)] %>%
  .[grepl("levelling-off", names(.), fixed = TRUE)] %>%
  purrr::map_df(~ load_and_do(.x, hpd_interval, pars = c("omega1")), .id = "model") %>%
  readr::write_csv(
    here("results2", "omega1_hpd.csv"),
    append = file.exists(here("results2", "omega1_hpd.csv"))
  )
set_names(as.list(fits$fit_path), fits$model) %>%
  .[setdiff(fits$model, existing_models)] %>%
  .[grepl("Gompertz", names(.))] %>%
  purrr::map_df(~ load_and_do(.x, hpd_interval, pars = c("lambda", "d")), .id = "model") %>%
  readr::write_csv(
    here("results2", "lambda_hpd.csv"),
    append = file.exists(here("results2", "lambda_hpd.csv"))
  )

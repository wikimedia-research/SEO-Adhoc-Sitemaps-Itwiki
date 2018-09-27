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
  model_data = model_data, p = 3, q = 2, creg = NULL, dates = itwiki_pvs$date,
  xreg = mlr::createDummyFeatures(itwiki_pvs[, "weekday", drop = FALSE], method = "reference"),
  stan_control = list(adapt_delta = 0.999), stan_iter = 2e3, n_chains = 4,
  fit_dir = "test", file_name = "arma32r3ev3.rds"
)

fits <- list(
  A = readr::read_rds("test/fit_arma32r5ev2.rds"),
  B = readr::read_rds("test/fit_arma32r3ev2.rds"),
  C = readr::read_rds("test/fit_arma32r3ev3.rds")
)
lmls <- list(
  A = readr::read_rds("test/lml_arma32r5ev2.rds"),
  B = readr::read_rds("test/lml_arma32r3ev2.rds"),
  # C = readr::read_rds("test/lml_arma32r3ev3.rds")
  C = NULL
)
View(summary(fits$A, pars = c("delta0", "lambda", "d", "sigma", "tau", "beta", "alpha"))$summary)
plot(fits$A, pars = "alpha")
# alpha[1:12] = months (Jan-Dec)
View(summary(fits$B, pars = c("delta0", "omega1", "sigma", "tau", "beta", "alpha"))$summary)
plot(fits$B, pars = "alpha")
View(summary(fits$C, pars = c("delta0", "omega1", "sigma", "tau", "beta", "gamma", "alpha"))$summary)
plot(fits$C, pars = "gamma")

source("summarizing.R")
source("data.R")
search_engine_traffic_model <- "ARMA(3,2) w/ levelling-off change & regressors (v3)"
search_engine_traffic_fit <- readr::read_rds("test/fit_arma32r3ev3.rds")
p1 <- search_engine_traffic_fit %>%
  posterior_predictive_plot(
    dplyr::select(itwiki_pvs, date, day, actual = desktop),
    title = "Model-predicted desktop traffic from all recognized search engines",
    subtitle = search_engine_traffic_model
  ) +
  scale_x_date(date_labels = "%d %b", date_breaks = "2 weeks", date_minor_breaks = "1 week",
               limits = as.Date(c("2016-03-01", "2018-09-17"))) +
  geom_vline(aes(xintercept = date), linetype = "dashed",
             data = dplyr::filter(events, event == "sitemap deployment")) +
  labs(caption = "Actual traffic displayed in black, predictions (estimates and 95% confidence intervals) in red") +
  wmf::theme_min(14)

CIs <- list(
  ci95 = broom::tidyMCMC(search_engine_traffic_fit, pars = "z", estimate.method = "median", conf.int = TRUE, conf.method = "HPDinterval", conf.level = 0.95) %>%
    dplyr::mutate(date = itwiki_pvs$date),
  ci80 = broom::tidyMCMC(search_engine_traffic_fit, pars = "z", estimate.method = "median", conf.int = TRUE, conf.method = "HPDinterval", conf.level = 0.80) %>%
    dplyr::mutate(date = itwiki_pvs$date),
  ci50 = broom::tidyMCMC(search_engine_traffic_fit, pars = "z", estimate.method = "median", conf.int = TRUE, conf.method = "HPDinterval", conf.level = 0.50) %>%
    dplyr::mutate(date = itwiki_pvs$date)
)
p2 <- ggplot(CIs$ci95, aes(x = date)) +
  geom_ribbon(aes(ymin = 1e6 * conf.low, ymax = 1e6 * conf.high), alpha = 0.2) +
  geom_ribbon(aes(ymin = 1e6 * conf.low, ymax = 1e6 * conf.high), alpha = 0.2, data = CIs$ci80) +
  geom_ribbon(aes(ymin = 1e6 * conf.low, ymax = 1e6 * conf.high), alpha = 0.2, data = CIs$ci50) +
  geom_line(aes(y = 1e6 * estimate), size = 1.2) +
  geom_vline(
    aes(xintercept = date), linetype = "dashed",
    data = dplyr::filter(events, event == "sitemap deployment")
  ) +
  scale_x_date(
    limits = as.Date(c("2018-08-08", "2018-09-17")),
    date_breaks = "7 days", date_minor_breaks = "1 day",
    date_labels = "%b %d"
  ) +
  scale_y_continuous(labels = compress) +
  wmf::theme_min(14) +
  labs(
    x = "Date", y = "Pageviews",
    title = "Desktop traffic from all recognized search engines attributable to sitemaps",
    subtitle = "Day-by-day point estimates with 95%, 80%, 50% confidence intervals",
    caption = "The model could not detect a significant effect from sitemaps in traffic from all search engines."
  )

library(patchwork)
p1 + p2 + plot_layout(ncol = 1)

source("modeling_core.R")
source("data.R")

model_data <- list(
  y = itwiki_pvs$desktop,
  N = nrow(itwiki_pvs),
  T = event_days["sitemap deployment"]
)

fit_stan_model("ARMA({p},{q}) w/ Gompertz change & regressors (v2)", p = 7, q = 5,
               model_data = model_data,
               creg = itwiki_pvs$month, # random intercepts; use dummy regression vars for days of week:
               xreg = mlr::createDummyFeatures(itwiki_pvs[, "weekday", drop = FALSE], method = "reference"),
               stan_control = list(adapt_delta = 0.9999, max_treedepth = 15), stan_iter = 4e3,
               fit_dir = "fits2", file_name = "arma75r5ev2.rds")

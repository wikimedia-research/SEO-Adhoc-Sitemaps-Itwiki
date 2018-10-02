load_and_do <- function(path, .f, ...) {
  message("Loading ", path)
  x <- readr::read_rds(path)
  return(.f(x, ...))
}

compress <- function(x, ...) {
  y <- polloi::compress(abs(x, ...))
  return(paste0(ifelse(x < 0, "-", ""), y))
}

hpd_interval <- function(x, ...) {
  return(broom::tidyMCMC(
    x, estimate.method = "median", rhat = TRUE, droppars = "lp__",
    conf.method = "HPDinterval", conf.int = TRUE, conf.level = 0.95,
    ...
  ))
}

refine_hpd <- function(hpd_summary, digits = 3) {
  refined <- hpd_summary %>%
    dplyr::mutate(
      est = sprintf(glue::glue("%.{digits}f (%.{digits}f)"), estimate, std.error),
      ci95 = sprintf(glue::glue("(%.{digits}f, %.{digits}f)"), conf.low, conf.high)
    ) %>%
    dplyr::select(-c(estimate, std.error, conf.low, conf.high))
  return(refined)
}

summarize_predictions <- function(stan_fit) {
  output <- stan_fit %>%
    rstan::extract("yhat") %>%
    .$yhat %>%
    apply(2, quantile, probs = c(0.025, 0.5, 0.975)) %>%
    t %>%
    as.data.frame() %>%
    magrittr::set_colnames(c("conf.low", "point.est", "conf.high")) %>%
    dplyr::mutate(day = 1:n())
  return(output)
}

calculate_mse <- function(stan_fit, model_data) {
  squared_errors <- (model_data$y - summarize_predictions(stan_fit)$point.est) ^ 2
  mse_before <- mean(squared_errors[7:(model_data$T)])
  mse_after <- mean(squared_errors[(model_data$T + 1):model_data$N])
  return(data.frame(mse_before = mse_before, mse_after = mse_after))
}

posterior_predictive_plot <- function(stan_fit, data, title,
                                      subtitle = "Search engine traffic to the Italian Wikipedia") {
  stan_fit %>%
    summarize_predictions %>%
    dplyr::left_join(data, by = "day") %>%
    dplyr::filter(day > 1) %>%
    ggplot(aes(x = date)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "red", alpha = 0.4) +
    geom_line(aes(y = point.est), color = "red", size = 1) +
    geom_line(aes(y = actual), color = "black") +
    labs(x = "Date", y = "Pageviews (in millions)", title = title, subtitle = subtitle)
}

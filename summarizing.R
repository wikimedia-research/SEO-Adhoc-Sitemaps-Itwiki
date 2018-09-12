load_and_do <- function(path, .f, ...) {
  message("Loading ", path)
  x <- readr::read_rds(path)
  return(.f(x, ...))
}

hpd_interval <- function(x, ...) {
  return(broom::tidyMCMC(
    x, estimate.method = "median", rhat = TRUE, droppars = "lp__",
    conf.method = "HPDinterval", conf.int = TRUE, conf.level = 0.95,
    ...
  ))
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

posterior_predictive_plot <- function(stan_fit, title,
                                      subtitle = "Search engine traffic to the Italian Wikipedia") {
  stan_fit %>%
    summarize_predictions %>%
    dplyr::left_join(itwiki_pvs, by = "day") %>%
    dplyr::filter(day > 1) %>%
    ggplot(aes(x = date)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "red", alpha = 0.5) +
    geom_line(aes(y = point.est), color = "red", size = 1) +
    geom_line(aes(y = both), color = "black") +
    coord_cartesian(ylim = c(-5, 15)) +
    scale_x_date(limits = as.Date(c("2018-05-01", "2018-09-10"))) +
    labs(x = "Date", y = "Pageviews (in millions)", title = title, subtitle = subtitle) +
    wmf::theme_min()
}
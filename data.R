library(here)
library(magrittr)

itwiki_pvs <- here("data", "data.csv") %>%
  readr::read_csv() %>%
  dplyr::filter(date > "2016-02-03", referrer_type == "external (search engine)")

message("Reminder: pageviews have been scaled down to be in millions")
itwiki_pvs <- dplyr::left_join(
  itwiki_pvs %>%
    dplyr::filter(referrer_type == "external (search engine)", access_method != "mobile app") %>%
    dplyr::group_by(date) %>%
    dplyr::summarize(both = sum(pageviews) / 1e6),
  itwiki_pvs %>%
    dplyr::filter(referrer_type == "external (search engine)", access_method != "mobile app") %>%
    dplyr::select(date, access_method, pageviews) %>%
    dplyr::mutate(pageviews = pageviews / 1e6) %>%
    tidyr::spread(access_method, pageviews),
  by = "date"
) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(
    day = 1:n(),
    year = factor(lubridate::year(date)),
    month = factor(lubridate::month(date, label = TRUE, abbr = FALSE), month.name),
    weekday = factor(lubridate::wday(date, label = TRUE, abbr = FALSE, week_start = 7), c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
    season = factor(
      lubridate::quarter(date, fiscal_start = 12),
      1:4, c("Winter", "Spring", "Summer", "Autumn")
    )
  )

# https://en.wikipedia.org/wiki/Public_holidays_in_Italy
it_holidays <- purrr::map_df(levels(itwiki_pvs$year), ~ dplyr::data_frame(
  date = as.Date(paste0(.x, "-", c("01-01", "01-06", "04-25", "05-01", "06-02", "08-15", "11-01", "12-08", "12-25", "12-26"))),
  holiday = c("New Year's Day", "Epiphany", "Liberation Day", "Int'l Workers' Day", "Republic Day", "Ferragosto Day", "All Saints' Day", "Immaculate Conception", "Christmas Day", "Saint Stephen's Day")
))

itwiki_pvs <- dplyr::select(it_holidays, date) %>%
  dplyr::mutate(is_holiday = 1) %>%
  dplyr::left_join(itwiki_pvs, ., by = "date") %>%
  dplyr::mutate(is_holiday = dplyr::if_else(is.na(is_holiday), 0, is_holiday))

events <- dplyr::data_frame(
  date = as.Date(c("2018-07-03", "2018-07-05", "2018-07-10", "2018-08-10", "2018-08-12")),
  event = c("block start", "block stop", "traffic report", "sitemap deployment", "GSC submission")
) %>%
  dplyr::left_join(dplyr::select(itwiki_pvs, date, day), by = "date")
event_days <- set_names(events$day, events$event)

google_traffic <- readr::read_csv("data/google.csv") %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(
    pageviews = pageviews / 1e6,
    day = 1:n(),
    weekday = factor(
      lubridate::wday(date, label = TRUE, abbr = FALSE, week_start = 7),
      c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    )
  )
submission_day <- which(google_traffic$date == "2018-08-12")

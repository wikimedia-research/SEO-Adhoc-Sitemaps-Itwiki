library(here)
library(magrittr)

itwiki_pvs <- here("data", "data.csv") %>%
  readr::read_csv() %>%
  dplyr::filter(date > "2016-02-03", referrer_type == "external (search engine)") %>%
  dplyr::filter(!(lubridate::month(date) == 2 & lubridate::mday(date) == 29)) # remove leap days

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

events <- dplyr::data_frame(
  date = as.Date(c("2018-07-03", "2018-07-05", "2018-07-10", "2018-08-10")),
  event = c("block start", "block stop", "traffic report", "sitemap deployment & GSC submission")
) %>%
  dplyr::left_join(dplyr::select(itwiki_pvs, date, day), by = "date")
event_days <- set_names(events$day, events$event)

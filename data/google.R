library(glue)

start_date <- as.Date("2018-07-01")
end_date <- as.Date("2018-09-17")
dates <- seq(start_date, end_date, by = "day")

query <- paste0(readr::read_lines("google.hql"), collapse = "\n")

get_result <- function(date) {
  year <- lubridate::year(date)
  month <- lubridate::month(date)
  day <- lubridate::mday(date)
  query <- glue(query, .open = "${", .close = "}")
  result <- wmf::query_hive(query)
  return(result)
}

results <- purrr::map_dfr(dates, get_result)

readr::write_csv(results, "google.csv", append = file.exists("google.csv"))

library(here)
library(magrittr)
library(tidyverse)

itwiki_pageviews <- here("data", "data.csv") %>%
  read_csv() %>%
  arrange(date, access_method, referrer_type) %>%
  filter(date > "2016-02-03") # when external (search engine) was implemented

itwiki_totals <- itwiki_pageviews %>%
  group_by(date, referrer_type) %>%
  summarize(pageviews = sum(pageviews)) %>%
  ungroup

annotations <- data_frame(
  date = as.Date(c("2018-07-03", "2018-07-05", "2018-07-10", "2018-08-10")),
  event = c("A", NA, "B", "C")
)

itwiki_totals %>%
  filter(date >= "2018-07-01", referrer_type %in% c("external (search engine)", "internal", "none")) %>%
  mutate(referrer_type = if_else(referrer_type == "none", "none (direct traffic)", referrer_type)) %>%
  ggplot(aes(x = date, y = pageviews)) +
  geom_line(aes(color = referrer_type), size = 0.8) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(breaks = seq(2, 10, 1) * 1e6, label = polloi::compress) +
  scale_x_date(date_breaks = "2 days", date_labels = "%d\n%b") +
  geom_vline(aes(xintercept = date), data = annotations, linetype = "dashed") +
  geom_label(aes(x = date, y = 2e6, label = event), data = annotations) +
  labs(
    x = "Date", y = "Pageviews", color = "Referrer Type",
    title = "Traffic to the Italian Wikipedia",
    caption = "A: while it was blacked out, all traffic to Italian Wikipedia was redirected to Wikipedia:Comunicato_3_luglio_2018
B: report came in on July 10th that search engines are still linking to the blackout notice (T199252)
C: sitemap created and deployed"
  ) +
  wmf::theme_min(panel.grid.minor.y = element_blank()) +
  theme(plot.caption = element_text(hjust = 0))

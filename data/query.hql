SELECT
  DATE(CONCAT(year, '-', LPAD(month, 2, '0'), '-', LPAD(day, 2, '0'))) AS date,
  access_method,
  referer_class AS referrer_type,
  SUM(view_count) AS pageviews
FROM pageview_hourly
WHERE year = ${year} AND month = ${month} AND day >= ${day} -- data starts 2016-02-03
  AND DATE(CONCAT(year, '-', LPAD(month, 2, '0'), '-', LPAD(day, 2, '0'))) < CURRENT_DATE
  AND project = 'it.wikipedia'
  AND referer_class IN('external (search engine)', 'external', 'internal', 'none')
  AND agent_type = 'user'
GROUP BY
  DATE(CONCAT(year, '-', LPAD(month, 2, '0'), '-', LPAD(day, 2, '0'))),
  access_method,
  referer_class;

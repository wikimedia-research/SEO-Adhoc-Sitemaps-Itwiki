ADD JAR hdfs:///wmf/refinery/current/artifacts/refinery-hive.jar;
CREATE TEMPORARY FUNCTION get_engine AS 'org.wikimedia.analytics.refinery.hive.IdentifySearchEngineUDF';
USE wmf;
SELECT
  DATE(CONCAT(year, '-', LPAD(month, 2, '0'), '-', LPAD(day, 2, '0'))) AS date,
  COUNT(1) AS pageviews
FROM webrequest
WHERE year = ${year} AND month = ${month} AND day = ${day}
  AND webrequest_source = 'text'
  AND is_pageview
  AND referer_class = 'external (search engine)'
  AND normalized_host.project_family = 'wikipedia'
  AND normalized_host.project = 'it'
  AND agent_type = 'user'
  AND access_method = 'desktop'
  AND get_engine(referer) = 'Google'
GROUP BY DATE(CONCAT(year, '-', LPAD(month, 2, '0'), '-', LPAD(day, 2, '0')));

context("test-create-metrics")

skip_if_not_installed("nycflights13")

library(dplyr)
library(nycflights13)

test_that("Can create metrics based on an Rmd", {
  # find flight delays by week, month, and quarter
  # To make it faster, filter for only flights < 10 AM
  flight_summary <- flights %>%
    filter(dep_time <= 1000) %>%
    mutate(date = as.Date(ISOdate(year, month, day))) %>%
    cross_by_dimensions(origin, carrier) %>%
    cross_by_periods() %>%
    summarize(nb_flights = n(),
              avg_arr_delay = mean(arr_delay, na.rm = TRUE))

  rmd <- system.file("extdata", "metrics_nycflight_stats.Rmd", package = "tidymetrics")
  metrics <- create_metrics(flight_summary, rmd_file = rmd)

  expect_equal(length(metrics), 2)

  for (m in metrics) {
    check_metric(m)

    expect_equal(sort(unique(m$period)), c("month", "quarter", "week"))
    expect_gt(nrow(m), 1000)
  }
})

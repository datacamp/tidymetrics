context("test-metric-group")

skip_if_not_installed("nycflights13")

library(dplyr)
library(nycflights13)

test_that("Can create a metric group", {
  # find flight delays by week, month, and quarter
  flight_summary <- flights %>%
    mutate(date = as.Date(ISOdate(year, month, day))) %>%
    cross_by_dimensions(origin, carrier) %>%
    cross_by_periods() %>%
    summarize(nb_flights = n(),
              avg_arr_delay = mean(arr_delay, na.rm = TRUE))

  rmd <- system.file("extdata", "metrics_flights_nyc.Rmd", package = "tidymetrics")
  mg <- create_metric_group(flight_summary, rmd_file = rmd)

  expect_is(mg, "tbl_metric_group")

  mg_metadata <- attr(mg, "metadata")

  expect_equal(mg_metadata$category, "flights")
  expect_equal(mg_metadata$subcategory, "nyc")
  expect_equal(names(mg_metadata$metrics), c("nb_flights", "avg_arr_delay"))
  expect_equal(length(mg_metadata$dimensions), 2)
})

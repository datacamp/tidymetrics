context("test-create-metrics")

skip_if_not_installed("nycflights13")

suppressPackageStartupMessages(library(dplyr))
library(stringr)
library(nycflights13)

test_that("Can create metrics based on an Rmd, and operate on them", {
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

    # print
    output <- capture.output(print(m))
    expect_true(any(stringr::str_detect(output, "Dimensions: origin, carrier")))
    expect_true(any(stringr::str_detect(output, "Periods:.*month")))
    expect_true(any(stringr::str_detect(output, "tibble")))
    expect_true(any(stringr::str_detect(output, "more rows")))

    # dplyr verbs
    filtered <- m %>%
      filter(period == "month")

    expect_is(filtered, "tbl_metric")
    expect_equal(unique(filtered$period), "month")
    expect_gt(nrow(filtered), 100)

    counted <- m %>%
      count(date, period)

    expect_is(counted, "tbl_metric")
    expect_equal(colnames(counted), c("date", "period", "n"))

    arranged <- m %>%
      arrange(value)

    expect_is(arranged, "tbl_metric")
    expect_equal(arranged$value, sort(m$value))

    # condense
    condensed <- m %>%
      condense_metric(max_dimensions = 1)

    expect_false(any(condensed$origin != "All" & condensed$carrier != "All"))
    expect_true(any(condensed$origin != "All" & condensed$carrier == "All"))
    expect_true(any(condensed$origin == "All" & condensed$carrier != "All"))
  }
})

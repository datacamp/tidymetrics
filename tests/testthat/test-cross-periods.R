context("test-cross-periods")

skip_if_not_installed("nycflights13")

library(dplyr)
library(lubridate)

flight_data <- nycflights13::flights %>%
  mutate(date = as.Date(ISOdate(year, month, day))) %>%
  select(date, carrier, origin, dep_delay, arr_delay, distance)

test_that("can cross a local table by calendar periods", {
  p <- c("day", "week", "month", "quarter", "year")

  crossed <- flight_data %>%
    cross_by_periods(periods = p)

  expect_equal(group_vars(crossed), c("period", "date"))
  expect_true(all(c("period", "date", "date_original", "carrier") %in% colnames(crossed)))

  # has all the periods
  expect_equal(sort(unique(crossed$period)), sort(p))

  expect_equal(min(crossed$date), as.Date(floor_date(as.Date("2013-01-01"), "week")))
  expect_equal(max(crossed$date), as.Date("2013-12-31"))
})


test_that("can cross a local table by windows", {
  w <- c(7, 14)

  crossed <- flight_data %>%
    cross_by_periods(periods = c(), windows = w)

  expect_equal(group_vars(crossed), c("period", "date"))
  expect_true(all(c("period", "date", "date_original", "carrier") %in% colnames(crossed)))

  # has all the periods
  expect_equal(sort(unique(crossed$period)), c("rolling_14d", "rolling_7d"))
  expect_equal(min(crossed$date), as.Date("2013-01-01"))
  expect_equal(max(crossed$date), as.Date("2013-12-31") + 13)
})

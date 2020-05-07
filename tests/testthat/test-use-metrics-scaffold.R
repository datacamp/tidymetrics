context("test-use-metrics-scaffold")

library(dplyr)

test_that("use_metrics_scaffold works", {
  car_metrics <- mtcars %>%
    cross_by_dimensions(cyl, am) %>%
    summarize(
      nb_cars = n(),
      avg_wt = mean(wt)
    )

  capture.output(scaffold <- use_metrics_scaffold(car_metrics))

  # It returns YAML, make sure it found the metrics and dimensions appropriately
  expect_equal(names(scaffold), c("metrics", "dimensions"))
  expect_equal(names(scaffold$metrics), c("nb_cars", "avg_wt"))
  expect_equal(names(scaffold$dimensions), c("cyl", "am"))
})

context("test-keep-dimensions")

library(dplyr)

mtcars_by_cyl_gear_am <- mtcars %>%
  cross_by_dimensions(cyl, gear, am) %>%
  summarize(nb_cars = n(),
            avg_mpg = mean(mpg))

test_that("keep_dimensions retains desired dimensions", {
  kept_am <- mtcars_by_cyl_gear_am %>%
    keep_dimensions(am)

  expect_equal(nrow(kept_am), 2)
  expect_equal(colnames(kept_am), c("am", "nb_cars", "avg_mpg"))
  expect_equal(sort(kept_am$am), c("0", "1"))

  kept_cyl_am <- mtcars_by_cyl_gear_am %>%
    keep_dimensions(cyl, am)

  expect_equal(nrow(kept_cyl_am), 6)
  expect_equal(colnames(kept_cyl_am), c("cyl", "am", "nb_cars", "avg_mpg"))
})

test_that("keep_dimensions works with select helpers", {
  kept_all_but_am <- mtcars_by_cyl_gear_am %>%
    keep_dimensions(-am)

  expect_equal(colnames(kept_all_but_am), c("cyl", "gear", "nb_cars", "avg_mpg"))

  kept_gear_am <- mtcars_by_cyl_gear_am %>%
    keep_dimensions(contains("a"))

  expect_equal(colnames(kept_gear_am), c("gear", "am", "nb_cars", "avg_mpg"))
})

test_that("keep_dimensions works with keep_attribute_all = TRUE", {
  kept_with_all <- mtcars_by_cyl_gear_am %>%
    keep_dimensions(am, cyl, keep_attribute_all = TRUE)

  expect_equal(colnames(kept_with_all), c("cyl", "am", "nb_cars", "avg_mpg"))

  expect_equal(sort(unique(kept_with_all$cyl)), c("4", "6", "8", "All"))
  expect_equal(sort(unique(kept_with_all$am)), c("0", "1", "All"))
})

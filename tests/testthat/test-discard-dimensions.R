context("test-discard-dimensions")

library(dplyr)

mtcars_by_cyl_gear_am <- mtcars %>%
  cross_by_dimensions(cyl, gear, am) %>%
  summarize(nb_cars = n(),
            avg_mpg = mean(mpg))

test_that("discard_dimensions drops desired dimensions", {
  discarded_am <- mtcars_by_cyl_gear_am %>%
    discard_dimensions(am)

  expect_equal(colnames(discarded_am), c("cyl", "gear", "nb_cars", "avg_mpg"))
  expect_equal(sort(unique(discarded_am$cyl)), c("4", "6", "8", "All"))

  discarded_cyl_am <- mtcars_by_cyl_gear_am %>%
    discard_dimensions(cyl, am)

  expect_equal(nrow(discarded_cyl_am), 4)
  expect_equal(colnames(discarded_cyl_am), c("gear", "nb_cars", "avg_mpg"))
})

test_that("discard_dimensions works with select helpers", {
  discard_all_but_am <- mtcars_by_cyl_gear_am %>%
    discard_dimensions(-am)

  expect_equal(colnames(discard_all_but_am), c("am", "nb_cars", "avg_mpg"))

  discard_gear_am <- mtcars_by_cyl_gear_am %>%
    discard_dimensions(contains("a"))

  expect_equal(colnames(discard_gear_am), c("cyl", "nb_cars", "avg_mpg"))
})


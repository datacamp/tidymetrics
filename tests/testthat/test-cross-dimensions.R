context("cross by dimensions")

test_that("cross_by_dimensions works with a local table", {
  cyl_am_crossed <- mtcars %>%
    cross_by_dimensions(cyl, am) %>%
    count()

  expect_equal(nrow(cyl_am_crossed), 12)
  expect_equal(group_vars(cyl_am_crossed), c("cyl", "am"))
  expect_equal(sort(unique(cyl_am_crossed$cyl)), c("4", "6", "8", "All"))
  expect_equal(sort(unique(cyl_am_crossed$am)), c("0", "1", "All"))

  cyl_am_vs_crossed <- mtcars %>%
    cross_by_dimensions(cyl, am, vs) %>%
    count()

  expect_equal(nrow(cyl_am_vs_crossed), 30)
  expect_equal(group_vars(cyl_am_vs_crossed), c("cyl", "am", "vs"))
  expect_equal(sort(unique(cyl_am_vs_crossed$cyl)), c("4", "6", "8", "All"))
  expect_equal(sort(unique(cyl_am_vs_crossed$am)), c("0", "1", "All"))
  expect_equal(sort(unique(cyl_am_vs_crossed$vs)), c("0", "1", "All"))
})

test_that("cross_by_dimensions works with max_dimensions", {
  cyl_am_crossed_0 <- mtcars %>%
    cross_by_dimensions(cyl, am, max_dimensions = 0) %>%
    count()

  expect_equal(nrow(cyl_am_crossed_0), 1)
  expect_equal(group_vars(cyl_am_crossed_0), c("cyl", "am"))
  expect_equal(sort(unique(cyl_am_crossed_0$cyl)), "All")
  expect_equal(sort(unique(cyl_am_crossed_0$am)), "All")

  cyl_am_crossed_1 <- mtcars %>%
    cross_by_dimensions(cyl, am, max_dimensions = 1) %>%
    count()

  expect_equal(nrow(cyl_am_crossed_1), 6)
  expect_equal(group_vars(cyl_am_crossed_1), c("cyl", "am"))
  expect_equal(sort(unique(cyl_am_crossed_1$cyl)), c("4", "6", "8", "All"))
  expect_equal(sort(unique(cyl_am_crossed_1$am)), c("0", "1", "All"))
  expect_equal(min((cyl_am_crossed_1$cyl == "All") + (cyl_am_crossed_1$am == "All")), 1)

  cyl_am_vs_crossed_2 <- mtcars %>%
    cross_by_dimensions(cyl, am, vs, max_dimensions = 2) %>%
    count()

  expect_equal(nrow(cyl_am_vs_crossed_2), 23)
  expect_equal(group_vars(cyl_am_vs_crossed_2), c("cyl", "am", "vs"))
  expect_equal(sort(unique(cyl_am_vs_crossed_2$cyl)), c("4", "6", "8", "All"))
  expect_equal(sort(unique(cyl_am_vs_crossed_2$am)), c("0", "1", "All"))
  expect_true(all((cyl_am_vs_crossed_2$cyl == "All") | (cyl_am_vs_crossed_2$am == "All") | (cyl_am_vs_crossed_2$vs == "All")))
})

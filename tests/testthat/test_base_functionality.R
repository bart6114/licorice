library(licorice)
library(dplyr)

context("Testing if plot is generated (doesn't check plot consistency)")

test_that("ggplot object is returned", {
  expect_true("ggplot" %in% class(licorice(pisatest, type = "center", middle_pos = 2.5)))
  expect_true("ggplot" %in% class(licorice(pisatest, type = "center", middle_pos = 3)))
  expect_true("ggplot" %in% class(licorice(pisatest, type = "fill")))
  expect_true("ggplot" %in% class(licorice(pisatest, type = "count")))
})

context("Test the basic functionality of the package")

library(dplyr)
library(maps)

setwd(system.file("data", package = "fars"))

test_that("fars_read() works correctly", {
  expect_is(fars_read("accident_2015.csv.bz2"), "tbl_df")
  expect_error(fars_read("accident_2016.csv.bz2"))
})

test_that("fars_summarize_years() works correctly", {
  expect_is(fars_summarize_years(2013:2015), "tbl_df")
  expect_is(fars_summarize_years(list(2013, 2014)), "tbl_df")
  expect_equal(names(fars_summarize_years(2013:2015)), c("MONTH", 2013:2015))
  expect_error(fars_summarize_years(2016))
})

test_that("fars_map_state() works correctly", {
  expect_silent(fars_map_state(1, 2015))
  expect_error(fars_map_state(100, 2015))
  expect_error(fars_map_state(1, 2016))
})

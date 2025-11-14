test_that("predictor(s) and outcome are of the same length", {
  exposures <- list(cities$life_expectancy, cities$happiness_level)
  outcome <- cities$obesity
  expect_equal(unique(lengths(exposures)), length(cities$obesity_level))
})


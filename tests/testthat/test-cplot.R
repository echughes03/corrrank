test_that("(1) predictor(s) and outcome are of the same length", {
  exposures <- list(cities$life_expectancy, cities$happiness_level)
  outcome <- cities$obesity_level
  expect_equal(unique(lengths(exposures)), length(cities$obesity_level))
})

test_that("(2) cplot returns a list", {
  expect_equal(class(list()), class(cplot(exposures, cities$obesity_level)))
})

test_that("(3) cplot indexed outputs are ggplot objects", {
  expect_equal(c("gg","ggplot"), class(cplot(exposures, cities$obesity_level)$`Exposure 1`))
})

test_that("(4) number of plots correspond to number of predictors listed", {
  expect_equal(length(exposures), length(cplot(exposures, cities$obesity_level)))
})

test_that("(5) outcome variable is numeric", {
  n <- as.numeric(rep(10,10))
  outcome <- cities$obesity_level
  expect_equal(class(n), class(outcome))
})

test_that("(6) All predictors are numeric", {
  exposures <- list(cities$life_expectancy, cities$happiness_level)
  compiled <- unlist(exposures)
  n <- as.numeric(rep(10,10))
  expect_equal(class(n), class(compiled))
})


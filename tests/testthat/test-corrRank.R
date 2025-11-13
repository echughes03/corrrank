test_that("one predictor correlation works", {
  output = corrRank(list(Sunshine = cities$sunshine_hours), cities$water_bottle_cost)
  expect_equal(output$Correlation,-0.35)
})

test_that("one predictor p-value works", {
  output = corrRank(list(Sunshine = cities$sunshine_hours), cities$water_bottle_cost)
  expect_equal(output$Significance, "Yes")
})

test_that("one predictor naming works", {
  output = corrRank(list(Sunshine = cities$sunshine_hours), cities$water_bottle_cost)
  expect_equal(output$Predictor,"Sunshine")
})

test_that("two predictor ranking works", {
  output = corrRank(list(Sunshine = cities$sunshine_hours, Obesity= cities$obesity_level), cities$water_bottle_cost)
  expect_equal(output$Correlation[1] <= output$Correlation[2], TRUE)
})

test_that("three predictor ranking works", {
  output = corrRank(list(Work_hrs = cities$avg_hours_worked_annual, Sunshine=cities$sunshine_hours, "Take out"=cities$num_take_out_places), cities$happiness_level)
  expect_equal(output$Correlation[1] <= output$Correlation[2] & output$Correlation[2]<= output$Correlation[3], TRUE)
})

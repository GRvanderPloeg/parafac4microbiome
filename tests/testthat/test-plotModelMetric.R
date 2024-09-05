test_that("plotModelMetric throws no errors", {
  numIterations = array(round(runif(100*2)*100), c(100, 2))
  expect_no_error(plotModelMetric(numIterations))
})

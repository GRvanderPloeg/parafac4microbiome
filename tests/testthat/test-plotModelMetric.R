test_that("plotModelMetric throws no errors", {
  numIterations = array(round(runif(100*2)*100), c(100, 2))
  SSE = array(rnorm(100*2, mean=1e4, sd=100), c(100, 2))
  CORCONDIA = array(runif(100*2, min=90, max=100), c(100,2))
  varExp = array(runif(100*2, min=50, max=100), c(100,2))
  TCC = list(NULL, array(rnorm(3*2*2*100), c(3,2,2,100)))
  expect_no_error(plotModelMetric(numIterations, SSE, CORCONDIA, varExp, TCC))
})

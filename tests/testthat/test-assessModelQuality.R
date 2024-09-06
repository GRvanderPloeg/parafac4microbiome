test_that("Output in metrics has the right shape", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  output = assessModelQuality(processedFujita$data, minNumComponents=1, maxNumComponents=3, numRepetitions=5)
  expect_equal(dim(output$metrics$numIterations), c(5,3))
})

test_that("Output models is a list of maxNumComponents objects", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  output = assessModelQuality(processedFujita$data, minNumComponents=1, maxNumComponents=3, numRepetitions=5)
  expect_equal(length(output$models), 3)
})

test_that("Each item in output models is length numRepetitions", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  output = assessModelQuality(processedFujita$data, minNumComponents=1, maxNumComponents=3, numRepetitions=5)
  expect_equal(length(output$models[[1]]), 5)
})

test_that("A nonsensical minNumComponents throws an error", {
  expect_error(assessModelQuality(Fujita2023$data, minNumComponents=-3, maxNumComponents=3, numRepetitions=5))
})

test_that("A nonsensical numRepetitions throws an error", {
  expect_error(assessModelQuality(Fujita2023$data, minNumComponents=-3, maxNumComponents=3, numRepetitions=-5))
})

test_that("assessModelQuality can run in parallel", {
  skip_on_cran()
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  expect_no_error(assessModelQuality(processedFujita$data, minNumComponents=1, maxNumComponents=3, numRepetitions=5, numCores=2))
})

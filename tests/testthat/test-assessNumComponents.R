test_that("Output in metrics has the right shape", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  output = assessNumComponents(processedFujita$data, minNumComponents=1, maxNumComponents=3, numRepetitions=10)
  expect_equal(dim(output$metrics$numIterations), c(10,3))
})

test_that("Output models is a list of maxNumComponents objects", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  output = assessNumComponents(processedFujita$data, minNumComponents=1, maxNumComponents=3, numRepetitions=10)
  expect_equal(length(output$models), 3)
})

test_that("Each item in output models is length numRepetitions", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  output = assessNumComponents(processedFujita$data, minNumComponents=1, maxNumComponents=3, numRepetitions=10)
  expect_equal(length(output$models[[1]]), 10)
})

test_that("A nonsensical minNumComponents throws an error", {
  expect_error(assessNumComponents(Fujita2023$data, minNumComponents=-3, maxNumComponents=3, numRepetitions=10))
})

test_that("A nonsensical numRepetitions throws an error", {
  expect_error(assessNumComponents(Fujita2023$data, minNumComponents=-3, maxNumComponents=3, numRepetitions=-10))
})

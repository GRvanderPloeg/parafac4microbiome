test_that("modelStability produces a length 4 list", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  output = modelStabilityCheck(processedFujita, numComponents=1, numFolds=10)
  expect_equal(length(output), 4)
})

test_that("Length of the list of A is equal to the number of components", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  output = modelStabilityCheck(processedFujita, numComponents=5, numFolds=10)
  expect_equal(length(output$As), 5)
})

test_that("Dim of A is equal to numRows(X) by numFolds", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  output = modelStabilityCheck(processedFujita, numComponents=1, numFolds=10)
  expect_equal(dim(output$As[[1]]), c(nrow(processedFujita$data), 10))
})

test_that("modelStability works with minimum input given", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  expect_no_error(modelStabilityCheck(processedFujita, numComponents=1))
})

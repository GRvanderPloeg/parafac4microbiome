test_that("modelStability produces a length 3 list", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  output = modelStabilityCheck(processedFujita$data, processedFujita$mode1, numComponents=1, numFolds=10)
  expect_equal(length(output), 3)
})

test_that("Length of the list of A is equal to the number of components", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  output = modelStabilityCheck(processedFujita$data, Fujita2023$mode1, numComponents=5, numFolds=10)
  expect_equal(length(output[[1]]), 5)
})

test_that("Dim of A is equal to numRows(X) by numFolds", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  output = modelStabilityCheck(processedFujita$data, processedFujita$mode1, numComponents=1, numFolds=10)
  expect_equal(dim(output[[1]][[1]]), c(nrow(processedFujita$data), 10))
})

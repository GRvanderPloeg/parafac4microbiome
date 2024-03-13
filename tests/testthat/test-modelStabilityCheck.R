test_that("modelStabilityCheck produces a length 4 list", {
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

test_that("modelStabilityCheck works with minimum input given", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  expect_no_error(modelStabilityCheck(processedFujita, numComponents=1))
})

test_that("modelStabilityCheck can consider groups", {
  processedShao = processDataCube(Shao2019, sparsityThreshold=0.5, considerGroups=TRUE, groupVariable="Delivery_mode", centerMode=1, scaleMode=2)
  expect_no_error(modelStabilityCheck(processedShao, numComponents=1, numFolds=5, considerGroups=TRUE, groupVariable="Delivery_mode"))
})

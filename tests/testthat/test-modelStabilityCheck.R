test_that("modelStabilityCheck works with minimum input given", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  expect_no_error(modelStabilityCheck(processedFujita, numComponents=2))
})

test_that("modelStabilityCheck works with one component", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  expect_no_error(modelStabilityCheck(processedFujita, numComponents=1, numFolds=5))
})

test_that("modelStabilityCheck can consider groups", {
  processedShao = processDataCube(Shao2019, sparsityThreshold=0.5, considerGroups=TRUE, groupVariable="Delivery_mode", centerMode=1, scaleMode=2)
  expect_no_error(modelStabilityCheck(processedShao, numComponents=2, numFolds=5, considerGroups=TRUE, groupVariable="Delivery_mode"))
})

test_that("plotModelTCCs throws no errors", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  models = parafac(processedFujita$data, 2, nstart=10, output="all")
  expect_no_error(plotModelTCCs(models))
})

test_that("plotModelTCCs returns null in the one-component case", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  models = parafac(processedFujita$data, 1, nstart=10, output="all")
  expect_true(is.null(plotModelTCCs(models)))
})

test_that("plotModelStability throws no errors", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  models = parafac(processedFujita$data, 2, nstart=10, output="all")
  expect_no_error(plotModelStability(models, processedFujita))
})

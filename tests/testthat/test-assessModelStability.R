test_that("assessModelStability throws no errors", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  expect_no_error(assessModelStability(processedFujita, minNumComponents=1, maxNumComponents=2))
})

test_that("assessModelStability can consider groups", {
  processedShao = processDataCube(Shao2019, sparsityThreshold=0.5, considerGroups=TRUE, groupVariable="Delivery_mode", centerMode=1, scaleMode=2)
  expect_no_error(assessModelStability(processedShao, minNumComponents=1, maxNumComponents=2, numFolds=5, considerGroups=TRUE, groupVariable="Delivery_mode"))
})

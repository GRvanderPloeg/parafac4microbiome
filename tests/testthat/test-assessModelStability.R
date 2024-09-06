test_that("assessModelStability throws no errors", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  expect_no_error(assessModelStability(processedFujita, minNumComponents=1, maxNumComponents=2))
})

test_that("assessModelStability can consider groups", {
  processedShao = processDataCube(Shao2019, sparsityThreshold=0.5, considerGroups=TRUE, groupVariable="Delivery_mode", centerMode=1, scaleMode=2)
  expect_no_error(assessModelStability(processedShao, minNumComponents=1, maxNumComponents=2, numFolds=5, considerGroups=TRUE, groupVariable="Delivery_mode"))
})

test_that("assessModelStability works with the full plot settings on", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, CLR=TRUE, centerMode=1, scaleMode=2)

  numFolds = 8
  colourCols = c("", "Genus", "")
  legendTitles = c("", "Genus", "")
  xLabels = c("Replicate", "Feature index", "Time point")
  legendColNums = c(0,5,0)
  arrangeModes = c(FALSE, TRUE, FALSE)
  continuousModes = c(FALSE,FALSE,TRUE)
  ctol=1e-4
  maxit=500
  numCores=1
  expect_no_error(assessModelStability(processedFujita, minNumComponents=1, maxNumComponents=3, numFolds=numFolds, considerGroups=FALSE,
                                       groupVariable="", colourCols, legendTitles, xLabels, legendColNums, arrangeModes,
                                       ctol=ctol, maxit=maxit, numCores=numCores))
})

test_that("assessModelStability works in parallel", {
  skip_on_cran()
  processedShao = processDataCube(Shao2019, sparsityThreshold=0.5, considerGroups=TRUE, groupVariable="Delivery_mode", centerMode=1, scaleMode=2)
  expect_no_error(assessModelStability(processedShao, minNumComponents=1, maxNumComponents=2, numFolds=5, considerGroups=TRUE, groupVariable="Delivery_mode", numCores=2))
})

test_that("plotPARAFACmodel produces a figure when only a model and a dataset is supplied", {
  model = parafac(Fujita2023$data, nfac=1, nstart=1)
  expect_no_error(plotPARAFACmodel(model$Fac, Fujita2023, 1))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting length for colourCols is supplied", {
  model = parafac(Fujita2023$data, nfac=1, nstart=1)
  expect_warning(plotPARAFACmodel(model$Fac, Fujita2023, 1, colourCols = c("","")))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting length for legendTitles is supplied", {
  model = parafac(Fujita2023$data, nfac=1, nstart=1)
  expect_warning(plotPARAFACmodel(model$Fac, Fujita2023, 1, legendTitles = c("","")))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting length for xLabels is supplied", {
  model = parafac(Fujita2023$data, nfac=1, nstart=1)
  expect_warning(plotPARAFACmodel(model$Fac, Fujita2023, 1, xLabels = c("","")))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting length for legendColNums is supplied", {
  model = parafac(Fujita2023$data, nfac=1, nstart=1)
  expect_warning(plotPARAFACmodel(model$Fac, Fujita2023, 1, legendColNums = c(1,2)))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting length for arrangeModes is supplied", {
  model = parafac(Fujita2023$data, nfac=1, nstart=1)
  expect_warning(plotPARAFACmodel(model$Fac, Fujita2023, 1, arrangeModes = c(FALSE,FALSE)))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting length for continuousModes is supplied", {
  model = parafac(Fujita2023$data, nfac=1, nstart=1)
  expect_warning(plotPARAFACmodel(model$Fac, Fujita2023, 1, continuousModes = c(FALSE,FALSE)))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting type for colourCols is supplied", {
  model = parafac(Fujita2023$data, nfac=1, nstart=1)
  expect_warning(plotPARAFACmodel(model$Fac, Fujita2023, 1, colourCols = c(11,6,7)))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting type for legendTitles is supplied", {
  model = parafac(Fujita2023$data, nfac=1, nstart=1)
  expect_warning(plotPARAFACmodel(model$Fac, Fujita2023, 1, legendTitles = c(11,6,7)))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting type for xLabels is supplied", {
  model = parafac(Fujita2023$data, nfac=1, nstart=1)
  expect_warning(plotPARAFACmodel(model$Fac, Fujita2023, 1, xLabels = c(11,6,7)))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting type for legendColNums is supplied", {
  model = parafac(Fujita2023$data, nfac=1, nstart=1)
  expect_warning(plotPARAFACmodel(model$Fac, Fujita2023, 1, legendColNums = c("bla", "x500", "42")))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting type for arrangeModes is supplied", {
  model = parafac(Fujita2023$data, nfac=1, nstart=1)
  expect_warning(plotPARAFACmodel(model$Fac, Fujita2023, 1, arrangeModes = c("bla", "x500", "42")))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting type for continuousModes is supplied", {
  model = parafac(Fujita2023$data, nfac=1, nstart=1)
  expect_warning(plotPARAFACmodel(model$Fac, Fujita2023, 1, continuousModes = c("bla", "x500", "42")))
})

test_that("plotPARAFACmodel produces a plot even when wrong settings are supplied", {
  model = parafac(Fujita2023$data, nfac=1, nstart=1)
  expect_no_error(plotPARAFACmodel(model$Fac, Fujita2023, 1, colourCols = c("bla", "x500", "42")))
})

test_that("plotPARAFAC model works with the full settings of the readme", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, CLR=TRUE, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=3)

  colourCols = c("", "Genus", "")
  legendTitles = c("", "Genus", "")
  xLabels = c("Replicate", "Feature index", "Time point")
  legendColNums = c(0,5,0)
  arrangeModes = c(FALSE, TRUE, FALSE)
  continuousModes = c(FALSE,FALSE,TRUE)

  expect_no_error(plotPARAFACmodel(model$Fac, processedFujita, 3, colourCols, legendTitles, xLabels, legendColNums, arrangeModes,
                                   continuousModes = c(FALSE,FALSE,TRUE),
                                   overallTitle = "Fujita PARAFAC model"))
})

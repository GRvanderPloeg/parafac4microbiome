test_that("plotPARAFACmodel produces a figure when only a model and a dataset is supplied", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
  expect_no_error(plotPARAFACmodel(model$Fac, Fujita2023, 1))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting length for colourCols is supplied", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
  expect_warning(plotPARAFACmodel(model$Fac, Fujita2023, 1, colourCols = c("","")))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting length for legendTitles is supplied", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
  expect_warning(plotPARAFACmodel(model$Fac, Fujita2023, 1, legendTitles = c("","")))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting length for xLabels is supplied", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
  expect_warning(plotPARAFACmodel(model$Fac, Fujita2023, 1, xLabels = c("","")))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting length for legendColNums is supplied", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
  expect_warning(plotPARAFACmodel(model$Fac, Fujita2023, 1, legendColNums = c(1,2)))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting length for arrangeModes is supplied", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
  expect_warning(plotPARAFACmodel(model$Fac, Fujita2023, 1, arrangeModes = c(FALSE,FALSE)))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting length for continuousModes is supplied", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
  expect_warning(plotPARAFACmodel(model$Fac, Fujita2023, 1, continuousModes = c(FALSE,FALSE)))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting type for colourCols is supplied", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
  expect_warning(plotPARAFACmodel(model$Fac, Fujita2023, 1, colourCols = c(11,6,7)))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting type for legendTitles is supplied", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
  expect_warning(plotPARAFACmodel(model$Fac, Fujita2023, 1, legendTitles = c(11,6,7)))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting type for xLabels is supplied", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
  expect_warning(plotPARAFACmodel(model$Fac, Fujita2023, 1, xLabels = c(11,6,7)))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting type for legendColNums is supplied", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
  expect_warning(plotPARAFACmodel(model$Fac, Fujita2023, 1, legendColNums = c("bla", "x500", "42")))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting type for arrangeModes is supplied", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
  expect_warning(plotPARAFACmodel(model$Fac, Fujita2023, 1, arrangeModes = c("bla", "x500", "42")))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting type for continuousModes is supplied", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
  expect_warning(plotPARAFACmodel(model$Fac, Fujita2023, 1, continuousModes = c("bla", "x500", "42")))
})

test_that("plotPARAFACmodel produces a plot even when wrong settings are supplied", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
  expect_no_error(plotPARAFACmodel(model$Fac, Fujita2023, 1, colourCols = c("bla", "x500", "42")))
})

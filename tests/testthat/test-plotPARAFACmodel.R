test_that("plotPARAFACmodel produces a figure when only a model is supplied", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
  convertedModel = convertModelFormat(model)
  expect_no_error(plotPARAFACmodel(convertedModel))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting length is supplied", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
  convertedModel = convertModelFormat(model)
  expect_warning(plotPARAFACmodel(convertedModel, colourCols = c("","")))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting type is supplied", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
  convertedModel = convertModelFormat(model)
  expect_warning(plotPARAFACmodel(convertedModel, colourCols = c(11,6,7)))
})

test_that("plotPARAFACmodel produces a plot even when wrong settings are supplied", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
  convertedModel = convertModelFormat(model)
  expect_no_error(plotPARAFACmodel(convertedModel, colourCols = c("bla", "x500", "42")))
})

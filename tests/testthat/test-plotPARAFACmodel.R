test_that("plotPARAFACmodel produces a figure when only a model and a dataset is supplied", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
  expect_no_error(plotPARAFACmodel(model, Fujita2023))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting length is supplied", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
  expect_warning(plotPARAFACmodel(model, Fujita2023, colourCols = c("","")))
})

test_that("plotPARAFACmodel gives a warning when an incorrect setting type is supplied", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
  expect_warning(plotPARAFACmodel(model, Fujita2023, colourCols = c(11,6,7)))
})

test_that("plotPARAFACmodel produces a plot even when wrong settings are supplied", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
  expect_no_error(plotPARAFACmodel(model, Fujita2023, colourCols = c("bla", "x500", "42")))
})

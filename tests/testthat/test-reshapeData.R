test_that("reshapeData works without errors when the subject metadata contains strings", {
  # withr::local_package("dplyr")

  Xlong = array(rnorm(108*5*10), c(108*5, 10))
  subjects = as.character(rep(1:108, 5))
  features = rep(1:10)
  timepoints = rep(1:5, each=108)

  expect_no_error(reshapeData(Xlong, subjects, features, timepoints))
})

test_that("reshapeData works without errors when the subject metadata contains numbers", {
  # withr::local_package("dplyr")

  Xlong = array(rnorm(108*5*10), c(108*5, 10))
  subjects = rep(1:108, 5)
  features = rep(1:10)
  timepoints = rep(1:5, each=108)

  expect_no_error(reshapeData(Xlong, subjects, features, timepoints))
})

test_that("reshapeData mode1 is the correct size", {
  # withr::local_package("dplyr")

  Xlong = array(rnorm(108*5*10), c(108*5, 10))
  subjects = rep(1:108, 5)
  features = rep(1:10)
  timepoints = rep(1:5, each=108)

  result = reshapeData(Xlong, subjects, features, timepoints)
  expect_equal(nrow(result$mode1), 108)
})

test_that("reshapeData mode2 is the correct size", {
  # withr::local_package("dplyr")

  Xlong = array(rnorm(108*5*10), c(108*5, 10))
  subjects = rep(1:108, 5)
  features = rep(1:10)
  timepoints = rep(1:5, each=108)

  result = reshapeData(Xlong, subjects, features, timepoints)
  expect_equal(nrow(result$mode2), 10)
})

test_that("reshapeData mode3 is the correct size", {
  # withr::local_package("dplyr")

  Xlong = array(rnorm(108*5*10), c(108*5, 10))
  subjects = rep(1:108, 5)
  features = rep(1:10)
  timepoints = rep(1:5, each=108)

  result = reshapeData(Xlong, subjects, features, timepoints)
  expect_equal(nrow(result$mode3), 5)
})

test_that("reshapeData throws an error when subject metadata is the incorrect length", {
  # withr::local_package("dplyr")

  Xlong = array(rnorm(108*5*10), c(108*5, 10))
  subjects = rep(1:108, 6)
  features = rep(1:10)
  timepoints = rep(1:5, each=108)

  expect_error(reshapeData(Xlong, subjects, features, timepoints))
})

test_that("reshapeData throws an error when feature metadata is the incorrect length", {
  # withr::local_package("dplyr")

  Xlong = array(rnorm(108*5*10), c(108*5, 10))
  subjects = rep(1:108, 5)
  features = rep(1:11)
  timepoints = rep(1:5, each=108)

  expect_error(reshapeData(Xlong, subjects, features, timepoints))
})

test_that("reshapeData throws an error when time point metadata is the incorrect length", {
  # withr::local_package("dplyr")

  Xlong = array(rnorm(108*5*10), c(108*5, 10))
  subjects = rep(1:108, 5)
  features = rep(1:10)
  timepoints = rep(1:6, each=108)

  expect_error(reshapeData(Xlong, subjects, features, timepoints))
})

test_that("reshapeData throws an error when subject metadata is a matrix", {
  # withr::local_package("dplyr")

  Xlong = array(rnorm(108*5*10), c(108*5, 10))
  subjects = cbind(rep(1:108, 5), rep(1:108, 5))
  features = rep(1:10)
  timepoints = rep(1:5, each=108)

  expect_error(reshapeData(Xlong, subjects, features, timepoints))
})

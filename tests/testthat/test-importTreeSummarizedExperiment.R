test_that("importTreeSummarizedExperiment works without errors", {
  testthat::skip_if_not_installed("TreeSummarizedExperiment")
  withr::local_package("TreeSummarizedExperiment")

  fakeOTU = t(rTensor::k_unfold(rTensor::as.tensor(Fujita2023$data), 2)@data)
  fakeTaxa = as.matrix(Fujita2023$mode2)
  fakeSam = as.data.frame(cbind(rep(1:8, 110), rep(1:110, each=8)))
  colnames(fakeSam) = c("replicate.id", "timepoint")

  fakeTreeObj = TreeSummarizedExperiment(assays = list(Count = fakeOTU),
                                         rowData = fakeSam,
                                         colData = fakeTaxa)
  expect_no_error(importTreeSummarizedExperiment(fakeTreeObj, subjectIDs="replicate.id", thirdMode="timepoint", taxa_are_rows=FALSE))
})

test_that("importTreeSummarizedExperiment works without errors when taxa are rows", {
  testthat::skip_if_not_installed("TreeSummarizedExperiment")
  withr::local_package("TreeSummarizedExperiment")

  fakeOTU = rTensor::k_unfold(rTensor::as.tensor(Fujita2023$data), 2)@data
  fakeTaxa = as.matrix(Fujita2023$mode2)
  fakeSam = as.data.frame(cbind(rep(1:8, 110), rep(1:110, each=8)))
  colnames(fakeSam) = c("replicate.id", "timepoint")

  fakeTreeObj = TreeSummarizedExperiment(assays = list(Count = fakeOTU),
                                         rowData = fakeTaxa,
                                         colData = fakeSam)
  expect_no_error(importTreeSummarizedExperiment(fakeTreeObj, subjectIDs="replicate.id", thirdMode="timepoint", taxa_are_rows=TRUE))
})

test_that("importTreeSummarizedExperiment restructuring works as expected", {
  testthat::skip_if_not_installed("TreeSummarizedExperiment")
  withr::local_package("TreeSummarizedExperiment")

  fakeOTU = t(rTensor::k_unfold(rTensor::as.tensor(Fujita2023$data), 2)@data)
  fakeTaxa = as.matrix(Fujita2023$mode2)
  fakeSam = as.data.frame(cbind(rep(1:8, 110), rep(1:110, each=8)))
  colnames(fakeSam) = c("replicate.id", "timepoint")

  fakeTreeObj = TreeSummarizedExperiment(assays = list(Count = fakeOTU),
                                         rowData = fakeSam,
                                         colData = fakeTaxa)
  result = importTreeSummarizedExperiment(fakeTreeObj, subjectIDs="replicate.id", thirdMode="timepoint", taxa_are_rows=FALSE)
  expect_equal(Fujita2023$data, result$data)
})

test_that("importTreeSummarizedExperiment restructuring works as expected when taxa are rows", {
  testthat::skip_if_not_installed("TreeSummarizedExperiment")
  withr::local_package("TreeSummarizedExperiment")

  fakeOTU = rTensor::k_unfold(rTensor::as.tensor(Fujita2023$data), 2)@data
  fakeTaxa = as.matrix(Fujita2023$mode2)
  fakeSam = as.data.frame(cbind(rep(1:8, 110), rep(1:110, each=8)))
  colnames(fakeSam) = c("replicate.id", "timepoint")

  fakeTreeObj = TreeSummarizedExperiment(assays = list(Count = fakeOTU),
                                         rowData = fakeTaxa,
                                         colData = fakeSam)
  result = importTreeSummarizedExperiment(fakeTreeObj, subjectIDs="replicate.id", thirdMode="timepoint", taxa_are_rows=TRUE)
  expect_equal(Fujita2023$data, result$data)
})

test_that("importTreeSummarizedExperiment throws errors without a TreeSummarizedExperiment object", {
  testthat::skip_if_not_installed("TreeSummarizedExperiment")
  expect_error(importTreeSummarizedExperiment(Fujita2023, subjectIDs="replicate.id", thirdMode="timepoint", taxa_are_rows=FALSE))
})

test_that("importTreeSummarizedExperiment gives an error if OTU info is missing", {
  testthat::skip_if_not_installed("TreeSummarizedExperiment")
  withr::local_package("TreeSummarizedExperiment")

  fakeTaxa = as.matrix(Fujita2023$mode2)
  fakeSam = as.data.frame(cbind(rep(1:8, 110), rep(1:110, each=8)))
  colnames(fakeSam) = c("replicate.id", "timepoint")

  fakeTreeObj = TreeSummarizedExperiment(rowData = fakeTaxa,
                                         colData = fakeSam)
  expect_error(importTreeSummarizedExperiment(fakeTreeObj, subjectIDs="replicate.id", thirdMode="timepoint", taxa_are_rows=FALSE))
})

test_that("importTreeSummarizedExperiment gives a warning if taxonomy info is missing", {
  testthat::skip_if_not_installed("TreeSummarizedExperiment")
  withr::local_package("TreeSummarizedExperiment")

  fakeOTU = t(rTensor::k_unfold(rTensor::as.tensor(Fujita2023$data), 2)@data)
  fakeSam = as.data.frame(cbind(rep(1:8, 110), rep(1:110, each=8)))
  colnames(fakeSam) = c("replicate.id", "timepoint")

  fakeTreeObj = TreeSummarizedExperiment(assays = list(Count = fakeOTU),
                                         rowData = fakeSam)
  expect_warning(importTreeSummarizedExperiment(fakeTreeObj, subjectIDs="replicate.id", thirdMode="timepoint", taxa_are_rows=FALSE))
})

test_that("importTreeSummarizedExperiment gives an error if sample info is missing", {
  testthat::skip_if_not_installed("TreeSummarizedExperiment")
  withr::local_package("TreeSummarizedExperiment")

  fakeOTU = t(rTensor::k_unfold(rTensor::as.tensor(Fujita2023$data), 2)@data)
  fakeTaxa = as.matrix(Fujita2023$mode2)

  fakeTreeObj = TreeSummarizedExperiment(assays = list(Count = fakeOTU),
                                         colData = fakeTaxa)
  expect_error(importTreeSummarizedExperiment(fakeTreeObj, subjectIDs="replicate.id", thirdMode="timepoint", taxa_are_rows=FALSE))
})

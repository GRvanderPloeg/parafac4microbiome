test_that("importPhyloseq works without errors", {
  testthat::skip_if_not_installed("phyloseq")
  withr::local_package("phyloseq")

  fakeOTU = t(rTensor::k_unfold(rTensor::as.tensor(Fujita2023$data), 2)@data)
  fakeTaxa = as.matrix(Fujita2023$mode2)
  fakeSam = as.data.frame(cbind(rep(1:8, 110), rep(1:110, each=8)))
  colnames(fakeSam) = c("replicate.id", "timepoint")

  fakePhyloseq = phyloseq(otu_table(fakeOTU, taxa_are_rows=FALSE), tax_table(fakeTaxa), sample_data(fakeSam))
  expect_no_error(importPhyloseq(fakePhyloseq, subjectIDs="replicate.id", thirdMode="timepoint"))
})

test_that("importPhyloseq works without errors when taxa are rows", {
  testthat::skip_if_not_installed("phyloseq")
  withr::local_package("phyloseq")

  fakeOTU = rTensor::k_unfold(rTensor::as.tensor(Fujita2023$data), 2)@data
  fakeTaxa = as.matrix(Fujita2023$mode2)
  fakeSam = as.data.frame(cbind(rep(1:8, 110), rep(1:110, each=8)))
  colnames(fakeSam) = c("replicate.id", "timepoint")

  fakePhyloseq = phyloseq(otu_table(fakeOTU, taxa_are_rows=TRUE), tax_table(fakeTaxa), sample_data(fakeSam))
  expect_no_error(importPhyloseq(fakePhyloseq, subjectIDs="replicate.id", thirdMode="timepoint"))
})

test_that("importPhyloseq restructuring works as expected", {
  testthat::skip_if_not_installed("phyloseq")
  withr::local_package("phyloseq")

  fakeOTU = t(rTensor::k_unfold(rTensor::as.tensor(Fujita2023$data), 2)@data)
  fakeTaxa = as.matrix(Fujita2023$mode2)
  fakeSam = as.data.frame(cbind(rep(1:8, 110), rep(1:110, each=8)))
  colnames(fakeSam) = c("replicate.id", "timepoint")

  fakePhyloseq = phyloseq(otu_table(fakeOTU, taxa_are_rows=FALSE), tax_table(fakeTaxa), sample_data(fakeSam))
  result = importPhyloseq(fakePhyloseq, subjectIDs="replicate.id", thirdMode="timepoint")
  expect_equal(Fujita2023$data, result$data)
})

test_that("importPhyloseq restructuring works as expected when taxa are rows", {
  testthat::skip_if_not_installed("phyloseq")
  withr::local_package("phyloseq")

  fakeOTU = rTensor::k_unfold(rTensor::as.tensor(Fujita2023$data), 2)@data
  fakeTaxa = as.matrix(Fujita2023$mode2)
  fakeSam = as.data.frame(cbind(rep(1:8, 110), rep(1:110, each=8)))
  colnames(fakeSam) = c("replicate.id", "timepoint")

  fakePhyloseq = phyloseq(otu_table(fakeOTU, taxa_are_rows=TRUE), tax_table(fakeTaxa), sample_data(fakeSam))
  result = importPhyloseq(fakePhyloseq, subjectIDs="replicate.id", thirdMode="timepoint")
  expect_equal(Fujita2023$data, result$data)
})

test_that("importPhyloseq throws errors without a phyloseq object", {
  testthat::skip_if_not_installed("phyloseq")
  expect_error(importPhyloseq(Fujita2023, subjectIDs="replicate.id", thirdMode="timepoint"))
})

test_that("importPhyloseq gives a warning if taxonomy info is missing", {
  testthat::skip_if_not_installed("phyloseq")
  withr::local_package("phyloseq")

  fakeOTU = t(rTensor::k_unfold(rTensor::as.tensor(Fujita2023$data), 2)@data)
  fakeSam = as.data.frame(cbind(rep(1:8, 110), rep(1:110, each=8)))
  colnames(fakeSam) = c("replicate.id", "timepoint")

  fakePhyloseq = phyloseq(otu_table(fakeOTU, taxa_are_rows=FALSE), sample_data(fakeSam))
  expect_warning(importPhyloseq(fakePhyloseq, subjectIDs="replicate.id", thirdMode="timepoint"))
})

test_that("importPhyloseq gives an error if sample info is missing", {
  testthat::skip_if_not_installed("phyloseq")
  withr::local_package("phyloseq")

  fakeOTU = t(rTensor::k_unfold(rTensor::as.tensor(Fujita2023$data), 2)@data)
  fakeTaxa = as.matrix(Fujita2023$mode2)

  fakePhyloseq = phyloseq(otu_table(fakeOTU, taxa_are_rows=FALSE), tax_table(fakeTaxa))
  expect_error(importPhyloseq(fakePhyloseq, subjectIDs="replicate.id", thirdMode="timepoint"))
})

test_that("importPhyloseq throws no errors for GlobalPatterns example dataset", {
  testthat::skip_if_not_installed("phyloseq")
  withr::local_package("phyloseq")
  data(GlobalPatterns)
  GP = GlobalPatterns

  alteredSampleData = sample_data(GP)
  alteredSampleData$subjectID = c(1,2,3,1,2,1,2,3,1,2,1,2,1,2,3,1,2,3,1,2,3,3,4,1,2,3)
  df = phyloseq(otu_table(GP), tax_table(GP), alteredSampleData)
  expect_no_error(importPhyloseq(df, subjectIDs = "subjectID", thirdMode="SampleType"))
})

test_that("importPhyloseq restructuring works as expected for the GlobalPatterns example dataset", {
  testthat::skip_if_not_installed("phyloseq")
  withr::local_package("phyloseq")
  data(GlobalPatterns)
  GP = GlobalPatterns

  alteredSampleData = sample_data(GP)
  alteredSampleData$subjectID = c(1,2,3,1,2,1,2,3,1,2,1,2,1,2,3,1,2,3,1,2,3,3,4,1,2,3)
  df = phyloseq(otu_table(GP), tax_table(GP), alteredSampleData)
  result = importPhyloseq(df, subjectIDs = "subjectID", thirdMode="SampleType")
  expect_equal(result$data[1,,1], as.vector(otu_table(GP)[,4]))
})


test_that("importMicrobiotaProcess works without errors", {
  withr::local_package("phyloseq")
  withr::local_package("MicrobiotaProcess")

  fakeOTU = t(rTensor::k_unfold(rTensor::as.tensor(Fujita2023$data), 2)@data)
  fakeTaxa = as.matrix(Fujita2023$mode2)
  fakeSam = as.data.frame(cbind(rep(1:8, 110), rep(1:110, each=8)))
  colnames(fakeSam) = c("replicate.id", "timepoint")

  fakePhyloseq = phyloseq(otu_table(fakeOTU, taxa_are_rows=FALSE), phyloseq::tax_table(fakeTaxa), sample_data(fakeSam))
  mpse = as.MPSE(fakePhyloseq)
  expect_no_error(importMicrobiotaProcess(mpse, subjectIDs="replicate.id", thirdMode="timepoint", taxa_are_rows=TRUE))
})

test_that("importMicrobiotaProcess works without errors when taxa are rows", {
  withr::local_package("phyloseq")
  withr::local_package("MicrobiotaProcess")

  fakeOTU = rTensor::k_unfold(rTensor::as.tensor(Fujita2023$data), 2)@data
  fakeTaxa = as.matrix(Fujita2023$mode2)
  fakeSam = as.data.frame(cbind(rep(1:8, 110), rep(1:110, each=8)))
  colnames(fakeSam) = c("replicate.id", "timepoint")

  fakePhyloseq = phyloseq(otu_table(fakeOTU, taxa_are_rows=TRUE), phyloseq::tax_table(fakeTaxa), sample_data(fakeSam))
  mpse = as.MPSE(fakePhyloseq)
  expect_no_error(importMicrobiotaProcess(mpse, subjectIDs="replicate.id", thirdMode="timepoint", taxa_are_rows=TRUE))
})

test_that("importMicrobiotaProcess restructuring works as expected", {
  withr::local_package("phyloseq")
  withr::local_package("MicrobiotaProcess")

  fakeOTU = t(rTensor::k_unfold(rTensor::as.tensor(Fujita2023$data), 2)@data)
  fakeTaxa = as.matrix(Fujita2023$mode2)
  fakeSam = as.data.frame(cbind(rep(1:8, 110), rep(1:110, each=8)))
  colnames(fakeSam) = c("replicate.id", "timepoint")

  fakePhyloseq = phyloseq(otu_table(fakeOTU, taxa_are_rows=FALSE), phyloseq::tax_table(fakeTaxa), sample_data(fakeSam))
  mpse = as.MPSE(fakePhyloseq)
  result = importMicrobiotaProcess(mpse, subjectIDs="replicate.id", thirdMode="timepoint", taxa_are_rows=TRUE)
  expect_equal(Fujita2023$data, result$data)
})

test_that("importMicrobiotaProcess restructuring works as expected when taxa are rows", {
  withr::local_package("phyloseq")
  withr::local_package("MicrobiotaProcess")

  fakeOTU = t(rTensor::k_unfold(rTensor::as.tensor(Fujita2023$data), 2)@data)
  fakeTaxa = as.matrix(Fujita2023$mode2)
  fakeSam = as.data.frame(cbind(rep(1:8, 110), rep(1:110, each=8)))
  colnames(fakeSam) = c("replicate.id", "timepoint")

  fakePhyloseq = phyloseq(otu_table(fakeOTU, taxa_are_rows=FALSE), phyloseq::tax_table(fakeTaxa), sample_data(fakeSam))
  mpse = as.MPSE(fakePhyloseq)
  result = importMicrobiotaProcess(mpse, subjectIDs="replicate.id", thirdMode="timepoint", taxa_are_rows=TRUE)
  expect_equal(Fujita2023$data, result$data)
})

test_that("importMicrobiotaProcess throws errors without a TreeSummarizedExperiment object", {
  expect_error(importMicrobiotaProcess(Fujita2023, subjectIDs="replicate.id", thirdMode="timepoint", taxa_are_rows=TRUE))
})

test_that("importMicrobiotaProcess gives an error if sample info is missing", {
  withr::local_package("phyloseq")
  withr::local_package("MicrobiotaProcess")

  fakeOTU = t(rTensor::k_unfold(rTensor::as.tensor(Fujita2023$data), 2)@data)
  fakeTaxa = as.matrix(Fujita2023$mode2)

  fakePhyloseq = phyloseq(otu_table(fakeOTU, taxa_are_rows=FALSE), phyloseq::tax_table(fakeTaxa))
  mpse = as.MPSE(fakePhyloseq)
  expect_error(importMicrobiotaProcess(mpse, subjectIDs="replicate.id", thirdMode="timepoint", taxa_are_rows=TRUE))
})

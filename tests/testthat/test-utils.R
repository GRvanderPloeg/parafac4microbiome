test_that("convertModelFormat throws an error with incorrect input model", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = "bla"

  subjectMetadata = Fujita2023$sampleMetadata %>%
    filter(treat2=="WC") %>%
    select(replicate.id) %>%
    unique()

  featureMetadata = Fujita2023$taxonomy

  conditionMetadata = Fujita2023$sampleMetadata %>%
    select(time) %>%
    unique()

  metadataPerMode = list(subjectMetadata, featureMetadata, conditionMetadata)
  expect_error(convertModelFormat(model, metadataPerMode))
})

test_that("convertModelFormat throws an error with incorrect metadataPerMode object", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
  metadataPerMode = 1:5
  expect_error(convertModelFormat(model, metadataPerMode))
})

test_that("convertModelFormat throws a warning with metadataPerMode with the wrong number of rows", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)

  subjectMetadata = Fujita2023$sampleMetadata %>%
    filter(treat2=="WC") %>%
    select(replicate.id) %>%
    unique()

  featureMetadata = Fujita2023$taxonomy[1:10,]

  conditionMetadata = Fujita2023$sampleMetadata %>%
    select(time) %>%
    unique()

  metadataPerMode = list(subjectMetadata, featureMetadata, conditionMetadata)
  expect_warning(convertModelFormat(model, metadataPerMode))
})

test_that("convertModelFormat does not throw an error with correct input model", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)

  subjectMetadata = Fujita2023$sampleMetadata %>%
    filter(treat2=="WC") %>%
    select(replicate.id) %>%
    unique()

  featureMetadata = Fujita2023$taxonomy

  conditionMetadata = Fujita2023$sampleMetadata %>%
    select(time) %>%
    unique()

  metadataPerMode = list(subjectMetadata, featureMetadata, conditionMetadata)
  expect_no_error(convertModelFormat(model, metadataPerMode))
})

test_that("convertModelFormat throws an error even if model input is a list but not a parafac object", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = list("A"=c(1,1,1), "B"=c(2,2,2), "C"=c(3,3,3))

  subjectMetadata = Fujita2023$sampleMetadata %>%
    filter(treat2=="WC") %>%
    select(replicate.id) %>%
    unique()

  featureMetadata = Fujita2023$taxonomy

  conditionMetadata = Fujita2023$sampleMetadata %>%
    select(time) %>%
    unique()

  metadataPerMode = list(subjectMetadata, featureMetadata, conditionMetadata)
  expect_error(convertModelFormat(model, metadataPerMode))
})

test_that("convertModelFormat produces a list of length 3 for 3-way parafac models", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)

  subjectMetadata = Fujita2023$sampleMetadata %>%
    filter(treat2=="WC") %>%
    select(replicate.id) %>%
    unique()

  featureMetadata = Fujita2023$taxonomy

  conditionMetadata = Fujita2023$sampleMetadata %>%
    select(time) %>%
    unique()

  metadataPerMode = list(subjectMetadata, featureMetadata, conditionMetadata)
  convertedModel = convertModelFormat(model, metadataPerMode)
  expect_equal(length(convertedModel), 3)
})

test_that("convertModelFormat produces a list of length 4 for 4-way parafac models", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  X = array(rnorm(10*20*30*4), dim=c(10,20,30,4))
  model = parafac(X, nfac=1, nstart=1, verbose=FALSE)

  # Fake metadata
  subjectMetadata = tibble(var=1:10)
  featureMetadata = tibble(var=1:20)
  conditionMetadata = tibble(var=1:30)
  conditionMetadata2 = tibble(var=1:4)

  metadataPerMode = list(subjectMetadata, featureMetadata, conditionMetadata, conditionMetadata2)
  convertedModel = convertModelFormat(model, metadataPerMode)
  expect_equal(length(convertedModel), 4)
})

test_that("convertModelFormat produces output even when metadataPerMode is not supplied", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
  expect_no_error(convertModelFormat(model))
})

test_that("convertModelFormat throws an error with incorrect input model", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = "bla"

  metadataPerMode = list(Fujita2023$mode1, Fujita2023$mode2, Fujita2023$mode3)
  expect_error(convertModelFormat(model, metadataPerMode))
})

test_that("convertModelFormat throws an error with incorrect metadataPerMode object", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=1, nstart=1, verbose=FALSE)
  metadataPerMode = 1:5
  expect_error(convertModelFormat(model, metadataPerMode))
})

test_that("convertModelFormat throws a warning with metadataPerMode with the wrong number of rows", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=1, nstart=1, verbose=FALSE)

  brokenFeatureMetadata = processedFujita$taxonomy[1:10,]

  metadataPerMode = list(processedFujita$mode1, brokenFeatureMetadata, processedFujita$mode3)
  expect_warning(convertModelFormat(model, metadataPerMode))
})

test_that("convertModelFormat does not throw an error with correct input model", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=1, nstart=1, verbose=FALSE)

  metadataPerMode = list(processedFujita$mode1, processedFujita$mode2, processedFujita$mode3)
  expect_no_error(convertModelFormat(model, metadataPerMode))
})

test_that("convertModelFormat throws an error even if model input is a list but not a parafac object", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  model = list("A"=c(1,1,1), "B"=c(2,2,2), "C"=c(3,3,3))

  metadataPerMode = list(Fujita2023$mode1, Fujita2023$mode2, Fujita2023$mode3)
  expect_error(convertModelFormat(model, metadataPerMode))
})

test_that("convertModelFormat produces a list of length 3 for 3-way parafac models", {
  withr::local_package("multiway")
  withr::local_package("dplyr")

  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=1, nstart=1, verbose=FALSE)

  metadataPerMode = list(processedFujita$mode1, processedFujita$mode2, processedFujita$mode3)
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

  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=1, nstart=1, verbose=FALSE)
  expect_no_error(convertModelFormat(model))
})

test_that("reinflateBlock gives a 2-way matrix if you give it two column vectors", {
  loadingVectors = list(matrix(rnorm(20), nrow=20, ncol=1),
                  matrix(rnorm(50), nrow=50, ncol=1))
  X = reinflateBlock(loadingVectors)
  expect_equal(length(dim(X)), 2)
})

test_that("reinflateBlock gives a 3-way cube if you give it three column vectors", {
  loadingVectors = list(matrix(rnorm(20), nrow=20, ncol=1),
                  matrix(rnorm(50), nrow=50, ncol=1),
                  matrix(rnorm(4), nrow=4, ncol=1))
  X = reinflateBlock(loadingVectors)
  expect_equal(length(dim(X)), 3)
})

test_that("reinflateBlock throws an error if you give it unequal components over the vectors", {
  loadingVectors = list(matrix(rnorm(40), nrow=20, ncol=2),
                        matrix(rnorm(150), nrow=50, ncol=3),
                        matrix(rnorm(8), nrow=4, ncol=2))
  expect_error(reinflateBlock(loadingVectors))
})

test_that("reinflateBlock cannot deal with more than 3 components", {
  loadingVectors = list(matrix(rnorm(40), nrow=20, ncol=2),
                        matrix(rnorm(100), nrow=50, ncol=2),
                        matrix(rnorm(8), nrow=4, ncol=2),
                        matrix(rnorm(6), nrow=3, ncol=2))
  expect_error(reinflateBlock(loadingVectors))
})

test_that("reinflateBlock cannot deal with non-lists", {
  loadingVectors = c(rnorm(20), rnorm(50), rnorm(4))
  expect_error(reinflateBlock(loadingVectors))
})

test_that("reinflateBlock can deal with parafac objects", {
  loadingVectors = list(matrix(rnorm(40), nrow=20, ncol=2),
                        matrix(rnorm(100), nrow=50, ncol=2),
                        matrix(rnorm(8), nrow=4, ncol=2))
  X = reinflateBlock(loadingVectors)
  model = parafac(X, nfac=2, nstart=1, verbose=FALSE)
  expect_no_error(reinflateBlock(model))
})

test_that("Size of A is the same as the transformed version of A in transformPARAFACloadings", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=2, nstart=1, verbose=FALSE)
  expect_equal(dim(model$A), dim(transformPARAFACloadings(model, 1)))
})

test_that("transformPARAFACloadings changes A", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=2, nstart=1, verbose=FALSE)

  expect_false(identical(transformPARAFACloadings(model, 1), model$A))
})

test_that("Size of B is the same as the transformed version of B in transformPARAFACloadings", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=2, nstart=1, verbose=FALSE)
  expect_equal(dim(model$B), dim(transformPARAFACloadings(model, 2)))
})

test_that("transformPARAFACloadings changes B", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=1, nstart=1, verbose=FALSE)

  expect_false(identical(transformPARAFACloadings(model, 2), model$B))
})

test_that("Size of C is the same as the transformed version of C in transformPARAFACloadings", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=2, nstart=1, verbose=FALSE)
  expect_equal(dim(model$C), dim(transformPARAFACloadings(model, 3)))
})

test_that("transformPARAFACloadings changes C", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=2, nstart=1, verbose=FALSE)

  expect_false(identical(transformPARAFACloadings(model, 3), model$C))
})

test_that("transformPARAFACloadings can deal with a PARAFAC object", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=2, nstart=1, verbose=FALSE)
  expect_equal(dim(model$C), dim(transformPARAFACloadings(model, 3)))
})

test_that("transformPARAFACloadings can deal with a list object", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=2, nstart=1, verbose=FALSE)

  model_as_list = list()
  model_as_list[[1]] = model$A
  model_as_list[[2]] = model$B
  model_as_list[[3]] = model$C
  expect_no_error(transformPARAFACloadings(model_as_list, 1))
})

test_that("transformPARAFACloadings can deal with a one-component model in a list", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=1, nstart=1, verbose=FALSE)

  model_as_list = list()
  model_as_list[[1]] = model$A
  model_as_list[[2]] = model$B
  model_as_list[[3]] = model$C
  expect_no_error(transformPARAFACloadings(model_as_list, 1))
})

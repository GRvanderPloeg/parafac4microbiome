test_that("modelStability produces a length 3 list", {
  output = modelStabilityCheck(Fujita2023$data, Fujita2023$sampleMetadata, numComponents=1, numRepetitions=10)
  expect_equal(length(output), 3)
})

test_that("Diag(A) in comp 1 is a vector of NAs because it was removed in regular LOOCV", {
  output = modelStabilityCheck(Fujita2023$data, Fujita2023$sampleMetadata, numComponents=1, numRepetitions=10)
  expect_equal(is.na(diag(output[[1]][[1]])), rep(TRUE, nrow(Fujita2023$data)))
})

test_that("Length of the list of A is equal to the number of components", {
  output = modelStabilityCheck(Fujita2023$data, Fujita2023$sampleMetadata, numComponents=5, numRepetitions=10)
  expect_equal(length(output[[1]]), 5)
})

test_that("Dim of A is equal to numRows(X) by numReptitions", {
  output = modelStabilityCheck(Fujita2023$data, Fujita2023$sampleMetadata, numComponents=1, numRepetitions=10)
  expect_equal(dim(output[[1]][[1]]), c(nrow(Fujita2023$data), 10))
})

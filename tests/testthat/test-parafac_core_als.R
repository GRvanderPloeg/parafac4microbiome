test_that("parafac core ALS does not throw errors", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  init = initializePARAFAC(X, 2)
  expect_no_error(parafac_core_als(X, 2, init))
})

test_that("Fac object contains the right number of modes", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  init = initializePARAFAC(X, 2)
  model = parafac_core_als(X, 2, init)
  expect_equal(length(model$Fac), 3)
})

test_that("The norm of B is always 1", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  init = initializePARAFAC(X, 2)
  model = parafac_core_als(X, 2, init)
  expect_equal(apply(model$Fac[[2]], 2, function(x){norm(as.matrix(x))}), c(1,1))
})

test_that("The norm of C is always 1", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  init = initializePARAFAC(X, 2)
  model = parafac_core_als(X, 2, init)
  expect_equal(apply(model$Fac[[3]], 2, function(x){norm(as.matrix(x))}), c(1,1))
})

test_that("Input mode 1 is found", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  init = initializePARAFAC(X, 2)
  model = parafac_core_als(X, 2, init)
  expect_equal(abs(cor(model$Fac[[1]], A))[1,1], 1, tolerance=0.01)
})

test_that("Input mode 2 is found", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  init = initializePARAFAC(X, 2)
  model = parafac_core_als(X, 2, init)
  expect_equal(abs(cor(model$Fac[[2]], B))[1,1], 1, tolerance=0.01)
})

test_that("Input mode 3 is found", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  init = initializePARAFAC(X, 2)
  model = parafac_core_als(X, 2, init)
  expect_equal(abs(cor(model$Fac[[3]], C))[1,1], 1, tolerance=0.01)
})

test_that("All fs are larger than zero", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  init = initializePARAFAC(X, 2)
  model = parafac_core_als(X, 2, init)
  expect_true(all(model$fs > 0))
})

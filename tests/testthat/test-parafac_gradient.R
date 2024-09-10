test_that("g can be computed with a fac object", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  init = initializePARAFAC(X, 2, output="Fac")
  expect_no_error(parafac_gradient(init, X))
})

test_that("g can be computed with a vect object", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  init = initializePARAFAC(X, 2, output="vect")
  expect_no_error(parafac_gradient(init, X))
})

test_that("g is nonzero if any solution is found", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  Fac = initializePARAFAC(X, 2)
  g = parafac_gradient(Fac, X)
  expect_true(any(g >= 0))
})

test_that("g is zero if the perfect solution is found", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  Fac = list(A, B, C)
  g = parafac_gradient(Fac, X)
  expect_true(all(g==0))
})

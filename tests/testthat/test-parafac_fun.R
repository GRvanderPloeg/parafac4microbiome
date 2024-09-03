test_that("f is nonzero if any solution is found", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  model = parafac(X, 2, maxit=2)
  f = parafac_fun(X, model$Fac)
  expect_gt(f, 0)
})

test_that("f is zero if the perfect solution is found", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  Fac = list(A, B, C)
  f = parafac_fun(X, Fac)
  expect_equal(f, 0)
})

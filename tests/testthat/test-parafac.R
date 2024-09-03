test_that("Array input throws no errors", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=FALSE)
  expect_no_error(parafac(X, 2))
})

test_that("Tensor input throws no errors", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  expect_no_error(parafac(X, 2))
})

test_that("Xhat has the right dimensions", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  model = parafac(X, 2)
  expect_equal(dim(model$Xhat), c(108,100,10))
})

test_that("Number of iterations is more than 1", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  model = parafac(X, 2)
  expect_gt(model$iter, 1)
})

test_that("SSE is larger than 0", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  model = parafac(X, 2)
  expect_gt(model$SSE, 0)
})

test_that("Varexp is greater than 0", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  model = parafac(X, 2)
  expect_gt(model$varExp, 0)
})

test_that("Varexp is smaller than or equal to 100", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  model = parafac(X, 2)
  expect_true(model$varExp <= 100.01)
})

test_that("nstart larger than 1 returns nstart models if output is all", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  models = parafac(X, 2, nstart=10, output="all")
  expect_equal(length(models), 10)
})

test_that("ALS and multiway yield similar models", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=FALSE)
  model_als = parafac(X, 2, nstart=10)
  model_mw = multiway::parafac(X, 2, nstart=10, verbose=FALSE)

  Xhat_als = reinflateTensor(model_als$Fac[[1]], model_als$Fac[[2]], model_als$Fac[[3]])
  Xhat_mw = reinflateTensor(model_mw$A, model_mw$B, model_mw$C)
  expect_equal(Xhat_als, Xhat_mw)
})

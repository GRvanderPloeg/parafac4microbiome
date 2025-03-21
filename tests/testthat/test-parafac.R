test_that("Array input throws no errors (ALS case)", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=FALSE)
  expect_no_error(parafac(X, 2, method="als", maxit=2))
})

test_that("Array input throws no errors (OPT case)", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=FALSE)
  expect_no_error(parafac(X, 2, method="opt", maxit=2))
})

test_that("Tensor input throws no errors (ALS case)", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  expect_no_error(parafac(X, 2, method="als", maxit=2))
})

test_that("Tensor input throws no errors (OPT case)", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  expect_no_error(parafac(X, 2, method="opt", maxit=2))
})

test_that("Xhat has the right dimensions (ALS case)", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  model = parafac(X, 2, method="als", maxit=2)
  expect_equal(dim(model$Xhat), c(108,100,10))
})

test_that("Xhat has the right dimensions (OPT case)", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  model = parafac(X, 2, method="opt", maxit=2)
  expect_equal(dim(model$Xhat), c(108,100,10))
})

test_that("Number of iterations is more than 1 (ALS case)", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  model = parafac(X, 2, method="als", maxit=20)
  expect_gt(model$iter, 1)
})

test_that("Number of iterations is more than 1 (OPT case)", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  model = parafac(X, 2, method="opt", maxit=20)
  expect_gt(model$iter, 1)
})

test_that("SSE is larger than 0 (ALS case)", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  model = parafac(X, 2, method="als", maxit=2)
  expect_gt(model$SSE, 0)
})

test_that("SSE is larger than 0 (OPT case)", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  model = parafac(X, 2, method="opt", maxit=2)
  expect_gt(model$SSE, 0)
})

test_that("Varexp is greater than 0 (ALS case)", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  model = parafac(X, 2, method="als", maxit=2)
  expect_gt(model$varExp, 0)
})

test_that("Varexp is greater than 0 (OPT case)", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  model = parafac(X, 2, method="opt", maxit=2)
  expect_gt(model$varExp, 0)
})

test_that("Varexp is smaller than or equal to 100 (ALS case)", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  model = parafac(X, 2, method="als", maxit=20)
  expect_true(model$varExp <= 100.01)
})

test_that("Varexp is smaller than or equal to 100 (OPT case)", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  model = parafac(X, 2, method="opt", maxit=20)
  expect_true(model$varExp <= 100.01)
})

test_that("nstart larger than 1 returns nstart models if output is all (ALS case)", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  models = parafac(X, 2, nstart=10, output="all", method="als", maxit=2)
  expect_equal(length(models), 10)
})

test_that("nstart larger than 1 returns nstart models if output is all (OPT case)", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  models = parafac(X, 2, nstart=10, output="all", method="opt", maxit=2)
  expect_equal(length(models), 10)
})

test_that("ALS and multiway yield similar models in the easy case", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=FALSE)
  model_als = parafac(X, 2, nstart=10, ctol=1e-4, method="als")
  model_mw = multiway::parafac(X, 2, nstart=10, verbose=FALSE)

  Xhat_als = reinflateTensor(model_als$Fac[[1]], model_als$Fac[[2]], model_als$Fac[[3]], returnAsTensor=TRUE)
  Xhat_mw = reinflateTensor(model_mw$A, model_mw$B, model_mw$C, returnAsTensor=TRUE)
  expect_equal(rTensor::fnorm(Xhat_als), rTensor::fnorm(Xhat_mw), tolerance=1e-4)
})

test_that("ALS and OPT yield similar models in the easy case", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=FALSE)
  model_als = parafac(X, 2, nstart=10, ctol=1e-4, method="als")
  model_opt = parafac(X, 2, nstart=10, rel_tol=1e-6, abs_tol=1e-6, grad_tol=1e-6, maxit=250, method="opt")

  Xhat_als = reinflateTensor(model_als$Fac[[1]], model_als$Fac[[2]], model_als$Fac[[3]], returnAsTensor=TRUE)
  Xhat_opt = reinflateTensor(model_opt$Fac[[1]], model_opt$Fac[[2]], model_opt$Fac[[3]], returnAsTensor=TRUE)
  expect_equal(rTensor::fnorm(Xhat_als), rTensor::fnorm(Xhat_opt), tolerance=0.1)
})

test_that("ALS and multiway yield similar models in the fujita case", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, CLR=TRUE, centerMode=1, scaleMode=2)
  X = processedFujita$data
  model_als = parafac(X, 3, nstart=10, ctol=1e-4, method="als")
  model_mw = multiway::parafac(X, 3, nstart=10, verbose=FALSE)

  Xhat_als = reinflateTensor(model_als$Fac[[1]], model_als$Fac[[2]], model_als$Fac[[3]], returnAsTensor=TRUE)
  Xhat_mw = reinflateTensor(model_mw$A, model_mw$B, model_mw$C, returnAsTensor=TRUE)
  expect_equal(rTensor::fnorm(Xhat_als), rTensor::fnorm(Xhat_mw), tolerance=0.1)
})

test_that("ALS and multiway yield similar models in the shao case", {
  processedShao = processDataCube(Shao2019, sparsityThreshold=0.9, considerGroups=TRUE, groupVariable="Delivery_mode", CLR=TRUE, centerMode=1, scaleMode=2)
  X = processedShao$data
  X[is.na(X)] = 0 # force NAs to zero otherwise discrepancies occur
  model_als = parafac(X, 3, nstart=10, ctol=1e-4, method="als")
  model_mw = multiway::parafac(X, 3, nstart=10, verbose=FALSE)

  Xhat_als = reinflateTensor(model_als$Fac[[1]], model_als$Fac[[2]], model_als$Fac[[3]], returnAsTensor=TRUE)
  Xhat_mw = reinflateTensor(model_mw$A, model_mw$B, model_mw$C, returnAsTensor=TRUE)
  expect_equal(rTensor::fnorm(Xhat_als), rTensor::fnorm(Xhat_mw), tolerance=0.1)
})

test_that("ALS and multiway yield similar models in the ploeg case", {
  processedPloeg = processDataCube(vanderPloeg2024$upper_jaw_lingual, sparsityThreshold=0.50, considerGroups=TRUE, groupVariable="RFgroup", CLR=TRUE, centerMode=1, scaleMode=2)
  X = processedPloeg$data
  X[is.na(X)] = 0 # force NAs to zero otherwise discrepancies occur
  model_als = parafac(X, 3, nstart=10, ctol=1e-4, method="als")
  model_mw = multiway::parafac(X, 3, nstart=10, verbose=FALSE)

  Xhat_als = reinflateTensor(model_als$Fac[[1]], model_als$Fac[[2]], model_als$Fac[[3]], returnAsTensor=TRUE)
  Xhat_mw = reinflateTensor(model_mw$A, model_mw$B, model_mw$C, returnAsTensor=TRUE)
  expect_equal(rTensor::fnorm(Xhat_als), rTensor::fnorm(Xhat_mw), tolerance=0.1)
})

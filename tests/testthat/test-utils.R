test_that("flipLoadings throws no errors", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  X = reinflateTensor(A, B, C)
  models = parafac(X, 2, nstart=10, output="all", sortComponents=TRUE)
  expect_no_error(flipLoadings(models, X))
})

test_that("flipLoadings produces a same length list as models input", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  X = reinflateTensor(A, B, C)
  models = parafac(X, 2, nstart=10, output="all", sortComponents=TRUE)
  flippedModels = flipLoadings(models, X)
  expect_equal(length(models), length(flippedModels))
})

test_that("flipLoadings Fac is not the same as models fac", {
  set.seed(123)
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, CLR=TRUE, centerMode=1, scaleMode=2)
  X = processedFujita$data
  models = parafac(X, 2, nstart=10, output="all", sortComponents=TRUE)
  flippedModels = flipLoadings(models, X)

  allFacs1 = lapply(models, function(x){x$Fac})
  allFacs2 = lapply(flippedModels, function(x){x$Fac})

  expect_false(isTRUE(all.equal(allFacs1, allFacs2)))
})

test_that("reinflateTensor throws no errors", {
  set.seed(123)
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  expect_no_error(reinflateTensor(A, B, C))
})

test_that("reinflateTensor throws no errors in the one-component case", {
  set.seed(123)
  A = rnorm(108)
  B = rnorm(100)
  C = rnorm(10)
  expect_no_error(reinflateTensor(A, B, C))
})

test_that("reinflateFac throws no errors", {
  X = array(rnorm(108*100*10), c(108,100,10))
  model = parafac(X, 2)
  expect_no_error(reinflateFac(model$Fac, X))
})

test_that("reinflateFac throws no errors in the one-component case", {
  X = array(rnorm(108*100*10), c(108,100,10))
  model = parafac(X, 1)
  expect_no_error(reinflateFac(model$Fac, X))
})

test_that("reinflateFac throws no errors when lambdas are supplied", {
  X = array(rnorm(108*100*10), c(108,100,10))
  model = parafac(X, 2)
  model$Fac[[4]] = c(1,1)
  expect_no_error(reinflateFac(model$Fac, X))
})

test_that("calcVarExpPerComponent throws no errors", {
  X = array(rnorm(108*100*10), c(108,100,10))
  model = parafac(X, 2)
  expect_no_error(calcVarExpPerComponent(model$Fac, X))
})

test_that("sumsqr throws no errors in the vector case", {
  X = rnorm(100)
  expect_no_error(sumsqr(X))
})

test_that("sumsqr throws no errors in the matrix case", {
  X = array(rnorm(100*10), c(100,10))
  expect_no_error(sumsqr(X))
})

test_that("sumsqr throws no errors in the tensor case", {
  X = array(rnorm(108*100*10), c(108,100,10))
  expect_no_error(sumsqr(X))
})

test_that("calcVarExpPerComponent throws no errors", {
  X = array(rnorm(108*100*10), c(108,100,10))
  model = parafac(X, 2)
  expect_no_error(calcVarExpPerComponent(model$Fac, X))
})

test_that("calcVarExpPerComponent throws no errors in the one-component case", {
  X = array(rnorm(108*100*10), c(108,100,10))
  model = parafac(X, 1)
  expect_no_error(calcVarExpPerComponent(model$Fac, X))
})

test_that("calcVarExpPerComponent output is length numComponents", {
  X = array(rnorm(108*100*10), c(108,100,10))
  model = parafac(X, 5)
  result = calcVarExpPerComponent(model$Fac, X)
  expect_equal(length(result), 5)
})

test_that("sortComponents throws no errors", {
  X = array(rnorm(108*100*10), c(108,100,10))
  model = parafac(X, 5, sortComponents=FALSE)
  expect_no_error(sortComponents(model$Fac, X))
})

test_that("sortComponents resorts components", {
  set.seed(456)
  A = array(rnorm(108*5), c(108, 5))
  B = array(rnorm(100*5), c(100, 5))
  C = array(rnorm(10*5), c(10, 5))

  # Inject an ordering
  componentSizes = c(3, 5, 9, 8, 1)
  for(i in 1:5){
    A[,i] = A[,i] * componentSizes[i]
    B[,i] = B[,i] * componentSizes[i]
    C[,i] = C[,i] * componentSizes[i]
  }

  X = reinflateTensor(A, B, C)
  Fac = list(A, B, C)
  newFac = sortComponents(Fac, X)
  expect_false(all(unlist(Fac) == unlist(newFac)))
})

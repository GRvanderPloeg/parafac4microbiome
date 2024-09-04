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
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, CLR=TRUE, centerMode=1, scaleMode=2)
  X = processedFujita$data
  models = parafac(X, 2, nstart=10, output="all", sortComponents=TRUE)
  flippedModels = flipLoadings(models, X)

  allFacs1 = lapply(models, function(x){x$Fac})
  allFacs2 = lapply(flippedModels, function(x){x$Fac})

  expect_false(isTRUE(all.equal(allFacs1, allFacs2)))
})

test_that("calcVarExpPerComponent throws no errors", {
  X = array(rnorm(108*100*10), c(108,100,10))
  model = parafac(X, 2)
  expect_no_error(calcVarExpPerComponent(model$Fac, X))
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

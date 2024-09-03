test_that("Size of A is the same as the transformed version of A in transformPARAFACloadings", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=2, nstart=1, verbose=FALSE)
  expect_equal(dim(model$Fac[[1]]), dim(transformPARAFACloadings(model$Fac, 1)))
})

test_that("transformPARAFACloadings changes A", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=2, nstart=1, verbose=FALSE)

  expect_false(identical(transformPARAFACloadings(model$Fac, 1), model$Fac[[1]]))
})

test_that("Size of B is the same as the transformed version of B in transformPARAFACloadings", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=2, nstart=1, verbose=FALSE)
  expect_equal(dim(model$Fac[[2]]), dim(transformPARAFACloadings(model$Fac, 2)))
})

test_that("transformPARAFACloadings changes B", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=1, nstart=1, verbose=FALSE)

  expect_false(identical(transformPARAFACloadings(model$Fac, 2), model$Fac[[2]]))
})

test_that("Size of C is the same as the transformed version of C in transformPARAFACloadings", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=2, nstart=1, verbose=FALSE)
  expect_equal(dim(model$Fac[[3]]), dim(transformPARAFACloadings(model$Fac, 3)))
})

test_that("transformPARAFACloadings changes C", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=2, nstart=1, verbose=FALSE)

  expect_false(identical(transformPARAFACloadings(model$Fac, 3), model$C))
})

test_that("transformPARAFACloadings can deal with a list object", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=2, nstart=1, verbose=FALSE)
  expect_no_error(transformPARAFACloadings(model$Fac, 1))
})

test_that("transformPARAFACloadings can deal with a one-component model in a list", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=1, nstart=1, verbose=FALSE)
  expect_no_error(transformPARAFACloadings(model$Fac, 1))
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

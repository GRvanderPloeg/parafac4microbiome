test_that("The norm of B is always 1", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=3, nstart=1, ctol=1e-6, maxit=2500, verbose=FALSE)
  expect_equal(apply(model$Fac[[2]], 2, function(x){norm(as.matrix(x))}), c(1,1,1))
})

test_that("The norm of C is always 1", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=3, nstart=1, ctol=1e-6, maxit=2500, verbose=FALSE)
  expect_equal(apply(model$Fac[[3]], 2, function(x){norm(as.matrix(x))}), c(1,1,1))
})

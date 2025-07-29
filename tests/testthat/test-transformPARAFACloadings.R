test_that("transformPARAFACloadings can deal with a list object", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=2, nstart=1)
  expect_no_error(transformPARAFACloadings(model$Fac, 1))
})

test_that("transformPARAFACloadings can deal with a one-component model in a list", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=1, nstart=1)
  expect_no_error(transformPARAFACloadings(model$Fac, 1))
})

test_that("Size of A is the same as the transformed version of A in transformPARAFACloadings", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=2, nstart=1)
  expect_equal(dim(model$Fac[[1]]), dim(transformPARAFACloadings(model$Fac, 1)))
})

test_that("Size of B is the same as the transformed version of B in transformPARAFACloadings", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=2, nstart=1)
  expect_equal(dim(model$Fac[[2]]), dim(transformPARAFACloadings(model$Fac, 2)))
})

test_that("Size of C is the same as the transformed version of C in transformPARAFACloadings", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=2, nstart=1)
  expect_equal(dim(model$Fac[[3]]), dim(transformPARAFACloadings(model$Fac, 3)))
})

test_that("transformPARAFACloadings changes A", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=2, nstart=1)

  expect_false(identical(transformPARAFACloadings(model$Fac, 1), model$Fac[[1]]))
})

test_that("transformPARAFACloadings changes B", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=1, nstart=1)

  expect_false(identical(transformPARAFACloadings(model$Fac, 2), model$Fac[[2]]))
})

test_that("transformPARAFACloadings changes C", {
  processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)
  model = parafac(processedFujita$data, nfac=2, nstart=1)

  expect_false(identical(transformPARAFACloadings(model$Fac, 3), model$C))
})

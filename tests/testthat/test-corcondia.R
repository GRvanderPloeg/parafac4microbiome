test_that("CORCONDIA works with normal input", {
  X = Fujita2023$data
  model = parafac(X, nfac=2, nstart=1, verbose=FALSE)
  expect_no_error(corcondia(X, model$Fac))
})

test_that("CORCONDIA works for a cube with NAs", {
  processedShao = processDataCube(Shao2019, sparsityThreshold=0.9, considerGroups=TRUE, groupVariable="Delivery_mode", centerMode=1, scaleMode=2)
  X = processedShao$data
  model = parafac4microbiome::parafac(X, nfac=2, nstart=1, verbose=FALSE)
  expect_no_error(corcondia(X, model$Fac))
})

test_that("The CORCONDIA of a one-component model is always 100", {
  X = Fujita2023$data
  model = parafac4microbiome::parafac(X, nfac=1, nstart=1, verbose=FALSE)
  expect_equal(corcondia(X, model$Fac), 100)
})

test_that("CORCONDIA throws no errors when missing values are present in X", {
  X = Fujita2023$data
  X[1,1,1] = NA
  model = parafac4microbiome::parafac(X, nfac=1, nstart=1, verbose=FALSE)
  expect_no_error(corcondia(X, model$Fac))
})

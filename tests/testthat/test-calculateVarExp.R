test_that("The function works when there are no NAs in the cube", {
  X = Fujita2023$data
  model = multiway::parafac(X, nfac=1, nstart=1, verbose=FALSE)
  expect_no_error(calculateVarExp(model, X))
})

test_that("The function works when there are NAs in the cube", {
  X = Shao2019$data
  model = multiway::parafac(X, nfac=1, nstart=1, verbose=FALSE)
  expect_no_error(calculateVarExp(model, X))
})

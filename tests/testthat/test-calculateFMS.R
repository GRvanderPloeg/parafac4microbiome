test_that("calculateFMS throws no errors", {
  set.seed(123)
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  X = reinflateTensor(A, B, C)

  models = parafac(X, 2, initialization="random", nstart=2, maxit=2, output="all")
  expect_no_error(calculateFMS(models))
})

test_that("calculateFMS finds that identical models have FMS 1", {
  set.seed(123)
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  X = reinflateTensor(A, B, C)

  model1 = list()
  model1$Fac = list(A, B, C)
  model2 = list()
  model2$Fac = list(A, B, C)
  expect_equal(calculateFMS(list(model1, model2)), 1)
})

test_that("calculateFMS finds that non-identical models have FMS less than 1", {
  set.seed(123)
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  X = reinflateTensor(A, B, C)

  models = parafac(X, 2, initialization="random", nstart=2, maxit=2, output="all")
  expect_lt(calculateFMS(models), 1)
})

test_that("calculateFMS finds that non-identical models have FMS >= 0", {
  set.seed(123)
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  X = reinflateTensor(A, B, C)

  models = parafac(X, 2, initialization="random", nstart=2, maxit=2, output="all")
  expect_true(calculateFMS(models) >= 0)
})

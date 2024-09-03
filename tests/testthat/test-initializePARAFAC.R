test_that("Matrix input works", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C)
  expect_no_error(initializePARAFAC(X, 2))
})

test_that("Tensor input works", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  expect_no_error(initializePARAFAC(X, 2))
})

test_that("Random initialization throws no errors", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  expect_no_error(initializePARAFAC(X, 2, initialization="random"))
})

test_that("Randomly initialized inits are different between runs", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  init1 = initializePARAFAC(X,2)
  init2 = initializePARAFAC(X,2)
  expect_false(all(init1[[1]] == init2[[1]]))
})

test_that("Nvec initialization throws no errors", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  expect_no_error(initializePARAFAC(X, 2, initialization="nvec"))
})

test_that("List size is equal to the number of modes", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  init = initializePARAFAC(X,2)
  expect_equal(length(init), 3)
})

test_that("Element size is equal to the number of components", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  init = initializePARAFAC(X,2)
  expect_equal(ncol(init[[1]]), 2)
})

test_that("The correct mode 1 components are found using nvec", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  init = initializePARAFAC(X, 2, initialization="nvec")
  expect_equal(abs(cor(init[[1]], A))[1,1], 1, tolerance=0.01)
})

test_that("The correct mode 2 components are found using nvec", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  init = initializePARAFAC(X, 2, initialization="nvec")
  expect_equal(abs(cor(init[[2]], B))[1,1], 1, tolerance=0.01)
})

test_that("The correct mode 3 components are found using nvec", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  init = initializePARAFAC(X, 2, initialization="nvec")
  expect_equal(abs(cor(init[[3]], C))[1,1], 1, tolerance=0.01)
})

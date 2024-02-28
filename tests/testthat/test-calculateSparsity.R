test_that("The minimum value of the sparsity output vector is larger than 0", {
  expect_gte(min(calculateSparsity(Fujita2023$data)), 0)
})

test_that("The maximum value of the sparsity output vector is less than 1", {
  expect_lte(max(calculateSparsity(Fujita2023$data)), 1)
})

test_that("The length of the sparsity output vector is equal to J", {
  cube = Fujita2023$data
  J = dim(cube)[2]
  expect_length(calculateSparsity(cube), J)
})

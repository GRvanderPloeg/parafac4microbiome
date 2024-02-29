test_that("The minimum value of the sparsity output vector is larger than 0", {
  expect_gte(min(calculateSparsity(Fujita2023)), 0)
})

test_that("The maximum value of the sparsity output vector is less than 1", {
  expect_lte(max(calculateSparsity(Fujita2023)), 1)
})

test_that("The length of the sparsity output vector is equal to J", {
  J = dim(Fujita2023$data)[2]
  expect_length(calculateSparsity(Fujita2023), J)
})

test_that("Sparsity calculation ignores NAs", {
  expect_false(any(is.na(calculateSparsity(Shao2019))))
})

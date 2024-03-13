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

test_that("Sparsity calculation does not throw an error when setting only a group variable", {
  expect_no_error(calculateSparsity(Shao2019, groupVariable="Delivery_mode"))
})

test_that("Sparsity calculation gives a warning when setting only considerGroups", {
  expect_warning(calculateSparsity(Shao2019, considerGroups=TRUE))
})

test_that("Sparsity calculation can be done taking groups into account", {
  expect_no_error(calculateSparsity(Shao2019, considerGroups=TRUE, groupVariable="Delivery_mode"))
})

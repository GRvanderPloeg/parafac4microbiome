test_that("Input and output cube sizes are equal", {
  cube = Fujita2023$data
  cubeCLR = multiwayCLR(cube)
  expect_equal(dim(cubeCLR), dim(cube))
})

test_that("multiwayCLR propagates NAs corresponding to missing samples", {
  expect_true(any(is.na(multiwayCLR(Shao2019$data))))
})

test_that("a sample sums to zero", {
  X = Fujita2023$data
  X_clr = multiwayCLR(X)
  expect_equal(sum(X_clr[1,,1]), 0)
})

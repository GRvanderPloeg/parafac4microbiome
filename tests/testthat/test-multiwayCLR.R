test_that("Input and output cube sizes are equal", {
  cube = Fujita2023$data
  cubeCLR = multiwayCLR(cube)
  expect_equal(dim(cubeCLR), dim(cube))
})

test_that("multiwayCLR propagates NAs corresponding to missing samples", {
  expect_true(any(is.na(multiwayCLR(Shao2019$data))))
})

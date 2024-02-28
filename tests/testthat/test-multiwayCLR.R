test_that("Input and output cube sizes are equal", {
  cube = Fujita2023$data
  cubeCLR = multiwayCLR(cube)
  expect_equal(dim(cubeCLR), dim(cube))
})

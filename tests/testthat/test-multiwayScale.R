test_that("Output size is equal to input size", {
  cube_scl = multiwayScale(Fujita2023$data)
  expect_equal(dim(cube_scl), dim(Fujita2023$data))
})

test_that("Mode 1 is scaled correctly", {
  cube_scl = multiwayScale(Fujita2023$data, mode=1)
  expect_equal(sd(cube_scl[1,,]), 1)
})

test_that("Mode 2 is scaled correctly", {
  cube_scl = multiwayScale(Fujita2023$data, mode=2)
  expect_equal(sd(cube_scl[,1,]), 1)
})

test_that("Mode 3 is scaled correctly", {
  cube_scl = multiwayScale(Fujita2023$data, mode=3)
  expect_equal(sd(cube_scl[,,1]), 1)
})

test_that("NAs propagate correctly", {
  cube_scl = multiwayScale(Shao2019$data)
  expect_true(any(is.na(cube_scl)))
})

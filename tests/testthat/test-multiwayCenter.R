test_that("Output size is equal to input size", {
  cube_cnt = multiwayCenter(Fujita2023$data)
  expect_equal(dim(cube_cnt), dim(Fujita2023$data))
})

test_that("Mode 1 is centered correctly", {
  cube_cnt = multiwayCenter(Fujita2023$data, mode=1)
  result = colMeans(cube_cnt[,,1])
  expect_equal(result, rep(0, length(result)))
})

test_that("Mode 2 is centered correctly", {
  cube_cnt = multiwayCenter(Fujita2023$data, mode=2)
  result = colMeans(cube_cnt[1,,])
  expect_equal(result, rep(0, length(result)))
})

test_that("Mode 3 is centered correctly", {
  cube_cnt = multiwayCenter(Fujita2023$data, mode=3)
  result = rowMeans(cube_cnt[1,,])
  expect_equal(result, rep(0, length(result)))
})

test_that("NAs propagate correctly", {
  cube_cnt = multiwayCenter(Shao2019$data)
  expect_true(any(is.na(cube_cnt)))
})

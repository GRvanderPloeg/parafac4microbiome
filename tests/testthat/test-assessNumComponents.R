test_that("Output in metrics has the right shape", {
  output = assessNumComponents(Fujita2023$data, minNumComponents=1, maxNumComponents=3, numRepetitions=10)
  expect_equal(dim(output$metrics$numIterations), c(10,3))
})

test_that("Output models is a list of maxNumComponents objects", {
  output = assessNumComponents(Fujita2023$data, minNumComponents=1, maxNumComponents=3, numRepetitions=10)
  expect_equal(length(output$models), 3)
})

test_that("Each item in output models is length numRepetitions", {
  output = assessNumComponents(Fujita2023$data, minNumComponents=1, maxNumComponents=3, numRepetitions=10)
  expect_equal(length(output$models[[1]]), 10)
})

test_that("A nonsensical minNumComponents throws an error", {
  expect_error(assessNumComponents(Fujita2023$data, minNumComponents=-3, maxNumComponents=3, numRepetitions=10))
})

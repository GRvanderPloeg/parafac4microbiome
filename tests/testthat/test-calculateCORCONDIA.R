test_that("Any one-component PARAFAC model results CORCONDIA 100", {
  withr::local_package("multiway")
  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
  expect_equal(calculateCORCONDIA(Fujita2023$data, model), 100)
})

test_that("A problematic minComponent setting throws an error", {
  withr::local_package("multiway")
  model = parafac(Fujita2023$data, nfac=1, nstart=1, verbose=FALSE)
  expect_error(calculateCORCONDIA(Fujita2023$data, model, minComponents=-1))
})

test_that("Not giving a PARAFAC object throws an error", {
  withr::local_package("multiway")
  model = list("A"=c(1:10), "B"=c("2:20"), "C"=c("3:30"))
  expect_error(calculateCORCONDIA(Fujita2023$data, model))
})

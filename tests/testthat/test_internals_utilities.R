library(testthat)
set.seed(1)
test_that("colCumsum works correctly", {
  
  x <- matrix(rnorm(100), nrow = 10, ncol = 10)
  expect_equal(colCumsum(x), apply(x, 2, cumsum))
  
})
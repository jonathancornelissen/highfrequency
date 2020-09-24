library(testthat)

library(highfrequency)

context("leadLag")
test_that("leadLag", {
  #skip_on_os("solaris")
  ## Test that the contrasts are as expected
  ll <- leadLag(100 + cumsum(sbux), 100 + cumsum(lltc), seq(-10,10), normalize = TRUE)
  
  expected <- c(0.033229285,0.049053654,0.075348393,0.090688956,0.122192922,0.153177587,0.176829968,0.198500572,0.229967633,0.285702625,0.298824786,
  0.267945966,0.179464691,0.137441360,0.098206314,0.081605315,0.068544813,0.055210317,0.040167523,0.025934535,0.004943045)
  expect_equal(ll$contrasts, expected)
  ## Test that the parallelized code gives the same results.
  ll2 <- leadLag(100 + cumsum(sbux), 100 + cumsum(lltc), seq(-10,10), normalize = TRUE, parallelize = TRUE, nCores = 2)
  expect_true(all.equal(ll,ll2))
  
  
  ## Test that the contrasts are the same when using 10000 ms and 10 sec
  llMS <- leadLag(100 + cumsum(sbux), 100 + cumsum(lltc), seq(-10,10) * 1000, resolution = "ms", normalize = TRUE)
  
  expect_equal(ll$contrasts, llMS$contrasts)
  
})
library(testthat)
library(xts)
library(highfrequency)
set.seed(123)
start <- strptime("1970-01-01", format = "%Y-%m-%d", tz = "UTC")
timestamps <- start + seq(34200, 57600, length.out = 23401)

dat <- cbind(rnorm(23401) * sqrt(1/23401), rnorm(23401) * sqrt(1/23401))

dat <- exp(cumsum(xts(dat, timestamps)))
price1 <- dat[,1]
price2 <- dat[,2]
context("leadLag")
test_that("leadLag", {
  # skip_on_os("solaris")
  ## Test that the contrasts are as expected
  ll <- leadLag(price1, price2, seq(-10,10), normalize = FALSE)
  expected <- c(0.0078886083678, 0.0031970569232, 0.0034059655020, 0.0005827035825, 0.0092298910309, 0.0005371077285, 0.0062552641572, 0.0066002420885, 
                0.0071338769373, 0.0003748876188, 0.0082603952221, 0.0052754845599, 0.0023557973579, 0.0045319982919, 0.0053951517228, 0.0036254634821,
                0.0024632036144, 0.0034178755991, 0.0054939861704, 0.0050025020966, 0.0061489374362)
  
  
  expect_equal(ll$contrasts, expected)
  ## Test that the parallelized code gives the same results.
  ll2 <- leadLag(price1, price2, seq(-10,10), normalize = FALSE, parallelize = TRUE, nCores = 2)
  expect_true(all.equal(ll,ll2))
  
  
  ## Test that the contrasts are the same when using 10000 ms and 10 sec
  llMS <- leadLag(price1, price2, seq(-10,10) * 1000, resolution = "ms", normalize = FALSE)
  
  expect_equal(ll$contrasts, llMS$contrasts)
  
})


# 
# # skip_on_os("solaris")
# ## Test that the contrasts are as expected
# ll <- leadLag(10 + cumsum(sbux), 10 + cumsum(lltc), seq(-10,10), normalize = TRUE)
# 
# expected <- c(0.033229285,0.049053654,0.075348393,0.090688956,0.122192922,0.153177587,0.176829968,0.198500572,0.229967633,0.285702625,0.298824786,
#               0.267945966,0.179464691,0.137441360,0.098206314,0.081605315,0.068544813,0.055210317,0.040167523,0.025934535,0.004943045)
# expect_equal(ll$contrasts, expected)
# ## Test that the parallelized code gives the same results.
# ll2 <- leadLag(100 + cumsum(sbux), 100 + cumsum(lltc), seq(-10,10), normalize = TRUE, parallelize = TRUE, nCores = 2)
# expect_true(all.equal(ll,ll2))
# 
# 
# ## Test that the contrasts are the same when using 10000 ms and 10 sec
# llMS <- leadLag(10 + cumsum(sbux), 10 + cumsum(lltc), seq(-10,10) * 1000, resolution = "ms", normalize = TRUE)
# 
# expect_equal(ll$contrasts, llMS$contrasts)

library(testthat)
library(xts)
library(highfrequency)
set.seed(123)
start <- strptime("1970-01-01", format = "%Y-%m-%d", tz = "UTC")
timestamps <- start + seq(34200, 57600, length.out = 11701)

dat <- cbind(rnorm(11701) * sqrt(1/11701), rnorm(11701) * sqrt(1/11701))

dat <- exp(cumsum(xts(dat, timestamps)))
price1 <- dat[,1]
price2 <- dat[,2]
context("leadLag")
test_that("leadLag", {
  # skip_on_os("solaris")
  ## Test that the contrasts are as expected
  ll <- leadLag(price1, price2, seq(-3,3), normalize = FALSE)
  expected <- c(0.002859176333, 0.006440878454, 0.005419688578, 0.001021189876, 0.002162166165, 0.003172115215, 0.009944935852)
  
  
  ll_swapped <- leadLag(price2, price1, seq(-3,3), normalize = FALSE)
  expect_equal(ll$contrasts, expected)
  
  
  ## Test that the contrasts are the same when using 10000 ms and 10 sec
  llMS <- leadLag(price1, price2, seq(-3,3) * 1000, resolution = "ms", normalize = FALSE)
  
  expect_equal(ll$contrasts, llMS$contrasts)
  
  
  foo <- spreadPrices(sampleMultiTradeData[SYMBOL %in% c("ETF", "AAA")])
  llEmpirical <- leadLag(foo[!is.na(AAA), list(DT, PRICE = AAA)], foo[!is.na(ETF), list(DT, PRICE = ETF)], seq(-5,5))
  expect_equal(llEmpirical$`lead-lag-ratio`, 0.9525131289)
  
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

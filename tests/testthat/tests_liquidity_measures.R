library(testthat)
library(highfrequency)
context("matchTradesQuotes & getTradeDirection trades")
test_that("matchTradesQuotes & getTradeDirection trades", {
expect_equal(
  {tqdata <- matchTradesQuotes(sampleTData, sampleQData)
    directions <- getTradeDirection(tqdata)
    sum(directions)
  },
  -833
)})
context("matchTradesQuotes & getTradeDirection quotes")
test_that("matchTradesQuotes & getTradeDirection quotes", {
expect_equal(
  {tqdata <- matchTradesQuotes(sampleTDataMicroseconds, sampleQDataMicroseconds)
  directions <- getTradeDirection(tqdata)
  sum(directions)},
  -1403
)})
# 
# expect_equal(
#   {tqdata <- matchTradesQuotes(sampleTData, sampleQData)
#   getLiquidityMeasures(tqdata, type = "prs")
#   }
#   
#   sum(prs)
# )
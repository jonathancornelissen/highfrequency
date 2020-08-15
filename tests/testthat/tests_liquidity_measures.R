library(testthat)
library(highfrequency)
context("matchTradesQuotes & getTradeDirection trades")
test_that("matchTradesQuotes & getTradeDirection trades", {
expect_equal(
  {tqdata <- matchTradesQuotes(sampleTData, sampleQData)
    directions <- getTradeDirection(tqdata)
    sum(directions)
  },
  -886
)})
context("matchTradesQuotes & getTradeDirection quotes")
test_that("matchTradesQuotes & getTradeDirection quotes", {
  tqData <- matchTradesQuotes(sampleTDataMicroseconds, sampleQDataMicroseconds, adjustment = 2)
  directions <- getTradeDirection(tqData)
  sum(directions)
expect_equal(
  sum(directions), 
  -1472
)})
# 
# expect_equal(
#   {tqdata <- matchTradesQuotes(sampleTData, sampleQData)
#   getLiquidityMeasures(tqdata, type = "prs")
#   }
#   
#   sum(prs)
# )
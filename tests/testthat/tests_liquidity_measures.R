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
  tqData <- matchTradesQuotes(sampleTDataMicroseconds, sampleQDataMicroseconds, lagQuotes = 2)
  directions <- getTradeDirection(tqData)
  sum(directions)
expect_equal(
  sum(directions), 
  -1472
)})

context("getLiquidityMeasures")
test_that("testing getLiquidityMeasures",{
  
  tqData <- tradesCleanupUsingQuotes(tData = tradesCleanup(tDataRaw = sampleTDataRaw, report = FALSE, exchanges = "N"), qData = quotesCleanup(qDataRaw = sampleQDataRaw, report = FALSE, exchanges = "N"), lagQuotes = 0)
  liquidityMeasures <- getLiquidityMeasures(tqData)
  
  liquidityMeasures <- liquidityMeasures[,! sapply(colnames(liquidityMeasures), function(x) x %in% colnames(tqData))]
  storage.mode(liquidityMeasures) <- "numeric"
  expect_equal(apply(liquidityMeasures, 2, function(x) mean(x[is.finite(x)], na.rm = TRUE)),
             c("midpoints" = 1.910741e+02, "direction" = -1.101332e-01, "effectiveSpread" = 6.018167e-02, "realizedSpread" = -3.454333e-02, "valueTrade" = 6.019575e+04,
               "signedValueTrade" = -8.105118e+03, "depthImbalanceDifference" = -8.553353e-03, "depthImbalanceRatio" = -3.829903e-01, "proportionalEffectiveSpread" = 3.149855e-04,
               "proportionalRealizedSpread" = -1.741446e-04, "priceImpact" = 4.734846e-02, "proportionalPriceImpact" = 2.445116e-04, "halfTradedSpread" = 3.009084e-02,
               "proportionalHalfTradedSpread" = 1.574928e-04, "squaredLogReturn" = 7.705755e-08, "absLogReturn" = 1.728890e-04, "quotedSpread" = 8.047506e-02,
               "proportionalQuotedSpread" = 4.212890e-04, "logQuotedSpread" = 4.212890e-04, "logQuotedSize" = 1.514534e+00, "quotedSlope" = 2.969190e-02,
               "logQSlope" = 1.553795e-04, "midQuoteSquaredReturn" = 5.981065e-08, "midQuoteAbsReturn" = 1.300114e-04, "signedTradeSize" = -4.230986e+01))

})
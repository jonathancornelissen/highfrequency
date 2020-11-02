library(testthat)
library(highfrequency)
library(xts)

context("matchTradesQuotes & getTradeDirection quotes")
test_that("matchTradesQuotes & getTradeDirection quotes", {
  tqData <- matchTradesQuotes(sampleTDataMicroseconds, sampleQDataMicroseconds, lagQuotes = 2)
  directions <- getTradeDirection(tqData)
  sum(directions)
expect_equal(
  sum(directions), 
  -1412
)})

context("getLiquidityMeasures")
test_that("testing getLiquidityMeasures",{
  
  tqData <- tradesCleanupUsingQuotes(tData = tradesCleanup(tDataRaw = sampleTDataRawMicroseconds, report = FALSE, exchanges = "N"), 
                                     qData = quotesCleanup(qDataRaw = sampleQDataRawMicroseconds, report = FALSE, exchanges = "N"), lagQuotes = 0)
  liquidityMeasures <- getLiquidityMeasures(tqData)
  liquidityMeasures <- liquidityMeasures[, -(1:ncol(tqData))]
  
  
  expect_equal(apply(liquidityMeasures, 2, function(x) mean(x[is.finite(x)], na.rm = TRUE)),
               # c("midpoints" = 1.910741e+02, "direction" = -1.083277e-01, "effectiveSpread" = 6.018167e-02, "realizedSpread" = -3.355992e-02, 
               #   "valueTrade" = 6.019575e+04,"signedValueTrade" = -8.033535e+03, "depthImbalanceDifference" = -8.267488e-03,
               #   "depthImbalanceRatio" = -3.803479e-01, "proportionalEffectiveSpread" = 3.149855e-04, "proportionalRealizedSpread" = -1.690378e-04,
               #   "priceImpact" = 4.685675e-02,"proportionalPriceImpact" = 2.419582e-04, "halfTradedSpread" = 3.009084e-02,
               #   "proportionalHalfTradedSpread" = 1.574928e-04, "squaredLogReturn" = 7.705755e-08, "absLogReturn" = 1.728890e-04,
               #   "quotedSpread" = 8.047506e-02, "proportionalQuotedSpread" = 4.212890e-04, "logQuotedSpread" = 4.212890e-04, "logQuotedSize" = 1.514534e+00,
               #   "quotedSlope" = 2.969190e-02, "logQSlope" = 1.553795e-04, "midQuoteSquaredReturn" = 5.981065e-08,  "midQuoteAbsReturn" = 1.300114e-04,
               #   "signedTradeSize" = -4.193749e+01)
               c("midpoints" = 1.568660851e+02, "direction" = -1.739413681e-01, "effectiveSpread" = 2.085700326e-02, 
                 "realizedSpread" = -2.111404110e-02, "valueTrade" = 2.715119360e+04, "signedValueTrade" = -3.843162486e+03,
                 "depthImbalanceDifference" = 2.044038195e-03, "depthImbalanceRatio" = -3.216294609e-01,
                 "proportionalEffectiveSpread" = 1.326840320e-04,  "proportionalRealizedSpread" = -1.318068210e-04,
                 "priceImpact" = 2.124058219e-02,  "proportionalPriceImpact" = 1.338690288e-04,
                 "halfTradedSpread" = 1.042850163e-02,  "proportionalHalfTradedSpread" = 6.634201601e-05,
                 "squaredLogReturn" = 2.829216378e-08,  "absLogReturn" = 1.087853666e-04,  "quotedSpread" = 4.498941368e-02,
                 "proportionalQuotedSpread" = 2.863292509e-04,  "logQuotedSpread" = 2.863292594e-04,  "logQuotedSize" = 3.507993108e+00,
                 "quotedSlope" = 1.848731511e-02,  "logQSlope" = 1.176115204e-04,  "midQuoteSquaredReturn" = 2.179025826e-08,
                 "midQuoteAbsReturn" = 9.503283753e-05,  "signedTradeSize" = -2.456156352e+01)
               )
  
  
})

library(testthat)
library(highfrequency)
library(xts)

data.table::setDTthreads(2)


# matchTradesQuotes & getTradeDirection quotes ----------------------------
test_that("matchTradesQuotes & getTradeDirection quotes", {
  skip_on_cran()
  tqData <- matchTradesQuotes(sampleTData, sampleQData, lagQuotes = 2)
  directions <- getTradeDirection(tqData)
  sum(directions)
expect_equal(
  sum(directions), 
  -1572
)})


# getLiquidityMeasures ----------------------------------------------------
test_that("testing getLiquidityMeasures",{
  
  skip_on_cran()
  
  tqData <- tradesCleanupUsingQuotes(tData = tradesCleanup(tDataRaw = sampleTDataRaw, report = FALSE, exchanges = "N"), 
                                     qData = quotesCleanup(qDataRaw = sampleQDataRaw, report = FALSE, exchanges = "N"), lagQuotes = 0)
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
               #   
               #   
               
               c("midpoints" = 1.568751203e+02, "direction" = -1.646205357e-01, "effectiveSpread" = 2.116099330e-02,
                 "realizedSpread" = -1.730314502e-02, "valueTrade" = 2.587442590e+04, "signedValueTrade" = -3.525860704e+03,
                 "depthImbalanceDifference" = 2.679893658e-03, "depthImbalanceRatio" = -3.060161477e-01, "proportionalEffectiveSpread" = 1.346079032e-04,
                 "proportionalRealizedSpread" = -1.082144587e-04, "priceImpact" = 1.944379732e-02, "proportionalPriceImpact" = 1.227587276e-04,
                 "halfTradedSpread" = 1.058049665e-02, "proportionalHalfTradedSpread" = 6.730395161e-05, "squaredLogReturn" = 2.510765091e-08,
                 "absLogReturn" = 1.014482187e-04, "quotedSpread" = 4.554896763e-02, "proportionalQuotedSpread" = 2.898679017e-04,
                 "logQuotedSpread" = 2.898679107e-04, "logQuotedSize" = 3.406315538e+00, "quotedSlope" = 1.939226740e-02,
                 "logQSlope" = 1.233647337e-04, "midQuoteSquaredReturn" = 1.752787460e-08, "midQuoteAbsReturn" = 8.566642940e-05,
                 "signedTradeSize" = -2.252273996e+01))
               
                  
  
})

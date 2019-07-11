# # 
# ##' Liquidity Measures are in the \pkg{highfrequency} by Boudt,
# ##' Cornelissen and Payseur are computed, but this time in the context
# ##' of a \code{data.table} object which is much more efficient.
# ##'
# ##' Note that this assumes a regular time grid. The Lee + Ready measure
# ##' uses two lags for the Tick Rule.
# ##'
# ##' @title Compute Liquidity Measure
# ##' @param tqdata A \code{data.table} as in the \pkg{highfrequency} merged
# ##' trades and quotes data (that is included).
# ##' @param win A windows length for the forward-prices used for \sQuote{realized}
# ##' spread
# ##' @return A modified (enlarged) \code{data.table} with the new measures.
# ##' @examples
# ##' #data(tqdata)
# ##' #res <- getLiquidityMeasures(tqdata)
# getLiquidityMeasures <- function(tqdata, win = 300) {
#   .tqdatacheck(tqdata)
#   data <- .check_data(tqdata)
# 
#   data[, midpoints := 0.5 * (BID + OFR)]
# 
#   data[, direction := getTradeDirection(data)]
#   data[, effectiveSpread := 2 * direction * (PRICE - midpoints)]
# 
#   ## realized spread imposed fixed time window.  hm. time offset arbitrary
#   data[, realizedSpread := 2 * direction * (PRICE - shift(midpoints, win, type = "lead"))]
# 
#   data[, valueTrade := SIZE * PRICE]
#   data[, signedValueTrade := direction * valueTrade]
# 
#   data[, depthImbalanceDifference := direction * (OFRSIZ - BIDSIZ) / (OFRSIZ + BIDSIZ)]
# 
#   data[, depthImbalanceRatio := (direction * OFRSIZ / BIDSIZ) ^ direction]
# 
#   data[, proportionalEffectiveSpread := effectiveSpread/midpoints]
#   data[, proportionalRealizedSpread := realizedSpread/midpoints]
# 
#   data[, priceImpact := (effectiveSpread - realizedSpread)/2]
#   data[, proportionalPriceImpact := priceImpact / midpoints]
# 
#   data[, halfTradedSpread := direction*(PRICE-midpoints)]
#   data[, proportionalHalfTradedSpread := halfTradedSpread/midpoints]
# 
#   data[, squaredLogReturn := (log(PRICE) - log(shift(PRICE, 1, type="lag")))^2]
#   data[, absLogReturn := abs(log(PRICE) - log(shift(PRICE, 1, type="lag")))]
# 
#   data[, quotedSpread := OFR - BID]
#   data[, proportionalQuotedSpread := quotedSpread/midpoints]
# 
#   data[, logQuotedSpread := log(OFR/BID)]
#   data[, logQuotedSize := log(OFRSIZ) - log(BIDSIZ)]
# 
#   data[, quotedSlope := quotedSpread/logQuotedSize]
#   data[, logQSlope := logQuotedSpread/logQuotedSize]
# 
#   data[, midQuoteSquaredReturn := (log(midpoints) - log(shift(midpoints,1,type="lag")))^2]
#   data[, midQuoteAbsReturn := abs(log(midpoints) - log(shift(midpoints,1,type="lag")))]
# 
#   data[, signedTradeSize := direction * SIZE]
# }
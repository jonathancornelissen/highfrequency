#
##' Liquidity Measures are in the \pkg{highfrequency} by Boudt,
##' Cornelissen and Payseur are computed, but this time in the context
##' of a \code{data.table} object which is much more efficient.
##'
##' Note that this assumes a regular time grid. The Lee + Ready measure
##' uses two lags for the Tick Rule.
##'
##' @title Compute Liquidity Measure
##' @param tqdata A \code{data.table} or xts object as in the \pkg{highfrequency} merged
##' trades and quotes data (that is included).
##' @param win A windows length for the forward-prices used for \sQuote{realized}
##' spread
##' @param type Legacy option. Default is to return all liquidity measures.
##' @return A modified (enlarged) \code{data.table} with the new measures.
##' 
##' @details NOTE: xts or data.table should only contain one day of observations
##' 
##' @examples
##' tqdata <- matchTradesQuotes(sample_tdata, sample_qdata)
##' res <- getLiquidityMeasures(tqdata)
##' head(res)
##' @importFrom data.table shift
##' @export
getLiquidityMeasures <- function(tqdata, win = 300, type = NULL) {
  
  BID = PRICE = OFR = SIZE = OFRSIZ = BIDSIZ = NULL
  
  midpoints = direction = effectiveSpread = realizedSpread = valueTrade = signedValueTrade = NULL
  depthImbalanceRatio = depthImbalanceDifference = proportionalEffectiveSpread = proportionalRealizedSpread = NULL
  priceImpact = proportionalPriceImpact = halfTradedSpread = proportionalHalfTradedSpread = squaredLogReturn = NULL
  squaredLogReturn = absLogReturn = quotedSpread = proportionalQuotedSpread = logQuotedSpread = logQuotedSize = NULL
  quotedSlope = logQSlope = midQuoteSquaredReturn = midQuoteAbsReturn = signedTradeSize = NULL
  
  tqdata <- checkColumnNames(tqdata)
  checktdata(tqdata)
  checkqdata(tqdata)
  
  dummy_was_xts <- FALSE
  if (is.data.table(tqdata) == FALSE) {
    if (is.xts(tqdata) == TRUE) {
      tqdata <- setnames(as.data.table(tqdata), old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(tqdata)) == FALSE) {
      
      stop("Data.table neeeds DT column (date-time ).")
    }
  }
  
  tqdata[, BID := as.numeric(as.character(BID))]
  tqdata[, PRICE := as.numeric(as.character(PRICE))] # just for being sure
  tqdata[, OFR := as.numeric(as.character(OFR))]      # if one converts from xts back and forth.
  tqdata[, SIZE := as.numeric(as.character(SIZE))]
  tqdata[, OFRSIZ := as.numeric(as.character(OFRSIZ))]
  tqdata[, BIDSIZ := as.numeric(as.character(BIDSIZ))]
  
  tqdata[, midpoints := 0.5 * (BID + OFR)]

  tqdata[, direction := getTradeDirection(tqdata)]
  tqdata[, effectiveSpread := 2 * direction * (PRICE - midpoints)]

  ## realized spread imposed fixed time window. hm. time offset arbitrary
  tqdata[, realizedSpread := 2 * direction * (PRICE - shift(midpoints, win, type = "lead"))]

  tqdata[, valueTrade := SIZE * PRICE]
  tqdata[, signedValueTrade := direction * valueTrade]

  tqdata[, depthImbalanceDifference := direction * (OFRSIZ - BIDSIZ) / (OFRSIZ + BIDSIZ)]

  tqdata[, depthImbalanceRatio := (direction * OFRSIZ / BIDSIZ) ^ direction]

  tqdata[, proportionalEffectiveSpread := effectiveSpread/midpoints]
  tqdata[, proportionalRealizedSpread := realizedSpread/midpoints]

  tqdata[, priceImpact := (effectiveSpread - realizedSpread)/2]
  tqdata[, proportionalPriceImpact := priceImpact / midpoints]

  tqdata[, halfTradedSpread := direction*(PRICE-midpoints)]
  tqdata[, proportionalHalfTradedSpread := halfTradedSpread/midpoints]

  tqdata[, squaredLogReturn := (log(PRICE) - log(shift(PRICE, 1, type = "lag")))^2]
  tqdata[, absLogReturn := abs(log(PRICE) - log(shift(PRICE, 1, type = "lag")))]

  tqdata[, quotedSpread := OFR - BID]
  tqdata[, proportionalQuotedSpread := quotedSpread/midpoints]

  tqdata[, logQuotedSpread := log(OFR/BID)]
  tqdata[, logQuotedSize := log(OFRSIZ) - log(BIDSIZ)]

  tqdata[, quotedSlope := quotedSpread/logQuotedSize]
  tqdata[, logQSlope := logQuotedSpread/logQuotedSize]

  tqdata[, midQuoteSquaredReturn := (log(midpoints) - log(shift(midpoints,1, type = "lag")))^2]
  tqdata[, midQuoteAbsReturn := abs(log(midpoints) - log(shift(midpoints,1, type = "lag")))]

  tqdata[, signedTradeSize := direction * SIZE]
  
  if (dummy_was_xts == TRUE) {
    if (is.null(type) == TRUE) {
      return(xts(as.matrix(tqdata[, -c("DT")]), order.by = tqdata$DT))
    } else {
      return(xts(as.matrix(tqdata[, -c("DT")]), order.by = tqdata$DT)[, type])
    }
  } else {
    return(tqdata)
  }
}


# tqdata <- matchTradesQuotes(sample_tdata, sample_qdata)
# tqdata_dt <- as.data.table(tqdata)[, DT := index]
# tqdata_dt[, BID := as.numeric(as.character(BID))]
# tqdata_dt[, OFR := as.numeric(as.character(OFR))]
# tqdata_dt[, PRICE := as.numeric(as.character(PRICE))]

#' Get trade direction
#' 
#' @description Function returns a vector with the inferred trade direction which is 
#' determined using the Lee and Ready algorithym (Lee and Ready, 1991). 
#' 
#' @param tqdata data.table or xts object, containing joined trades and quotes (e.g. using \code{\link{matchTradesQuotes}})
#' 
#' @details NOTE: The value of the first (and second) observation of the output should be ignored if price == midpoint
#' for the first (second) observation.
#' 
#' @return A vector which has values 1 or (-1) if the inferred trade direction
#' is buy or sell respectively.
#' 
#' @references  Lee, C. M. C. and M. J. Ready (1991). Inferring trade direction from intraday data. Journal of Finance 46, 733-746.
#' 
#' @author Jonathan Cornelissen, Kris Boudt and Onno Kleen. Special thanks to Dirk Eddelbuettel.
#' 
#' @examples 
#' # generate matched trades and quote data set
#' tqdata <- matchTradesQuotes(sample_tdata, sample_qdata)
#' directions <- getTradeDirection(tqdata)
#' head(directions)
#' 
#' @keywords liquidity
#' @export
getTradeDirection <- function(tqdata) {
  
  BID = OFR = PRICE = NULL
  
  tqdata <- checkColumnNames(tqdata)
  checktdata(tqdata)
  checkqdata(tqdata)

  dummy_was_xts <- FALSE
  if (is.data.table(tqdata) == FALSE) {
    if (is.xts(tqdata) == TRUE) {
      tqdata <- setnames(as.data.table(tqdata), old = "index", new = "DT")
      tqdata[, BID := as.numeric(as.character(BID))]
      tqdata[, PRICE := as.numeric(as.character(PRICE))]
      tqdata[, OFR := as.numeric(as.character(OFR))]
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(tqdata)) == FALSE) {
      stop("Data.table neeeds DT column (date-time ).")
    }
  }
  
  ## Function returns a vector with the inferred trade direction:
  ## NOTE: the value of the first (and second) observation should be
  ## ignored if price == midpoint for the first (second) observation.
  bid <- tqdata[, BID]
  offer <- tqdata[, OFR]
  midpoints <- (bid + offer)/2
  price <- tqdata[, PRICE]
  
  buy1 <- price > midpoints           # definitely a buy
  equal <- price == midpoints
  
  ## for trades=midpoints: if uptick=>buy (ie p_t - p_{t-1} > 0)
  dif1 <- c(TRUE, 0 < price[2:length(price)] - price[1:(length(price)-1)])
  ## for trades=midpoints: zero-uptick=>buy
  equal1 <- c(TRUE, 0 == price[2:length(price)] - price[1:(length(price)-1)])
  dif2 <- c(TRUE, TRUE, 0 < price[3:length(price)] - price[1:(length(price)-2)])
  
  buy <- buy1 | (dif1 & equal) | (equal1 & dif2 & equal)
  
  buy[buy == TRUE] <- 1                 # 2*as.integer(buy) - 1   does the same
  buy[buy == FALSE] <- -1
  
  
  ## The tick rule is the most commonly used level-1 algorithm. This
  ## rule is rather simple and classifies a trade as buyer-initiated if the
  ## trade price is above the preceding trade price (an uptick trade) and
  ## as seller-initiated if the trade price is below the preceding trade
  ## price (a downtick trade). If the trade price is the same as the
  ## previous trade price (a zero-tick trade), the rule looks for the
  ## closest prior price that differs from the current trade price.
  ## Zero-uptick trades are classified as buys, and zero-downtick trades
  ## are classified as sells.
  ##
  ## -- Chakrabarty, Pascual and Shkilko (2013), "Trade
  ##    Classification Algorithms: A Horse Race between then
  ##    Builk-based and Tick-based Rules"
  ##
  ##    http://dee.uib.es/digitalAssets/234/234006_Pascual.pdf
  
  return(buy)
}


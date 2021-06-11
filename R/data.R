#' Sample of cleaned quotes for stock XXX for 2 days measured in microseconds
#' 
#' @description A \code{data.table} object containing the quotes for the pseudonymized stock XXX for 2 days. This is the cleaned version of the data sample \code{\link{sampleQDataRaw}}, using \code{quotesCleanup}.
#' 
#' @format data.table object
#' @examples 
#' \dontrun{
#' # The code to create the sampleQData dataset from raw data is
#' sampleQData <- quotesCleanup(qDataRaw = sampleQDataRaw,
#'                                          exchanges = "N", type = "standard", report = FALSE)
#' }
#' @keywords datasets
"sampleQData"

#' Sample of raw quotes for stock XXX for 2 days measured in microseconds
#' @description  A \code{data.table} object containing the raw quotes the pseudonymized stock XXX for 2 days, in the typical NYSE TAQ database format.
#' 
#' @format data.table object
#' 
#' @keywords datasets
"sampleQDataRaw"
#' Sample of cleaned trades for stock XXX for 2 days
#' 
#' @description A \code{data.table} object containing the trades for the pseudonymized stock XXX for 2 days, in the typical NYSE TAQ database format.
#' This is the cleaned version of the data sample \code{\link{sampleTDataRaw}}, using \code{\link{tradesCleanupUsingQuotes}}.
#' 
#' @docType data
#' @examples 
#' \dontrun{
#' # The code to create the sampleTData dataset from raw data is
#' sampleQData <- quotesCleanup(qDataRaw = sampleQDataRaw,
#'                                          exchanges = "N", type = "standard", report = FALSE)
#' 
#' tradesAfterFirstCleaning <- tradesCleanup(tDataRaw = sampleTDataRaw, 
#'                                           exchanges = "N", report = FALSE)
#' 
#' sampleTData <- tradesCleanupUsingQuotes(
#'   tData = tradesAfterFirstCleaning,
#'   qData = sampleQData,
#'   lagQuotes = 0)[, c("DT", "EX", "SYMBOL", "PRICE", "SIZE")]
#' # Only some columns are included. These are the ones that were historically included.
#' 
#' # For most applications, we recommend aggregating the data at a high frequency
#' # For example, every second.
#' aggregated <- aggregatePrice(sampleTData[, list(DT, PRICE)],
#'               alignBy = "seconds", alignPeriod = 1)
#' acf(diff(aggregated[as.Date(DT) == "2018-01-02", PRICE]))
#' acf(diff(aggregated[as.Date(DT) == "2018-01-03", PRICE]))
#' 
#' signature <- function(x, q){
#' res <- x[, (rCov(diff(log(PRICE), lag = q, differences = 1))/q), by = as.Date(DT)]
#' return(res[[2]])
#' }
#' rvAgg <- matrix(nrow = 100, ncol = 2)
#' for(i in 1:100) rvAgg[i, ] <- signature(aggregated, i)
#' plot(rvAgg[,1], type = "l")
#' plot(rvAgg[,2], type = "l")
#' }
#'
#' @format A data.table object.
#' 
#' @keywords datasets
"sampleTData"

#' Sample of raw trades for stock XXX for 2 days
#' 
#' @description An imaginary \code{data.table} object containing the raw trades the pseudonymized stock XXX for 2 days, in the typical NYSE TAQ database format.
#' 
#' @docType data
#' 
#' @format A data.table object.
#' 
#' @keywords datasets
"sampleTDataRaw"
#' One minute data
#' 
#' @description One minute data price of one stock and a market proxy. This is data from the US market.
#' @format A \code{data.table} object
#' @keywords datasets
"sampleOneMinuteData"

#' European data
#' 
#' @description Trade data of one stock on one day in the European stock market.
#' @format A \code{data.table} object
#' @keywords datasets
"sampleTDataEurope"

#' SPY realized measures
#' @description Realized measures for the SPY ETF calculated at 1 and 5 minute sampling. 
#' @note The CLOSE column is NOT the official close price, but simply the last recorded price of the day. Thus, this may be slightly different from other sources.
#' @format A \code{data.table} object
#' @keywords datasets
"SPYRM"




#' Multivariate tick by tick data
#' @description Cleaned Tick by tick data for a sector ETF, called \code{ETF} and two stock components of that ETF,
#' these stocks are named \code{AAA} and \code{BBB}.
#' @format A \code{data.table} object
#' @keywords datasets
"sampleMultiTradeData"
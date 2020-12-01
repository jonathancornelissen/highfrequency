#' Sample of cleaned quotes for stock XXX for 2 days measured in microseconds
#' 
#' @description A data.table object containing the quotes for the imaginary stock XXX for 2 days. This is the cleaned version of the data sample \code{\link{sampleQDataRaw}}, using \code{quotesCleanup}.
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
#' @description  An data.table object containing the raw quotes for stock XXX for 2 days, in the typical NYSE TAQ database format.
#' 
#' @format data.table object
#' 
#' @keywords datasets
"sampleQDataRaw"
#' Sample of cleaned trades for stock XXX for 2 days
#' 
#' @description A data.table object containing the trades for the imaginary stock XXX for 2 days, in the typical NYSE TAQ database format.
#' This is the cleaned version of the data sample \code{\link{sampleTDataRaw}}, using \code{tradesCleanup}.
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
#'   lagQuotes = 0)[, c("DT", "SYMBOL", "PRICE", "SIZE")]
#' # Only some columns are included. These are the ones that were historically included.
#' }
#'
#' @format A data.table object.
#' 
#' @keywords datasets
"sampleTData"

# #' Sample of raw trades for stock XXX for 1 day
# #' 
# #' @description An imaginary xts object containing the raw trades for stock XXX for 1 day, in the typical NYSE TAQ database format.
# #' 
# #' @docType data
# #' 
# #' @format A large \code{xts} object.
# #' 
# #' @keywords datasets
# "sampleTDataRaw"

#' Sample of raw trades for stock XXX for 2 days
#' 
#' @description An imaginary data.table object containing the raw trades for stock XXX for 2 days, in the typical NYSE TAQ database format.
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
#' @note The CLOSE column is NOT the official close price, but simply the last recorded price of the day. Thus this may be slightly different from other sources
#' @format A \code{data.table} object
#' @keywords datasets
"SPYRM"
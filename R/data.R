# #' LLTC Data
# #'
# #' @description Tick data for LLTC 2011/07/01, cleaned with \code{tradesCleanup}.
# #'
# #' @examples
# #' data(lltc)
# #' plot(lltc)
# #'
# #' @format xts object
# #' @keywords datasets
# "lltc"

# #' The realized library from the Oxford-Man Institute of Quantitative Finance
# #' 
# #' @description A data.frame object containing the daily open-close returns, daily realized variances based on 
# #' five-minute intraday returns and daily realized kernels ranging from 2000-01-03 up to 2019-06-10 for the S&P 500. 
# #' Use \code{colnames(realizedLibrary)} to see which realized measures. 
# #' The full library of the Oxford-Man Institute of Quantitative Finance can be found on their website: \url{https://realized.oxford-man.ox.ac.uk/}.
# #'  
# #' @references  
# #' Gerd Heber, Asger Lunde, Neil Shephard, and Kevin Sheppard (2009)
# #' "Oxford-Man Institute's realized library, version 0.3", Oxford-Man
# #' Institute, University of Oxford. 
# #' 
# #' @format data.frame
# #' @keywords datasets
# "realizedLibrary"


# #' Ten artificial time series for the NYSE trading days during January 2010
# #' 
# #' @description  Ten simulated price series for the 19 trading days in January 2010:
# #' 
# #' Ten hypothetical price series were simulated 
# #' according to the factor diffusion process discussed in Barndorff-Nielsen et al.
# #' We assume that prices are only observed when a transaction takes place. 
# #' The intensity of transactions follows a Poisson process and consequently,
# #' the inter-transaction times are exponentially distributed. 
# #' Therefore, we generated the inter transaction times of the price series 
# #' by an independent exponential distributions with lambda = 0.1,
# #' which we keep constant over time. This means we expect one transaction every ten seconds.
# #' In a final step, the time series were aggregated to the 5-minute frequency by previous tick aggregation.
# #' 
# #' @format xts object
# #' 
# #' @references 
# #' Barndorff-Nielsen, O. E., P. R. Hansen, A. Lunde and N. Shephard (2011). 
# #' Multivariate realised kernels: consistent positive semi-definite
# #' estimators of the covariation of equity prices with noise and non-synchronous
# #' trading. Journal of Econometrics, 162, 149-169.
# #' 
# #' @keywords datasets        
# "sample5MinPrices"
# 
# #' Ten artificial time series (including jumps) for the NYSE trading days during January 2010
# #' 
# #' @description Ten simulated price series for the 19 trading days in January 2010:
# #' Ten hypothetical price series were simulated 
# #' according to the factor diffusion process discussed in Barndorff-Nielsen et al.
# #' On top of this process we added a jump process, 
# #' with jump occurrences governed by the Poisson process with 1 expected jump per day and
# #' jump magnitude modelled as in Boudt et al. (2008). We assume that prices are only observed when a transaction takes place. 
# #' The intensity of transactions follows a Poisson process and consequently,
# #' the inter transaction times are exponentially distributed. 
# #' Therefore, we generated the inter transaction times of the price series
# #' by an independent exponential distributions with lambda = 0.1,
# #' which we keep constant over time. This means we expect one transaction every ten seconds.
# #' In a final step, the time series were aggregated to the 5-minute frequency by previous tick aggregation.
# #' 
# #' @format xts object
# #' 
# #' @references 
# #' Barndorff-Nielsen, O. E., P. R. Hansen, A. Lunde and N. Shephard (2011). 
# #' Multivariate realised kernels: consistent positive semi-definite
# #' estimators of the covariation of equity prices with noise and non-synchronous
# #' trading. Journal of Econometrics, 162, 149-169.
# #' 
# #' Boudt, K., C. Croux, and S. Laurent (2008). Outlyingness weighted covariation. Mimeo.
# #' 
# #' @keywords datasets
# "sample5MinPricesJumps"
# 
# #' Sample of imaginary price data for 61 days
# #'
# #' @description An xts object containing the 5-min aggregated imaginary price series for the trading
# #' days between 2005-03-04 and 2005-06-01.
# #' 
# #' @format xts object
# #' 
# #' @keywords datasets
# "sampleReal5MinPrices"

# #' Sample of cleaned quotes for stock XXX for 1 day
# #' 
# #' @description An xts object containing the raw quotes for the imaginary stock XXX for 1 day, in the typical NYSE TAQ database format. 
# #' This is the cleaned version of the data sample \code{\link{sampleQDataRaw}}, using \code{quotesCleanup}. 
# #' @examples
# #' \dontrun{
# #' #The code to create sampleQData from raw data is 
# #' sampleQData <- quotesCleanup(
# #' qDataRaw = sampleQDataRaw, exchanges = "N", type = "standard", 
# #' report = FALSE)[, c("SYMBOL", "BID", "BIDSIZ", "OFR", "OFRSIZ", "MODE")]
# #' # These are the historically included columns.
# #' }
# #' 
# #' @format xts object
# #' 
# #' @keywords datasets
# "sampleQData"
# 
# #' Sample of raw quotes for stock XXX for 1 day
# #' @description  An imaginary xts object containing the raw quotes for stock XXX for 1 day, in the typical NYSE TAQ database format.
# #' 
# #' @format xts object
# #' 
# #' @keywords datasets
# "sampleQDataRaw"

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

# #' Sample returns data
# #' 
# #' @description  EUR/USD returns from January to September 2004
# #' 
# #' @docType data
# #' 
# #' @format A large \code{xts} object.
# #' 
# #' @keywords datasets
# "sampleReturns5Min"

# #' Sample of cleaned trades for stock XXX for 1 day
# #' 
# #' @description An xts object containing the trades for the imaginary stock XXX for 1 day, in the typical NYSE TAQ database format.
# #' This is the cleaned version of the data sample \code{\link{sampleTDataRaw}}, using \code{tradesCleanup} and \code{tradesCleanupUsingQuotes}.
# #' @examples 
# #' \dontrun{
# #' #The code to create sampleTData from raw data is 
# #' tradesAfterFirstCleaning <- tradesCleanup(tDataRaw = sampleTDataRaw, 
# #'                                           exchanges = "N", report = FALSE)
# #'                                           
# #' cleanedQuotes <- quotesCleanup(qDataRaw = sampleQDataRaw, exchanges = "N", 
# #'                                type = "standard", report = FALSE)
# #' sampleTData <- tradesCleanupUsingQuotes(tData = tradesAfterFirstCleaning,
# #'                                         qData = cleanedQuotes, lagQuotes = 2)
# #' sampleTData <- sampleTData[,c("SYMBOL", "EX",  "PRICE", "SIZE", "COND", "CORR", "G127")]
# #' # These are the historically included columns.
# #' }
# #' 
# #' @docType data
# #'
# #' @format A large \code{xts} object.
# #' 
# #' @keywords datasets
# "sampleTData"

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

# #' Starbucks Data
# #' 
# #' @description Tick data for Starbucks 2011/07/01, cleaned with \code{tradesCleanup}.
# #' 
# #' @docType data
# #' 
# #' @examples
# #' data(sbux)
# #' plot(sbux) 
# #' 
# #' @format xts object
# #' @keywords datasets
# "sbux"

# #' SP500 Realized Measures calculated with 5 minute sampling
# #' 
# #' @description Realized measures from the SP500 index from April 1997 to August 2013.
# #' 
# #' @format A large \code{xts} object.
# #' 
# #' @source \url{http://public.econ.duke.edu/~ap172/code.html}
# #' @references Bollerslev, T., A. J. Patton, and R. Quaedvlieg, 2016, Exploiting the Errors: A Simple Approach for Improved Volatility Forecasting, Journal of Econometrics, 192, 1-18.
# #' 
# #' @keywords datasets
# "SP500RM"

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
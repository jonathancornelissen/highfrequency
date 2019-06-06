

#' LLTC Data
#'
#' @description Tick data for LLTC 2011/07/01, cleaned with \code{tradesCleanup}.
#'
#' @examples
#' data(lltc)
#' plot(lltc)
#'
#' @format xts object
#' @keywords datasets
"lltc"

#' The realized library from the Oxford-Man Institute of Quantitative Finance
#' 
#' @description An xts object containing the daily returns, daily Realized Variance and daily Realized Kernels ranging from 1996-01-03 up to 2009-03-01 for several indices and exchange rates. 
#'  Use \code{colnames(realized_library)} to see which assets are included. 
#'  The full library of the Oxford-Man Institute of Quantitative Finance can be found on their website: \url{http://realized.oxford-man.ox.ac.uk}.
#'  
#' @references  
#'  Gerd Heber, Asger Lunde, Neil Shephard, and Kevin Sheppard (2009)
#'  "Oxford-Man Institute's realized library, version 0.1", Oxford-Man
#'  Institute, University of Oxford. 
#'  
#'  Shephard, N. and K. Sheppard (2010). Realising the future: forecasting with high 
#'  frequency based volatility (heavy) models. Journal of Applied Econometrics 25, 
#'  197-231.
#' @format xts object
#' @keywords datasets
"realized_library"


#' Ten artificial time series for the NYSE trading days during January 2010
#' 
#' @description  Ten simulated price series for the 19 trading days in January 2010:
#' 
#' Ten hypothetical price series were simulated 
#' according to the factor diffusion process discussed in Barndorff-Nielsen et al.
#' We assume that prices are only observed when a transaction takes place. 
#' The intensity of transactions follows a Poisson process and consequently,
#' the inter-transaction times are exponentially distributed. 
#' Therefore, we generated the inter transaction times of the price series 
#' by an independent exponential distributions with lambda = 0.1,
#' which we keep constant over time. This means we expect one transaction every ten seconds.
#' In a final step, the time series were aggregated to the 5-minute frequency by previous tick aggregation.
#' 
#' @format xts object
#' 
#' @references 
#' Barndorff-Nielsen, O. E., P. R. Hansen, A. Lunde and N. Shephard (2009). 
#' Multivariate realised kernels: consistent positive semi-definite
#' estimators of the covariation of equity prices with noise and non-synchronous
#' trading. Journal of Econometrics, forthcoming.
#' 
#' @keywords datasets        
"sample_5minprices"

#' Ten artificial time series (including jumps) for the NYSE trading days during January 2010
#' 
#' @description Ten simulated price series for the 19 trading days in January 2010:
#' Ten hypothetical price series were simulated 
#' according to the factor diffusion process discussed in Barndorff-Nielsen et al.
#' On top of this process we added a jump process, 
#' with jump occurrences governed by the Poisson process with 1 expected jump per day and
#' jump magnitude modelled as in Boudt et al. (2008). We assume that prices are only observed when a transaction takes place. 
#' The intensity of transactions follows a Poisson process and consequently,
#' the inter transaction times are exponentially distributed. 
#' Therefore, we generated the inter transaction times of the price series
#' by an independent exponential distributions with lambda = 0.1,
#' which we keep constant over time. This means we expect one transaction every ten seconds.
#' In a final step, the time series were aggregated to the 5-minute frequency by previous tick aggregation.
#' 
#' @format xts object
#' 
#' @references 
#' Barndorff-Nielsen, O. E., P. R. Hansen, A. Lunde and N. Shephard (2009). 
#' Multivariate realised kernels: consistent positive semi-definite
#' estimators of the covariation of equity prices with noise and non-synchronous
#' trading. Journal of Econometrics, forthcoming.
#' 
#' Boudt, K., C. Croux, and S. Laurent (2008). Outlyingness weighted covariation. Mimeo.
#' 
#' @keywords datasets
"sample_5minprices_jumps"

#' Sample of imaginary price data for 61 days
#'
#' @description An xts object containing the 5-min aggregated imaginary price series for the trading
#' days between 2005-03-04 and 2005-06-01.
#' 
#' @format xts object
#' 
#' @keywords datasets
"sample_real5minprices"

#' Sample of cleaned quotes for stock XXX for 1 day
#' 
#' @description An xts object containing the raw quotes for the imaginary stock XXX for 1 day, in the typical NYSE TAQ database format. This is the cleaned version of the data sample \code{\link{sample_qdataraw}}, using \code{quotesCleanup}.
#' 
#' @format xts object
#' 
#' @keywords datasets
"sample_qdata"

#' Sample of raw quotes for stock XXX for 1 day
#' @description  An imaginary xts object containing the raw quotes for stock XXX for 1 day, in the typical NYSE TAQ database format.
#' 
#' @format xts object
#' 
#' @keywords datasets
"sample_qdataraw"

#' Sample of cleaned quotes for stock XXX for 2 days measured in microseconds
#' 
#' @description A data.table object containing the raw quotes for the imaginary stock XXX for 2 days. This is the cleaned version of the data sample \code{\link{sample_qdataraw_microseconds}}, using \code{quotesCleanup}.
#' 
#' @format data.table object
#' 
#' @keywords datasets
"sample_qdata_microseconds"

#' Sample of raw quotes for stock XXX for 2 days measured in microseconds
#' @description  An imaginary xts object containing the raw quotes for stock XXX for 2 days, in the typical NYSE TAQ database format.
#' 
#' @format data.table object
#' 
#' @keywords datasets
"sample_qdataraw_microseconds"

#' Sample returns data
#' 
#' @description  EUR/USD returns from January to September 2004
#' 
#' @docType data
#' 
#' @format A large \code{xts} object.
#' 
#' @keywords datasets
"sample_returns_5min"

#' Sample of cleaned trades for stock XXX for 1 day
#' 
#' @description An xts object containing the trades for the imaginary stock XXX for 1 day, in the typical NYSE TAQ database format.
#' This is the cleaned version of the data sample \code{\link{sample_tdataraw}}, using \code{tradesCleanup}.
#' 
#' @docType data
#'
#' @format A large \code{xts} object.
#' 
#' @keywords datasets
"sample_tdata"

#' Sample of cleaned trades for stock XXX for 2 days
#' 
#' @description An data.table object containing the trades for the imaginary stock XXX for 2 days, in the typical NYSE TAQ database format.
#' This is the cleaned version of the data sample \code{\link{sample_tdataraw_microseconds}}, using \code{tradesCleanup}.
#' 
#' @docType data
#'
#' @format A data.table object.
#' 
#' @keywords datasets
"sample_tdata_microseconds"

#' Sample of raw trades for stock XXX for 1 day
#' 
#' @description An imaginary xts object containing the raw trades for stock XXX for 1 day, in the typical NYSE TAQ database format.
#' 
#' @docType data
#' 
#' @format A large \code{xts} object.
#' 
#' @keywords datasets
"sample_tdataraw"

#' Sample of raw trades for stock XXX for 2 days
#' 
#' @description An imaginary data.table object containing the raw trades for stock XXX for 2 days, in the typical NYSE TAQ database format.
#' 
#' @docType data
#' 
#' @format A data.table object.
#' 
#' @keywords datasets
"sample_tdataraw_microseconds"

#' Starbucks Data
#' 
#' @description Tick data for Starbucks 2011/07/01, cleaned with \code{tradesCleanup}.
#' 
#' @docType data
#' 
#' @examples
#' data(sbux)
#' plot(sbux) 
#' 
#' @format xts object
#' @keywords datasets
"sbux"

#' SP500 Realized Measures calculated with 5 minute sampling
#' 
#' @description Realized measures from the SP500 index from April 1997 to August 2013.
#' 
#' @format A large \code{xts} object.
#' 
#' @source \url{http://public.econ.duke.edu/~ap172/code.html}
#' @references Bollerslev, T., A. J. Patton, and R. Quaedvlieg, 2016, Exploiting the Errors: A Simple Approach for Improved Volatility Forecasting, Journal of Econometrics, 192, 1-18.
#' 
#' @keywords datasets
"SP500RM"


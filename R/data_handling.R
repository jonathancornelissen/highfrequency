#' Get price column(s) from a timeseries
#' @description Will attempt to locate price column(s) from a time series with rational defaults.
#' 
#' @param x A data object with columns containing data to be extracted
#' @param symbol text string containing the symbol to extract
#' @param prefer preference for any particular type of price, see Details
#' 
#' @details  May be subset by symbol and preference.
#'  \code{prefer} Preference will be for any commonly used financial time series price description,
#'  e.g. 'trade', 'close', 'bid', 'ask' with specific tests and matching for types and column names
#'  currently supported in R, but a default grep match will be performed if one of the supported types doesn't match.
#'
#' The functionality was taken from the quantmod-package
getPrice <- function (x, symbol = NULL, prefer = NULL) {
  # first subset on symbol, if present
  if (is.null(symbol) == FALSE) {
    loc <- grep(symbol, colnames(x))
    if (identical(loc, integer(0)) == FALSE) {
      x<-x[, loc]
    } else {
      stop(paste("Subscript out of bounds: no column name containing ",symbol,"."))
    }
  }
  if (is.null(prefer) == TRUE) {
    # default to trying Price, then Trade, then Close
    if(has.Price(x)) prefer = 'price'
    else if(has.Trade(x)) prefer = 'trade'
    else if(has.Cl(x))    prefer = 'close'
    else stop("Subscript out of bounds, no price was discernible from the data.")
  }
  if (is.null(prefer) == FALSE) {
    loc <- NULL
    switch(prefer,
           Op =, open =, Open = { loc <- has.Op(x,which=TRUE) },
           Hi =, high =, High = { loc <- has.Hi(x,which=TRUE) },
           Lo =, low =, Low = { loc <- has.Lo(x,which=TRUE) },
           Cl =, close =, Close = { loc <- has.Cl(x,which=TRUE) },
           Bid =, bid = { loc <- has.Bid(x,which=TRUE) },
           Ask =, ask =, Offer =, offer = { loc <- has.Ask(x,which=TRUE) },
           Mid =, mid =, Midpoint =, midpoint = { loc <- has.Mid(x,which=TRUE) },
           Trade =, trade = { loc <- has.Trade(x,which=TRUE) },
           Price =, price = { loc <- has.Price(x,which=TRUE) },
           {loc <- grep(prefer,colnames(x))}
    )
    if (identical(loc, integer(0)) == FALSE) {
      return(x[, loc])
    } else {
      stop("Subscript out of bounds, no price was discernible from the data.")
    }
  }
}

#' Compute log returns
#' @description Function returns an xts object with the log returns as xts object.
#' 
#' \deqn{
#' \mbox{log return}_t =  (\log(\mbox{PRICE}_{t})-\log(\mbox{PRICE}_{t-1})).
#' }
#' 
#' @param ts xts object
#' 
#' @return an xts object containing the log returns.
#' 
#' @details Note: the first (row of) observation(s) is set to zero.
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' @importFrom xts xts 
#' @importFrom zoo index
#' @export
makeReturns <- function(ts) {
  l <- dim(ts)[1]
  col_names <- names(ts)
  x <- matrix(as.numeric(ts), nrow = l)
  x[(2:l), ] <- log(x[(2:l), ]) - log(x[(1:(l - 1)), ])
  x[1, ] <- rep(0, dim(ts)[2])
  x <- xts(x, order.by = index(ts))
  names(x) <- col_names
  return(x)
}

#' Merge multiple quote entries with the same time stamp
#' 
#' @description Function replaces multiple quote entries that have the same time stamp 
#' by a single one and returns an xts object with unique time stamps only.
#' 
#' @param qdata an xts object or data.table containing the time series data, with 
#' at least two columns named "BID" and "OFR" indicating the bid and ask price 
#' and two columns "BIDSIZ", "OFRSIZ" indicating the number of round lots available at these 
#' prices. For data.table an additional column "DT" is necessary that stores the date/time information.
#' @param selection indicates how the bid and ask price for a certain time stamp
#' should be calculated in case of multiple observation for a certain time
#' stamp. By default, selection = "median", and the median price is taken. Alternatively:
#' \itemize{
#' \item selection = "max.volume": use the (bid/ask) price of the entry with
#' largest (bid/ask) volume.
#' \item selection = "weighted.average": take the weighted average of all bid (ask) prices,
#' weighted by "BIDSIZ" ("OFRSIZ").
#' }
#' 
#' @return xts or data.table object depending on input
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' @keywords cleaning 
#' @export
mergeQuotesSameTimestamp <- function(qdata, selection = "median") {
  BID = OFR = DT = SYMBOL = .SD = BIDSIZ = OFRSIZ = MAXBID = MAXOFR = NULL 
  qdata <- checkColumnNames(qdata)
  checkQdata(qdata)
  
  condition <- selection == "median" | selection == "max.volume" | selection == "weighted.average"
  if (condition == FALSE) {
    stop(paste("Selection has to be \"median\", \"max.volume\" or \"weighted.average\" "))
  }
  
  dummy_was_xts <- FALSE
  if (is.data.table(qdata) == FALSE) {
    if (is.xts(qdata) == TRUE) {
      qdata <- setnames(as.data.table(qdata)[, BID := as.numeric(as.character(BID))][, OFR := as.numeric(as.character(OFR))]
                        [, BIDSIZ := as.numeric(as.character(BIDSIZ))][, OFRSIZ := as.numeric(as.character(OFRSIZ))], 
                        old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(qdata)) == FALSE) {
      stop("Data.table neeeds DT column (date-time ).")
    }
  }
  
  if (selection == "median") {
    qdata <- qdata[,  lapply(.SD, median), by = list(DT, SYMBOL), .SDcols = c("BID", "OFR")]
  }
  
  if (selection == "max.volume") {
    qdata <- qdata[, MAXBID := max(BIDSIZ), by = "DT"][, MAXOFR := max(OFRSIZ), by = "DT"][
      , BIDSIZ := ifelse(BIDSIZ == MAXBID, 1, 0)][
      , OFRSIZ := ifelse(OFRSIZ == MAXOFR, 1, 0)][
      , BID := BID * BIDSIZ][
      , OFR := OFR * OFRSIZ][
      , BID := max(BID), by = "DT"][, OFR := max(OFR), by = "DT"][, -c("MODE", "MAXBID", "MAXOFR", "BIDSIZ", "OFRSIZ")][
      , lapply(.SD, unique), by = list(DT, SYMBOL), .SDcols = c("BID", "OFR")]
  }
  if (selection == "weighted.average") {
    qdata <- qdata[, `:=` (BIDSIZ = BIDSIZ / sum(BIDSIZ), OFRSIZ = OFRSIZ / sum(OFRSIZ)), by = "DT"][
        , `:=` (BID = sum(BID * BIDSIZ), OFR = sum(OFR * OFRSIZ)), by = "DT"][, -c("MODE", "BIDSIZ", "OFRSIZ")][
        , lapply(.SD, unique), by = list(DT, SYMBOL), .SDcols = c("BID", "OFR")]
  }
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(qdata[, -c("DT")]), order.by = qdata$DT))
  } else {
    return(qdata)
  }
}
# microbenchmark::microbenchmark(quotesCleanup(qdataraw = sample_qdataraw, exchanges = "N", selection = "max.volume"), times = 10, unit = "s")
# microbenchmark::microbenchmark(quotesCleanup(qdataraw = sample_qdataraw, exchanges = "N", selection = "maxvolume"), times = 10, unit = "s")

#' Delete the observations where the bid or ask is zero
#' @description Function deletes the observations where the bid or ask is zero.
#' 
#' @param qdata an xts or data.table object at least containing the columns "BID" and "OFR".
#' 
#' @return xts object or data.table depending on type of input
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' @keywords cleaning
#' @export
noZeroQuotes <- function(qdata) {
  BID = OFR = DT = NULL
  qdata <- checkColumnNames(qdata)
  checkQdata(qdata)
  
  dummy_was_xts <- FALSE
  if (is.data.table(qdata) == FALSE) {
    if (is.xts(qdata) == TRUE) {
      qdata <- setnames(as.data.table(qdata)[, BID := as.numeric(as.character(BID))][, OFR := as.numeric(as.character(OFR))], old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(qdata)) == FALSE) {
      stop("Data.table neeeds DT column.")
    }
  }
  
  qdata <- qdata[BID != 0 & OFR != 0]
  
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(qdata[, -c("DT")]), order.by = qdata$DT))
  } else {
    return(qdata)
  }
}


#' Cleans quote data
#' 
#' @description This is a wrapper function for cleaning the quote data of all stocks in "ticker" over the interval [from,to]. 
#' The result is saved in the folder datadestination. 
#' 
#' In case you supply the argument "rawqdata", the on-disk functionality is ignored
#' and the function returns a list with the cleaned quotes as xts object (see examples).
#' 
#' The following cleaning steps are performed sequentially:
#' \code{\link{noZeroQuotes}}, \code{\link{selectExchange}}, rmLargeSpread,
#' \code{\link{mergeQuotesSameTimestamp}}, \code{\link{rmOutliersQuotes}}.
#' @param from character indicating first date to clean, e.g. "2008-01-30".
#' @param to character indicating last date to clean, e.g. "2008-01-31".
#' @param datasource character indicating the folder in which the original data is stored.
#' @param datadestination character indicating the folder in which the cleaned data is stored.
#' @param ticker vector of tickers for which the data should be cleaned, e.g. ticker = c("AAPL","AIG").
#' @param exchanges vector of stock exchange symbols for all tickers in vector "ticker". It thus should have the same length as the vector ticker.
#' Only data from one single exchange will be retained for each stock respectively, e.g. exchanges = c("T","N").
#' The possible exchange symbols are:
#' \itemize{
#' \item A: AMEX
#' \item N: NYSE
#' \item B: Boston
#' \item P: Arca
#' \item C: NSX
#' \item T/Q: NASDAQ
#' \item D: NASD ADF and TRF
#' \item X: Philadelphia
#' \item I: ISE
#' \item M: Chicago
#' \item W: CBOE
#' \item Z: BATS
#' }
#' @param qdataraw xts or data.table object containing (ONE stock only) raw quote data. This argument is NULL by default. Enabling it means the arguments
#' from, to, datasource and datadestination will be ignored. (only advisable for small chunks of data)
#' @param report boolean and TRUE by default. In case it is true the function returns (also) a vector indicating how many quotes remained after each cleaning step.
#' @param selection argument to be passed on to the cleaning routine \code{\link{mergeQuotesSameTimestamp}}. The default is "median".
#' @param maxi spreads which are greater than median(spreads of day) times maxi are excluded.
#' @param window argument to be passed on to the cleaning routine \code{\link{rmOutliersQuotes}}. 
#' @param type argument to be passed on to the cleaning routine \code{\link{rmOutliersQuotes}}.
#' @param rmoutliersmaxi argument to be passed on to the cleaning routine \code{\link{rmOutliersQuotes}}.
#' 
#' @return For each day an xts object is saved into the folder of that date, containing the cleaned data.
#' This procedure is performed for each stock in "ticker".
#' The function returns a vector indicating how many quotes remained after each cleaning step.
#' 
#' In case you supply the argument "rawqdata", the on-disk functionality is ignored
#' and the function returns a list with the cleaned quotes as xts object (see examples).
#' 
#' @references Barndorff-Nielsen, O. E., P. R. Hansen, A. Lunde, and N. Shephard (2009). Realized kernels in practice: Trades and quotes. Econometrics Journal 12, C1-C32.
#' Brownlees, C.T. and Gallo, G.M. (2006). Financial econometric analysis at ultra-high frequency: Data handling concerns. Computational Statistics & Data Analysis, 51, pages 2232-2245.
#' Falkenberry, T.N. (2002). High frequency data filtering. Unpublished technical report.
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' 
#' @examples
#' # Consider you have raw quote data for 1 stock for 1 day 
#' # data("sample_qdataraw")
#' # head(sample_qdataraw)
#' # dim(sample_qdataraw)
#' # qdata_aftercleaning = quotesCleanup(qdataraw=sample_qdataraw,exchanges="N")
#' # qdata_aftercleaning$report
#' # barplot(qdata_aftercleaning$report)
#' # dim(qdata_aftercleaning$qdata)
#' 
#' # In case you have more data it is advised to use the on-disk functionality
#' # via "from","to","datasource",etc. arguments
#' 
#' @keywords cleaning
quotesCleanup <- function(from, to, datasource, datadestination, ticker, exchanges, qdataraw = NULL, report = TRUE, 
                          selection = "median", maxi = 50, window = 50, type = "advanced", rmoutliersmaxi = 10) {
  BID = OFR = DT = SPREAD = SPREAD_MEDIAN = EX = DATE = BIDSIZ = OFRSIZ = NULL
  nresult <- c(initial_number = 0,
               no_zero_quotes = 0,
               select_exchange = 0,
               remove_negative_spread = 0,
               remove_large_spread = 0,
               merge_same_timestamp = 0,
               remove_outliers = 0)
  
  qdataraw <- checkColumnNames(qdataraw)
  checkQdata(qdataraw)
  
  dummy_was_xts <- FALSE
  if (is.data.table(qdataraw) == FALSE) {
    if (is.xts(qdataraw) == TRUE) {
      qdataraw <- setnames(as.data.table(qdataraw)[, BID := as.numeric(as.character(BID))][, OFR := as.numeric(as.character(OFR))]
                           [, BIDSIZ := as.numeric(as.character(BIDSIZ))][, OFRSIZ := as.numeric(as.character(OFRSIZ))], old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(qdataraw)) == FALSE) {
      stop("Data.table neeeds DT column.")
    }
  }
  
  nresult[1] <- dim(qdataraw)[1] 
  qdataraw <- qdataraw[BID != 0 & OFR != 0]
  nresult[2] <- dim(qdataraw)[1] 
  qdataraw <- qdataraw[EX %in% exchanges]
  nresult[3] <- dim(qdataraw)[1] 
  qdataraw <- qdataraw[OFR > BID][, SPREAD := OFR - BID][, DATE := as.Date(DT)][, SPREAD_MEDIAN := median(SPREAD), by = "DATE"]
  nresult[4] <- dim(qdataraw)[1] 
  qdataraw <- qdataraw[SPREAD < (SPREAD_MEDIAN * maxi)]
  nresult[5] <- dim(qdataraw)[1]
  qdataraw <- mergeQuotesSameTimestamp(qdata = qdataraw, selection = selection)
  nresult[6] <- dim(qdataraw)[1]
  
  qdataraw <- rmOutliersQuotes(qdataraw, window = window, type = "advanced", maxi = rmoutliersmaxi)
  nresult[7] <- dim(qdataraw)[1]
  if (dummy_was_xts == TRUE) {
    df_result <- xts(as.matrix(qdataraw[, -c("DT",  "DATE")]), order.by = qdataraw$DT)
  } else {
    df_result <- qdataraw[, -c( "DATE")]
  }
  
  if (report == TRUE) {
    return(list(qdata = df_result, report = nresult))
  } else {
    return(df_result)
  }
}

#' Delete entries for which the spread is more than "maxi" times the median spread
#' 
#' @description Function deletes entries for which the spread is more than "maxi" times the median
#' spread on that day.
#' 
#' @param qdata an xts or data.table object at least containing the columns "BID" and "OFR".
#' @param maxi an integer. By default maxi = "50", which means that entries are deleted 
#' if the spread is more than 50 times the median spread on that day.
#' 
#' @return xts or data.table object depending on input.
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' @keywords cleaning
rmLargeSpread <- function(qdata, maxi = 50) {
  BID = OFR = DATE = DT = SPREAD = SPREAD_MEDIAN = NULL
  qdataraw <- checkColumnNames(qdata)
  checkQdata(qdata)
  dummy_was_xts <- FALSE
  if (is.data.table(qdata) == FALSE) {
    if (is.xts(qdata) == TRUE) {
      qdata <- setnames(as.data.table(qdata)[, BID := as.numeric(as.character(BID))][, OFR := as.numeric(as.character(OFR))], old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } 
  
  qdataraw <- qdataraw[, DATE := as.Date(DT)][, SPREAD_MEDIAN := median(SPREAD), by = "DATE"][SPREAD < (SPREAD_MEDIAN * maxi)]
  
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(qdata[, -c("DATE")]), order.by = qdata$DT))
  } else {
    return(qdata[, -c("DATE")])
  }
}

#' Delete entries for which the transaction price is outlying with respect to surrounding entries
#' 
#' @description If type = "standard": Function deletes entries for which the price deviated by more than "maxi"
#' median absolute deviations from a rolling centered median (excluding
#' the observation under consideration) of "window" observations.
#' 
#' If type = "advanced": Function deletes entries for which the price deviates by more than "maxi"
#' median absolute deviations from the value closest to the price of
#' these three options:
#' \enumerate{
#'  \item Rolling centered median (excluding the observation under consideration)
#'  \item Rolling median of the following "window" observations
#'  \item Rolling median of the previous "window" observations
#' }
#'  
#' The advantage of this procedure compared to the "standard" proposed
#' by Barndorff-Nielsen et al. (2010) is that it will not incorrectly remove
#' large price jumps. Therefore this procedure has been set as the default
#' for removing outliers. 
#' 
#' Note that the median absolute deviation is taken over the entire
#' day. In case it is zero (which can happen if mid-quotes don't change much), 
#' the median absolute deviation is taken over a subsample without constant mid-quotes.
#' 
#' @param pdata an data.table or xts object at least containing the columns "BID" and "OFR".
#' @param maxi an integer, indicating the maximum number of median absolute deviations allowed.
#' @param window an integer, indicating the time window for which the "outlyingness" is considered.
#' @param type should be "standard" or "advanced" (see description).
#' 
#' @details NOTE: This function works only correct if supplied input data consists of 1 day.
#' 
#' @return xts object or data.table depending on type of input
#' 
#' @references Barndorff-Nielsen, O. E., P. R. Hansen, A. Lunde, and N. Shephard (2009). Realized kernels in practice: Trades and quotes. Econometrics Journal 12, C1-C32.
#' 
#' Brownlees, C.T. and Gallo, G.M. (2006). Financial econometric analysis at ultra-high frequency: Data handling concerns. Computational Statistics & Data Analysis, 51, pages 2232-2245.
#' 
#' @author Onno Kleen
#' 
#' @keywords cleaning
#' @importFrom stats mad median
#' @importFrom data.table as.data.table is.data.table setnames
#' @importFrom xts is.xts as.xts
#' @importFrom RcppRoll roll_median
#' @export
rmOutliersTrades <- function(pdata, maxi = 10, window = 50, type = "advanced") {
  # NOTE: Median Absolute deviation chosen contrary to Barndorff-Nielsen et al.
  # Setting those variables equal NULL is for suppressing NOTES in devtools::check
  # References inside data.table-operations throw "no visible binding for global variable ..." error
  BID = OFR = MIDQUOTE = DATE = DT = MADALL = CRITERION = PRICE = NULL
  if ((window %% 2) != 0) {
    stop("Window size can't be odd.")
  }
  
  # 
  qdata <- checkColumnNames(pdata)
  checkQdata(qdata)
  
  dummy_was_xts <- FALSE
  if (is.data.table(pdata) == FALSE) {
    if (is.xts(pdata) == TRUE) {
      pdata <- setnames(as.data.table(pdata)[, PRICE := as.numeric(as.character(PRICE))], old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(pdata)) == FALSE) {
      stop("Data.table neeeds DT column.")
    }
  }
  
  if (length(unique(pdata$SYMBOL)) > 1) {
    stop("Please provide only one symbol at a time.")
  }
  
  if ((type %in% c("standard", "advanced")) == FALSE) {
    stop("type has to be \"standard\" or \"advanced\".")
  }
  
  # weights_med_center_incl <- rep(1, times = window + 1)
  weights_med_center_excl <- c(rep(1, times = window / 2), 0, rep(1, times = window / 2))
  weights_med_follow  <- c(0 , rep(1, times = window))
  weights_med_trail    <- c(rep(1, times = window), 0)
  
  pdata <- pdata[, DATE := as.Date(DT)][, MADALL := mad(PRICE), by = "DATE"]
  
  if (type == "standard") {
    pdata <- pdata[ , CRITERION := abs(PRICE - rolling_median_incl_ends(PRICE, window = window, weights = weights_med_center_excl))][
      CRITERION < maxi * MADALL]
  }
  if (type == "advanced") {
    pdata <- pdata[, CRITERION := pmin(abs(PRICE - rolling_median_incl_ends(PRICE, window = window, weights = weights_med_center_excl, direction = "center")),
                                       abs(PRICE - rolling_median_incl_ends(PRICE, window = window, weights = weights_med_trail, direction = "left")),
                                       abs(PRICE - rolling_median_incl_ends(PRICE, window = window, weights = weights_med_follow, direction = "right")),
                                       na.rm = TRUE)][
                                         CRITERION < maxi * MADALL]
  }
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(pdata[, -c("DT", "DATE", "MADALL", "CRITERION")]), order.by = pdata$DT))
  } else {
    return(pdata[, -c("MADALL", "CRITERION")])
  }
}

#' Delete entries for which the mid-quote is outlying with respect to surrounding entries
#' 
#' @description If type = "standard": Function deletes entries for which the mid-quote deviated by more than "maxi"
#' median absolute deviations from a rolling centered median (excluding
#' the observation under consideration) of "window" observations.
#' 
#' If type = "advanced":  Function deletes entries for which the mid-quote deviates by more than "maxi"
#' median absolute deviations from the value closest to the mid-quote of
#' these three options:
#' \enumerate{
#'  \item Rolling centered median (excluding the observation under consideration)
#'  \item Rolling median of the following "window" observations
#'  \item Rolling median of the previous "window" observations
#' }
#'  
#' The advantage of this procedure compared to the "standard" proposed
#' by Barndorff-Nielsen et al. (2010) is that it will not incorrectly remove
#' large price jumps. Therefore this procedure has been set as the default
#' for removing outliers. 
#' 
#' Note that the median absolute deviation is taken over the entire
#' day. In case it is zero (which can happen if mid-quotes don't change much), 
#' the median absolute deviation is taken over a subsample without constant mid-quotes.
#' 
#' @param qdata an data.table or xts object at least containing the columns "BID" and "OFR".
#' @param maxi an integer, indicating the maximum number of median absolute deviations allowed.
#' @param window an integer, indicating the time window for which the "outlyingness" is considered.
#' @param type should be "standard" or "advanced" (see description).
#' 
#' @details NOTE: This function works only correct if supplied input data consists of 1 day.
#' 
#' @return xts object or data.table depending on type of input
#' 
#' @references Barndorff-Nielsen, O. E., P. R. Hansen, A. Lunde, and N. Shephard (2009). Realized kernels in practice: Trades and quotes. Econometrics Journal 12, C1-C32.
#' 
#' Brownlees, C.T. and Gallo, G.M. (2006). Financial econometric analysis at ultra-high frequency: Data handling concerns. Computational Statistics & Data Analysis, 51, pages 2232-2245.
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' 
#' @keywords cleaning
#' @importFrom stats mad median
#' @importFrom data.table as.data.table is.data.table setnames
#' @importFrom xts is.xts as.xts
#' @importFrom RcppRoll roll_median
#' @export
rmOutliersQuotes <- function (qdata, maxi = 10, window = 50, type = "advanced") {
  # NOTE: Median Absolute deviation chosen contrary to Barndorff-Nielsen et al.
  # Setting those variables equal NULL is for suppressing NOTES in devtools::check
  # References inside data.table-operations throw "no visible binding for global variable ..." error
  BID = OFR = MIDQUOTE = DATE = DT = MADALL = CRITERION = NULL
  if ((window %% 2) != 0) {
    stop("Window size can't be odd.")
  }
  
  qdata <- checkColumnNames(qdata)
  checkQdata(qdata)
  
  dummy_was_xts <- FALSE
  if (is.data.table(qdata) == FALSE) {
    if (is.xts(qdata) == TRUE) {
      qdata <- setnames(as.data.table(qdata)[, BID := as.numeric(as.character(BID))][, OFR := as.numeric(as.character(OFR))], old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(qdata)) == FALSE) {
      stop("Data.table neeeds DT column.")
    }
  }
  
  if (length(unique(qdata$SYMBOL)) > 1) {
    stop("Please provide only one symbol at a time.")
  }
  
  if ((type %in% c("standard", "advanced")) == FALSE) {
    stop("type has to be \"standard\" or \"advanced\".")
  }
  
  # weights_med_center_incl <- rep(1, times = window + 1)
  weights_med_center_excl <- c(rep(1, times = window / 2), 0, rep(1, times = window / 2))
  weights_med_follow  <- c(0 , rep(1, times = window))
  weights_med_trail    <- c(rep(1, times = window), 0)
  
  qdata <- qdata[, MIDQUOTE := (BID + OFR) / 2][, DATE := as.Date(DT)][, MADALL := mad(MIDQUOTE), by = "DATE"]
  
  if (type == "standard") {
    qdata <- qdata[ , CRITERION := abs(MIDQUOTE - rolling_median_incl_ends(MIDQUOTE, window = window, weights = weights_med_center_excl))][
      CRITERION < maxi * MADALL]
    
  }
  if (type == "advanced") {
    qdata <- qdata[, CRITERION := pmin(abs(MIDQUOTE - rolling_median_incl_ends(MIDQUOTE, window = window, weights = weights_med_center_excl, direction = "center")),
                                       abs(MIDQUOTE - rolling_median_incl_ends(MIDQUOTE, window = window, weights = weights_med_trail, direction = "left")),
                                       abs(MIDQUOTE - rolling_median_incl_ends(MIDQUOTE, window = window, weights = weights_med_follow, direction = "right")),
                                       na.rm = TRUE)][
                                         CRITERION < maxi * MADALL]
  }
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(qdata[, -c("DT", "DATE", "MADALL", "CRITERION", "MIDQUOTE")]), order.by = qdata$DT))
  } else {
    return(qdata[, -c("MADALL", "CRITERION")])
  }
}

#' Retain only data from a single stock exchange
#' @description Function returns an xts object containing the data of only 1 stock exchange.
#' 
#' @param data an xts or data.table object containing the time series data. 
#' The object should have a column "EX", indicating the exchange by its symbol.
#' @param exch The (vector of) symbol(s) of the stock exchange(s) that should be selected.
#' By default the NYSE is chosen (exch="N"). Other exchange symbols are:
#' \itemize{
#' \item A: AMEX
#' \item N: NYSE
#' \item B: Boston
#' \item P: Arca
#' \item C: NSX
#' \item T/Q: NASDAQ
#' \item D: NASD ADF and TRF
#' \item X: Philadelphia
#' \item I: ISE
#' \item M: Chicago
#' \item W: CBOE
#' \item Z: BATS
#' }
#' @return xts or data.table object depending on input
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' @keywords cleaning
#' @export
selectExchange <- function(data, exch = "N") { 
  EX = NULL
  data <- checkColumnNames(data)
  checkQdata(data)
  
  if (is.data.table(data) == FALSE) {
    if (is.xts(data) == TRUE) {
      filteredts <- data[is.element(data$EX , exch)]
      return(filteredts)
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(data)) == FALSE) {
      stop("Data.table neeeds DT column.")
    }
    return(data[EX %in% exch])
  }
}

tradesCleanup <- function(from, to, datasource, datadestination, ticker, exchanges, tdataraw = NULL, report = TRUE, selection = "median") {
  #requireNamespace('timeDate')
  nresult <- rep(0, 5)
  if (is.list(exchanges) == FALSE) { 
    exchanges <- as.list(exchanges)
  }
  
  if (is.null(tdataraw) == TRUE) {
    dates = timeDate::timeSequence(from, to, format = "%Y-%m-%d")
    dates = dates[timeDate::isBizday(dates, holidays = timeDate::holidayNYSE(1960:2050))]
    for (j in 1:length(dates)) {
      datasourcex = paste(datasource, "/", dates[j], sep = "")
      datadestinationx = paste(datadestination, "/", dates[j], sep = "")
      for (i in 1:length(ticker)) {
        dataname = paste(ticker[i], "_trades.RData", sep = "")
        if (file.exists(paste(datasourcex, "/", dataname, sep = ""))) {
          load(paste(datasourcex, "/", dataname, sep = ""))
          if (class(tdata)[1] != "try-error") {            
            exchange = exchanges[[i]]            
            if (length(tdata$PRICE) > 0) {
              tdata <- checkColumnNames(tdata);
              nresult[1] <- nresult[1] + dim(tdata)[1]
            } else {
              tdata <- NULL
            }
            
            if (length(tdata$PRICE) > 0) {
              tdata <- try(nozeroprices(tdata))
              nresult[2] <- nresult[2] + dim(tdata)[1]
            } else {
              tdata <- NULL
            }
            
            if (length(tdata$PRICE) > 0) {
              tdata <- try(selectexchange(tdata, exch = exchange))
              nresult[3] <- nresult[3] + dim(tdata)[1]
            } else {
              tdata <- NULL
            }
            
            if (length(tdata$PRICE) > 0) {
              tdata <- try(salescond(tdata))
              nresult[4] <- nresult[4] + dim(tdata)[1]
            } else {
              tdata <- NULL
            }
            
            if (length(tdata$PRICE) > 0) {
              tdata <- try(mergeTradesSameTimestamp(tdata, selection = selection))
              nresult[5] <- nresult[5] + dim(tdata)[1]
            } else {
              tdata <-NULL
            }
            save(tdata, file = paste(datadestinationx,"/", dataname, sep = ""))
          }
          if (class(tdata) == "try-error") {
            abc = 1
            save(abc, file = paste(datadestinationx, "/missing_", 
                                   ticker[i], ".RData", sep = ""))
          }
          
        }else{
          next;
        }
      }   
    }
    if (report == TRUE) {
      names(nresult) = c("initial number", "no zero prices", 
                         "select exchange", "sales condition", "merge same timestamp")
      return(nresult)
    }
  }
  
  if (is.null(tdataraw) == FALSE) {
    if (class(tdataraw)[1] != "try-error") {
      if (length(exchanges) > 1) {
        print("The argument exchanges contains more than 1 element. Please select a single exchange, in case you provide tdataraw.")
      }
      exchange <- exchanges[[1]]
      tdata <- tdataraw
      rm(tdataraw)
      
      if (length(tdata) > 0) {
        tdata <- checkColumnNames(tdata)
        nresult[1] <- nresult[1] + dim(tdata)[1]
      } else {
        tdata <- NULL
      }
      
      if(length(tdata)>0){
        tdata = try(nozeroprices(tdata))
        nresult[2] = nresult[2] + dim(tdata)[1]
      } else {
        tdata <- NULL
      }
      
      if (length(tdata) > 0) {
        tdata = try(selectexchange(tdata, exch = exchange))
        nresult[3] = nresult[3] + dim(tdata)[1]
      } else {
        tdata <- NULL
      }
      
      if (length(tdata) > 0) {
        tdata = try(salescond(tdata))
        nresult[4] = nresult[4] + dim(tdata)[1]
      } else {
        tdata <- NULL
      }
      
      if (length(tdata) > 0) {
        tdata <- try(mergeTradesSameTimestamp(tdata, selection = selection))
        nresult[5] <- nresult[5] + dim(tdata)[1]
      } else {
        tdata <- NULL
      }
      
      if (report == TRUE) {
        names(nresult) = c("initial number", "no zero prices", 
                           "select exchange", "sales condition", "merge same timestamp")
        return(list(tdata = tdata, report = nresult))
      }
      if (report != TRUE) {
        return(tdata)
      }
    }
  }
  
}

# rmOutliersOld <- function (qdata, maxi = 10, window = 50, type = "advanced") {
#   qdata <- checkColumnNames(qdata)
#   checkQdata(qdata)
#   ## function to remove entries for which the mid-quote deviated by more than 10 median absolute deviations 
#   ## from a rolling centered median (excluding the observation under consideration) of 50 observations if type = "standard".
#   
#   ## if type = "advanced":
#   ## function removes entries for which the mid-quote deviates by more than 10 median absolute deviations
#   ## from the variable "mediani".
#   ## mediani is defined as the value closest to the midquote of these three options:
#   ## 1. Rolling centered median (excluding the observation under consideration)
#   ## 2. Rolling median of the following "window" observations
#   ## 3. Rolling median of the previous "window" observations
#   
#   ##NOTE: Median Absolute deviation chosen contrary to Barndorff-Nielsen et al.
#   window <- floor(window/2) * 2
#   condition <- c()
#   halfwindow <- window/2
#   midquote <- as.vector(as.numeric(qdata$BID) + as.numeric(qdata$OFR))/2
#   mad_all <- mad(midquote)
#   
#   midquote <- xts(midquote,order.by = index(qdata))
#   
#   if (mad_all == 0) {
#     m <- as.vector(as.numeric(midquote))
#     s <- c(TRUE, (m[2:length(m)] - m[1:(length(m) - 1)] != 0))
#     mad_all <- mad(as.numeric(midquote[s]))
#   }
#   
#   medianw <- function(midquote, n = window) {
#     m <- floor(n/2) + 1
#     q <- median(c(midquote[1:(m - 1)], midquote[(m + 1):(n + 1)]))
#     return(q)
#   }
#   
#   if (type == "standard") {
#     meds <- as.numeric(rollapply(midquote, width = (window + 1), FUN = medianw, align = "center"))
#   }
#   if (type == "advanced") {
#     advancedperrow <- function(qq) {
#       diff <- abs(qq[1:3] - qq[4])
#       select <- min(diff) == diff
#       value <- qq[select]
#       if (length(value) > 1) {
#         value <- median(value)
#       }
#       return(value)
#     }
#     n <- length(midquote)
#     allmatrix <- matrix(rep(0, 4 * (n)), ncol = 4)
#     median2 <- function(a){ 
#       median(a)
#     }
#     standardmed <- as.numeric(rollapply(midquote, width = c(window), 
#                                         FUN = median2, align = "center"))
#     standardmed <- standardmed[!is.na(standardmed)] 
#     
#     temp <- as.numeric(rollapply(midquote, 
#                                  width = (window + 1), 
#                                  FUN = medianw, 
#                                  align = "center"))
#     
#     allmatrix[(halfwindow + 1):(n - halfwindow), 1] = temp[!is.na(temp)]
#     allmatrix[(1:(n - window)), 2] <- standardmed[2:length(standardmed)]
#     allmatrix[(window + 1):(n), 3] <- standardmed[1:(length(standardmed) - 1)]
#     allmatrix[, 4] <- midquote
#     meds <- apply(allmatrix, 1, advancedperrow)[(halfwindow + 1):(n - halfwindow)]
#   }
#   
#   midquote <- as.numeric(midquote);
#   maxcriterion <- meds + maxi * mad_all
#   mincriterion <- meds - maxi * mad_all
#   
#   condition <- mincriterion < midquote[(halfwindow + 1):(length(midquote) - halfwindow)] & midquote[(halfwindow + 1):(length(midquote) - halfwindow)] < maxcriterion
#   condition <- c(rep(TRUE, halfwindow), condition, rep(TRUE, halfwindow))
#   qdata[condition]
# }


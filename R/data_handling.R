#' Aggregate a data.table or xts object containing trades data
#' 
#' @description Function returns new time series as a data.table or xts object where first observation is always the opening price
#' and subsequent observations are the closing prices over the interval with as endpoint the timestamp 
#' of the result.
#' 
#' @param tdata data.table or xts object to be aggregated, containing the intraday price series of a stock for possibly multiple days.
#' @param on character, indicating the time scale in which "k" is expressed. Possible values are: "secs", "seconds", "mins", "minutes", "hours".
#' @param k positive integer, indicating the number of periods to aggregate over. E.g. to aggregate an
#' object to the 5 minute frequency set k = 5 and on = "minutes".
#' @param marketopen the market opening time, by default: marketopen = "09:30:00".
#' @param marketclose the market closing time, by default: marketclose = "16:00:00".
#' @param tz time zone used, by default: tz = "GMT".
#' @details The timestamps of the new time series are the closing times and/or days of the intervals. 
#' 
#' The output "PRICE" column is constructed using previous tick aggregation.
#' 
#' The variable "SIZE" is aggregated by taking the sum over each interval.
#' 
#' The variable "VWPRICE" is the aggregated price weighted by volume.
#' 
#' The timestamps of the new time series are the closing times of the intervals. 
#' 
#' In case of previous tick aggregation or on = "seconds"/"minutes"/"hours",
#' the element of the returned series with e.g. timestamp 09:35:00 contains 
#' the last observation up to that point, including the value at 09:35:00 itself.
#' 
#' @return An data.table or xts object containing the aggregated time series.
#' 
#' @author Jonathan Cornelissen, Kris Boudt and Onno Kleen.
#' @keywords data manipulation
#' 
#' @examples 
#' # aggregate trade data to 5 minute frequency
#' tdata_aggregated <- aggregateTrades(sample_tdata, on = "minutes", k = 5)
#' head(tdata_aggregated)
#' @importFrom lubridate floor_date
#' @importFrom lubridate ceiling_date
#' @importFrom lubridate ymd_hms
#' @importFrom lubridate as_datetime
#' @export
aggregateTrades <- function(tdata, on = "minutes", k = 5, marketopen = "09:30:00", marketclose = "16:00:00", tz = "GMT") {
  DATE = SIZE = DT = FIRST_DT = DT_ROUND = LAST_DT = SYMBOL = PRICE = VWPRICE = SIZEPRICE = SIZESUM = NULL
  tdata <- checkColumnNames(tdata)
  checktdata(tdata)
  
  dummy_was_xts <- FALSE
  if (is.data.table(tdata) == FALSE) {
    if (is.xts(tdata) == TRUE) {
      tdata <- setnames(as.data.table(tdata)[, SIZE := as.numeric(as.character(SIZE))][, PRICE := as.numeric(as.character(PRICE))], 
                        old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(tdata)) == FALSE) {
      stop("Data.table neeeds DT column (date-time ).")
    }
  }
  
  tdata[, DATE := as.Date(DT)]
  tdata[, FIRST_DT := min(DT), by = "DATE"]
  tdata[, DT_ROUND := ifelse(DT == FIRST_DT,
                             floor_date(ymd_hms(DT), unit = paste(k, on)),
                             ceiling_date(ymd_hms(DT), unit = paste(k, on), change_on_boundary = FALSE))]
  tdata[, DT_ROUND := as_datetime(DT_ROUND)]
  tdata[, LAST_DT := max(DT), by = "DT_ROUND"]
  tdata[, SIZETPRICE := SIZE * PRICE]
  tdata[, SIZESUM := sum(SIZE), by = "DT_ROUND"]
  tdata[, VWPRICE := sum(SIZETPRICE/SIZESUM), by = "DT_ROUND"]
  
  tdata <- tdata[DT == LAST_DT][, DT := DT_ROUND][, c("DT", "SYMBOL", "PRICE", "SIZE", "VWPRICE")]
  
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(tdata[, -c("DT")]), order.by = tdata$DT, tzone = tz))
  } else {
    return(tdata)
  }
}

#' Aggregate a data.table or xts object containing quote data
#' 
#' @description Function returns a data.table or xts object containing the aggregated quote data with columns "SYMBOL", "EX", "BID","BIDSIZ","OFR","OFRSIZ". 
#' See \code{\link{sample_qdata}} for an example of the argument qdata.
#' 
#' @param qdata data.table or xts object to be aggregated, containing the intraday quote data of a stock for one day.
#' @param on character, indicating the time scale in which "k" is expressed. Possible values are: "secs", "seconds", "mins", "minutes","hours".
#' xts object to the 5 minute frequency, set k=5 and on="minutes".
#' @param k positive integer, indicating the number of periods to aggregate over. E.g. to aggregate an
#' object to the 5 minute frequency set k = 5 and on = "minutes".
#' @param marketopen the market opening time, by default: marketopen = "09:30:00".
#' @param marketclose the market closing time, by default: marketclose = "16:00:00".
#' @param tz time zone used, by default: tz = "GMT".
#' 
#' @return An data.table or xts object containing the aggregated time series.
#' 
#' @details The output "BID" and "OFR" columns are constructed using previous tick aggregation.
#' 
#' The variables "BIDSIZ" and "OFRSIZ" are aggregated by taking the sum of the respective inputs over each interval.
#' 
#' The timestamps of the new time series are the closing times of the intervals. 
#' 
#' Please Note: Returned objects always contain the first observation (i.e. opening quotes,...).
#' 
#' @return A data.table or xts object containing the aggregated quote data.
#' 
#' @author Jonathan Cornelissen, Kris Boudt and Onno Kleen
#' @keywords data manipulation
#' 
#' @examples
#' # aggregate quote data to the 30 second frequency
#' qdata_aggregated <- aggregateQuotes(sample_qdata, on = "seconds", k = 30)
#' head(qdata_aggregated)
#' @export
aggregateQuotes <- function(qdata, on = "minutes", k = 5, marketopen = "09:30:00", marketclose = "16:00:00", tz = "GMT") {
  DATE = BID = OFR = BIDSIZ = OFRSIZ = DT = FIRST_DT = DT_ROUND = LAST_DT = SYMBOL = NULL
  
  qdata <- checkColumnNames(qdata)
  checkqdata(qdata)
  
  dummy_was_xts <- FALSE
  if (is.data.table(qdata) == FALSE) {
    if (is.xts(qdata) == TRUE) {
      qdata <- setnames(as.data.table(qdata)[, BID := as.numeric(as.character(BID))][
        , OFR := as.numeric(as.character(OFR))][
          , BIDSIZ := as.numeric(as.character(BIDSIZ))][
            , OFRSIZ := as.numeric(as.character(OFRSIZ))], old = "index", new = "DT")
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
  
  
  # tdata <- sample_tdata_microseconds
  qdata[, DATE := as.Date(DT)]
  qdata[, FIRST_DT := min(DT), by = "DATE"]
  qdata[, DT_ROUND := ifelse(DT == FIRST_DT,
                             floor_date(ymd_hms(DT), unit = paste(k, on)),
                             ceiling_date(ymd_hms(DT), unit = paste(k, on), change_on_boundary = FALSE))]
  qdata[, DT_ROUND := as_datetime(DT_ROUND)]
  qdata[, LAST_DT := max(DT), by = "DT_ROUND"]
  qdata[, OFRSIZ := sum(OFRSIZ), by = "DT_ROUND"]
  qdata[, BIDSIZ := sum(BIDSIZ), by = "DT_ROUND"]
  
  qdata <- qdata[DT == LAST_DT][, DT := DT_ROUND][, c("DT", "SYMBOL", "BID", "BIDSIZ", "OFR", "OFRSIZ")]
  
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(qdata[, -c("DT")]), order.by = qdata$DT, tzone = tz))
  } else {
    return(qdata)
  }
}

exchangeHoursOnly <- function(data, daybegin = "09:30:00", dayend = "16:00:00") {
  data <- checkColumnNames(data)
  
  if(is(data, "xts") == FALSE) {
    stop("data must be an xts object.")
  }
  
  gettime <- function(z){unlist(strsplit(as.character(z)," "))[2]}
  times1 <- as.matrix(as.vector(as.character(index(data))))
  times <- apply(times1,1,gettime); 
  tdtimes <- as.POSIXct(times,format = "%H:%M:%S",tz = "GMT")
  
  #create timeDate begin and end
  tddaybegin <- as.POSIXct(daybegin,format = "%H:%M:%S", tz = "GMT")
  tddayend   <- as.POSIXct(dayend,format = "%H:%M:%S",   tz = "GMT")
  
  #select correct observations
  filteredts <- data[tdtimes>=tddaybegin & tdtimes<=tddayend]
  return(filteredts)
}

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
      x <- x[, loc]
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

#' Match trade and quote data
#' @description Function matches the trades and quotes and returns an xts-object containing both.
#' 
#' @param tdata data.table or xts-object containing the trade data (multiple days possible).
#' @param qdata data.table or xts-object containing the quote data (multiple days possible).
#' @param adjustment numeric, number of seconds the quotes are registered faster than
#' the trades (should be round and positive). Based on the research of
#' Vergote (2005), we set 2 seconds as the default.
#' 
#' @return data.table or xts-object containing the matched trade and quote data
#' 
#' @references  Vergote, O. (2005). How to match trades and quotes for NYSE stocks?
#' K.U.Leuven working paper.
#' 
#' @author Jonathan Cornelissen, Kris Boudt and Onno Kleen
#' 
#' @keywords data manipulation
#' 
#' @examples 
#' # match the trade and quote data
#' tqdata <- matchTradesQuotes(sample_tdata, sample_qdata)
#' head(tqdata)
#' # multi-day input allowed
#' tqdata <- matchTradesQuotes(sample_tdata_microseconds, sample_qdata_microseconds)
#' @importFrom lubridate seconds
#' @export
matchTradesQuotes <- function(tdata, qdata, adjustment = 2) {
  
  PRICE = BID = PFR = DATE = DT = FIRST_DT = TD_ROUND = SYMBOL = NULL
  
  tdata <- checkColumnNames(tdata)
  qdata <- checkColumnNames(qdata)
  checkqdata(qdata)
  checktdata(tdata)
  
  if (any(class(tdata) != class(qdata))) {
    stop("tdata and qdata should be of the same data type, either xts or data.table.")
  }
  
  dummy_was_xts <- FALSE
  if (is.data.table(tdata) == FALSE) {
    if (is.xts(tdata) == TRUE) {
      tdata <- setnames(as.data.table(tdata)[, PRICE := as.numeric(as.character(PRICE))], 
                        old = "index", new = "DT")
      qdata <- setnames(as.data.table(qdata)[, BID := as.numeric(as.character(BID))][, OFR := as.numeric(as.character(OFR))], 
                        old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(tdata)) == FALSE) {
      stop("tdata neeeds DT column.")
    }
  }
  
  qdata[, DATE := as.Date(DT)]
  qdata[, FIRST_DT := min(DT), by = "DATE"]
  qdata[, DT_ROUND := ifelse(DT == FIRST_DT, ymd_hms(DT), ymd_hms(DT) + seconds(2))]
  
  qdata <- qdata[, DT := ifelse(DT != min(DT), DT + seconds(2), DT)] 
  qdata <- qdata[, DT := as_datetime(DT_ROUND)][, -c("FIRST_DT", "DT_ROUND", "DATE")]
  
  setkey(tdata, SYMBOL, DT)
  setkey(qdata, SYMBOL, DT)
  
  tdata <- tdata[, c("DT", "SYMBOL", "PRICE", "SIZE")]
  tqdata <- qdata[tdata, roll = TRUE, on = c("SYMBOL", "DT")]
  
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(tqdata[, -c("DT")]), order.by = tqdata$DT))
  } else {
    return(tqdata)
  }
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
  checkqdata(qdata)
  
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
      stop("Data.table neeeds DT column (date-time).")
    }
  }
  
  if (selection == "median") {
    qdata <- qdata[,  lapply(.SD, median), by = list(DT, SYMBOL), .SDcols = c("BID", "OFR")]
  }
  
  if (selection == "max.volume") {
    qdata <- qdata[, MAXBID := max(BIDSIZ), by = list(DT, SYMBOL)][, MAXOFR := max(OFRSIZ), by = "DT"][
      , BIDSIZ := ifelse(BIDSIZ == MAXBID, 1, 0)][
      , OFRSIZ := ifelse(OFRSIZ == MAXOFR, 1, 0)][
      , BID := BID * BIDSIZ][
      , OFR := OFR * OFRSIZ][
      , BID := max(BID), by = "DT"][, OFR := max(OFR), by = list(DT, SYMBOL)][, -c("MAXBID", "MAXOFR", "BIDSIZ", "OFRSIZ")][
      , lapply(.SD, unique), by = list(DT, SYMBOL), .SDcols = c("BID", "OFR")]
  }
  if (selection == "weighted.average") {
    qdata <- qdata[, `:=` (BIDSIZ = BIDSIZ / sum(BIDSIZ), OFRSIZ = OFRSIZ / sum(OFRSIZ)), by = list(DT, SYMBOL)][
      , `:=` (BID = sum(BID * BIDSIZ), OFR = sum(OFR * OFRSIZ)), by = list(DT, SYMBOL)][
        , -c("BIDSIZ", "OFRSIZ")][
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

#' Merge multiple transactions with the same time stamp
#' 
#' @description Function replaces multiple transactions that have the same time stamp by a single one and returns an xts or data.table object with unique time stamps only.
#' 
#' @param tdata an xts object containing the time series data, with 
#' one column named "PRICE" indicating the transaction price 
#' and one column "SIZE" indicating the number of shares traded.
#' @param selection indicates how the price for a certain time stamp
#' should be calculated in case of multiple observation for a certain time
#' stamp. By default, selection = "median", and the median price is taken. Alternatively:
#' \itemize{
#' \item selection = "max.volume": use the price of the transaction with
#' largest volume.
#' \item selection = "weighted.average": take the weighted average of all prices.
#' }
#' 
#' @return xts or data.table object depending on input
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' @keywords cleaning
#' @export
mergeTradesSameTimestamp <- function(tdata, selection = "median") {
  SIZE = MAXSIZE = PRICE = DT = SYMBOL = .SD = SIZE_WEIGHT = NULL
  
  tdata <- checkColumnNames(tdata)
  checktdata(tdata)
  if (any(colnames(tdata) == "SIZE") == FALSE) {
    stop("The argument tdata should have a column SIZE indicating the number of shares traded. Could not find that column.")
  }
  
  condition <- selection == "median" | selection == "max.volume" | selection == "weighted.average"
  if (condition == FALSE) {
    stop(paste("Selection has to be \"median\", \"max.volume\" or \"weighted.average\" "))
  }
  
  dummy_was_xts <- FALSE
  if (is.data.table(tdata) == FALSE) {
    if (is.xts(tdata) == TRUE) {
      tdata <- setnames(as.data.table(tdata)[, SIZE := as.numeric(as.character(SIZE))][, PRICE := as.numeric(as.character(PRICE))], 
                        old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(tdata)) == FALSE) {
      tdata <- tdata[, SIZE := as.numeric(as.character(SIZE))][, PRICE := as.numeric(as.character(PRICE))]
      stop("Data.table neeeds DT column (date-time ).")
    }
  }
  
  if (selection == "median") {
    tdata[, PRICE := median(PRICE), by = list(DT, SYMBOL)]
    #If there is more than one observation at median price, take the average volume.
    tdata[, SIZE := as.numeric(SIZE)]
    tdata[, SIZE := mean(SIZE), by = list(DT, SYMBOL)] 
    tdata <- unique(tdata[, c("DT", "SYMBOL", "PRICE", "SIZE")])
  }
  
  if (selection == "max.volume") {
    tdata[, SIZE := as.numeric(SIZE)]
    tdata <- tdata[, MAXSIZE := max(SIZE), by = list(DT, SYMBOL)]
    tdata[, SIZE := ifelse(SIZE == MAXSIZE, 1, 0)]
    tdata[, PRICE := PRICE * SIZE]
    tdata[, PRICE := max(PRICE), by = "DT"]
    tdata[, SIZE := MAXSIZE]
    tdata[, -c("MAXSIZE")]
    tdata <- unique(tdata[, c("DT", "SYMBOL", "PRICE", "SIZE")])
  }
  if (selection == "weighted.average") {
    tdata[, SIZE := as.numeric(SIZE)]
    tdata <- tdata[, `:=` (SIZE_WEIGHT = SIZE / sum(SIZE)), by = list(DT, SYMBOL)]
    tdata[, `:=` (PRICE = sum(PRICE * SIZE_WEIGHT)), by = list(DT, SYMBOL)]
    tdata[, SIZE := mean(SIZE), by = list(DT, SYMBOL)]
    tdata <- unique(tdata[, c("DT", "SYMBOL", "PRICE", "SIZE")])
  }
  
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(tdata[, -c("DT")]), order.by = tdata$DT))
  } else {
    return(tdata)
  }
}


#' Delete the observations where the price is zero
#' 
#' @description Function deletes the observations where the price is zero.
#' 
#' @param tdata an xts or data.table object at least containing a column "PRICE". 
#' 
#' @return an xts or data.table object depending on input
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' @keywords cleaning
noZeroPrices <- function(tdata) {
  PRICE <- NULL
  tdata <- checkColumnNames(tdata)
  checktdata(tdata)
  
  dummy_was_xts <- FALSE
  if (is.data.table(tdata) == FALSE) {
    if (is.xts(tdata) == TRUE) {
      tdata <- setnames(as.data.table(tdata)[, PRICE := as.numeric(as.character(PRICE))], old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(tdata)) == FALSE) {
      stop("Data.table neeeds DT column.")
    }
  }
  
  tdata <- tdata[PRICE != 0]
  
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(tdata[, -c("DT")]), order.by = tdata$DT))
  } else {
    return(tdata)
  }
}

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
  checkqdata(qdata)
  
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
#' and the function returns the cleaned quotes as xts or data.table object (see examples).
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
#' # Consider you have raw quote data for 1 stock for 2 days
#' head(sample_qdataraw_microseconds)
#' dim(sample_qdataraw_microseconds)
#' qdata_aftercleaning <- quotesCleanup(qdataraw = sample_qdataraw_microseconds, exchanges = "N")
#' qdata_aftercleaning$report
#' dim(qdata_aftercleaning$qdata)
#' 
#' # In case you have more data it is advised to use the on-disk functionality
#' # via "from","to","datasource",etc. arguments
#' 
#' @keywords cleaning
#' @export
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
  
  if (is.null(qdataraw) == TRUE) {
    stop("On-disk functionality has to be added once again!")
  }
  
  if (is.null(qdataraw) == FALSE) {
    
    qdataraw <- checkColumnNames(qdataraw)
    checkqdata(qdataraw)
    
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
  qdata <- checkColumnNames(qdata)
  checkqdata(qdata)
  dummy_was_xts <- FALSE
  if (is.data.table(qdata) == FALSE) {
    if (is.xts(qdata) == TRUE) {
      qdata <- setnames(as.data.table(qdata)[, BID := as.numeric(as.character(BID))][, OFR := as.numeric(as.character(OFR))], old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } 
  
  qdata <- qdata[, DATE := as.Date(DT)][
    , SPREAD := OFR - BID][
    , SPREAD_MEDIAN := median(SPREAD), by = "DATE"][SPREAD < (SPREAD_MEDIAN * maxi)]
  
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(qdata[, -c("DATE")]), order.by = qdata$DT))
  } else {
    return(qdata[, -c("DATE")])
  }
}

#' @export
rmTradeOutliers <- function(tdata, qdata) {
  warning("Renamed as rmTradeOutliersUsingQuotes. Will be deprecated in future releases.")
  rmTradeOutliersUsingQuotes(tdata, qdata)
}

#' Delete transactions with unlikely transaction prices
#' 
#' @description Function deletes entries with prices that are above the ask plus the bid-ask spread.
#' Similar for entries with prices below the bid minus the bid-ask spread.
#' 
#' @param tdata a data.table or xts object containing the time series data, with at least the column "PRICE", containing the transaction price (ONE DAY ONLY).
#' @param qdata a data.table or xts object containing the time series data with at least the columns "BID" and "OFR", containing the bid and ask prices (ONE DAY ONLY).
#' 
#' @details Note: in order to work correctly, the input data of this function should be
#' cleaned trade (tdata) and quote (qdata) data respectively.
#' 
#' @return xts or data.table object depending on input
#' 
#' @author Jonathan Cornelissen, ris Boudt and Onno Kleen
#' @keywords cleaning
#' @importFrom data.table setkey
#' @export
rmTradeOutliersUsingQuotes <- function(tdata, qdata) {
  SPREAD = DT = PRICE = BID = OFR = SYMBOL = 0
  tdata <- checkColumnNames(tdata)
  qdata <- checkColumnNames(qdata)
  checkqdata(qdata)
  checktdata(tdata)
  
  if (any(class(tdata) != class(qdata))) {
    stop("tdata and qdata should be of the same data type, either xts or data.table.")
  }
  
  dummy_was_xts <- FALSE
  if (is.data.table(tdata) == FALSE) {
    if (is.xts(tdata) == TRUE) {
      tdata <- setnames(as.data.table(tdata)[, PRICE := as.numeric(as.character(PRICE))], 
                        old = "index", new = "DT")
      qdata <- setnames(as.data.table(qdata)[, BID := as.numeric(as.character(BID))][, OFR := as.numeric(as.character(OFR))], 
                        old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(tdata)) == FALSE) {
      stop("tdata neeeds DT column.")
    }
  }
  
  if (length(unique(as.Date(tdata$DT))) > 1) {
    stop("Both data sets should only include data for one day.")
  }
  
  qdata <- qdata[, DT := DT + 2]
  
  setkey(tdata, SYMBOL, DT)
  setkey(qdata, SYMBOL, DT)
  
  tdata <- tdata[, c("DT", "SYMBOL", "PRICE")]
  
  tdata <- qdata[tdata, roll = TRUE, on = c("SYMBOL", "DT")]
  
  tdata[is.na(BID)][, "BID"] <- tdata$BID[min(which(is.na(tdata$BID) == FALSE))]
  tdata[is.na(OFR)][, "OFR"] <- tdata$OFR[min(which(is.na(tdata$OFR) == FALSE))]
  
  tdata <- tdata[, SPREAD := OFR - BID][PRICE <= OFR + SPREAD][PRICE >= BID - SPREAD]
  
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(tdata[, -c("DT", "SPREAD")]), order.by = tdata$DT))
  } else {
    return(tdata[, -c("SPREAD")])
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
  checkqdata(qdata)
  
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
    qdata <- qdata[ , CRITERION := abs(MIDQUOTE - rollingMedianInclEnds(MIDQUOTE, window = window, weights = weights_med_center_excl))][
      CRITERION < maxi * MADALL]
    
  }
  if (type == "advanced") {
    qdata <- qdata[, CRITERION := pmin(abs(MIDQUOTE - rollingMedianInclEnds(MIDQUOTE, window = window, weights = weights_med_center_excl, direction = "center")),
                                       abs(MIDQUOTE - rollingMedianInclEnds(MIDQUOTE, window = window, weights = weights_med_trail, direction = "left")),
                                       abs(MIDQUOTE - rollingMedianInclEnds(MIDQUOTE, window = window, weights = weights_med_follow, direction = "right")),
                                       na.rm = TRUE)][
                                         CRITERION < maxi * MADALL]
  }
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(qdata[, -c("DT", "DATE", "MADALL", "CRITERION", "MIDQUOTE")]), order.by = qdata$DT))
  } else {
    return(qdata[, -c("MADALL", "CRITERION")])
  }
}

#' Delete entries with abnormal Sale Condition.
#' 
#' @description Function deletes entries with abnormal Sale Condition: 
#' trades where column "COND" has
#' a letter code, except for "E" and "F".
#' 
#' @param tdata an xts or data.table object containing the time series data, with 
#' one column named "COND" indicating the Sale Condition.
#' 
#' @return xts or data.table object depending on input
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' 
#' @keywords leaning
#' @export
salesCondition <- function(tdata) {
  COND = NULL
  tdata <- checkColumnNames(tdata)
  checktdata(tdata)
  
  if (any(colnames(tdata) == "COND") == FALSE) {
    stop("The argument tdata should have a column containing sales conditions named COND. Could not find that column.")
  }
  
  dummy_was_xts <- FALSE
  if (is.data.table(tdata) == FALSE) {
    if (is.xts(tdata) == TRUE) {
      tdata <- setnames(as.data.table(tdata), old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } 
  
  tdata <- tdata[COND %in% c("E", "F")]
  
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(tdata), order.by = tdata$DT))
  } else {
    return(tdata)
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
  # checkqdata(data)
  
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

#' Cleans trade data
#' 
#' @description This is a wrapper function for cleaning the trade data of all stocks in "ticker" over the interval [from,to]. 
#' The result is saved in the folder datadestination. The function returns a vector
#' indicating how many trades were removed at each cleaning step.
#' 
#' In case you supply the argument "rawtdata", the on-disk functionality is ignored
#' and the function returns an xts or data.table object.
#' 
#' The following cleaning functions are performed sequentially:
#' \code{\link{noZeroPrices}}, \code{\link{selectExchange}}, \code{\link{salesCondition}},
#' \code{\link{mergeTradesSameTimestamp}}.
#' 
#' Since the function \code{\link{rmTradeOutliersUsingQuotes}}
#' also requires cleaned quote data as input, it is not incorporated here and
#' there is a seperate wrapper called \code{\link{tradesCleanupUsingQuotes}}.
#' 
#' @param from character indicating first date to clean, e.g. "2008-01-30".
#' @param to character indicating last date to clean, e.g. "2008-01-31".
#' @param datasource character indicating the folder in which the original data is stored.
#' @param datadestination character indicating the folder in which the cleaned data is stored.
#' @param ticker vector of tickers for which the data should be cleaned, e.g. ticker = c("AAPL","AIG")
#' @param exchanges list of vectors of stock exchange(s) for all tickers in vector "ticker". It thus should have the same length as the vector ticker.
#' E.g. in case of two stocks; exchanges = list("N", c("Q","T")).
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
#' 
#' @param tdataraw xts object containing (for ONE stock only) raw trade data. This argument is NULL by default. Enabling it means the arguments
#' from, to, datasource and datadestination will be ignored. (only advisable for small chunks of data)
#' @param report boolean and TRUE by default. In case it is true the function returns (also) a vector indicating how many trades remained after each cleaning step.
#' @param selection argument to be passed on to the cleaning routine \code{\link{mergeTradesSameTimestamp}}. The default is "median".
#' 
#' @return For each day an xts or data.table object is saved into the folder of that date, containing the cleaned data.
#' This procedure is performed for each stock in "ticker".
#' The function returns a vector indicating how many trades remained after each cleaning step.
#' 
#' In case you supply the argument "rawtdata", the on-disk functionality is ignored
#' and the function returns a list with the cleaned trades as xts object (see examples).
#' 
#' @examples 
#' # Consider you have raw trade data for 1 stock for 2 days 
#' head(sample_tdataraw_microseconds)
#' dim(sample_tdataraw_microseconds)
#' tdata_afterfirstcleaning <- tradesCleanup(tdataraw = sample_tdataraw, exchanges = list("N"))
#' tdata_afterfirstcleaning$report
#' dim(tdata_afterfirstcleaning$tdata)
#' 
#' #In case you have more data it is advised to use the on-disk functionality
#' #via "from","to","datasource",etc. arguments
#' 
#' @references Barndorff-Nielsen, O. E., P. R. Hansen, A. Lunde, and N. Shephard (2009). Realized kernels in practice: Trades and quotes. Econometrics Journal 12, C1-C32.
#' 
#' Brownlees, C.T. and Gallo, G.M. (2006). Financial econometric analysis at ultra-high frequency: Data handling concerns. Computational Statistics & Data Analysis, 51, pp. 2232-2245.
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' @keywords cleaning
#' @export
tradesCleanup <- function(from, to, datasource, datadestination, ticker, exchanges, tdataraw = NULL, report = TRUE, selection = "median") {
  PRICE = EX = COND = NULL
  if (is.null(tdataraw) == TRUE) {
    stop("On-disk functionality has to be added once again!")
  }
  
  if (is.null(tdataraw) == FALSE) {
    nresult <- c(initial_number = 0,
                 no_zero_trades = 0,
                 select_exchange = 0,
                 remove_sales_condition = 0,
                 merge_same_timestamp = 0)
    
    tdataraw <- checkColumnNames(tdataraw)
    checktdata(tdataraw)
    
    dummy_was_xts <- FALSE
    if (is.data.table(tdataraw) == FALSE) {
      if (is.xts(tdataraw) == TRUE) {
        tdataraw <- setnames(as.data.table(tdataraw)[, PRICE := as.numeric(as.character(PRICE))], old = "index", new = "DT")
        dummy_was_xts <- TRUE
      } else {
        stop("Input has to be data.table or xts.")
      }
    } else {
      if (("DT" %in% colnames(tdataraw)) == FALSE) {
        stop("Data.table neeeds DT column.")
      }
    }
    nresult[1] <- dim(tdataraw)[1] 
    tdataraw <- tdataraw[PRICE != 0]
    nresult[2] <- dim(tdataraw)[1] 
    tdataraw <- tdataraw[EX %in% exchanges]
    nresult[3] <- dim(tdataraw)[1] 
    tdataraw <- tdataraw[COND %in% c("E", "F")]
    nresult[4] <- dim(tdataraw)[1] 
    tdataraw <- mergeTradesSameTimestamp(tdataraw, selection = selection)
    nresult[5] <- dim(tdataraw)[1] 
    
    if (dummy_was_xts == TRUE) {
      df_result <- xts(as.matrix(tdataraw[, -c("DT")]), order.by = tdataraw$DT)
    } else {
      df_result <- tdataraw
    }
    
    if (report == TRUE) {
      return(list(tdata = df_result, report = nresult))
    } else {
      return(df_result)
    }
  }
  
  # 
  # nresult <- rep(0, 5)
  # if (is.list(exchanges) == FALSE) { 
  #   exchanges <- as.list(exchanges)
  # }
  # 
  # if (is.null(tdataraw) == TRUE) {
  #   dates = timeDate::timeSequence(from, to, format = "%Y-%m-%d")
  #   dates = dates[timeDate::isBizday(dates, holidays = timeDate::holidayNYSE(1960:2050))]
  #   for (j in 1:length(dates)) {
  #     datasourcex = paste(datasource, "/", dates[j], sep = "")
  #     datadestinationx = paste(datadestination, "/", dates[j], sep = "")
  #     for (i in 1:length(ticker)) {
  #       dataname = paste(ticker[i], "_trades.RData", sep = "")
  #       if (file.exists(paste(datasourcex, "/", dataname, sep = ""))) {
  #         load(paste(datasourcex, "/", dataname, sep = ""))
  #         if (class(tdata)[1] != "try-error") {            
  #           exchange = exchanges[[i]]            
  #           if (length(tdata$PRICE) > 0) {
  #             tdata <- checkColumnNames(tdata);
  #             nresult[1] <- nresult[1] + dim(tdata)[1]
  #           } else {
  #             tdata <- NULL
  #           }
  #           
  #           if (length(tdata$PRICE) > 0) {
  #             tdata <- try(nozeroprices(tdata))
  #             nresult[2] <- nresult[2] + dim(tdata)[1]
  #           } else {
  #             tdata <- NULL
  #           }
  #           
  #           if (length(tdata$PRICE) > 0) {
  #             tdata <- try(selectexchange(tdata, exch = exchange))
  #             nresult[3] <- nresult[3] + dim(tdata)[1]
  #           } else {
  #             tdata <- NULL
  #           }
  #           
  #           if (length(tdata$PRICE) > 0) {
  #             tdata <- try(salescond(tdata))
  #             nresult[4] <- nresult[4] + dim(tdata)[1]
  #           } else {
  #             tdata <- NULL
  #           }
  #           
  #           if (length(tdata$PRICE) > 0) {
  #             tdata <- try(mergeTradesSameTimestamp(tdata, selection = selection))
  #             nresult[5] <- nresult[5] + dim(tdata)[1]
  #           } else {
  #             tdata <-NULL
  #           }
  #           save(tdata, file = paste(datadestinationx,"/", dataname, sep = ""))
  #         }
  #         if (class(tdata) == "try-error") {
  #           abc = 1
  #           save(abc, file = paste(datadestinationx, "/missing_", 
  #                                  ticker[i], ".RData", sep = ""))
  #         }
  #         
  #       }else{
  #         next;
  #       }
  #     }   
  #   }
  #   if (report == TRUE) {
  #     names(nresult) = c("initial number", "no zero prices", 
  #                        "select exchange", "sales condition", "merge same timestamp")
  #     return(nresult)
  #   }
  # }
  # 
  # if (is.null(tdataraw) == FALSE) {
  #   if (class(tdataraw)[1] != "try-error") {
  #     if (length(exchanges) > 1) {
  #       print("The argument exchanges contains more than 1 element. Please select a single exchange, in case you provide tdataraw.")
  #     }
  #     exchange <- exchanges[[1]]
  #     tdata <- tdataraw
  #     rm(tdataraw)
  #     
  #     if (length(tdata) > 0) {
  #       tdata <- checkColumnNames(tdata)
  #       nresult[1] <- nresult[1] + dim(tdata)[1]
  #     } else {
  #       tdata <- NULL
  #     }
  #     
  #     if(length(tdata)>0){
  #       tdata = try(nozeroprices(tdata))
  #       nresult[2] = nresult[2] + dim(tdata)[1]
  #     } else {
  #       tdata <- NULL
  #     }
  #     
  #     if (length(tdata) > 0) {
  #       tdata = try(selectexchange(tdata, exch = exchange))
  #       nresult[3] = nresult[3] + dim(tdata)[1]
  #     } else {
  #       tdata <- NULL
  #     }
  #     
  #     if (length(tdata) > 0) {
  #       tdata = try(salescond(tdata))
  #       nresult[4] = nresult[4] + dim(tdata)[1]
  #     } else {
  #       tdata <- NULL
  #     }
  #     
  #     if (length(tdata) > 0) {
  #       tdata <- try(mergeTradesSameTimestamp(tdata, selection = selection))
  #       nresult[5] <- nresult[5] + dim(tdata)[1]
  #     } else {
  #       tdata <- NULL
  #     }
  #     
  #     if (report == TRUE) {
  #       names(nresult) = c("initial number", "no zero prices", 
  #                          "select exchange", "sales condition", "merge same timestamp")
  #       return(list(tdata = tdata, report = nresult))
  #     }
  #     if (report != TRUE) {
  #       return(tdata)
  #     }
  #   }
  # }
}

#' @export
tradesCleanupUsingQuotes <- function(from, to, datasource, datadestination, ticker, tdata = NULL, qdata = NULL) {
  warning("Please use tradesCleanupUsingQuotes instead of tradesCleanupFinal.")
  tradesCleanupFinal(from, to, datasource, datadestination, ticker, tdata = NULL, qdata = NULL)
}

#' Perform a final cleaning procedure on trade data
#' 
#' @description Function performs cleaning procedure \code{\link{rmTradeOutliersUsingQuotes}} 
#' for the trades of all stocks in "ticker" over the interval 
#' [from,to] and saves the result in "datadestination". 
#' Note that preferably the input data for this function 
#' is trade and quote data cleaned by respectively e.g. \code{\link{tradesCleanup}}
#' and \code{\link{quotesCleanup}}.
#' 
#' @param from character indicating first date to clean, e.g. "2008-01-30".
#' @param to character indicating last date to clean, e.g. "2008-01-31".
#' @param datasource character indicating the folder in which the original data is stored.
#' @param datadestination character indicating the folder in which the cleaned data is stored.
#' @param ticker vector of tickers for which the data should be cleaned.
#' @param tdata data.table or xts object containing (ONE day and for ONE stock only) trade data cleaned by \code{\link{tradesCleanup}}. This argument is NULL by default. Enabling it, means the arguments
#' from, to, datasource and datadestination will be ignored. (only advisable for small chunks of data)
#' @param qdata xts object containing (ONE day and for ONE stock only) cleaned quote data. This argument is NULL by default. Enabling it means the arguments
#' from, to, datasource, datadestination will be ignored. (only advisable for small chunks of data)
#' 
#' @return For each day an xts object is saved into the folder of that date, containing the cleaned data.
#' This procedure is performed for each stock in "ticker".
#' 
#' In case you supply the arguments "tdata" and "qdata", the on-disk functionality is ignored
#' and the function returns a list with the cleaned trades as xts object (see examples).
#' 
#' @references Barndorff-Nielsen, O. E., P. R. Hansen, A. Lunde, and N. Shephard (2009). Realized kernels in practice: Trades and quotes. Econometrics Journal 12, C1-C32.
#' 
#' Brownlees, C.T. and Gallo, G.M. (2006). Financial econometric analysis at ultra-high frequency: Data handling concerns. Computational Statistics & Data Analysis, 51, pages 2232-2245.
#' 
#' @author Jonathan Cornelissen and Kris Boudt.
#' 
#' @examples 
#' # Consider you have raw trade data for 1 stock for 2 days 
#' tdata_afterfirstcleaning <- tradesCleanup(tdataraw = sample_tdataraw_microseconds, 
#'                                           exchanges = "N", report = FALSE)
#' # 
#' qdata <- quotesCleanup(qdataraw = sample_qdataraw_microseconds, 
#'                        exchanges = "N", report = FALSE)
#' dim(tdata_afterfirstcleaning)
#' tdata_afterfinalcleaning <- 
#'   tradesCleanupUsingQuotes(qdata = qdata[as.Date(DT) == "2018-01-02"],
#'                            tdata = tdata_afterfirstcleaning[as.Date(DT) == "2018-01-02"])
#' dim(tdata_afterfinalcleaning)
#' #In case you have more data it is advised to use the on-disk functionality
#' #via "from","to","datasource", etc. arguments
#' @keywords cleaning
#' @export
tradesCleanupUsingQuotes <- function(from, to, datasource, datadestination, ticker, tdata = NULL, qdata = NULL) {
  
  if ((!is.null(tdata)) & (!is.null(qdata))) {
    tdata <- checkColumnNames(tdata)
    qdata <- checkColumnNames(qdata)
    
    #1 cleaning procedure that needs cleaned trades and quotes
    tdata <- rmTradeOutliersUsingQuotes(tdata, qdata)
    return(tdata)
  }
}
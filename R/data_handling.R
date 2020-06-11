#' Aggregate a time series
#' 
#' @description Function returns aggregated time series as xts object. 
#' It can handle irregularly spaced timeseries and returns a regularly spaced one.
#' Use univariate timeseries as input for this function, and check out \code{\link{aggregateTrades}}
#' and \code{\link{aggregateQuotes}} to aggregate Trade or Quote data objects.
#' 
#' @param ts xts object to aggregate.
#' @param FUN function to apply over each interval. By default, previous tick aggregation is done. 
#' Alternatively one can set e.g. FUN = "mean".
#' In case weights are supplied, this argument is ignored and a weighted average is taken.
#' @param on character, indicating the time scale in which "k" is expressed. Possible values are: "secs", "seconds", "mins", "minutes", "hours", "days", "weeks".
#' @param k positive integer, indicating the number of periods to aggregate over. For example, to aggregate an 
#' xts object to the five-minute frequency set k = 5 and on = "minutes".
#' @param weights By default, no weighting scheme is used. 
#' When you assign an xts object with wheights to this argument, a weighted mean is taken over each interval. 
#' Of course, the weights should have the same timestamps as the supplied time series.
#' @param dropna boolean, which determines whether empty intervals should be dropped.
#' By default, an NA is returned in case an interval is empty, except when the user opts
#' for previous tick aggregation, by setting FUN = "previoustick" (default).
#' 
#' @details The timestamps of the new time series are the closing times and/or days of the intervals. 
#' E.g. for a weekly aggregation the new timestamp is the last day in that particular week (namely sunday).
#' 
#' In case of previous tick aggregation, 
#' for on = "seconds"/"minutes"/"hours",
#' the element of the returned series with e.g. timestamp 09:35:00 contains 
#' the last observation up to that point, excluding the value at 09:35:00 itself.
#' 
#' Please note: In case an interval is empty, by default an NA is returned.. In case e.g. previous 
#' tick aggregation it makes sense to fill these NA's by the function \code{na.locf}
#' (last observation carried forward) from the zoo package.
#' 
#' @return An xts object containing the aggregated time series.
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' @keywords data manipulation
#' 
#' @examples 
#' #load sample price data
#' ts <- sampletData$PRICE
#' 
#' #Previous tick aggregation to the 5-minute sampling frequency:
#' tsagg5min <- aggregateTS(ts, on = "minutes", k = 5)
#' head(tsagg5min)
#' #Previous tick aggregation to the 30-second sampling frequency:
#' tsagg30sec <- aggregateTS(ts, on = "seconds", k = 30)
#' tail(tsagg30sec)
#' 
#' @importFrom zoo zoo na.locf
#' @importFrom stats start end
#' @importFrom xts period.apply	
#' @export
aggregateTS <- function (ts, FUN = "previoustick", on = "minutes", k = 1, weights = NULL, dropna = FALSE) {
  
  makethispartbetter <- ((!is.null(weights))| on=="days"| on=="weeks" | (FUN!="previoustick") | dropna)
  
  if (FUN == "previoustick") {
    FUN <- previoustick 
  } else {
    FUN <- match.fun(FUN)
  }
  
  if (makethispartbetter == TRUE)  {
    
    if (is.null(weights) == TRUE) {
      ep <- endpoints(ts, on, k)
      if (dim(ts)[2] == 1) { 
        ts2 <- period.apply(ts, ep, FUN) 
      }
      if (dim(ts)[2] > 1) {  
        ts2 <- xts(apply(ts, 2, FUN = periodApply2, FUN2 = FUN, INDEX = ep), order.by = index(ts)[ep],)
      }
    } else {
      tsb <- cbind(ts, weights)
      ep  <- endpoints(tsb, on, k)
      ts2 <- period.apply(tsb, ep, FUN = match.fun(weightedaverage))
    }
    if (on == "minutes" | on == "mins" | on == "secs" | on == 
        "seconds") {
      if (on == "minutes" | on == "mins") {
        secs = k * 60
      }
      if (on == "secs" | on == "seconds") {
        secs <- k
      }
      a <- index(ts2) + (secs - as.numeric(index(ts2)) %% secs)
      ts3 <- xts(ts2, a, tzone = "GMT")
    }
    if (on == "hours") {
      secs = 3600
      a <- index(ts2) + (secs - as.numeric(index(ts2)) %% secs)
      ts3 <- xts(ts2, a, tzone = "GMT")
    }
    if (on == "days") {
      secs = 24 * 3600
      a   <- index(ts2) + (secs - as.numeric(index(ts2)) %% secs) - (24 * 3600)
      ts3 <- xts(ts2, a, tzone = "GMT")
    }
    if (on == "weeks") {
      secs = 24 * 3600 * 7
      a <- (index(ts2) + (secs - (index(ts2) + (3L * 86400L)) %% secs)) - 
        (24 * 3600)
      ts3 <- xts(ts2, a, tzone = "GMT")
    }
    
    if (dropna == FALSE) {
      if (on != "weeks" & on != "days") {
        if (on == "secs" | on == "seconds") {
          tby <- "s"
        }
        if (on == "mins" | on == "minutes") {
          tby <- "min"
        }
        if (on == "hours") {
          tby <- "h"
        }
        by <- paste(k, tby, sep = " ")
        allindex <- as.POSIXct(seq(start(ts3), end(ts3), by = by))
        xx <- xts(rep("1", length(allindex)), order.by = allindex)
        ts3 <- merge(ts3, xx)[, (1:dim(ts)[2])]
      }
    }
    
    ts3 <- xts(ts3, as.POSIXct(index(ts3)))
    return(ts3)
  }
  
  if(!makethispartbetter){
    if (on == "secs" | on == "seconds") { 
      secs <- k 
      tby <- paste(k, "sec", sep = " ")
    }
    if (on == "mins" | on == "minutes") { 
      secs <- 60*k; tby = paste(60*k,"sec",sep=" ")
    }
    if (on == "hours") { 
      secs <- 3600 * k 
      tby <- paste(3600 * k, "sec", sep=" ")
    }
    
    FUN <- match.fun(FUN)
    
    g <- base::seq(start(ts), end(ts), by = tby)
    rawg <- as.numeric(as.POSIXct(g, tz = "GMT"))
    newg <- rawg + (secs - rawg %% secs)
    
    if(as.numeric(end(ts)) == newg[length(newg)-1]){
      newg  <- newg[-length(newg)]
    }

    g    <- as.POSIXct(newg, origin = "1970-01-01", tz = "GMT")
    ts3  <- na.locf(merge(ts, zoo(, g)))[as.POSIXct(g, tz = "GMT")]
    ts3 <- ts3[!duplicated(index(ts3), fromLast = TRUE)]
    
    return(ts3) 
  }
}

#' Aggregate a time series but keep first and last observation
#' @description Function returns new time series as xts object where first observation is always the opening price
#' and subsequent observations are the closing prices over the interval with as endpoint the timestamp 
#' of the result.
#' 
#' @param pData data.table or xts object to be aggregated containing the intraday price series, possibly across multiple days.
#' @param on character, indicating the time scale in which "k" is expressed. Possible values are: "milliseconds", "secs", "seconds", "mins", "minutes","hours".
#' @param k positive integer, indicating the number of periods to aggregate over; e.g. to aggregate a 
#' xts object to the 5 minute frequency set k = 5 and on = "minutes".
#' @param marketOpen the market opening time, by default: marketOpen = "09:30:00". 
#' @param marketClose the market closing time, by default: marketClose = "16:00:00". 
#' @param fill indicates whether rows without trades should be added with the most recent value, FALSE by default.
#' @param tz time zone used, by default: tz = timezone of DT column/index of xts.
#' 
#' @details 
#' The timestamps of the new time series are the closing times and/or days of the intervals. 
#' 
#' In case of previous tick aggregation or on = "seconds"/"minutes"/"hours",
#' the element of the returned series with e.g. timestamp 09:35:00 contains 
#' the last observation up to that point, including the value at 09:35:00 itself.
#' 
#' @return A data.table or xts object containing the aggregated time series.
#' 
#' @author Jonathan Cornelissen, Kris Boudt and Onno Kleen.
#' @keywords data manipulation
#' @examples 
#' # aggregate price data to the 30 second frequency
#' aggregatePrice(sampletDataMicroseconds, on = "secs", k = 30)
#' # aggregate price data to the 30 second frequency including zero return price changes
#' aggregatePrice(sampletDataMicroseconds, on = "secs", k = 30)
#' 
#' # aggregate price data to half a second frequency including zero return price changes
#' aggregatePrice(sampletDataMicroseconds, on = "milliseconds", k = 500, fill = TRUE)
#' @keywords internal
#' @importFrom xts last
#' @export
aggregatePrice <- function(pData, on = "minutes", k = 1, marketOpen = "09:30:00", marketClose = "16:00:00" , fill = FALSE, tz = NULL) {
  
  DATE = DT = FIRST_DT = DT_ROUND = LAST_DT = SYMBOL = PRICE = NULL
  
  on_true <- NULL

  if ("PRICE" %in% colnames(pData) == FALSE) {
    stop("data.table or xts needs column named PRICE.")
  }
  
  if (on == "milliseconds") {
    on_true <- "milliseconds"
    on <- "secs"
    k <- k / 1000
  }
  
  dummy_was_xts <- FALSE
  if (is.data.table(pData) == FALSE) {
    if (is.xts(pData) == TRUE) {
      
      dummy_was_xts <- TRUE
      # If there is only one day of input and input is xts,
      # use old code because it's faster
      # However, multi-day input not possible
      if (length(unique(as.Date(index(pData)))) == 1) {
        if (is.null(tz) == TRUE) {
          tz <-tz(pData)
        }
        ts2 <- fastTickAgregation(pData, on, k, tz)
        date <- strsplit(as.character(index(pData)), " ")[[1]][1]

        #open
        a <- as.POSIXct(paste(date, marketOpen), tz = tz)
        b <- as.xts(matrix(as.numeric(pData[1]), nrow = 1), a)
        storage.mode(ts2) <- "numeric"
        ts3 <- c(b, ts2)

        #close
        aa <- as.POSIXct(paste(date, marketClose), tz = tz)
        condition <- index(ts3) < aa
        ts3 <- ts3[condition]
        bb <- as.xts(matrix(as.numeric(last(pData)), nrow = 1), aa)
        ts3 <- c(ts3, bb)
        return(ts3)
      }
      pData <- setnames(as.data.table(pData)[, PRICE := as.numeric(as.character(PRICE))],
                        old = "index", new = "DT")
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(pData)) == FALSE) {
      stop("Data.table neeeds DT column (date-time ).")
    }
    if (("PRICE" %in% colnames(pData)) == FALSE) {
      stop("Data.table neeeds PRICE column (date-time).")
    }
  }
  
  pData <- pData[DT >= ymd_hms(paste(as.Date(pData$DT, tz = tz(pData$DT)), marketOpen), tz = tz(pData$DT))]
  pData <- pData[DT <= ymd_hms(paste(as.Date(pData$DT, tz = tz(pData$DT)), marketClose), tz = tz(pData$DT))]

  pData[, DATE := as.Date(DT, tz = tz(pData))]
  pData[, FIRST_DT := min(DT), by = "DATE"]
  pData[, DT_ROUND := ifelse(DT == FIRST_DT,
                             floor_date(ymd_hms(DT), unit = paste(k, on)),
                             ceiling_date(ymd_hms(DT), unit = paste(k, on), change_on_boundary = FALSE))]
  pData[, DT_ROUND := as_datetime(DT_ROUND)]
  pData[, LAST_DT := max(DT), by = "DT_ROUND"]
  
  pData_open <- data.table::copy(pData[DT == FIRST_DT])
  pData_open[, DT := ymd_hms(paste(as.Date(pData_open$DT), marketOpen), tz = tz(pData_open$DT))]
  pData_open <- pData_open[, c("DT", "PRICE")]

  pData <- pData[DT == LAST_DT][, DT := DT_ROUND][, c("DT", "PRICE")]
  lubridate::tz(pData$DT) <- tz(pData_open$DT)
  # due to rounding there may be an observation that is refered to the opening time
  pData <- pData[!(DT %in% pData_open$DT)]
  
  pData <- merge(pData, pData_open, all = TRUE)
  
  if (fill == TRUE) {
    if (on == "minutes") {
      on = "mins"
    }
    if (on == "seconds") {
      on <- "secs"
    }
    
    # if/else seems unnecessary but otherwise seq.POSIXt won't work for milliseconds
    if (is.null(on_true) == FALSE) {
      dt_full_index <-
        rbindlist(lapply(unique(as.Date(pData$DT)),
                         FUN = function(x) data.frame(DT = seq.POSIXt(from = as.POSIXct(paste0(x, marketOpen, tz = tz(pData$DT))), 
                                                                      to   = as.POSIXct(paste0(x, marketClose, tz = tz(pData$DT))), 
                                                                      units = on,
                                                                      by = k))))
    } else {
      dt_full_index <-
        rbindlist(lapply(unique(as.Date(pData$DT)),
                         FUN = function(x) data.frame(DT = seq.POSIXt(from = as.POSIXct(paste0(x, marketOpen, tz = tz(pData$DT))), 
                                                                      to   = as.POSIXct(paste0(x, marketClose, tz = tz(pData$DT))),
                                                                      by = paste(k, on)))))
    }
    
    lubridate::tz(dt_full_index$DT) <- tz(pData$DT)
    pData <- merge(pData, dt_full_index, by = "DT", all = TRUE)
    
    setkeyv(pData, "DT")
    
    pData$PRICE <- na.locf(pData$PRICE)
  }

  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(pData[, -c("DT")]), order.by = pData$DT, tzone = tz(pData$DT)))
  } else {
    return(pData)
  }
}

#' Aggregate a data.table or xts object containing quote data
#' 
#' @description Function returns a data.table or xts object containing the aggregated quote data with columns "SYMBOL", "EX", "BID","BIDSIZ","OFR","OFRSIZ". 
#' See \code{\link{sampleqData}} for an example of the argument qData.
#' 
#' @param qData data.table or xts object to be aggregated, containing the intraday quote data of a stock for one day.
#' @param on character, indicating the time scale in which "k" is expressed. Possible values are: "secs", "seconds", "mins", "minutes","hours".
#' xts object to the 5 minute frequency, set k=5 and on = "minutes".
#' @param k positive integer, indicating the number of periods to aggregate over. E.g. to aggregate an
#' object to the 5 minute frequency set k = 5 and on = "minutes".
#' @param marketOpen the market opening time, by default: marketOpen = "09:30:00".
#' @param marketClose the market closing time, by default: marketClose = "16:00:00".
#' @param tz time zone used, by default: tz = "GMT".
#' 
#' @return a data.table or xts object containing the aggregated time series.
#' 
#' @details The output "BID" and "OFR" columns are constructed using previous tick aggregation.
#' 
#' The variables "BIDSIZ" and "OFRSIZ" are aggregated by taking the sum of the respective inputs over each interval.
#' 
#' The timestamps of the new time series are the closing times of the intervals. 
#' 
#' Please note: Returned objects always contain the first observation (i.e. opening quotes,...).
#' 
#' @return A data.table or an xts object containing the aggregated quote data.
#' 
#' @author Jonathan Cornelissen, Kris Boudt and Onno Kleen
#' @keywords data manipulation
#' 
#' @examples
#' # aggregate quote data to the 30 second frequency
#' qData_aggregated <- aggregateQuotes(sampleqData, on = "seconds", k = 30)
#' head(qData_aggregated)
#' @export
aggregateQuotes <- function(qData, on = "minutes", k = 5, marketOpen = "09:30:00", marketClose = "16:00:00", tz = "GMT") {
  DATE = BID = OFR = BIDSIZ = OFRSIZ = DT = FIRST_DT = DT_ROUND = LAST_DT = SYMBOL = NULL
  
  qData <- checkColumnNames(qData)
  checkqData(qData)
  
  dummy_was_xts <- FALSE
  if (is.data.table(qData) == FALSE) {
    if (is.xts(qData) == TRUE) {
      qData <- setnames(as.data.table(qData)[, BID := as.numeric(as.character(BID))][
        , OFR := as.numeric(as.character(OFR))][
          , BIDSIZ := as.numeric(as.character(BIDSIZ))][
            , OFRSIZ := as.numeric(as.character(OFRSIZ))], old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(qData)) == FALSE) {
      stop("Data.table neeeds DT column.")
    }
  }
  
  if ("SYMBOL" %in% colnames(qData)) {
    if (length(unique(qData$SYMBOL)) > 1) {
      stop("Please provide only one symbol at a time.")
    }
  }
  
  qData[, DATE := as.Date(DT)]
  qData[, FIRST_DT := min(DT), by = "DATE"]
  qData[, DT_ROUND := ifelse(DT == FIRST_DT,
                             floor_date(ymd_hms(DT), unit = paste(k, on)),
                             ceiling_date(ymd_hms(DT), unit = paste(k, on), change_on_boundary = FALSE))]
  qData[, DT_ROUND := as_datetime(DT_ROUND)]
  qData[, LAST_DT := max(DT), by = "DT_ROUND"]
  qData[, OFRSIZ := sum(OFRSIZ), by = "DT_ROUND"]
  qData[, BIDSIZ := sum(BIDSIZ), by = "DT_ROUND"]
  
  qData <- qData[DT == LAST_DT][, DT := DT_ROUND][, c("DT", "SYMBOL", "BID", "BIDSIZ", "OFR", "OFRSIZ")]
  
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(qData[, -c("DT")]), order.by = qData$DT, tzone = tz))
  } else {
    return(qData)
  }
}

#' Aggregate a data.table or xts object containing trades data
#' 
#' @description Function returns new time series as a data.table or xts object where first observation is always the opening price
#' and subsequent observations are the closing prices over the interval.
#' 
#' @param tData data.table or xts object to be aggregated, containing the intraday price series of a stock for possibly multiple days.
#' @param on character, indicating the time scale in which "k" is expressed. Possible values are: "secs", "seconds", "mins", "minutes", "hours".
#' @param k positive integer, indicating the number of periods to aggregate over. E.g. to aggregate an
#' object to the 5 minute frequency set k = 5 and on = "minutes".
#' @param marketOpen the market opening time, by default: marketOpen = "09:30:00".
#' @param marketClose the market closing time, by default: marketClose = "16:00:00".
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
#' @return A data.table or xts object containing the aggregated time series.
#' 
#' @author Jonathan Cornelissen, Kris Boudt and Onno Kleen.
#' @keywords data manipulation
#' 
#' @examples 
#' # aggregate trade data to 5 minute frequency
#' tData_aggregated <- aggregateTrades(sampletData, on = "minutes", k = 5)
#' head(tData_aggregated)
#' @importFrom lubridate floor_date
#' @importFrom lubridate ceiling_date
#' @importFrom lubridate ymd_hms
#' @importFrom lubridate as_datetime
#' @export
aggregateTrades <- function(tData, on = "minutes", k = 5, marketOpen = "09:30:00", marketClose = "16:00:00", tz = "GMT") {
  DATE = SIZE = DT = FIRST_DT = DT_ROUND = LAST_DT = SYMBOL = PRICE = VWPRICE = SIZETPRICE = SIZESUM = NULL
  tData <- checkColumnNames(tData)
  checktData(tData)
  
  dummy_was_xts <- FALSE
  if (is.data.table(tData) == FALSE) {
    if (is.xts(tData) == TRUE) {
      tData <- setnames(as.data.table(tData)[, SIZE := as.numeric(as.character(SIZE))][, PRICE := as.numeric(as.character(PRICE))], 
                        old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(tData)) == FALSE) {
      stop("Data.table neeeds DT column (date-time ).")
    }
  }
  
  tData[, DATE := as.Date(DT)]
  tData[, FIRST_DT := min(DT), by = "DATE"]
  tData[, DT_ROUND := ifelse(DT == FIRST_DT,
                             floor_date(ymd_hms(DT), unit = paste(k, on)),
                             ceiling_date(ymd_hms(DT), unit = paste(k, on), change_on_boundary = FALSE))]
  tData[, DT_ROUND := as_datetime(DT_ROUND)]
  tData[, LAST_DT := max(DT), by = "DT_ROUND"]
  tData[, SIZETPRICE := SIZE * PRICE]
  tData[, SIZESUM := sum(SIZE), by = "DT_ROUND"]
  tData[, VWPRICE := sum(SIZETPRICE/SIZESUM), by = "DT_ROUND"]
  tData[, SIZE := SIZESUM]
  
  tData <- tData[DT == LAST_DT][, DT := DT_ROUND][, c("DT", "SYMBOL", "PRICE", "SIZE", "VWPRICE")]
  
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(tData[, -c("DT")]), order.by = tData$DT, tzone = tz))
  } else {
    return(tData)
  }
}

#' Retain only data from the stock exchange with the highest trading volume
#' 
#' @description Function returns a data.table or xts object containing only observations of the 
#' exchange with the highest value for the variable "SIZE", 
#' i.e. the highest trade volume.
#' @param tData an xts object with at least a column "EX", 
#' indicating the exchange symbol and "SIZE", 
#' indicating the trade volume. The chosen exchange is printed on the console.
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
#' @return data.table or xts object depending on input
#' 
#' @examples autoSelectExchangeTrades(sampletDataRawMicroseconds)
#' 
#' @author Jonathan Cornelissen, Kris Boudt and Onno Kleen
#' 
#' @keywords cleaning
#' @export
autoSelectExchangeTrades <- function(tData) {
  DATE = SIZE = DT = SIZESUM = NULL
  
  exchanges <- c("Q", "A", "P", "B", "C", "N", "D", "X", "I", "M", "W", "Z")
  exchangenames <- c("NASDAQ", "AMEX", "ARCA", "Boston", "NSX", "NYSE", "NASD ADF and TRF", "Philadelphia", "ISE", "Chicago", "CBOE", "BATS")
  
  tData <- checkColumnNames(tData)
  checktData(tData)
  
  dummy_was_xts <- FALSE
  if (is.data.table(tData) == FALSE) {
    if (is.xts(tData) == TRUE) {
      tData <- setnames(as.data.table(tData)[, SIZE := as.numeric(as.character(SIZE))], 
                        old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(tData)) == FALSE) {
      stop("Data.table neeeds DT column (date-time ).")
    }
  } 
  
  
  tData[, SIZESUM := sum(SIZE), by = "EX"]
  tData <- tData[SIZESUM == max(SIZESUM)][, -c("SIZESUM")]
  
  exch <- unique(tData$EX)
  
  namechosen <- exchangenames[exch == exchanges]  
  print(paste("The ", namechosen, "is the exchange with the highest volume."))
  
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(tData[, -c("DT")]), order.by = tData$DT, tzone = tz(tData$DT)))
  } else {
    return(tData)
  }
}


#' Retain only data from the stock exchange with the highest volume
#' 
#' @description Function returns an xts object containing only observations 
#' of the exchange with highest
#' value for the sum of "BIDSIZ" and "OFRSIZ", i.e. the highest quote volume.
#' 
#' @param qData a data.table or xts object with at least a column "EX", indicating the exchange symbol 
#' and columns "BIDSIZ" and "OFRSIZ", indicating 
#' the volume available at the bid and ask respectively.
#' The chosen exchange is printed on the console.
#' The possible exchanges are:
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
#' @return data.table or xts object depending on input
#' 
#' @examples 
#' autoSelectExchangeQuotes(sampleqDataRawMicroseconds)
#'
#' @author Jonathan Cornelissen, Kris Boudt and Onno Kleen
#' 
#' @keywords cleaning
#' @export
autoSelectExchangeQuotes <- function(qData) {
  
  BIDSIZ = OFRSIZ = DT = EX = SUMVOL = NULL
  
  exchanges = c("Q","A","P","B","C","N","D","X","I","M","W","Z");
  exchangenames = c("NASDAQ","AMEX","ARCA","Boston","NSX","NYSE","NASD ADF and TRF","Philadelphia","ISE","Chicago","CBOE","BATS");
  
  qData <- checkColumnNames(qData)
  checkqData(qData)
  
  dummy_was_xts <- FALSE
  if (is.data.table(qData) == FALSE) {
    if (is.xts(qData) == TRUE) {
      qData <- setnames(as.data.table(qData)[, BIDSIZ := as.numeric(as.character(BIDSIZ))][
            , OFRSIZ := as.numeric(as.character(OFRSIZ))], old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(qData)) == FALSE) {
      stop("Data.table neeeds DT column.")
    }
  }
  
  if (length(unique(qData$SYMBOL)) > 1) {
    stop("Please provide only one symbol at a time.")
  }
  
  qData[, SUMVOL := sum(BIDSIZ + OFRSIZ), by = "EX"]
  qData <- qData[SUMVOL == max(SUMVOL)][, -c("SUMVOL")]
  
  exch <- unique(qData$EX)
  
  namechosen <- exchangenames[exch == exchanges]  
  print(paste("The ", namechosen, "is the exchange with the highest volume."))
  
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(qData[, -c("DT")]), order.by = qData$DT, tzone = tz(qData$DT)))
  } else {
    return(qData)
  }
}


#' Extract data from an xts object for the Exchange Hours Only
#' 
#' @description The function returns data within exchange trading hours
#' "dayBegin" and "dayEnd". By default, dayBegin and dayEnd
#' are set to "09:30:00" and "16:00:00" respectively (see Brownlees and Gallo (2006) for more information on good choices for these arguments).
#' 
#' @param data a data.table or xts object containing the time series data. 
#' Multiple days of input are allowed.
#' @param dayBegin character in the format of \"HH:MM:SS\",
#' specifying the starting hour, minute and second of an exhange
#' trading day.
#' @param dayEnd character in the format of \"HH:MM:SS\^",
#' specifying the closing hour, minute and second of an exchange
#' trading day.
#' 
#' @return xts or data.table object depending on input
#'
#' @references Brownlees, C.T. and Gallo, G.M. (2006). Financial econometric analysis at ultra-high frequency: Data handling concerns. Computational Statistics & Data Analysis, 51, pages 2232-2245.
#' @author Jonathan Cornelissen, Kris Boudt and Onno Kleen.
#' @examples 
#' exchangeHoursOnly(sampletDataRawMicroseconds)
#' @keywords cleaning
#' @importFrom lubridate tz
#' @importFrom lubridate ymd_hms
#' @export
exchangeHoursOnly <- function(data, dayBegin = "09:30:00", dayEnd = "16:00:00") {
  DT = NULL # needed for data table (otherwise notes pop up in check())
  data <- checkColumnNames(data)
  
  dummy_was_xts <- FALSE
  if (is.data.table(data) == FALSE) {
    if (is.xts(data) == TRUE) {
      data <- setnames(as.data.table(data), old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(data)) == FALSE) {
      stop("Data.table neeeds DT column.")
    }
  }
  
  data <- data[DT >= ymd_hms(paste(as.Date(data$DT), dayBegin), tz = tz(data$DT))]
  data <- data[DT <= ymd_hms(paste(as.Date(data$DT), dayEnd), tz = tz(data$DT))]
  
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(data[, -c("DT")]), order.by = data$DT, tzone = tz(data$DT)))
  } else {
    return(data)
  }
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
#' @param tData data.table or xts-object containing the trade data (multiple days possible).
#' @param qData data.table or xts-object containing the quote data (multiple days possible).
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
#' tqData <- matchTradesQuotes(sampletData, sampleqData)
#' head(tqData)
#' # multi-day input allowed
#' tqData <- matchTradesQuotes(sampletDataMicroseconds, sampleqDataMicroseconds)
#' @importFrom lubridate seconds
#' @export
matchTradesQuotes <- function(tData, qData, adjustment = 2) {
  
  PRICE = BID = OFR = PFR = DATE = DT = FIRST_DT = DT_ROUND = SYMBOL = NULL
  
  tData <- checkColumnNames(tData)
  qData <- checkColumnNames(qData)
  checkqData(qData)
  checktData(tData)
  
  if (any(class(tData) != class(qData))) {
    stop("tData and qData should be of the same data type, either xts or data.table.")
  }
  
  dummy_was_xts <- FALSE
  if (is.data.table(tData) == FALSE) {
    if (is.xts(tData) == TRUE) {
      tData <- setnames(as.data.table(tData)[, PRICE := as.numeric(as.character(PRICE))], 
                        old = "index", new = "DT")
      qData <- setnames(as.data.table(qData)[, BID := as.numeric(as.character(BID))][, OFR := as.numeric(as.character(OFR))], 
                        old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(tData)) == FALSE) {
      stop("tData neeeds DT column.")
    }
  }
  
  qData[, DATE := as.Date(DT)]
  qData[, FIRST_DT := min(DT), by = "DATE"]
  qData[, DT_ROUND := ifelse(DT == FIRST_DT, ymd_hms(DT), ymd_hms(DT) + seconds(2))]
  
  qData <- qData[, DT := ifelse(DT != min(DT), DT + seconds(2), DT)] 
  qData <- qData[, DT := as_datetime(DT_ROUND)][, -c("FIRST_DT", "DT_ROUND", "DATE")]
  
  setkey(tData, SYMBOL, DT)
  setkey(qData, SYMBOL, DT)
  
  tData <- tData[, c("DT", "SYMBOL", "PRICE", "SIZE")]
  tqData <- qData[tData, roll = TRUE, on = c("SYMBOL", "DT")]
  
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(tqData[, -c("DT")]), order.by = tqData$DT))
  } else {
    return(tqData)
  }
}

#' Merge multiple quote entries with the same time stamp
#' 
#' @description Function replaces multiple quote entries that have the same time stamp 
#' by a single one and returns an xts object with unique time stamps only.
#' 
#' @param qData an xts object or data.table containing the time series data, with 
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
mergeQuotesSameTimestamp <- function(qData, selection = "median") {
  BID = OFR = DT = SYMBOL = .SD = BIDSIZ = OFRSIZ = MAXBID = MAXOFR = NULL 
  qData <- checkColumnNames(qData)
  checkqData(qData)
  
  condition <- selection == "median" | selection == "max.volume" | selection == "weighted.average"
  if (condition == FALSE) {
    stop(paste("Selection has to be \"median\", \"max.volume\" or \"weighted.average\" "))
  }
  
  dummy_was_xts <- FALSE
  if (is.data.table(qData) == FALSE) {
    if (is.xts(qData) == TRUE) {
      qData <- setnames(as.data.table(qData)[, BID := as.numeric(as.character(BID))][, OFR := as.numeric(as.character(OFR))]
                        [, BIDSIZ := as.numeric(as.character(BIDSIZ))][, OFRSIZ := as.numeric(as.character(OFRSIZ))], 
                        old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(qData)) == FALSE) {
      stop("Data.table neeeds DT column (date-time).")
    }
  }
  # qData <- sampleqDataRawMicroseconds
  # qData <- checkColumnNames(qData)
  
  # keep summed size columns
  qData_size <- qData[, lapply(.SD, sum), by = list(DT, SYMBOL), .SDcols = c("BIDSIZ", "OFRSIZ")]
  if (selection == "median") {
    qData <- qData[,  lapply(.SD, median), by = list(DT, SYMBOL), .SDcols = c("BID", "OFR")]
  }
  
  if (selection == "max.volume") {
    qData <- qData[, MAXBID := max(BIDSIZ), by = list(DT, SYMBOL)][, MAXOFR := max(OFRSIZ), by = "DT"][
      , BIDSIZ := ifelse(BIDSIZ == MAXBID, 1, 0)][
      , OFRSIZ := ifelse(OFRSIZ == MAXOFR, 1, 0)][
      , BID := BID * BIDSIZ][
      , OFR := OFR * OFRSIZ][
      , BID := max(BID), by = "DT"][, OFR := max(OFR), by = list(DT, SYMBOL)][, -c("MAXBID", "MAXOFR", "BIDSIZ", "OFRSIZ")][
      , lapply(.SD, unique), by = list(DT, SYMBOL), .SDcols = c("BID", "OFR")]
  }
  if (selection == "weighted.average") {
    qData <- qData[, `:=` (BIDSIZ = BIDSIZ / sum(BIDSIZ), OFRSIZ = OFRSIZ / sum(OFRSIZ)), by = list(DT, SYMBOL)][
      , `:=` (BID = sum(BID * BIDSIZ), OFR = sum(OFR * OFRSIZ)), by = list(DT, SYMBOL)][
        , -c("BIDSIZ", "OFRSIZ")][
        , lapply(.SD, unique), by = list(DT, SYMBOL), .SDcols = c("BID", "OFR")]
  }
  qData <- merge(qData, qData_size)
  
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(qData[, -c("DT")]), order.by = qData$DT))
  } else {
    return(qData)
  }
}
# microbenchmark::microbenchmark(quotesCleanup(qDataraw = sampleqDataRaw, exchanges = "N", selection = "max.volume"), times = 10, unit = "s")
# microbenchmark::microbenchmark(quotesCleanup(qDataraw = sampleqDataRaw, exchanges = "N", selection = "maxvolume"), times = 10, unit = "s")

#' Merge multiple transactions with the same time stamp
#' 
#' @description Function replaces multiple transactions that have the same time stamp by a single one and returns an xts or data.table object with unique time stamps only.
#' 
#' @param tData an xts object containing the time series data, with 
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
#' @return data.table or xts object depending on input
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' @keywords cleaning
#' @export
mergeTradesSameTimestamp <- function(tData, selection = "median") {
  SIZE = MAXSIZE = PRICE = DT = SYMBOL = .SD = SIZE_WEIGHT = NULL
  
  tData <- checkColumnNames(tData)
  checktData(tData)
  if (any(colnames(tData) == "SIZE") == FALSE) {
    stop("The argument tData should have a column SIZE indicating the number of shares traded. Could not find that column.")
  }
  
  condition <- selection == "median" | selection == "max.volume" | selection == "weighted.average"
  if (condition == FALSE) {
    stop(paste("Selection has to be \"median\", \"max.volume\" or \"weighted.average\" "))
  }
  
  dummy_was_xts <- FALSE
  if (is.data.table(tData) == FALSE) {
    if (is.xts(tData) == TRUE) {
      tData <- setnames(as.data.table(tData)[, SIZE := as.numeric(as.character(SIZE))][, PRICE := as.numeric(as.character(PRICE))], 
                        old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(tData)) == FALSE) {
      tData <- tData[, SIZE := as.numeric(as.character(SIZE))][, PRICE := as.numeric(as.character(PRICE))]
      stop("Data.table neeeds DT column (date-time ).")
    }
  }
  
  if (selection == "median") {
    tData[, PRICE := median(PRICE), by = list(DT, SYMBOL)]
    #If there is more than one observation at median price, take the average volume.
    tData[, SIZE := as.numeric(SIZE)]
    tData[, SIZE := mean(SIZE), by = list(DT, SYMBOL)] 
    tData <- unique(tData[, c("DT", "SYMBOL", "PRICE", "SIZE")])
  }
  
  if (selection == "max.volume") {
    tData[, SIZE := as.numeric(SIZE)]
    tData <- tData[, MAXSIZE := max(SIZE), by = list(DT, SYMBOL)]
    tData[, SIZE := ifelse(SIZE == MAXSIZE, 1, 0)]
    tData[, PRICE := PRICE * SIZE]
    tData[, PRICE := max(PRICE), by = "DT"]
    tData[, SIZE := MAXSIZE]
    tData[, -c("MAXSIZE")]
    tData <- unique(tData[, c("DT", "SYMBOL", "PRICE", "SIZE")])
  }
  if (selection == "weighted.average") {
    tData[, SIZE := as.numeric(SIZE)]
    tData <- tData[, `:=` (SIZE_WEIGHT = SIZE / sum(SIZE)), by = list(DT, SYMBOL)]
    tData[, `:=` (PRICE = sum(PRICE * SIZE_WEIGHT)), by = list(DT, SYMBOL)]
    tData[, SIZE := mean(SIZE), by = list(DT, SYMBOL)]
    tData <- unique(tData[, c("DT", "SYMBOL", "PRICE", "SIZE")])
  }
  
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(tData[, -c("DT")]), order.by = tData$DT))
  } else {
    return(tData)
  }
}


#' Delete the observations where the price is zero
#' 
#' @description Function deletes the observations where the price is zero.
#' 
#' @param tData an xts or data.table object at least containing a column "PRICE". 
#' 
#' @return an xts or data.table object depending on input
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' @keywords cleaning
noZeroPrices <- function(tData) {
  PRICE <- NULL
  tData <- checkColumnNames(tData)
  checktData(tData)
  
  dummy_was_xts <- FALSE
  if (is.data.table(tData) == FALSE) {
    if (is.xts(tData) == TRUE) {
      tData <- setnames(as.data.table(tData)[, PRICE := as.numeric(as.character(PRICE))], old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(tData)) == FALSE) {
      stop("Data.table neeeds DT column.")
    }
  }
  
  tData <- tData[PRICE != 0]
  
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(tData[, -c("DT")]), order.by = tData$DT))
  } else {
    return(tData)
  }
}

#' Delete the observations where the bid or ask is zero
#' @description Function deletes the observations where the bid or ask is zero.
#' 
#' @param qData an xts or data.table object at least containing the columns "BID" and "OFR".
#' 
#' @return xts object or data.table depending on type of input
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' @keywords cleaning
#' @export
noZeroQuotes <- function(qData) {
  BID = OFR = DT = NULL
  qData <- checkColumnNames(qData)
  checkqData(qData)
  
  dummy_was_xts <- FALSE
  if (is.data.table(qData) == FALSE) {
    if (is.xts(qData) == TRUE) {
      qData <- setnames(as.data.table(qData)[, BID := as.numeric(as.character(BID))][, OFR := as.numeric(as.character(OFR))], old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(qData)) == FALSE) {
      stop("Data.table neeeds DT column.")
    }
  }
  
  qData <- qData[BID != 0 & OFR != 0]
  
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(qData[, -c("DT")]), order.by = qData$DT))
  } else {
    return(qData)
  }
}


#' Cleans quote data
#' 
#' @description This is a wrapper function for cleaning the quote data in the entire folder dataSource. 
#' The result is saved in the folder dataDestination. 
#' 
#' In case you supply the argument "qDataraw", the on-disk functionality is ignored
#' and the function returns the cleaned quotes as xts or data.table object (see examples).
#' 
#' The following cleaning steps are performed sequentially:
#' \code{\link{noZeroQuotes}}, \code{\link{selectExchange}}, rmLargeSpread,
#' \code{\link{mergeQuotesSameTimestamp}}, \code{\link{rmOutliersQuotes}}.
#' @param dataSource character indicating the folder in which the original data is stored.
#' @param dataDestination character indicating the folder in which the cleaned data is stored.
#' @param exchanges vector of stock exchange symbols for all data in dataSource, 
#' e.g. exchanges = c("T","N") retrieves all stock market data from both NYSE and NASDAQ.
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
#' @param qDataraw xts or data.table object containing (ONE stock only) raw quote data. This argument is NULL by default. Enabling it means the arguments
#' from, to, dataSource and dataDestination will be ignored. (only advisable for small chunks of data)
#' @param report boolean and TRUE by default. In case it is true the function returns (also) a vector indicating how many quotes remained after each cleaning step.
#' @param selection argument to be passed on to the cleaning routine \code{\link{mergeQuotesSameTimestamp}}. The default is "median".
#' @param maxi spreads which are greater than median(spreads of day) times maxi are excluded.
#' @param window argument to be passed on to the cleaning routine \code{\link{rmOutliersQuotes}}. 
#' @param type argument to be passed on to the cleaning routine \code{\link{rmOutliersQuotes}}.
#' @param rmoutliersmaxi argument to be passed on to the cleaning routine \code{\link{rmOutliersQuotes}}.
#' @param saveasxts indicates whether data should be saved in xts format instead of data.table when using on-disk functionality. TRUE by default.
#' 
#' @return The function converts every csv file in dataSource into multiple xts or data.table files.
#' In dataDestination, there will be one folder for each symbol containing .rds files with cleaned data stored either in data.table or xts format.
#' 
#' In case you supply the argument "qDataraw", the on-disk functionality is ignored
#' and the function returns a list with the cleaned quotes as an xts or data.table object depending on input (see examples).
#' 
#' @references Barndorff-Nielsen, O. E., P. R. Hansen, A. Lunde, and N. Shephard (2009). Realized kernels in practice: Trades and quotes. Econometrics Journal 12, C1-C32.
#' Brownlees, C.T. and Gallo, G.M. (2006). Financial econometric analysis at ultra-high frequency: Data handling concerns. Computational Statistics & Data Analysis, 51, pages 2232-2245.
#' Falkenberry, T.N. (2002). High frequency data filtering. Unpublished technical report.
#' 
#' @author Jonathan Cornelissen, Kris Boudt and Onno Kleen.
#' 
#' @examples
#' # Consider you have raw quote data for 1 stock for 2 days
#' head(sampleqDataRawMicroseconds)
#' dim(sampleqDataRawMicroseconds)
#' qData_aftercleaning <- quotesCleanup(qDataraw = sampleqDataRawMicroseconds, exchanges = "N")
#' qData_aftercleaning$report
#' dim(qData_aftercleaning$qData)
#' 
#' # In case you have more data it is advised to use the on-disk functionality
#' # via "from","to","dataSource",etc. arguments
#' 
#' @importFrom readr read_csv
#' @keywords cleaning
#' @export
quotesCleanup <- function(dataSource = NULL, dataDestination = NULL, exchanges, qDataraw = NULL, report = TRUE, 
                          selection = "median", maxi = 50, window = 50, type = "advanced", rmoutliersmaxi = 10, saveasxts = TRUE) {
  
  BID = OFR = DT = SPREAD = SPREAD_MEDIAN = EX = DATE = BIDSIZ = OFRSIZ = TIME_M  = NULL
  nresult <- c(initial_number = 0,
               no_zero_quotes = 0,
               select_exchange = 0,
               remove_negative_spread = 0,
               remove_large_spread = 0,
               merge_same_timestamp = 0,
               remove_outliers = 0)
  
  if (is.null(qDataraw) == TRUE) {
    try(dir.create(dataDestination), silent = TRUE)
    
    quotesfiles <- list.files(dataSource, recursive = TRUE)[grepl("quotes", list.files(dataSource, recursive = TRUE))]
    for (ii in quotesfiles) {
      readdata <- try(as.data.table(read_csv(paste0(dataSource, "/", ii))), silent = TRUE)
      readdata <- try(readdata[, DT := as.POSIXct(substring(paste(as.character(DATE), TIME_M, sep = " "), 1, 20), tz = "EST", format = "%Y%m%d %H:%M:%OS")], silent = TRUE)
      qData <- try(quotesCleanup(qDataraw = readdata,
                                 selection = selection,
                                 exchanges = exchanges,
                                 maxi = maxi,
                                 window = window,
                                 type = type,
                                 rmoutliersmaxi = rmoutliersmaxi))$qData
      
      qData <- qData[, DATE := as.Date(DT, tz = "EST")]
      qData <- split(qData, by = "DATE")
      
      try(dir.create(paste0(dataDestination, "/", strsplit(ii, "/")[[1]][1])), silent = TRUE)
      for (jj in qData) {
        if (saveasxts == TRUE) {
          df_result <- xts(as.matrix(jj[, -c("DT", "DATE")]), order.by = jj$DT)
        } else {
          df_result <- jj[, -c( "DATE")]
        }
        saveRDS(df_result, paste0(dataDestination, "/", strsplit(ii, "/")[[1]][1], "/", unique(as.Date(jj$DT, tz = "EST")), "quotes.rds"))
        # saveRDS(df_result, paste0(dataDestination, "/", strsplit(ii, "/")[[1]][1], "/", strsplit(strsplit(ii, "/")[[1]][2], ".zip")[1], ".rds"))
      }
    }
  }
  
  if (is.null(qDataraw) == FALSE) {
    
    qDataraw <- checkColumnNames(qDataraw)
    checkqData(qDataraw)
    
    dummy_was_xts <- FALSE
    if (is.data.table(qDataraw) == FALSE) {
      if (is.xts(qDataraw) == TRUE) {
        qDataraw <- setnames(as.data.table(qDataraw)[, BID := as.numeric(as.character(BID))][, OFR := as.numeric(as.character(OFR))]
                             [, BIDSIZ := as.numeric(as.character(BIDSIZ))][, OFRSIZ := as.numeric(as.character(OFRSIZ))], old = "index", new = "DT")
        dummy_was_xts <- TRUE
      } else {
        stop("Input has to be data.table or xts.")
      }
    } else {
      if (("DT" %in% colnames(qDataraw)) == FALSE) {
        stop("Data.table neeeds DT column.")
      }
    }
    
    nresult[1] <- dim(qDataraw)[1] 
    qDataraw <- qDataraw[BID != 0 & OFR != 0]
    nresult[2] <- dim(qDataraw)[1] 
    qDataraw <- qDataraw[EX %in% exchanges]
    nresult[3] <- dim(qDataraw)[1] 
    qDataraw <- qDataraw[OFR > BID][, SPREAD := OFR - BID][, DATE := as.Date(DT)][, SPREAD_MEDIAN := median(SPREAD), by = "DATE"]
    nresult[4] <- dim(qDataraw)[1] 
    qDataraw <- qDataraw[SPREAD < (SPREAD_MEDIAN * maxi)]
    nresult[5] <- dim(qDataraw)[1]
    qDataraw <- mergeQuotesSameTimestamp(qData = qDataraw, selection = selection)
    nresult[6] <- dim(qDataraw)[1]
    
    qDataraw <- rmOutliersQuotes(qDataraw, window = window, type = "advanced", maxi = rmoutliersmaxi)
    nresult[7] <- dim(qDataraw)[1]
    if (dummy_was_xts == TRUE) {
      df_result <- xts(as.matrix(qDataraw[, -c("DT",  "DATE")]), order.by = qDataraw$DT)
    } else {
      df_result <- qDataraw[, -c( "DATE")]
    }
    
    if (report == TRUE) {
      return(list(qData = df_result, report = nresult))
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
#' @param qData an xts or data.table object at least containing the columns "BID" and "OFR".
#' @param maxi an integer. By default maxi = "50", which means that entries are deleted 
#' if the spread is more than 50 times the median spread on that day.
#' 
#' @return xts or data.table object depending on input.
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' @keywords cleaning
rmLargeSpread <- function(qData, maxi = 50) {
  BID = OFR = DATE = DT = SPREAD = SPREAD_MEDIAN = NULL
  qData <- checkColumnNames(qData)
  checkqData(qData)
  dummy_was_xts <- FALSE
  if (is.data.table(qData) == FALSE) {
    if (is.xts(qData) == TRUE) {
      qData <- setnames(as.data.table(qData)[, BID := as.numeric(as.character(BID))][, OFR := as.numeric(as.character(OFR))], old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } 
  
  qData <- qData[, DATE := as.Date(DT)][
    , SPREAD := OFR - BID][
    , SPREAD_MEDIAN := median(SPREAD), by = "DATE"][SPREAD < (SPREAD_MEDIAN * maxi)]
  
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(qData[, -c("DT", "DATE", "SPREAD", "SPREAD_MEDIAN")]), order.by = qData$DT))
  } else {
    return(qData[, -c("DATE", "SPREAD", "SPREAD_MEDIAN")])
  }
}

#' Delete entries for which the spread is negative
#' @description Function deletes entries for which the spread is negative.
#' 
#' @param qData an xts object at least containing the columns "BID" and "OFR".
#' 
#' @return data.table or xts object
#' 
#' @author Jonathan Cornelissen, Kris Boudt and Onno Kleen
#' 
#' @examples 
#' rmNegativeSpread(sampleqDataRawMicroseconds)
#' 
#' @keywords cleaning
#' @export
rmNegativeSpread <- function(qData) {
  BID = OFR = DATE = DT = NULL
  qData <- checkColumnNames(qData)
  checkqData(qData)
  dummy_was_xts <- FALSE
  if (is.data.table(qData) == FALSE) {
    if (is.xts(qData) == TRUE) {
      qData <- setnames(as.data.table(qData), old = "index", new = "DT")
      qData[, BID := as.numeric(as.character(BID))]
      qData[, OFR := as.numeric(as.character(OFR))]
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } 
  
  qData <- qData[OFR > BID]
  
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(qData[, -c("DT")]), order.by = qData$DT))
  } else {
    return(qData)
  }
}

#' Delete transactions with unlikely transaction prices
#' @description Deprecated - use rmTradeOutliers instead.
#' @param tData a data.table or xts object containing the time series data, with at least the column "PRICE", containing the transaction price (ONE DAY ONLY).
#' @param qData a data.table or xts object containing the time series data with at least the columns "BID" and "OFR", containing the bid and ask prices (ONE DAY ONLY).
#' @return xts or data.table object depending on input
#' @export
rmTradeOutliers <- function(tData, qData) {
  warning("Renamed as rmTradeOutliersUsingQuotes. Will be deprecated in future releases.")
  rmTradeOutliersUsingQuotes(tData, qData)
}

#' Delete transactions with unlikely transaction prices
#' 
#' @description Function deletes entries with prices that are above the ask plus the bid-ask spread.
#' Similar for entries with prices below the bid minus the bid-ask spread.
#' 
#' @param tData a data.table or xts object containing the time series data, with at least the column "PRICE", containing the transaction price (ONE DAY ONLY).
#' @param qData a data.table or xts object containing the time series data with at least the columns "BID" and "OFR", containing the bid and ask prices (ONE DAY ONLY).
#' 
#' @details Note: in order to work correctly, the input data of this function should be
#' cleaned trade (tData) and quote (qData) data respectively.
#' 
#' @return xts or data.table object depending on input
#' 
#' @author Jonathan Cornelissen, Kris Boudt and Onno Kleen
#' @keywords cleaning
#' @importFrom data.table setkey
#' @export
rmTradeOutliersUsingQuotes <- function(tData, qData) {
  SPREAD = DT = PRICE = BID = OFR = SYMBOL = 0
  tData <- checkColumnNames(tData)
  qData <- checkColumnNames(qData)
  checkqData(qData)
  checktData(tData)
  
  if (any(class(tData) != class(qData))) {
    stop("tData and qData should be of the same data type, either xts or data.table.")
  }
  
  dummy_was_xts <- FALSE
  if (is.data.table(tData) == FALSE) {
    if (is.xts(tData) == TRUE) {
      tData <- setnames(as.data.table(tData)[, PRICE := as.numeric(as.character(PRICE))], 
                        old = "index", new = "DT")
      qData <- setnames(as.data.table(qData)[, BID := as.numeric(as.character(BID))][, OFR := as.numeric(as.character(OFR))], 
                        old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(tData)) == FALSE) {
      stop("tData neeeds DT column.")
    }
  }
  
  if (length(unique(as.Date(tData$DT))) > 1) {
    stop("Both data sets should only include data for one day.")
  }
  
  qData <- qData[, DT := DT + 2]
  
  setkey(tData, SYMBOL, DT)
  setkey(qData, SYMBOL, DT)
  
  tData <- tData[, c("DT", "SYMBOL", "PRICE")]
  
  tData <- qData[tData, roll = TRUE, on = c("SYMBOL", "DT")]
  
  tData[is.na(BID)][, "BID"] <- tData$BID[min(which(is.na(tData$BID) == FALSE))]
  tData[is.na(OFR)][, "OFR"] <- tData$OFR[min(which(is.na(tData$OFR) == FALSE))]
  
  tData <- tData[, SPREAD := OFR - BID][PRICE <= OFR + SPREAD][PRICE >= BID - SPREAD]
  
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(tData[, -c("DT", "SPREAD")]), order.by = tData$DT))
  } else {
    return(tData[, -c("SPREAD")])
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
#' @param qData a data.table or xts object at least containing the columns "BID" and "OFR".
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
rmOutliersQuotes <- function (qData, maxi = 10, window = 50, type = "advanced") {
  # NOTE: Median Absolute deviation chosen contrary to Barndorff-Nielsen et al.
  # Setting those variables equal NULL is for suppressing NOTES in devtools::check
  # References inside data.table-operations throw "no visible binding for global variable ..." error
  BID = OFR = MIDQUOTE = DATE = DT = MADALL = CRITERION = NULL
  if ((window %% 2) != 0) {
    stop("Window size can't be odd.")
  }
  
  qData <- checkColumnNames(qData)
  checkqData(qData)
  
  dummy_was_xts <- FALSE
  if (is.data.table(qData) == FALSE) {
    if (is.xts(qData) == TRUE) {
      qData <- setnames(as.data.table(qData)[, BID := as.numeric(as.character(BID))][, OFR := as.numeric(as.character(OFR))], old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(qData)) == FALSE) {
      stop("Data.table neeeds DT column.")
    }
  }
  
  if (length(unique(qData$SYMBOL)) > 1) {
    stop("Please provide only one symbol at a time.")
  }
  
  if ((type %in% c("standard", "advanced")) == FALSE) {
    stop("type has to be \"standard\" or \"advanced\".")
  }
  
  # weights_med_center_incl <- rep(1, times = window + 1)
  weights_med_center_excl <- c(rep(1, times = window / 2), 0, rep(1, times = window / 2))
  weights_med_follow  <- c(0 , rep(1, times = window))
  weights_med_trail    <- c(rep(1, times = window), 0)
  
  qData <- qData[, MIDQUOTE := (BID + OFR) / 2][, DATE := as.Date(DT)][, MADALL := mad(MIDQUOTE), by = "DATE"]
  
  if (type == "standard") {
    qData <- qData[ , CRITERION := abs(MIDQUOTE - rollingMedianInclEnds(MIDQUOTE, window = window, weights = weights_med_center_excl))][
      CRITERION < maxi * MADALL]
    
  }
  if (type == "advanced") {
    qData <- qData[, CRITERION := pmin(abs(MIDQUOTE - rollingMedianInclEnds(MIDQUOTE, window = window, weights = weights_med_center_excl, direction = "center")),
                                       abs(MIDQUOTE - rollingMedianInclEnds(MIDQUOTE, window = window, weights = weights_med_trail, direction = "left")),
                                       abs(MIDQUOTE - rollingMedianInclEnds(MIDQUOTE, window = window, weights = weights_med_follow, direction = "right")),
                                       na.rm = TRUE)][
                                         CRITERION < maxi * MADALL]
  }
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(qData[, -c("DT", "DATE", "MADALL", "CRITERION", "MIDQUOTE")]), order.by = qData$DT))
  } else {
    return(qData[, -c("MADALL", "CRITERION")])
  }
}

#' Delete entries with abnormal Sale Condition.
#' 
#' @description Function deletes entries with abnormal Sale Condition: 
#' trades where column "COND" has
#' a letter code, except for "E" and "F".
#' 
#' @param tData an xts or data.table object containing the time series data, with 
#' one column named "COND" indicating the Sale Condition.
#' 
#' @return xts or data.table object depending on input
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' 
#' @keywords leaning
#' @export
salesCondition <- function(tData) {
  COND = NULL
  tData <- checkColumnNames(tData)
  checktData(tData)
  
  if (any(colnames(tData) == "COND") == FALSE) {
    stop("The argument tData should have a column containing sales conditions named COND. Could not find that column.")
  }
  
  dummy_was_xts <- FALSE
  if (is.data.table(tData) == FALSE) {
    if (is.xts(tData) == TRUE) {
      tData <- setnames(as.data.table(tData), old = "index", new = "DT")
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } 
  
  tData <- tData[COND %in% c("E", "F")]
  
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(tData), order.by = tData$DT))
  } else {
    return(tData)
  }
}

#' Retain only data from a single stock exchange
#' @description Function returns an xts object containing the data of only 1 stock exchange.
#' 
#' @param data an xts or data.table object containing the time series data. 
#' The object should have a column "EX", indicating the exchange by its symbol.
#' @param exch The (vector of) symbol(s) of the stock exchange(s) that should be selected.
#' By default the NYSE is chosen (exch = "N"). Other exchange symbols are:
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
  # checkqData(data)
  
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
#' @description This is a wrapper function for cleaning the trade data of all stock data inside the folder dataSource. 
#' The result is saved in the folder dataDestination. 
#' 
#' In case you supply the argument "rawtData", the on-disk functionality is ignored. The function returns a vector
#' indicating how many trades were removed at each cleaning step in this case.
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
#' @param dataSource character indicating the folder in which the original data is stored.
#' @param dataDestination character indicating the folder in which the cleaned data is stored.
#' @param exchanges vector of stock exchange symbols for all data in dataSource, 
#' e.g. exchanges = c("T","N") retrieves all stock market data from both NYSE and NASDAQ.
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
#' @param tDataraw xts object containing (for ONE stock only) raw trade data. This argument is NULL by default. Enabling it means the arguments
#' from, to, dataSource and dataDestination will be ignored. (only advisable for small chunks of data)
#' @param report boolean and TRUE by default. In case it is true the function returns (also) a vector indicating how many trades remained after each cleaning step.
#' @param selection argument to be passed on to the cleaning routine \code{\link{mergeTradesSameTimestamp}}. The default is "median".
#' @param saveasxts indicates whether data should be saved in xts format instead of data.table when using on-disk functionality. TRUE by default.
#' 
#' @return For each day an xts or data.table object is saved into the folder of that date, containing the cleaned data.
#' This procedure is performed for each stock in "ticker".
#' The function returns a vector indicating how many trades remained after each cleaning step.
#' 
#' In case you supply the argument "rawtData", the on-disk functionality is ignored
#' and the function returns a list with the cleaned trades as xts object (see examples).
#' 
#' @examples 
#' # Consider you have raw trade data for 1 stock for 2 days 
#' head(sampletDataRawMicroseconds)
#' dim(sampletDataRawMicroseconds)
#' tData_afterfirstcleaning <- tradesCleanup(tDataraw = sampletDataRaw, exchanges = list("N"))
#' tData_afterfirstcleaning$report
#' dim(tData_afterfirstcleaning$tData)
#' 
#' #In case you have more data it is advised to use the on-disk functionality
#' #via "from","to","dataSource",etc. arguments
#' 
#' @references Barndorff-Nielsen, O. E., P. R. Hansen, A. Lunde, and N. Shephard (2009). Realized kernels in practice: Trades and quotes. Econometrics Journal 12, C1-C32.
#' 
#' Brownlees, C.T. and Gallo, G.M. (2006). Financial econometric analysis at ultra-high frequency: Data handling concerns. Computational Statistics & Data Analysis, 51, pp. 2232-2245.
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' @keywords cleaning
#' @export
tradesCleanup <- function(dataSource = NULL, dataDestination = NULL, exchanges, tDataraw = NULL, report = TRUE, selection = "median", saveasxts = TRUE) {
  PRICE = EX = COND = DT = DATE = TIME_M = NULL
  
  if (is.null(tDataraw) == TRUE) {
    try(dir.create(dataDestination), silent = TRUE)
    
    tradesfiles <- list.files(dataSource, recursive = TRUE)[!grepl("quotes", list.files(dataSource, recursive = TRUE))]
    for (ii in tradesfiles) {
      readdata <- try(as.data.table(read_csv(paste0(dataSource, "/", ii))), silent = TRUE)
      readdata <- try(readdata[, DT := as.POSIXct(substring(paste(as.character(DATE), TIME_M, sep = " "), 1, 20), tz = "EST", format = "%Y%m%d %H:%M:%OS")], silent = TRUE)
      tData <- try(tradesCleanup(tDataraw = readdata,
                                 selection = selection,
                                 exchanges = exchanges))$tData
      tData <- tData[, DATE := as.Date(DT, tz = "EST")]
      tData <- split(tData, by = "DATE")
      
      try(dir.create(paste0(dataDestination, "/", strsplit(ii, "/")[[1]][1])), silent = TRUE)
      for (jj in tData) {
        if (saveasxts == TRUE) {
          df_result <- xts(as.matrix(jj[, -c("DT", "DATE")]), order.by = jj$DT)
        } else {
          df_result <- jj[, -c( "DATE")]
        }
        saveRDS(df_result, paste0(dataDestination, "/", strsplit(ii, "/")[[1]][1], "/", unique(as.Date(jj$DT, tz = "EST")), ".rds"))
        # saveRDS(df_result, paste0(dataDestination, "/", strsplit(ii, "/")[[1]][1], "/", strsplit(strsplit(ii, "/")[[1]][2], ".zip")[1], ".rds"))
      }
    }
  }
  
  if (is.null(tDataraw) == FALSE) {
    nresult <- c(initial_number = 0,
                 no_zero_trades = 0,
                 select_exchange = 0,
                 remove_sales_condition = 0,
                 merge_same_timestamp = 0)
    
    tDataraw <- checkColumnNames(tDataraw)
    checktData(tDataraw)
    
    dummy_was_xts <- FALSE
    if (is.data.table(tDataraw) == FALSE) {
      if (is.xts(tDataraw) == TRUE) {
        tDataraw <- setnames(as.data.table(tDataraw)[, PRICE := as.numeric(as.character(PRICE))], old = "index", new = "DT")
        dummy_was_xts <- TRUE
      } else {
        stop("Input has to be data.table or xts.")
      }
    } else {
      if (("DT" %in% colnames(tDataraw)) == FALSE) {
        stop("Data.table neeeds DT column.")
      }
    }
    nresult[1] <- dim(tDataraw)[1] 
    tDataraw <- tDataraw[PRICE != 0]
    nresult[2] <- dim(tDataraw)[1] 
    tDataraw <- tDataraw[EX %in% exchanges]
    nresult[3] <- dim(tDataraw)[1] 
    tDataraw <- tDataraw[COND %in% c("E", "F")]
    nresult[4] <- dim(tDataraw)[1] 
    tDataraw <- mergeTradesSameTimestamp(tDataraw, selection = selection)
    nresult[5] <- dim(tDataraw)[1] 
    
    if (dummy_was_xts == TRUE) {
      df_result <- xts(as.matrix(tDataraw[, -c("DT")]), order.by = tDataraw$DT)
    } else {
      df_result <- tDataraw
    }
    
    if (report == TRUE) {
      return(list(tData = df_result, report = nresult))
    } else {
      return(df_result)
    }
  }
}

#' @export
tradesCleanupUsingQuotes <- function(from, to, dataSource, dataDestination, ticker, tData = NULL, qData = NULL) {
  
  ## Deprecated
  warning("Please use tradesCleanupUsingQuotes instead of tradesCleanupFinal.") 
  tradesCleanupFinal(from, to, dataSource, dataDestination, ticker, tData = NULL, qData = NULL)
}

#' Perform a final cleaning procedure on trade data
#' 
#' @description Function performs cleaning procedure \code{\link{rmTradeOutliersUsingQuotes}} 
#' for the trades of all stocks data in "dataDestination". 
#' Note that preferably the input data for this function 
#' is trade and quote data cleaned by respectively e.g. \code{\link{tradesCleanup}}
#' and \code{\link{quotesCleanup}}.
#' 
#' @param dataSource character indicating the folder in which the original data is stored.
#' @param dataDestination character indicating the folder in which the cleaned data is stored, folder of dataSource by default.
#' @param tData data.table or xts object containing (ONE day and for ONE stock only) trade data cleaned by \code{\link{tradesCleanup}}. This argument is NULL by default. Enabling it, means the arguments
#' from, to, dataSource and dataDestination will be ignored. (only advisable for small chunks of data)
#' @param qData data.table or xts object containing (ONE day and for ONE stock only) cleaned quote data. This argument is NULL by default. Enabling it means the arguments
#' from, to, dataSource, dataDestination will be ignored. (only advisable for small chunks of data)
#' 
#' @return For each day an xts object is saved into the folder of that date, containing the cleaned data.
#' 
#' In case you supply the arguments "tData" and "qData", the on-disk functionality is ignored
#' and the function returns cleaned trades as a data.table or xts object (see examples).
#' 
#' @references Barndorff-Nielsen, O. E., P. R. Hansen, A. Lunde, and N. Shephard (2009). Realized kernels in practice: Trades and quotes. Econometrics Journal 12, C1-C32.
#' 
#' Brownlees, C.T. and Gallo, G.M. (2006). Financial econometric analysis at ultra-high frequency: Data handling concerns. Computational Statistics & Data Analysis, 51, pages 2232-2245.
#' 
#' @author Jonathan Cornelissen, Kris Boudt and Onno Kleen.
#' 
#' @examples 
#' # Consider you have raw trade data for 1 stock for 2 days 
#' tData_afterfirstcleaning <- tradesCleanup(tDataraw = sampletDataRawMicroseconds, 
#'                                           exchanges = "N", report = FALSE)
#' # 
#' qData <- quotesCleanup(qDataraw = sampleqDataRawMicroseconds, 
#'                        exchanges = "N", report = FALSE)
#' dim(tData_afterfirstcleaning)
#' tData_afterfinalcleaning <- 
#'   tradesCleanupUsingQuotes(qData = qData[as.Date(DT) == "2018-01-02"],
#'                            tData = tData_afterfirstcleaning[as.Date(DT) == "2018-01-02"])
#' dim(tData_afterfinalcleaning)
#' #In case you have more data it is advised to use the on-disk functionality
#' #via "from","to","dataSource", etc. arguments
#' @keywords cleaning
#' @export
tradesCleanupUsingQuotes <- function(dataSource = NULL, dataDestination = NULL, tData = NULL, qData = NULL) {
  
  if (is.null(dataDestination) == TRUE) {
    dataDestination <- dataSource
  }
  
  if ((!is.null(tData)) & (!is.null(qData))) {
    tData <- checkColumnNames(tData)
    qData <- checkColumnNames(qData)
    
    #1 cleaning procedure that needs cleaned trades and quotes
    tData <- rmTradeOutliersUsingQuotes(tData, qData)
    return(tData)
  } else {
    ticker <- list.files(dataSource)
    for (ii in ticker) {
      filesforticker <- list.files(paste0(dataSource, "/", ii, "/"))
      for (jj in filesforticker[!grepl("quotes", filesforticker)]) {
        tData <- try(readRDS(paste0(dataSource, "/", ii, "/", jj)))
        qData <- try(readRDS(paste0(dataSource, "/", ii, "/", substring(jj, 1, 10), "quotes.rds")))
        tData <- checkColumnNames(tData)
        qData <- checkColumnNames(qData)
        saveRDS(rmTradeOutliersUsingQuotes(tData, qData), paste0(dataDestination, "/", ii, "/", substring(jj, 1, 10), "tradescleanedbyquotes.rds"))
      }
    }
  }
}

#' Aggregate a time series
#' 
#' @description Aggregate a time series as \code{xts} or \code{data.table} object. 
#' It can handle irregularly spaced time series and returns a regularly spaced one.
#' Use univariate time series as input for this function and check out \code{\link{aggregateTrades}}
#' and \code{\link{aggregateQuotes}} to aggregate Trade or Quote data objects.
#' 
#' @param ts \code{xts} or \code{data.table} object to aggregate.
#' @param FUN function to apply over each interval. By default, previous tick aggregation is done. 
#' Alternatively one can set e.g. FUN = "mean".
#' In case weights are supplied, this argument is ignored and a weighted average is taken.
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. Possible values are: "secs", "seconds", "mins", "minutes", "hours", "days", "weeks", "ticks".
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. For example, to aggregate
#' based on a 5 minute frequency, set \code{alignPeriod} to 5 and \code{alignBy} to \code{"minutes"}.
#' @param weights By default, no weighting scheme is used. 
#' When you assign an \code{xts} object with weights to this argument, a weighted mean is taken over each interval. 
#' Of course, the weights should have the same time stamps as the supplied time series.
#' @param dropna boolean, which determines whether empty intervals should be dropped.
#' By default, an NA is returned in case an interval is empty, except when the user opts
#' for previous tick aggregation, by setting \code{FUN = "previoustick"} (default).
#' @param tz character denoting which timezone the output should be in. Defaults to \code{NULL}
#' @param ... extra parameters passed on to \code{FUN}
#' @details The time stamps of the new time series are the closing times and/or days of the intervals. 
#' For example, for a weekly aggregation the new time stamp is the last day in that particular week (namely Sunday).
#' 
#' In case of previous tick aggregation, 
#' for \code{alignBy} is either \code{"seconds"} \code{"minutes"}, or \code{"hours"},
#' the element of the returned series with e.g. timestamp 09:35:00 contains 
#' the last observation up to that point, including the value at 09:35:00 itself.
#' 
#' Please note: In case an interval is empty, by default an NA is returned.. In case e.g. previous 
#' tick aggregation it makes sense to fill these NAs by the function \code{na.locf}
#' (last observation carried forward) from the \pkg{zoo} package.
#' 
#' In case \code{alignBy = "ticks"}, the sampling is done such the sampling starts on the first tick and the last tick is always included.
#' For example, if 14 observations are made on one day, and these are 1, 2, 3, ... 14.
#' Then, with \code{alignBy = "ticks"} and \code{alignPeriod = 3}, the output will be 1, 4, 7, 10, 13, 14.
#' 
#' @return An \code{xts} object containing the aggregated time series.
#' 
#' @author Jonathan Cornelissen, Kris Boudt, and Emil Sjoerup.
#' @keywords data manipulation
#' 
#' @examples 
#' # Load sample price data
#' \dontrun{
#' library(xts)
#' ts <- as.xts(sampleTData[, list(DT, PRICE, SIZE)])
#' 
#' # Previous tick aggregation to the 5-minute sampling frequency:
#' tsagg5min <- aggregateTS(ts, alignBy = "minutes", alignPeriod = 5)
#' head(tsagg5min)
#' # Previous tick aggregation to the 30-second sampling frequency:
#' tsagg30sec <- aggregateTS(ts, alignBy = "seconds", alignPeriod = 30)
#' tail(tsagg30sec)
#' tsagg3ticks <- aggregateTS(ts, alignBy = "ticks", alignPeriod = 3)
#' }
#' 
#' 
#' @importFrom zoo zoo na.locf
#' @importFrom stats start end
#' @importFrom xts period.apply tzone	
#' @export
aggregateTS <- function (ts, FUN = "previoustick", alignBy = "minutes", alignPeriod = 1, weights = NULL, dropna = FALSE, tz = NULL, ...) {
  if(is.xts(ts)){
    return(internalAggregateTSXTS(ts, FUN = FUN, alignBy = alignBy, alignPeriod = alignPeriod, weights = weights, dropna = dropna, tz = tz, ...))
  } else if(is.data.table(ts)){
    return(internalAggregateTSDT(ts, FUN = FUN, alignBy = alignBy, alignPeriod = alignPeriod, weights = weights, dropna = dropna, tz = tz, ...))
  }
}


#' Aggregate a time series but keep first and last observation
#' @description Function to aggregate high frequency data by last tick aggregation to an arbitrary periodicity based on wall clocks.
#' Alternatively the aggregation can be done by number of ticks. In case we DON'T do tick-based aggregation, 
#' this function accepts arbitrary number of symbols over a arbitrary number of days. Although the function has the word Price in the name,
#' the function is general and works on arbitrary time series, either \code{xts} or \code{data.table} objects the latter requires a \code{DT}
#' column containing POSIXct time stamps.
#' 
#' @param pData \code{data.table} or \code{xts} object to be aggregated containing the intraday price series, possibly across multiple days.
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. Possible values are: "secs", "seconds", "mins", "minutes","hours", and "ticks".
#' To aggregate based on a 5 minute frequency, set \code{alignPeriod} to 5 and \code{alignBy} to \code{"minutes"}.
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. E.g. to aggregate
#' based on a 5 minute frequency, set \code{alignPeriod} to 5 and \code{alignBy} to \code{"minutes"}.
#' @param marketOpen the market opening time, by default: \code{marketOpen = "09:30:00"}.
#' @param marketClose the market closing time, by default: \code{marketClose = "16:00:00"}.
#' @param fill indicates whether rows without trades should be added with the most recent value, FALSE by default.
#' @param tz fallback time zone used in case we we are unable to identify the timezone of the data, by default: \code{tz = NULL}. We attempt to extract the timezone from the DT column (or index) of the data, which may fail. 
#' In case of failure we use \code{tz} if specified, and if it is not specified, we use \code{"UTC"}
#' @details
#' The time stamps of the new time series are the closing times and/or days of the intervals. 
#' The element of the returned series with e.g. time stamp 09:35:00 contains
#' the last observation up to that point, including the value at 09:35:00 itself.
#'
#' In case \code{alignBy = "ticks"}, the sampling is done such the sampling starts on the first tick, and the last tick is always included.
#' For example, if 14 observations are made on one day, and these are 1, 2, 3, ... 14.
#' Then, with \code{alignBy = "ticks"} and \code{alignPeriod = 3}, the output will be 1, 4, 7, 10, 13, 14.
#'
#' @return A \code{data.table} or \code{xts} object containing the aggregated time series.
#'
#' @author Jonathan Cornelissen, Kris Boudt, Onno Kleen, and Emil Sjoerup.
#' @keywords data manipulation
#' @examples
#' # Aggregate price data to the 30 second frequency
#' aggregatePrice(sampleTData, alignBy = "secs", alignPeriod = 30)
#' # Aggregate price data to the 30 second frequency including zero return price changes
#' aggregatePrice(sampleTData, alignBy = "secs", alignPeriod = 30)
#'
#' # Aggregate price data to half a second frequency including zero return price changes
#' aggregatePrice(sampleTData, alignBy = "milliseconds", alignPeriod = 500, fill = TRUE)
#' @importFrom xts last tzone
#' @importFrom data.table fifelse
#' @export
aggregatePrice <- function(pData, alignBy = "minutes", alignPeriod = 1, marketOpen = "09:30:00", marketClose = "16:00:00" , fill = FALSE, tz = NULL) {
  ## checking
  nm <- toupper(colnames(pData))
  pData <- checkColumnNames(pData)
  i.DATE <- .SD <- .N <- .I <- DATE <- DT <- FIRST_DT <- DT_ROUND <- LAST_DT <- SYMBOL <- PRICE <- NULL


  if (alignBy == "milliseconds") {
    alignBy <- "secs"
    alignPeriod <- alignPeriod / 1000
  }
  if(alignBy == "secs" | alignBy == "seconds"){
    scaleFactor <- alignPeriod
  }
  if(alignBy == "mins" | alignBy == "minutes"){
    scaleFactor <- alignPeriod * 60
  }
  if(alignBy == "hours"){
    scaleFactor <- alignPeriod * 60 * 60
  }

  if(! (alignBy %in% c("milliseconds", "secs", "seconds", "mins", "minutes", "hours", "ticks"))){
    stop("alignBy not valid value. Valid values are: \"milliseconds\", \"secs\", \"seconds\", \"mins\", \"minutes\", \"hours\", and \"ticks\".")
  }
  

  inputWasXts <- FALSE
  if (!is.data.table(pData)) {
    if (is.xts(pData)) {
      nm <- c("DT", nm)
      pData <- as.data.table(pData)
      pData <- setnames(pData , old = "index", new = "DT")
      inputWasXts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (!("DT" %in% colnames(pData))) {
      stop("Data.table neeeds DT column (date-time ).")
    }

  }
  
  if (!("SYMBOL" %in% nm)) {
    pData[, SYMBOL := "UKNOWN"]
  }
  
  
  if(alignBy == "ticks"){ ## Special case for alignBy = "ticks"
    if(alignPeriod == 1) return(pData[])
    if(alignPeriod < 1 | alignPeriod%%1 != 0){
      stop("When alignBy is `ticks`, must be a positive integer valued numeric")
    }
    # if(length(unique(as.Date(pData[,DT]))) > 1){
    #   stop("Multiday support for aggregatePrice with alignBy = \"ticks\" is not implemented yet.")
    # }
    return(pData[seqInclEnds(1, .N, alignPeriod), .SD, by = list(DATE = as.Date(DT)), .SDcols = 1:ncol(pData)][])
  }
  
  
  
  
  timeZone <- format(pData$DT[1], format = "%Z")
  if(is.null(timeZone) || timeZone == ""){
    if(is.null(tz)){
      tz <- "UTC"
    }
    if(!("POSIXct" %in% class(pData$DT))){
      pData[, DT := as.POSIXct(format(DT, digits = 20, nsmall = 20), tz = tz)]
    }
  } else {
    tz <- timeZone
  }
  setkeyv(pData, c("DT", "SYMBOL")) # The below code MAY fail with data with unordered DT column. Also setkey inceases speed of grouping
  ## Checking ends
  # Convert DT to numeric. This is much faster than dealing with the strings (POSIXct)
  pData[, DT := as.numeric(DT, tz = tz)]
  # Calculate the date in the data
  pData[, DATE := as.Date(floor(DT / 86400), origin = "1970-01-01", tz = tz)]
  # extract a vector of dates
  obsPerDay <- pData[, .N, by = list(DATE, SYMBOL)]
  
  ## Find the opening times of each of the days as numerics.
  marketOpenNumeric <- as.numeric(as.POSIXct(paste(obsPerDay$DATE, marketOpen), format = "%Y-%m-%d %H:%M:%OS", tz = tz), tz = tz)
  marketCloseNumeric <- as.numeric(as.POSIXct(paste(obsPerDay$DATE, marketClose), format = "%Y-%m-%d %H:%M:%OS", tz = tz), tz =tz)
  
  ## Here we make sure that we can correctly handle times that happen before midnight in the corrected timestamps from the flag if statements
  if(NROW(marketOpenNumeric) != NROW(obsPerDay)){
    if(NROW(marketOpenNumeric) < NROW(obsPerDay)){ ## Here we add entries
      marketOpenNumeric <- rep(marketOpenNumeric, NROW(obsPerDay))[1:NROW(obsPerDay)]
      marketCloseNumeric <- rep(marketCloseNumeric, NROW(obsPerDay))[1:NROW(obsPerDay)]
    } else {
      stop("unknown error occured in aggregatePrice")
    }

  }
  # Subset observations that does not fall between their respective market opening and market closing times.
  pData <- pData[between(DT, rep(marketOpenNumeric, obsPerDay$N), rep(marketCloseNumeric, obsPerDay$N))]
  obsPerDay <- pData[, .N, by = list(DATE, SYMBOL)]
  ## Here (AGAIN!) we make sure that we can correctly handle if we deleted a day in the subsetting step above
  if(NROW(marketOpenNumeric) != NROW(obsPerDay)){
    if(NROW(marketOpenNumeric) > NROW(obsPerDay)){ ## Here we delete entries
      marketOpenNumeric <- rep(marketOpenNumeric, NROW(obsPerDay))[1:NROW(obsPerDay)]
      marketCloseNumeric <- rep(marketCloseNumeric, NROW(obsPerDay))[1:NROW(obsPerDay)]
    } else {
      stop("unknown error occured in aggregatePrice")
    }
  }


  # Find the first observation per day.
  pData[, FIRST_DT := min(DT), by = list(SYMBOL, DATE)]
  # Use Dirks answer here: https://stackoverflow.com/a/42498175 to round the timestamps to the latest scaleFactor
  pData[, DT_ROUND := fifelse(DT == FIRST_DT,
                             floor(DT/scaleFactor) * scaleFactor,
                             ceiling(DT/scaleFactor) * scaleFactor)]

  pData[, LAST_DT := max(DT), by = list(SYMBOL, DT_ROUND)]

  # Create the first observation each day.
  pData_open <- pData[pData[DT == FIRST_DT, .I[1], by = list(SYMBOL, DATE)]$V1, ]
  pData_open[, DT := floor(DT/86400) * 86400 + marketOpenNumeric %% 86400]

  # Take the last observation of each group of LAST_DT

  pData <- pData[pData[DT == LAST_DT, .I[.N], by = list(SYMBOL, DT_ROUND)]$V1][, DT := DT_ROUND] ## Make sure we only take the last observation

  # due to rounding there may be an observation that is refered to the opening time
  pData <- pData[!(DT %in% pData_open$DT)]
  # Merge the opening observation onto the rest of the observations.
  pData <- merge(pData, pData_open, all = TRUE)


  if (fill) {
    # Construct timestamps that go from marketOpenNumeric to marketClose numeric each day with step of scaleFactor e.g. 1 min (60)
    symbs <- pData[, list(SYMBOL = unique(SYMBOL)), by = DATE]
    if(NROW(symbs) > NROW(obsPerDay)){ # One or more dates contain multiple symbols
      setkeyv(pData,c("DT", "SYMBOL"))
      pData <- pData[, lapply(.SD, nafill, type = "locf"), by = list(i.DATE, SYMBOL), .SDcols = setdiff(colnames(pData), c("i.DATE", "SYMBOL"))]
    } else {
    dt_full_index <- data.table(DT = as.numeric(mSeq(marketOpenNumeric, marketCloseNumeric, as.double(scaleFactor))),
                                SYMBOL = rep(obsPerDay$SYMBOL, (marketCloseNumeric-marketOpenNumeric)/as.double(scaleFactor) + 1))
    
    setkey(dt_full_index, "DT")

    setkey(pData, DT)

    pData <- unique(pData[dt_full_index, roll = TRUE, on = list(SYMBOL, DT)])
      
    }
  }
  pData[, DT := as.POSIXct(DT, origin = as.POSIXct("1970-01-01", tz = "UTC"), tz = tz)]
  pData <- pData[, nm, with = FALSE]
  if (inputWasXts) {
    return(xts(as.matrix(pData[, -c("DT")]), order.by = pData$DT, tzone = tz))
  } else {
    return(pData[])
  }
}




#' Aggregate a \code{data.table} or \code{xts} object containing quote data
#' 
#' @description Aggregate tick-by-tick quote data and return a \code{data.table} or \code{xts} object containing the aggregated quote data.
#' See \code{\link{sampleQData}} for an example of the argument qData. This function accepts arbitrary number of symbols over an arbitrary number of days.
#' 
#' @param qData \code{data.table} or \code{xts} object to be aggregated, containing the intraday quote data of a stock for one day.
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. Possible values are: "secs", "seconds", "mins", "minutes","hours", and "ticks".
#' To aggregate based on a 5 minute frequency, set \code{alignPeriod} to 5 and \code{alignBy} to \code{"minutes"}.
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. E.g. to aggregate
#' based on a 5 minute frequency, set \code{alignPeriod} to 5 and \code{alignBy} to \code{"minutes"}.
#' @param marketOpen the market opening time, by default: \code{marketOpen = "09:30:00"}.
#' @param marketClose the market closing time, by default: \code{marketClose = "16:00:00"}.
#' @param tz fallback time zone used in case we we are unable to identify the timezone of the data, by default: \code{tz = NULL}. We attempt to extract the timezone from the DT column (or index) of the data, which may fail. 
#' In case of failure we use \code{tz} if specified, and if it is not specified, we use \code{"UTC"}
#' 
#' @details The output "BID" and "OFR" columns are constructed using previous tick aggregation.
#' 
#' The variables "BIDSIZ" and "OFRSIZ" are aggregated by taking the sum of the respective inputs over each interval.
#' 
#' The timestamps of the new time series are the closing times of the intervals. 
#' 
#' Please note: Returned objects always contain the first observation (i.e. opening quotes,...).
#' 
#' @return A \code{data.table} or an \code{xts} object containing the aggregated quote data.
#' 
#' @author Jonathan Cornelissen, Kris Boudt, Onno Kleen, and Emil Sjoerup.
#' @keywords data manipulation
#' 
#' @examples
#' # Aggregate quote data to the 30 second frequency
#' qDataAggregated <- aggregateQuotes(sampleQData, alignBy = "seconds", alignPeriod = 30)
#' qDataAggregated # Show the aggregated data
#' @export
aggregateQuotes <- function(qData, alignBy = "minutes", alignPeriod = 5, marketOpen = "09:30:00", marketClose = "16:00:00", tz = NULL) {
  .I <- .N <- N <- DATE <- BID <- OFR <- BIDSIZ <- OFRSIZ <- DT <- FIRST_DT <- DT_ROUND <-LAST_DT <- SYMBOL <- NULL
  nm <- toupper(colnames(qData))
  if (!("SYMBOL" %in% nm)) {
    if(is.data.table(qData)){
      qData[, SYMBOL := "UKNOWN"]
    } else {
      qData <- cbind(qData, SYMBOL = 'UNKNOWN')
    }
  }
  qData <- checkColumnNames(qData)
  checkqData(qData)
  if (alignBy == "milliseconds") {
    alignBy <- "secs"
    alignPeriod <- alignPeriod / 1000
  }
  if(alignBy == "secs" | alignBy == "seconds"){
    scaleFactor <- alignPeriod
  }
  if(alignBy == "mins" | alignBy == "minutes"){
    scaleFactor <- alignPeriod * 60
  }
  if(alignBy == "hours"){
    scaleFactor <- alignPeriod * 60 * 60
  }
  inputWasXts <- FALSE
  if (!is.data.table(qData)) {
    if (is.xts(qData)) {
      nm <- c("DT", nm)
      qData <- as.data.table(qData)
      qData <- setnames(qData , old = "index", new = "DT")
      for (col in names(qData)[-1]) {
        set(qData, j = col, value = as.character(qData[[col]]))
      }
      qData[, `:=`(BID = as.numeric(BID), BIDSIZ = as.numeric(BIDSIZ), OFR = as.numeric(OFR), OFRSIZ = as.numeric(OFRSIZ))]
      inputWasXts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (!("DT" %in% colnames(qData))) {
      stop("Data.table neeeds DT column.")
    }
  }

  
  
  timeZone <- format(qData$DT[1], format = "%Z")
  if(is.null(timeZone) || timeZone == ""){
    if(is.null(tz)){
      tz <- "UTC"
    }
    if(!("POSIXct" %in% class(qData$DT))) qData[, DT := as.POSIXct(format(DT, digits = 20, nsmall = 20), tz = tz)]
  } else {
    tz <- timeZone
  }
  setkey(qData, DT) # The below code MAY fail with data with unordered DT column. Also setkey inceases speed of grouping
  # Convert DT to numeric. This is much faster than dealing with the strings (POSIXct)
  qData[, DT := as.numeric(DT, tz = tz)]
  # Calculate the date in the data
  qData[, DATE := as.Date(floor(as.numeric(DT, tz = tz) / 86400), origin = "1970-01-01", tz = tz)]
  # extract a vector of dates
  obsPerDay <- qData[, .N, by = list(DATE, SYMBOL)]
  
  ## Find the opening times of each of the days as numerics.
  marketOpenNumeric <- as.numeric(as.POSIXct(paste(obsPerDay$DATE, marketOpen), format = "%Y-%m-%d %H:%M:%OS", tz = tz), tz = tz)
  marketCloseNumeric <- as.numeric(as.POSIXct(paste(obsPerDay$DATE, marketClose), format = "%Y-%m-%d %H:%M:%OS", tz = tz), tz =tz)
  
  
  ## Here we make sure that we can correctly handle times that happen before midnight in the corrected timestamps from the flag if statements
  if(NROW(marketOpenNumeric) != NROW(obsPerDay)){
    if(NROW(marketOpenNumeric) < NROW(obsPerDay)){ ## Here we add entries
      marketOpenNumeric <- rep(marketOpenNumeric, NROW(obsPerDay$N))[1:NROW(obsPerDay)]
      marketCloseNumeric <- rep(marketCloseNumeric, NROW(obsPerDay$N))[1:NROW(obsPerDay)]
    } else {
      stop("unknown error occured in aggregateQuotes")
    }
    
  }
  
  # Subset observations that does not fall between their respective market opening and market closing times.
  qData <- qData[between(DT, rep(marketOpenNumeric, obsPerDay$N), rep(marketCloseNumeric, obsPerDay$N))]
  obsPerDay <- qData[, .N, by = list(DATE, SYMBOL)]
  ## Here (AGAIN!) we make sure that we can correctly handle if we deleted a day in the subsetting step above
  if(NROW(marketOpenNumeric) != NROW(obsPerDay)){
    if(NROW(marketOpenNumeric) > NROW(obsPerDay)){ ## Here we delete entries
      marketOpenNumeric <- rep(marketOpenNumeric, NROW(obsPerDay$N))[1:NROW(obsPerDay)]
      marketCloseNumeric <- rep(marketCloseNumeric, NROW(obsPerDay$N))[1:NROW(obsPerDay)]
    } else {
      stop("unknown error occured in aggregateQuotes")
    }
    
    
  }
  
  # Find the first observation per day.  
  qData[, FIRST_DT := min(DT), by = "DATE"]
  # Use Dirks answer here: https://stackoverflow.com/a/42498175 to round the timestamps to the latest scaleFactor
  qData[, DT_ROUND := fifelse(DT == FIRST_DT,
                             floor(DT/scaleFactor) * scaleFactor,
                             ceiling(DT/scaleFactor) * scaleFactor)]
  
  qData[, LAST_DT := max(DT), by = list(SYMBOL, DT_ROUND)]
  qData[, OFRSIZ := sum(OFRSIZ), by = list(SYMBOL, DT_ROUND)]
  qData[, BIDSIZ := sum(BIDSIZ), by = list(SYMBOL, DT_ROUND)]
  
  # Create the first observation each day.
  qData_open <- qData[qData[DT == FIRST_DT, .I[1], list(SYMBOL, DATE)]$V1, ]
  qData_open[, DT := floor(DT/86400) * 86400 + marketOpenNumeric %% 86400]
  
  # Take the last observation of each group of LAST_DT 
  qData <- qData[qData[DT == LAST_DT, .I[.N], by = list(SYMBOL, LAST_DT)]$V1][, DT := DT_ROUND] ## Make sure we only take the last observation
  
  # due to rounding there may be an observation that is refered to the opening time
  qData <- qData[!(DT %in% qData_open$DT)]
  # Merge the opening observation onto the rest of the observations.
  qData <- merge(qData, qData_open, all = TRUE)
  
  qData[, DT := as.POSIXct(DT, origin = as.POSIXct("1970-01-01", tz = "UTC"), tz = tz)]
  qData <- qData[, nm, with = FALSE]
  
  if (inputWasXts) {
    return(xts(as.matrix(qData[, -c("DT")]), order.by = qData$DT, tzone = tz))
  } else {
    return(qData[])
  }
}

#' Aggregate a \code{data.table} or \code{xts} object containing trades data´
#' 
#' @description Aggregate tick-by-tick trade data and return a time series as a \code{data.table} or \code{xts} object where first observation is always the opening price
#' and subsequent observations are the closing prices over the interval. This function accepts arbitrary number of symbols over an arbitrary number of days.
#' 
#' @param tData \code{data.table} or \code{xts} object to be aggregated, containing the intraday price series of a stock for possibly multiple days.
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. Possible values are: \code{"secs"}, \code{"seconds"}, \code{"mins"}, \code{"minutes"}, \code{"hours"}.
#' To aggregate based on a 5 minute frequency, set \code{alignPeriod = 5} and \code{alignBy = "minutes"}.
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. For example, to aggregate
#' based on a 5 minute frequency, set \code{alignPeriod = 5} and \code{alignBy = "minutes"}.
#' @param marketOpen the market opening time, by default: \code{marketOpen = "09:30:00"}.
#' @param marketClose the market closing time, by default: \code{marketClose = "16:00:00"}.
#' @param tz fallback time zone used in case we we are unable to identify the timezone of the data, by default: \code{tz = NULL}. We attempt to extract the timezone from the DT column (or index) of the data, which may fail. 
#' In case of failure we use \code{tz} if specified, and if it is not specified, we use \code{"UTC"}
#' @details The time stamps of the new time series are the closing times and/or days of the intervals. 
#' 
#' The output \code{"PRICE"} column is constructed using previous tick aggregation.
#' 
#' The variable \code{"SIZE"} is aggregated by taking the sum over each interval.
#' 
#' The variable \code{"VWPRICE"} is the aggregated price weighted by volume.
#' 
#' The time stamps of the new time series are the closing times of the intervals. 
#' 
#' In case of previous tick aggregation or \code{alignBy = "seconds"/"minutes"/"hours"},
#' the element of the returned series with e.g. time stamp 09:35:00 contains 
#' the last observation up to that point, including the value at 09:35:00 itself.
#' 
#' @return A \code{data.table} or \code{xts} object containing the aggregated time series.
#' 
#' @author Jonathan Cornelissen, Kris Boudt, Onno Kleen, and Emil Sjoerup.
#' @keywords data manipulation
#' 
#' @examples 
#' # Aggregate trade data to 5 minute frequency
#' tDataAggregated <- aggregateTrades(sampleTData, alignBy = "minutes", alignPeriod = 5)
#' tDataAggregated
#' @export
aggregateTrades <- function(tData, alignBy = "minutes", alignPeriod = 5, marketOpen = "09:30:00", marketClose = "16:00:00", tz = NULL) {
  .I <- .N <- N <- DATE <- SIZE <- DT <- FIRST_DT <- DT_ROUND <- LAST_DT <- SYMBOL <- PRICE <- VWPRICE <- SIZETPRICE <- SIZESUM <- NULL
  nm <- c(toupper(colnames(tData)), "VWPRICE")
  if (!("SYMBOL" %in% nm)) {
    if(is.data.table(tData)){
      tData[, SYMBOL := "UKNOWN"]
    } else {
      tData <- cbind(tData, SYMBOL = 'UNKNOWN')
    }
  }

  tData <- checkColumnNames(tData)
  checktData(tData)
  
  if (alignBy == "milliseconds") {
    on_true <- "milliseconds"
    alignBy <- "secs"
    alignPeriod <- alignPeriod / 1000
  }
  if(alignBy == "secs" | alignBy == "seconds"){
    scaleFactor <- alignPeriod
  }
  if(alignBy == "mins" | alignBy == "minutes"){
    scaleFactor <- alignPeriod * 60
  }
  if(alignBy == "hours"){
    scaleFactor <- alignPeriod * 60 * 60
  }
  
  inputWasXts <- FALSE
  if (!is.data.table(tData)) {
    if (is.xts(tData)) {
      nm <- c("DT", nm)
      tData <- as.data.table(tData)
      tData <- setnames(tData , old = "index", new = "DT")
      for (col in names(tData)[-1]) {
        set(tData, j = col, value = as.character(tData[[col]]))
      }
      tData[, `:=` (SIZE = as.numeric(SIZE), PRICE = as.numeric(PRICE))]
      inputWasXts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (!("DT" %in% colnames(tData))) {
      stop("Data.table neeeds DT column (date-time).")
    }
  }
  
  timeZone <- format(tData$DT[1], format = "%Z")
  if(is.null(timeZone) || timeZone == ""){
    if(is.null(tz)){
      tz <- "UTC"
    }
    if(!("POSIXct" %in% class(tData$DT))) tData[, DT := as.POSIXct(format(DT, digits = 20, nsmall = 20), tz = tz)]
  } else {
    tz <- timeZone
  }
  
  setkey(tData, DT) # The below code MAY fail with data with unordered DT column. Also setkey inceases speed of grouping
  # Convert DT to numeric. This is much faster than dealing with the strings (POSIXct)
  tData[, DT := as.numeric(DT, tz = tz)]
  # Calculate the date in the data
  tData[, DATE := as.Date(floor(DT / 86400), origin = "1970-01-01", tz = tz)]
  # extract a vector of dates
  obsPerDay <- tData[, .N, by = list(DATE, SYMBOL)]
  
  ## Find the opening times of each of the days as numerics.
  marketOpenNumeric <- as.numeric(as.POSIXct(paste(obsPerDay$DATE, marketOpen), format = "%Y-%m-%d %H:%M:%OS", tz = tz), tz = tz) 
  marketCloseNumeric <- as.numeric(as.POSIXct(paste(obsPerDay$DATE, marketClose), format = "%Y-%m-%d %H:%M:%OS", tz = tz), tz =tz)
  
  ## Here we make sure that we can correctly handle times that happen before midnight in the corrected timestamps from the flag if statements
  if(NROW(marketOpenNumeric) != NROW(obsPerDay)){
    if(NROW(marketOpenNumeric) < NROW(obsPerDay)){ ## Here we add entries
      marketOpenNumeric <- rep(marketOpenNumeric, NROW(obsPerDay))[1:NROW(obsPerDay)]
      marketCloseNumeric <- rep(marketCloseNumeric, NROW(obsPerDay))[1:NROW(obsPerDay)]
    } else {
      stop("unknown error occured in aggregateTrades")
    }
    
  }
  
  # Subset observations that does not fall between their respective market opening and market closing times.
  tData <- tData[between(DT, rep(marketOpenNumeric, obsPerDay$N), rep(marketCloseNumeric, obsPerDay$N))]
  # Find observations per day again
  obsPerDay <- tData[, .N, by = list(DATE, SYMBOL)]
  
  ## Here (AGAIN!) we make sure that we can correctly handle if we deleted a day in the subsetting step above
  if(NROW(marketOpenNumeric) != NROW(obsPerDay)){
    if(NROW(marketOpenNumeric) > NROW(obsPerDay)){ ## Here we delete entries
      marketOpenNumeric <- rep(marketOpenNumeric, NROW(obsPerDay))[1:NROW(obsPerDay)]
      marketCloseNumeric <- rep(marketCloseNumeric, NROW(obsPerDay))[1:NROW(obsPerDay)]
    } else {
      stop("unknown error occured in aggregateTrades")
    }
  }
  
  # Find the first observation per day.  
  tData[, FIRST_DT := min(DT), by = "DATE"]
  # Use Dirks answer here: https://stackoverflow.com/a/42498175 to round the timestamps to the latest scaleFactor
  tData[, DT_ROUND := fifelse(DT == FIRST_DT,
                             floor(DT/scaleFactor) * scaleFactor,
                             ceiling(DT/scaleFactor) * scaleFactor)]
  tData[, LAST_DT := max(DT), by = list(SYMBOL, DT_ROUND)]
  tData[, SIZETPRICE := SIZE * PRICE]
  tData[, SIZESUM := sum(SIZE), by = list(SYMBOL, DT_ROUND)]
  tData[, VWPRICE := sum(SIZETPRICE/SIZESUM), by = list(SYMBOL, DT_ROUND)]
  tData[, SIZE := SIZESUM, by = list(SYMBOL)]
  
  # Create the first observation each day.
  tData_open <- tData[tData[DT == FIRST_DT, .I[1], by = list(SYMBOL, DATE)]$V1, ]
  tData_open[, DT := floor(DT/86400) * 86400 + marketOpenNumeric %% 86400]
  
  # Take the last observation of each group of LAST_DT 
  tData <- tData[tData[DT == LAST_DT, .I[.N], by = list(SYMBOL, LAST_DT)]$V1][, DT := DT_ROUND] ## Make sure we only take the last observation
  
  # due to rounding there may be an observation that is refered to the opening time
  tData <- tData[!(DT %in% tData_open$DT)]
  # Merge the opening observation onto the rest of the observations.
  tData <- merge(tData, tData_open, all = TRUE)
  
  tData[, DT := as.POSIXct(DT, origin = as.POSIXct("1970-01-01", tz = "UTC"), tz = tz)]
  tData <- tData[, nm, with = FALSE]
  
  
  if (inputWasXts) {
    return(xts(as.matrix(tData[, -c("DT")]), order.by = tData$DT, tzone = tz))
  } else {
    return(tData[])
  }
}

#' Retain only data from the stock exchange with the highest trading volume
#' 
#' @description Filters raw trade data and return only data that stems from the exchange with the highest
#'  value for the variable \code{"SIZE"}, i.e. the highest trade volume.
#' @param tData an \code{xts} object with at least a column \code{"EX"} 
#' indicating the exchange symbol and \code{"SIZE"}
#' indicating the trade volume. 
#' @param printExchange indicates whether the chosen exchange is printed on the console, default is \code{TRUE}.
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
#' @return \code{data.table} or \code{xts} object depending on input.
#' 
#' @examples autoSelectExchangeTrades(sampleTDataRaw)
#' 
#' @author Jonathan Cornelissen, Kris Boudt, Onno Kleen, and Emil Sjoerup.
#' 
#' @keywords cleaning
#' @export
autoSelectExchangeTrades <- function(tData, printExchange = TRUE) {
  MAXSIZESUM <- SYMBOL <- EX <- DATE <- SIZE <- DT <- SIZESUM <- NULL
  
  exchanges <- c("T", "Q", "A", "P", "B", "C", "N", "D", "X", "I", "M", "W", "Z")
  exchangenames <- c("NASDAQ", "NASDAQ", "AMEX", "ARCA", "Boston", "NSX", "NYSE", "NASD ADF and TRF", "Philadelphia", "ISE", "Chicago", "CBOE", "BATS")
  
  tData <- checkColumnNames(tData)
  checktData(tData)
  
  inputWasXts <- FALSE
  if (!is.data.table(tData)) {
    if (is.xts(tData)) {
      tData <- as.data.table(tData)
      tData <- setnames(tData , old = "index", new = "DT")
      for (col in names(tData)[-1]) {
        set(tData, j = col, value = as.character(tData[[col]]))
      }
      
      tData[, SIZE := as.numeric(SIZE)]
      
      inputWasXts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (!("DT" %in% colnames(tData))) {
      stop("Data.table neeeds DT column (date-time ).")
    }
  } 
  
  tData[, SIZESUM := sum(SIZE), by = list(EX,SYMBOL)]
  tData[, MAXSIZESUM := max(SIZESUM), by = "SYMBOL"]
  tData <- tData[SIZESUM == MAXSIZESUM][, -c("SIZESUM", "MAXSIZESUM")]
  
  if (printExchange) {
    exch <- unique(tData$EX)
    namechosen <- exchangenames[exch == exchanges]  
    print(paste("The ", namechosen, "is the exchange with the highest volume."))
  }
  
  if (inputWasXts) {
    return(xts(as.matrix(tData[, -c("DT")]), order.by = tData$DT, tzone = tzone(tData$DT)))
  } else {
    return(tData[])
  }
}


#' Retain only data from the stock exchange with the highest volume
#' 
#' @description Filters raw quote data and return only data that stems from the exchange with the highest
#' value for the sum of \code{"BIDSIZ"} and \code{"OFRSIZ"}, i.e. the highest quote volume.
#' 
#' @param qData a \code{data.table} or \code{xts} object with at least a column \code{"EX"}, indicating the exchange symbol 
#' and columns \code{"BIDSIZ"} and \code{"OFRSIZ"}, indicating 
#' the volume available at the bid and ask respectively.
#' @param printExchange indicates whether the chosen exchange is printed on the console, default is \code{TRUE}.
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
#' @return \code{data.table} or \code{xts} object depending on input.
#' 
#' @examples 
#' autoSelectExchangeQuotes(sampleQDataRaw)
#'
#' @author Jonathan Cornelissen, Kris Boudt, Onno Kleen, and Emil Sjoerup.
#' 
#' @keywords cleaning
#' @export
autoSelectExchangeQuotes <- function(qData, printExchange = TRUE) {
  
  BIDSIZ <- OFRSIZ <- DT <- EX <- SUMVOL <- NULL
  
  exchanges = c("T","Q","A","P","B","C","N","D","X","I","M","W","Z")
  exchangenames = c("NASDAQ","NASDAQ","AMEX","ARCA","Boston","NSX","NYSE","NASD ADF and TRF","Philadelphia","ISE","Chicago","CBOE","BATS")
  
  qData <- checkColumnNames(qData)
  checkqData(qData)
  
  inputWasXts <- FALSE
  if (!is.data.table(qData)) {
    if (is.xts(qData)) {
      qData <- as.data.table(qData)
      qData <- setnames(qData , old = "index", new = "DT")
      for (col in names(qData)[-1]) {
        set(qData, j = col, value = as.character(qData[[col]]))
      }
      
      qData[, `:=` (BIDSIZ = as.numeric(BIDSIZ), OFRSIZ = as.numeric(OFRSIZ))]
      inputWasXts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (!("DT" %in% colnames(qData))) {
      stop("Data.table neeeds DT column.")
    }
  }
  
  if (length(unique(qData$SYMBOL)) > 1) {
    stop("Please provide only one symbol at a time.")
  }
  
  qData <- qData[EX == qData[, list(SUMVOL = sum(BIDSIZ + OFRSIZ)), by = "EX"][SUMVOL == max(SUMVOL), EX]]
  
  if (printExchange) {
    exch <- unique(qData$EX)
    namechosen <- exchangenames[exch == exchanges]
    print(paste("The ", namechosen, "is the exchange with the highest volume."))
  }
  
  if (inputWasXts) {
    return(xts(as.matrix(qData[, -c("DT")]), order.by = qData$DT, tzone = tzone(qData$DT)))
  } else {
    return(qData[])
  }
}


#' Extract data from an \code{xts} object for the exchange hours only
#' 
#' @description Filter raw trade data such and return only data between market close and market open. 
#' By default, \code{marketOpen = "09:30:00"} and \code{marketClose = "16:00:00"} (see Brownlees and Gallo (2006) for more information on good choices for these arguments).
#' 
#' @param data a \code{data.table} or \code{xts} object containing the time series data. 
#' Multiple days of input are allowed.
#' @param marketOpen character in the format of \code{"HH:MM:SS"},
#' specifying the opening time of the exchange(s).
#' @param marketClose character in the format of \code{"HH:MM:SS"},
#' specifying the closing time of the exchange(s).
#' @param tz fallback time zone used in case we we are unable to identify the timezone of the data, by default: \code{tz = NULL}. We attempt to extract the timezone from the DT column of the data, which may fail. 
#' In case of failure we use \code{tz} if specified, and if it is not specified, we use \code{"UTC"}
#' 
#' @return \code{xts} or \code{data.table} object depending on input.
#'
#' @references Brownlees, C. T. and Gallo, G. M. (2006). Financial econometric analysis at ultra-high frequency: Data handling concerns. Computational Statistics & Data Analysis, 51, pages 2232-2245.
#' @author Jonathan Cornelissen, Kris Boudt, Onno Kleen, and Emil Sjoerup.
#' @examples 
#' exchangeHoursOnly(sampleTDataRaw)
#' @keywords cleaning
#' @importFrom xts tzone
#' @export
exchangeHoursOnly <- function(data, marketOpen = "09:30:00", marketClose = "16:00:00", tz = NULL) {
  .N <- N <- DATE <- DT <- NULL # needed for data table (otherwise notes pop up in check())
  data <- checkColumnNames(data)
  nm <- toupper(colnames(data))
  
  inputWasXts <- FALSE
  if (!is.data.table(data)) {
    if (is.xts(data)) {
      data <- setnames(as.data.table(data), old = "index", new = "DT")
      inputWasXts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (!("DT" %in% colnames(data))) {
      stop("Data.table neeeds DT column.")
    }
  }
  
  timeZone <- format(data$DT[1], format = "%Z")
  if(is.null(timeZone) || timeZone == ""){
    if(is.null(tz)){
      tz <- "UTC"
    }
    if(!("POSIXct" %in% class(data$DT))) data[, DT := as.POSIXct(format(DT, digits = 20, nsmall = 20), tz = tz)]
  } else {
    tz <- timeZone
  }
  setkey(data, DT) # The below code MAY fail with data with unordered DT column. Also setkey inceases speed of grouping
  
  obsPerDay <- data[, .N, by = list(DATE = as.Date(floor(as.numeric(DT, tz = tz) / 86400), origin = "1970-01-01", tz = tz))] # Dates and observations per day
  
  marketOpenNumeric <- as.numeric(as.POSIXct(paste(obsPerDay[[1]], marketOpen), format = "%Y-%m-%d %H:%M:%OS", tz = tz), tz = tz)
  marketCloseNumeric <- as.numeric(as.POSIXct(paste(obsPerDay[[1]], marketClose), format = "%Y-%m-%d %H:%M:%OS", tz = tz), tz =tz)
  # marketCloseNumeric <- marketCloseNumeric + days * 86400 # 60 * 60 * 24
  
  ## Here we make sure that we can correctly handle times that happen before midnight in the corrected timestamps from the flag if statements
  if(length(marketOpenNumeric) != nrow(obsPerDay)){
    if(length(marketOpenNumeric) < nrow(obsPerDay)){ ## Here we add entries
      marketOpenNumeric <- rep(marketOpenNumeric, nrow(obsPerDay))[1:nrow(obsPerDay)]
      marketCloseNumeric <- rep(marketCloseNumeric, nrow(obsPerDay))[1:nrow(obsPerDay)]
    } else { ## This shouldn't be possible but we check so we can make an error
      stop("unknown error occured in exchangeHoursOnly")
    }
    
  }
  
  # Subset observations that does not fall between their respective market opening and market closing times.
  data <- data[between(DT, rep(marketOpenNumeric, obsPerDay[[2]]), rep(marketCloseNumeric, obsPerDay[[2]])), nm, with = FALSE]
  if (inputWasXts) {
    return(xts(as.matrix(data[, -c("DT")]), order.by = data$DT, tzone = tzone(data$DT)))
  } else {
    return(data[])
  }
}





# #' Get price column(s) from a timeseries
# #' @description Will attempt to locate price column(s) from a time series with rational defaults.
# #' 
# #' @param x A data object with columns containing data to be extracted
# #' @param symbol text string containing the symbol to extract
# #' @param prefer preference for any particular type of price, see Details
# #' 
# #' @details  May be subset by symbol and preference.
# #'  \code{prefer} Preference will be for any commonly used financial time series price description,
# #'  e.g. 'trade', 'close', 'bid', 'ask' with specific tests and matching for types and column names
# #'  currently supported in R, but a default grep match will be performed if one of the supported types doesn't match.
# #'
# #' The functionality was taken from the quantmod-package
# getPrice <- function (x, symbol = NULL, prefer = NULL) {
#   # first subset on symbol, if present
#   if (!is.null(symbol)) {
#     loc <- grep(symbol, colnames(x))
#     if (!identical(loc, integer(0))) {
#       x <- x[, loc]
#     } else {
#       stop(paste("Subscript out of bounds: no column name containing ",symbol,"."))
#     }
#   }
#   if (is.null(prefer)) {
#     # default to trying Price, then Trade, then Close
#     if(has.Price(x)) prefer = 'price'
#     else if(has.Trade(x)) prefer = 'trade'
#     else if(has.Cl(x))    prefer = 'close'
#     else stop("Subscript out of bounds, no price was discernible from the data.")
#   }else {
#     loc <- NULL
#     switch(prefer,
#            Op =, open =, Open = { loc <- has.Op(x,which=TRUE) },
#            Hi =, high =, High = { loc <- has.Hi(x,which=TRUE) },
#            Lo =, low =, Low = { loc <- has.Lo(x,which=TRUE) },
#            Cl =, close =, Close = { loc <- has.Cl(x,which=TRUE) },
#            Bid =, bid = { loc <- has.Bid(x,which=TRUE) },
#            Ask =, ask =, Offer =, offer = { loc <- has.Ask(x,which=TRUE) },
#            Mid =, mid =, Midpoint =, midpoint = { loc <- has.Mid(x,which=TRUE) },
#            Trade =, trade = { loc <- has.Trade(x,which=TRUE) },
#            Price =, price = { loc <- has.Price(x,which=TRUE) },
#            {loc <- grep(prefer,colnames(x))}
#     )
#     if (!identical(loc, integer(0))) {
#       return(x[, loc])
#     } else {
#       stop("Subscript out of bounds, no price was discernible from the data.")
#     }
#   }
# }

#' Compute log returns
#' @description Convenience function to calculate log-returns, also used extensively internally.
#' Accepts \code{xts} and \code{matrix}-like objects. If you use this with a \code{data.table} object, remember to not pass the \code{DT} column.
#' \deqn{
#' \mbox{log return}_t =  (\log(\mbox{PRICE}_{t})-\log(\mbox{PRICE}_{t-1})).
#' }
#' 
#' @param ts a possibly multivariate matrix-like object containing prices in levels. If \code{ts} is an \code{xts} object, we return an \code{xts} object. Other types will result in a \code{matrix}
#' 
#' @return Depending on input, either a \code{matrix} or an \code{xts} object containing the log returns.
#' 
#' @details Note: the first (row of) observation(s) is set to zero.
#' 
#' @author Jonathan Cornelissen, Kris Boudt, and Emil Sjoerup
#' @importFrom xts xts 
#' @importFrom zoo index
#' @export
makeReturns <- function(ts) {
  inputWasXts <- is.xts(ts)
  l <- NROW(ts) # allows for numeric()s
  D <- NCOL(ts) # allows for numeric()s
  
  col_names <- colnames(ts)
  x <- log(matrix(as.numeric(ts), nrow = l))
  
  x[(2:l), ] <- x[(2:l), ] - x[(1:(l - 1)), ]
  x[1, ] <- rep(0, D)
  if(inputWasXts){
    x <- xts(x, order.by = index(ts))
  }
  colnames(x) <- col_names
  return(x[])
}

#' Match trade and quote data
#' @description Match the trades and quotes of the input data. All trades are retained and the latest bids and offers are retained,
#'  while 'old' quotes are discarded.
#' 
#' @param tData \code{data.table} or xts-object containing the trade data possibly with multiple symbols and over multiple days possible
#' @param qData \code{data.table} or xts-object containing the quote data possibly with multiple symbols and over multiple days possible
#' @param lagQuotes numeric, number of seconds the quotes are registered faster than
#' the trades (should be round and positive). Default is 0. For older datasets, i.e. before 2010, it may be a good idea to set this to e.g. 2. See Vergote (2005)
#' @param BFM a logical determining whether to conduct 'Backwards - Forwards matching' of trades and quotes.
#' The algorithm tries to match trades that fall outside the bid - ask and first tries to match a small window forwards and if this fails, it tries to match backwards in a bigger window.
#' The small window is a tolerance for inaccuracies in the timestamps of bids and asks. The backwards window allow for matching of late reported trades. I.e. block trades.
#' @param backwardsWindow a numeric denoting the length of the backwards window used when \code{BFM = TRUE}. Default is 3600, corresponding to one hour.
#' @param forwardsWindow a numeric denoting the length of the forwards window used when \code{BFM = TRUE}. Default is 0.5, corresponding to one half second.
#' @param plot a logical denoting whether to visualize the forwards, backwards, and unmatched trades in a plot.
#' @param ... used internally. Don't set this parameter
#' 
#' @return Depending on the input data type, we return either a \code{data.table} or an \code{xts} object containing the matched trade and quote data.
#' When using the BFM algorithm, a report of the matched and unmatched trades are also returned (This is omitted when we call this function from the \code{\link{tradesCleanupUsingQuotes}} function).
#' 
#' @references
#' 
#' Vergote, O. (2005). How to match trades and quotes for NYSE stocks? K.U.Leuven working paper.
#'
#' Christensen, K., Oomen, R. C. A., Podolskij, M. (2014): Fact or Friction: Jumps at ultra high frequency. \emph{Journal of Financial Economics}, 144, 576-599
#' 
#' @author Jonathan Cornelissen, Kris Boudt, Onno Kleen, and Emil Sjoerup.
#' 
#' @keywords data manipulation
#' 
#' @examples 
#' # Multi-day input allowed
#' tqData <- matchTradesQuotes(sampleTData, sampleQData)
#' # Show output
#' tqData
#' @importFrom xts tzone<- tzone
#' @export
matchTradesQuotes <- function(tData, qData, lagQuotes = 0, BFM = FALSE, backwardsWindow = 3600, forwardsWindow = 0.5, plot = FALSE, ...) {
  
  PRICE <- BID <- OFR <- DATE <- DT <- FIRST_DT <- SYMBOL <- NULL
  
  tData <- checkColumnNames(tData)
  qData <- checkColumnNames(qData)
  checkqData(qData)
  checktData(tData)
  
  if (any(class(tData) != class(qData))) {
    stop("tData and qData should be of the same data type, either xts or data.table.")
  }
  
  inputWasXts <- FALSE
  if (!is.data.table(tData)) {
    if (is.xts(tData)) {
      tData <- as.data.table(tData)
      tData <- setnames(tData , old = "index", new = "DT")
      for (col in names(tData)[-1]) {
        set(tData, j = col, value = as.character(tData[[col]]))
      }
      
      tData[, PRICE := as.numeric(PRICE)]
      
      qData <- as.data.table(qData)
      qData <- setnames(qData , old = "index", new = "DT")
      for (col in names(qData)[-1]) {
        set(qData, j = col, value = as.character(qData[[col]]))
      }
      
      qData[, `:=`(OFR = as.numeric(OFR), BID = as.numeric(BID))]
      
      inputWasXts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (!("DT" %in% colnames(tData)) ) {
      stop("tData neeeds DT column.")
    }
  }
  
  if(format(qData$DT[1], format = "%Z") != format(tData$DT[1], format = "%Z")){
    stop("timezone of the trade data is not the same as the timezone of the quote data")
  }
  tz <- format(tData$DT[1], format = "%Z")
  
  setnames(qData, old = "EX", new = "QUOTEEX", skip_absent = TRUE)
  
  if(!BFM){ ## We DONT conduct a backwards- forwards matching search
    
    # qData <- copy(qData[c(TRUE, diff(BID)|diff(OFR))])[, DT := as.numeric(DT, tz = tz)]
    qData[, DT := as.numeric(DT, tz = tz)]
    qData[, DATE := floor(DT / 86400)]
    qData[, FIRST_DT := min(DT), by = "DATE"]
    # Make the adjustments to the quote timestamps.
    qData <- qData[, DT := fifelse(DT == FIRST_DT, DT, DT + lagQuotes)][,-c("FIRST_DT", "DATE")]
    qData[, DT := as.POSIXct(DT, tz = tz, origin = "1970-01-01")]
  
    
    setkey(tData, SYMBOL, DT)
    setkey(qData, SYMBOL, DT)
    tqData <- qData[tData, roll = TRUE, on = c("SYMBOL", "DT"), ]
    tqData[, DT := as.POSIXct(DT, tz = tz, origin = "1970-01-01")]
    
    if (inputWasXts) {
      return(xts(as.matrix(tqData[, -c("DT")]), order.by = tqData$DT, tzone = tz))
    } else {
      return(tqData[])
    }
    
  } else {
    opt <- list(onlyTQ = FALSE)
    options <- list(...)
    opt[names(options)] <- options
    onlyTQ <- opt$onlyTQ
    
    
    qData[, DT := as.numeric(DT, tz = tz)][, DATE := floor(DT / 86400)]
    qData[, DT := fifelse(DT == min(DT), DT, DT + lagQuotes)]
    tData[, DT := as.numeric(DT, tz = tz)][, DATE := floor(DT / 86400)]
    out <- list()
    i <- 1
    for (date in unique(qData$DATE)) {
      for (symbol in unique(qData$SYMBOL)) {
        if(onlyTQ){
          out[[i]] <- BFMalgorithm(tData[DATE == date & SYMBOL == symbol, ], qData[DATE == date & SYMBOL == symbol, ], backwardsWindow = backwardsWindow, forwardsWindow = forwardsWindow, plot = plot, tz = tz)$tqData
        } else {
          out[[i]] <- BFMalgorithm(tData[DATE == date & SYMBOL == symbol, ], qData[DATE == date & SYMBOL == symbol, ], backwardsWindow = backwardsWindow, forwardsWindow = forwardsWindow, plot = plot, tz = tz)
        }
        i <- i + 1
      }
    }
    out <- rbindlist(out)
    setkey(out, "DT")
    
    
    if (inputWasXts) {
      return(xts(as.matrix(out[, -c("DT")]), order.by = out$DT, tzone = tz))
    } else {
      return(out[])
    }
    
  }
  

  
  
}

#' Merge multiple quote entries with the same time stamp
#' 
#' @description Merge quote entries that have the same time stamp to a single one and returns an \code{xts} or a \code{data.table} object
#'  with unique time stamps only.
#' 
#' @param qData an \code{xts} object or \code{data.table} containing the time series data, with 
#' at least two columns named \code{BID} and \code{OFR} indicating the bid and ask price 
#' as well as two columns \code{BIDSIZ}, \code{OFRSIZ} indicating the number of round lots available at these 
#' prices. For \code{data.table} an additional column \code{DT} is necessary that stores the date/time information.
#' @param selection indicates how the bid and ask price for a certain time stamp
#' should be calculated in case of multiple observation for a certain time
#' stamp. By default, \code{selection = "median"}, and the median price is taken. Alternatively:
#' \itemize{
#' \item \code{selection = "max.volume"}: use the (bid/ask) price of the entry with
#' largest (bid/ask) volume.
#' \item \code{selection = "weighted.average"}: take the weighted average of all bid (ask) prices,
#' weighted by "BIDSIZ" ("OFRSIZ").
#' }
#' 
#' @return Depending on the input data type, we return either a \code{data.table} or an \code{xts} object containing the quote data which has been cleaned.
#' 
#' @author Jonathan Cornelissen, Kris Boudt, Onno Kleen, and Emil Sjoerup.
#' @keywords cleaning 
#' @export
mergeQuotesSameTimestamp <- function(qData, selection = "median") {
  BID <- OFR <- DT <- SYMBOL <- .SD <- BIDSIZ <- OFRSIZ <- MAXBID <- MAXOFR <- NULL 
  qData <- checkColumnNames(qData)
  checkqData(qData)
  
  condition <- selection == "median" | selection == "max.volume" | selection == "weighted.average"
  if (!condition) {
    stop(paste("Selection has to be \"median\", \"max.volume\" or \"weighted.average\" "))
  }
  
  inputWasXts <- FALSE
  if (!is.data.table(qData)) {
    if (is.xts(qData)) {
      qData <- as.data.table(qData)
      qData <- setnames(qData , old = "index", new = "DT")
      for (col in names(qData)[-1]) {
        set(qData, j = col, value = as.character(qData[[col]]))
      }
      qData[, `:=` (BID = as.numeric(BID), OFR = as.numeric(OFR), BIDSIZ = as.numeric(BIDSIZ), OFRSIZ = as.numeric(OFRSIZ))]      
      
      inputWasXts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (!("DT" %in% colnames(qData))) {
      stop("Data.table neeeds DT column (date-time).")
    }
  }
  # qData <- sampleQDataRaw
  # qData <- checkColumnNames(qData)
  # keep summed size columns
  keepCols <- colnames(qData)[!(colnames(qData) %in% c("DT", "SYMBOL","BID", "OFR","BIDSIZ", "OFRSIZ"))]
  keptData <- qData[, lapply(.SD, last), by = list(DT, SYMBOL)][, keepCols, with = FALSE]
  if (selection == "median") {
    qData <- qData[, list(BID = median(BID), OFR = median(OFR), OFRSIZ = sum(OFRSIZ), BIDSIZ = sum(BIDSIZ)), by = list(DT, SYMBOL)]
  }
  
  if (selection == "max.volume") {
    qData_size <- qData[, lapply(.SD, sum), by = list(DT, SYMBOL), .SDcols = c("BIDSIZ", "OFRSIZ")]
    qData <- qData[, MAXBID := max(BIDSIZ), by = list(DT, SYMBOL)][, MAXOFR := max(OFRSIZ), by = list(DT, SYMBOL)][
      , BIDSIZ := fifelse(BIDSIZ == MAXBID, 1, 0)][
      , OFRSIZ := fifelse(OFRSIZ == MAXOFR, 1, 0)][
      , BID := BID * BIDSIZ][
      , OFR := OFR * OFRSIZ][
      , BID := max(BID), by = list(DT,SYMBOL)][, OFR := max(OFR), by = list(DT, SYMBOL)][, -c("MAXBID", "MAXOFR", "BIDSIZ", "OFRSIZ")][
      , lapply(.SD, unique), by = list(DT, SYMBOL), .SDcols = c("BID", "OFR")]
    qData <- merge(qData, qData_size, by = c("DT", "SYMBOL"))
  }
  if (selection == "weighted.average") {
    qData_size <- qData[, lapply(.SD, sum), by = list(DT, SYMBOL), .SDcols = c("BIDSIZ", "OFRSIZ")]
    qData[, `:=`(BIDSIZ = as.numeric(BIDSIZ), OFRSIZ = as.numeric(OFRSIZ))]
    qData <- qData[, `:=` (BIDSIZ = BIDSIZ / sum(BIDSIZ), OFRSIZ = OFRSIZ / sum(OFRSIZ)), by = list(DT, SYMBOL)][
      , `:=` (BID = sum(BID * BIDSIZ), OFR = sum(OFR * OFRSIZ)), by = list(DT, SYMBOL)][
        , -c("BIDSIZ", "OFRSIZ")][
        , lapply(.SD, unique), by = list(DT, SYMBOL), .SDcols = c("BID", "OFR")]
    qData <- merge(qData, qData_size, by = c("DT", "SYMBOL"))
  }
  
  
  qData <- cbind(qData, keptData)
  setkey(qData, DT, SYMBOL)
  if (inputWasXts) {
    return(xts(as.matrix(qData[, -c("DT")]), order.by = qData$DT))
  } else {
    return(qData[])
  }
}

#' Merge multiple transactions with the same time stamp
#' 
#' @description Merge trade entries that have the same time stamp to a single one and returns an \code{xts} or a \code{data.table} object
#'  with unique time stamps only.
#' 
#' @param tData an \code{xts} object containing the time series data, with 
#' one column named \code{PRICE} indicating the transaction price 
#' and one column \code{SIZE} indicating the number of shares traded.
#' @param selection indicates how the price for a certain time stamp
#' should be calculated in case of multiple observation for a certain time
#' stamp. By default, \code{selection = "median"}, and the median price is taken. Alternatively:
#' \itemize{
#' \item \code{selection = "max.volume"}: use the price of the transaction with
#' largest volume.
#' \item \code{selection = "weighted.average"}: take the weighted average of all prices.
#' }
#' @note previously this function returned the mean of the size of the merged trades (pre version 0.7 and when not using max.volume as the criterion), now it returns the sum.
#' @return \code{data.table} or \code{xts} object depending on input.
#' 
#' @author Jonathan Cornelissen, Kris Boudt, Onno Kleen, and Emil Sjoerup.
#' @keywords cleaning
#' @export
mergeTradesSameTimestamp <- function(tData, selection = "median") {
  .N <- SIZE <- MAXSIZE <- PRICE <- DT <- SYMBOL <- .SD <- SIZE_WEIGHT <- NULL
  
  tData <- checkColumnNames(tData)
  checktData(tData)
  if (!any(colnames(tData) == "SIZE")) {
    stop("The argument tData should have a column SIZE indicating the number of shares traded. Could not find that column.")
  }
  
  condition <- selection == "median" | selection == "max.volume" | selection == "weighted.average"
  if (!condition) {
    stop(paste("Selection has to be \"median\", \"max.volume\" or \"weighted.average\" "))
  }
  
  inputWasXts <- FALSE
  if (!is.data.table(tData)) {
    if (is.xts(tData)) {
      tData <- setnames(as.data.table(tData)[, SIZE := as.numeric(as.character(SIZE))][, PRICE := as.numeric(as.character(PRICE))], 
                        old = "index", new = "DT")
      inputWasXts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (!("DT" %in% colnames(tData))) {
      tData <- tData[, SIZE := as.numeric(as.character(SIZE))][, PRICE := as.numeric(as.character(PRICE))]
      stop("Data.table neeeds DT column (date-time ).")
    }
  }
  
  keepCols <- colnames(tData)[!(colnames(tData) %in% c("DT", "SYMBOL", "PRICE", "SIZE"))]
  if (selection == "median") {
    keptData <- tData[, lapply(.SD, last), by = list(DT, SYMBOL)][, keepCols, with = FALSE]
    tData <- tData[, list(PRICE = median(PRICE), NUMTRADES = .N, SIZE = sum(SIZE)), by = list(DT, SYMBOL)]
    tData <- cbind(tData, keptData)
  }
  
  if (selection == "max.volume") {
    keptData <- tData[, lapply(.SD, last), by = list(DT, SYMBOL)][, keepCols, with = FALSE]
    tData[, SIZE := as.numeric(as.character(SIZE))]
    tData <- tData[, `:=` (MAXSIZE = max(SIZE), NUMTRADES = .N), by = list(DT, SYMBOL)]
    tData[, SIZE := fifelse(SIZE == MAXSIZE, 1, 0)]
    tData[, PRICE := PRICE * SIZE]
    tData[, PRICE := max(PRICE), by = "DT"]
    tData[, SIZE := MAXSIZE]
    tData <- tData[, -c("MAXSIZE")][, lapply(.SD, last), by = list(DT, SYMBOL), .SDcols = c("PRICE", "SIZE", "NUMTRADES")]
    tData <- cbind(tData, keptData)
  }
  if (selection == "weighted.average") {
    keptData <- tData[, lapply(.SD, last), by = list(DT, SYMBOL)][, keepCols, with = FALSE]
    tData[, SIZE := as.numeric(as.character(SIZE))]
    tData <- tData[, `:=` (SIZE_WEIGHT = SIZE / sum(SIZE), NUMTRADES = .N), by = list(DT, SYMBOL)]
    tData[, `:=` (PRICE = sum(PRICE * SIZE_WEIGHT)), by = list(DT, SYMBOL)]
    tData <- tData[, SIZE := sum(SIZE), by = list(DT, SYMBOL)][, lapply(.SD, last), by = list(DT, SYMBOL), .SDcols = c("PRICE", "SIZE", "NUMTRADES")]
    tData <- cbind(tData, keptData)
  }
    
  if (inputWasXts) {
    return(xts(as.matrix(tData[, -c("DT")]), order.by = tData$DT))
  } else {
    return(tData[])
  }
}


#' Delete the observations where the price is zero
#' 
#' @description Function deletes the observations where the price is zero.
#' 
#' @param tData an \code{xts} or \code{data.table} object at least containing a column \code{PRICE}. 
#' 
#' @return an \code{xts} or \code{data.table} object depending on input.
#' 
#' @author Jonathan Cornelissen and Kris Boudt.
#' @keywords cleaning
noZeroPrices <- function(tData) {
  PRICE <- NULL
  tData <- checkColumnNames(tData)
  checktData(tData)
  
  inputWasXts <- FALSE
  if (!is.data.table(tData)) {
    if (is.xts(tData)) {
      tData <- setnames(as.data.table(tData)[, PRICE := as.numeric(as.character(PRICE))], old = "index", new = "DT")
      inputWasXts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (!("DT" %in% colnames(tData))) {
      stop("Data.table neeeds DT column.")
    }
  }
  
  tData <- tData[PRICE != 0]
  
  if (inputWasXts) {
    return(xts(as.matrix(tData[, -c("DT")]), order.by = tData$DT))
  } else {
    return(tData[])
  }
}

#' Delete the observations where the bid or ask is zero
#' @description Function deletes the observations where the bid or ask is zero.
#' 
#' @param qData an \code{xts} or \code{data.table} object at least containing the columns \code{BID} and \code{OFR}.
#' 
#' @return \code{xts} object or \code{data.table} depending on type of input.
#' 
#' @author Jonathan Cornelissen and Kris Boudt.
#' @keywords cleaning
#' @export
noZeroQuotes <- function(qData) {
  BID <- OFR <- DT <- NULL
  qData <- checkColumnNames(qData)
  checkqData(qData)
  
  inputWasXts <- FALSE
  if (!is.data.table(qData)) {
    if (is.xts(qData)) {
      qData <- setnames(as.data.table(qData)[, BID := as.numeric(as.character(BID))][, OFR := as.numeric(as.character(OFR))], old = "index", new = "DT")
      inputWasXts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (!("DT" %in% colnames(qData))) {
      stop("Data.table neeeds DT column.")
    }
  }
  
  qData <- qData[BID != 0 & OFR != 0]
  
  if (inputWasXts) {
    return(xts(as.matrix(qData[, -c("DT")]), order.by = qData$DT))
  } else {
    return(qData[])
  }
}


#' Cleans quote data
#' 
#' @description This is a wrapper function for cleaning the quote data in the entire folder \code{dataSource}. 
#' The result is saved in the folder \code{dataDestination}. 
#' 
#' In case you supply the argument \code{qDataRaw}, the on-disk functionality is ignored
#' and the function returns the cleaned quotes as \code{xts} or \code{data.table} object (see examples).
#' 
#' The following cleaning functions are performed sequentially:
#' \code{\link{noZeroQuotes}}, \code{\link{exchangeHoursOnly}}, \code{\link{autoSelectExchangeQuotes}} or \code{\link{selectExchange}}, \code{\link{rmNegativeSpread}}, \code{\link{rmLargeSpread}}
#' \code{\link{mergeQuotesSameTimestamp}}, \code{\link{rmOutliersQuotes}}.
#' @param dataSource character indicating the folder in which the original data is stored.
#' @param dataDestination character indicating the folder in which the cleaned data is stored.
#' @param exchanges vector of stock exchange symbols for all data in dataSource, 
#' e.g. \code{exchanges = c("T","N")} retrieves all stock market data from both NYSE and NASDAQ.
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
#' }. The default value is \code{"auto"} which automatically selects the exchange for the stocks and days independently using the \code{\link{autoSelectExchangeQuotes}}
#' @param qDataRaw \code{xts} or \code{data.table} object containing raw quote data, possibly for multiple symbols over multiple days. This argument is \code{NULL} by default. 
#' Enabling it means the arguments \code{dataSource} and \code{dataDestination} will be ignored. (only advisable for small chunks of data)
#' @param report boolean and \code{TRUE} by default. In case it is true and we don't use the on-disk functionality, the function returns (also) a vector indicating how many quotes were deleted by each cleaning step.
#' @param selection argument to be passed on to the cleaning routine \code{\link{mergeQuotesSameTimestamp}}. The default is \code{"median"}.
#' @param maxi spreads which are greater than median spreads of the day times \code{maxi} are excluded.
#' @param window argument to be passed on to the cleaning routine \code{\link{rmOutliersQuotes}}. 
#' @param type argument to be passed on to the cleaning routine \code{\link{rmOutliersQuotes}}.
#' @param rmoutliersmaxi argument to be passed on to the cleaning routine \code{\link{rmOutliersQuotes}}.
#' @param marketOpen passed to \code{\link{exchangeHoursOnly}}. A character in the format of \code{"HH:MM:SS"},
#' specifying the starting hour, minute and second of an exchange.
#' @param marketClose passed to \code{\link{exchangeHoursOnly}}. A character in the format of \code{"HH:MM:SS"},
#' specifying the closing hour, minute and second of an exchange.
#' @param printExchange Argument passed to \code{\link{autoSelectExchangeQuotes}} indicates whether the chosen exchange is printed on the console, 
#' default is \code{TRUE}. This is only used when \code{exchanges} is \code{"auto"}
#' @param saveAsXTS indicates whether data should be saved in \code{xts} format instead of \code{data.table} when using on-disk functionality. \code{FALSE} by default, which means we save as \code{data.table}.
#' @param tz fallback time zone used in case we we are unable to identify the timezone of the data, by default: \code{tz = NULL}. With the non-disk functionality, we attempt to extract the timezone from the DT column (or index) of the data, which may fail. 
#' In case of failure we use \code{tz} if specified, and if it is not specified, we use \code{"UTC"}. 
#' In the on-disk functionality, if \code{tz} is not specified, the timezone used will be the system default.
#' 
#' @return The function converts every (compressed) csv (or rds) file in \code{dataSource} into multiple \code{xts} or \code{data.table} files.
#' 
#' In \code{dataDestination}, there will be one folder for each symbol containing .rds files with cleaned data stored either in \code{data.table} or \code{xts} format.
#' 
#' In case you supply the argument \code{qDataRaw}, the on-disk functionality is ignored
#' and the function returns a list with the cleaned quotes as an \code{xts} or \code{data.table} object depending on input (see examples).
#' 
#' @references
#' Barndorff-Nielsen, O. E., Hansen, P. R., Lunde, A., and Shephard, N. (2009). Realized kernels in practice: Trades and quotes. Econometrics Journal 12, C1-C32.
#' 
#' Brownlees, C.T. and Gallo, G.M. (2006). Financial econometric analysis at ultra-high frequency: Data handling concerns. Computational Statistics & Data Analysis, 51, pages 2232-2245.
#' 
#' Falkenberry, T.N. (2002). High frequency data filtering. Unpublished technical report.
#' 
#' @details 
#' Using the on-disk functionality with .csv.zip files which is the standard from the WRDS database
#' will write temporary files on your machine - we try to clean up after it, but cannot guarantee that 
#' there won't be files that slip through the crack if the permission settings on your machine does not match 
#' ours.
#' 
#' If the input \code{data.table} does not contain a \code{DT} column but it does contain \code{DATE} and \code{TIME_M} columns, we create the \code{DT} column by REFERENCE, altering the \code{data.table} that may be in the user's environment!
#' 
#' @author Jonathan Cornelissen, Kris Boudt, Onno Kleen, and Emil Sjoerup.
#' 
#' @examples
#' # Consider you have raw quote data for 1 stock for 2 days
#' head(sampleQDataRaw)
#' dim(sampleQDataRaw)
#' qDataAfterCleaning <- quotesCleanup(qDataRaw = sampleQDataRaw, exchanges = "N")
#' qDataAfterCleaning$report
#' dim(qDataAfterCleaning$qData)
#' 
#' # In case you have more data it is advised to use the on-disk functionality
#' # via "dataSource" and "dataDestination" arguments
#' 
#' @importFrom data.table fread setnames `%chin%`
#' @importFrom utils unzip
#' @keywords cleaning
#' @export
quotesCleanup <- function(dataSource = NULL, dataDestination = NULL, exchanges = "auto", qDataRaw = NULL, report = TRUE, 
                          selection = "median", maxi = 50, window = 50, type = "standard", marketOpen = "09:30:00", 
                          marketClose = "16:00:00", rmoutliersmaxi = 10, printExchange = TRUE, saveAsXTS = FALSE, tz = NULL) {
  
  .SD <- BID <- OFR <- DT <- SPREAD <- SPREAD_MEDIAN <- EX <- DATE <- BIDSIZ <- OFRSIZ <- TIME_M <- SYMBOL <- SYM_SUFFIX <- NULL

  
  if (is.null(qDataRaw)) {
    
    quotesfiles <- list.files(dataSource, recursive = TRUE, full.names = TRUE)[grepl("quotes", list.files(dataSource, recursive = TRUE))]
    for (ii in quotesfiles) {
      extension <- strsplit(ii, "\\.")[[1]]
      extension <- extension[length(extension)]
      if(extension == "zip") {
        tmp <- tempdir()
        unzip(ii, exdir = tmp)
        files <- list.files(tmp, full.names = TRUE, pattern = "csv")
        if(length(files) >= 1){
          readdata <- try(rbindlist(lapply(files, fread)), silent = TRUE)
        }
        # Try to Cleanup - we don't force it though!
        unlink(tmp, recursive = TRUE)
      } else if(extension %in% c("csv", "gz", "gzip", "bz2")){
        readdata <- try(fread(ii), silent = TRUE)
      } else if(extension %in% c("rds")){
        readdata <- try(readRDS(ii))
      } else {
        readdata <- try(fread(ii), silent = TRUE)
      }
      
      if(inherits(readdata, "try-error")){
        stop(paste("Error encountered while opening data, error message is:",readdata))
      }
      if(is.null(tz)) tz <- "UTC"
      if(colnames(readdata)[1] == "index"){ # The data was saved from an xts object
        readdata <- try(readdata[, DT := as.POSIXct(index, tz = tz, format = "%Y-%m-%dT%H:%M:%OS")])
      } else if ("DT" %in% colnames(readdata)){
        readdata <- try(readdata[, DT := as.POSIXct(DT, tz = tz, format = "%Y-%m-%dT%H:%M:%OS")])
      } else {
        readdata <- try(copy(readdata)[,`:=`(DT = as.POSIXct(paste(DATE, TIME_M), tz = tz, format = "%Y%m%d %H:%M:%OS"),
                                      DATE = NULL, TIME_M = NULL)], silent = TRUE)
      }
      
      if(inherits(readdata, "try-error")){
        stop(paste("Error encountered while creating a DT column, error message is:",readdata))
      }
      qData <- try(quotesCleanup(qDataRaw = readdata,
                                 selection = selection,
                                 exchanges = exchanges,
                                 maxi = maxi,
                                 window = window,
                                 type = type,
                                 rmoutliersmaxi = rmoutliersmaxi, tz = tz))$qData
      
      qData <- qData[, DATE := as.Date(DT, tz = tz)]
      qData <- split(qData, by = "DATE")
      
      try(dir.create(paste0(dataDestination, "/", strsplit(ii, "/")[[1]][1]), recursive = TRUE), silent = TRUE)
      for (jj in qData) {
        if (saveAsXTS) {
          df_result <- xts(as.matrix(jj[, -c("DT", "DATE")]), order.by = jj$DT)
        } else {
          df_result <- jj[, -c( "DATE")]
        }
        saveRDS(df_result, paste0(dataDestination, "/", strsplit(ii, "/")[[1]][1], "/", unique(as.Date(jj$DT, tz = tz)), ".rds"))
        # saveRDS(df_result, paste0(dataDestination, "/", strsplit(ii, "/")[[1]][1], "/", strsplit(strsplit(ii, "/")[[1]][2], ".zip")[1], ".rds"))
      }
    }
    gc()
  }
  
  if (!is.null(qDataRaw)) {
    
    qDataRaw <- checkColumnNames(qDataRaw)
    nm <- toupper(colnames(qDataRaw))
    checkqData(qDataRaw)
    if(!"DT" %in% nm && c("DATE", "TIME_M") %in% nm){
      if(is.null(tz)) tz <- "UTC"
      qDataRaw[, `:=`(DT = as.POSIXct(paste(DATE, TIME_M), tz = tz, format = "%Y%m%d %H:%M:%OS"),
                      DATE = NULL, TIME_M = NULL)]
      }
    if("SYM_SUFFIX" %in% nm){
      qDataRaw[, `:=`(SYMBOL = fifelse(SYM_SUFFIX == "" | is.na(SYM_SUFFIX), yes = SYMBOL, no = paste0(SYMBOL, "_", SYM_SUFFIX)), SYM_SUFFIX = NULL)]
    }
  
    inputWasXts <- FALSE
    if (!is.data.table(qDataRaw)) {
      if (is.xts(qDataRaw)) {
        qDataRaw <- as.data.table(qDataRaw)
        qDataRaw[, `:=`(BID = as.numeric(as.character(BID)), OFR = as.numeric(as.character(OFR)), 
                     BIDSIZ = as.numeric(as.character(BIDSIZ)), OFRSIZ = as.numeric(as.character(OFRSIZ)),
                     SYMBOL = as.character(SYMBOL), EX = as.character(EX))]
        
        setnames(qDataRaw, old = "index", new = "DT")
        inputWasXts <- TRUE
      } else {
        stop("Input has to be data.table or xts.")
      }
    } else {
      if (!("DT" %in% colnames(qDataRaw))) {
        stop("Data.table neeeds DT column.")
      }
    }
    
    timeZone <- format(qDataRaw$DT[1], format = "%Z")
    if(is.null(timeZone) || timeZone == ""){
      if(is.null(tz)){
        tz <- "UTC"
      }
      if(!("POSIXct" %in% class(qDataRaw$DT))){
        qDataRaw[, DT := as.POSIXct(format(DT, digits = 20, nsmall = 20), tz = tz)]
      }
    } else {
      tz <- timeZone
    }
    REPORT <- c(initialObservations = 0,
                removedFromZeroQuotes = 0, 
                removedOutsideExchangeHours = 0,
                removedFromSelectingExchange = 0,
                removedFromNegativeSpread = 0,
                removedFromLargeSpread = 0,
                removedFromMergeTimestamp = 0,
                removedOutliers = 0,
                finalObservations = 0)
    
    nm <- toupper(colnames(qDataRaw))
    
    setkey(qDataRaw, DT, SYMBOL)
    REPORT[1] <- dim(qDataRaw)[1] 
    qDataRaw <- qDataRaw[BID != 0 & OFR != 0]
    REPORT[2] <- dim(qDataRaw)[1] 
    qDataRaw <- exchangeHoursOnly(qDataRaw, marketOpen = marketOpen, marketClose = marketClose, tz = tz)
    REPORT[3] <- dim(qDataRaw)[1] 
    if("EX" %in% nm){
      if(all(exchanges != "auto")){
        qDataRaw <- qDataRaw[EX %chin% exchanges]
      } else if (exchanges == "auto"){
        qDataRaw <- qDataRaw[, autoSelectExchangeQuotes(.SD, printExchange = printExchange), .SDcols = nm,by = list(SYMBOL, DATE = as.Date(DT, tz = tz))][, nm, with = FALSE]
      }
    }
    REPORT[4] <- dim(qDataRaw)[1]
    qDataRaw <- qDataRaw[OFR>BID, list(DT, SPREAD = OFR - BID,  SPREAD_MEDIAN = median(OFR-BID), OFR, BID, BIDSIZ, OFRSIZ,EX),by = list(DATE = as.Date(DT, tz = tz), SYMBOL)]
    REPORT[5] <- dim(qDataRaw)[1] 
    qDataRaw <- qDataRaw[SPREAD < (SPREAD_MEDIAN * maxi)][, -c("SPREAD","SPREAD_MEDIAN")]
    REPORT[6] <- dim(qDataRaw)[1]
    qDataRaw <- mergeQuotesSameTimestamp(qData = qDataRaw, selection = selection)
    REPORT[7] <- dim(qDataRaw)[1]
    
    qDataRaw <- rmOutliersQuotes(qDataRaw, window = window, type = type, maxi = rmoutliersmaxi, tz = tz)
    REPORT[8] <- dim(qDataRaw)[1]
    if (inputWasXts) {
      df_result <- xts(as.matrix(qDataRaw[, -c("DT",  "DATE")]), order.by = qDataRaw$DT)
    } else {
      df_result <- qDataRaw[, -c( "DATE")]
    }
    
    if (report) {
      return(list(qData = df_result, report = c(REPORT[1], -diff(REPORT))))
    } else {
      return(df_result[])
    }
  }
}

#' Delete entries for which the spread is more than \code{maxi} times the median spread
#' 
#' @description Function deletes entries for which the spread is more than \code{"maxi"} times the median
#' spread on that day.
#' 
#' @param qData an \code{xts} or \code{data.table} object at least containing the columns \code{"BID"} and \code{"OFR"}.
#' @param maxi an integer. By default \code{maxi = "50"}, which means that entries are deleted 
#' if the spread is more than 50 times the median spread on that day.
#' @param tz fallback time zone used in case we we are unable to identify the timezone of the data, by default: \code{tz = NULL}. With the non-disk functionality, we attempt to extract the timezone from the DT column (or index) of the data, which may fail. 
#' In case of failure we use \code{tz} if specified, and if it is not specified, we use \code{"UTC"}. 
#' In the on-disk functionality, if tz is not specified, the timezone used will be the system default.
#' 
#' @return \code{xts} or \code{data.table} object depending on input.
#' 
#' @author Jonathan Cornelissen, Kris Boudt, Onno Kleen, and Emil Sjoerup.
#' @keywords cleaning
#' @export
rmLargeSpread <- function(qData, maxi = 50, tz = NULL) {
  BID <- OFR <- DATE <- DT <- SPREAD <- SPREAD_MEDIAN <- NULL
  qData <- checkColumnNames(qData)
  checkqData(qData)
  inputWasXts <- FALSE
  if (!is.data.table(qData)) {
    if (is.xts(qData)) {
      qData <- setnames(as.data.table(qData)[, BID := as.numeric(as.character(BID))][, OFR := as.numeric(as.character(OFR))], old = "index", new = "DT")
      inputWasXts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } 
  
  timeZone <- format(qData$DT[1], format = "%Z")
  if(is.null(timeZone) || timeZone == ""){
    if(is.null(tz)){
      tz <- "UTC"
    }
    if(!("POSIXct" %in% class(qData$DT))){
      qData[, DT := as.POSIXct(format(DT, digits = 20, nsmall = 20), tz = tz)]
    }
  } else {
    tz <- timeZone
  }
  
  
  
  qData <- qData[, DATE := as.Date(DT, tz = tz)][
    , SPREAD := OFR - BID][
    , SPREAD_MEDIAN := medianCase(SPREAD), by = "DATE"][SPREAD < (SPREAD_MEDIAN * maxi)]
  
  if (inputWasXts) {
    return(xts(as.matrix(qData[, -c("DT", "DATE", "SPREAD", "SPREAD_MEDIAN")]), order.by = qData$DT))
  } else {
    return(qData[, -c("DATE", "SPREAD", "SPREAD_MEDIAN")][])
  }
}

#' Delete entries for which the spread is negative
#' @description Function deletes entries for which the spread is negative.
#' 
#' @param qData an \code{xts} object at least containing the columns "BID" and "OFR".
#' 
#' @return \code{data.table} or \code{xts} object
#' 
#' @author Jonathan Cornelissen, Kris Boudt and Onno Kleen
#' 
#' @examples 
#' rmNegativeSpread(sampleQDataRaw)
#' 
#' @keywords cleaning
#' @export
rmNegativeSpread <- function(qData) {
  BID <- OFR <- DATE <- DT <- NULL
  qData <- checkColumnNames(qData)
  checkqData(qData)
  inputWasXts <- FALSE
  if (!is.data.table(qData)) {
    if (is.xts(qData)) {
      qData <- setnames(as.data.table(qData), old = "index", new = "DT")
      qData[, BID := as.numeric(as.character(BID))]
      qData[, OFR := as.numeric(as.character(OFR))]
      inputWasXts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } 
  
  qData <- qData[OFR > BID]
  
  if (inputWasXts) {
    return(xts(as.matrix(qData[, -c("DT")]), order.by = qData$DT))
  } else {
    return(qData[])
  }
}

#' Delete transactions with unlikely transaction prices
#' 
#' @description Function deletes entries with prices that are above the ask plus the bid-ask spread.
#' Similar for entries with prices below the bid minus the bid-ask spread.
#' 
#' @param tData a \code{data.table} or \code{xts} object containing the time series data, with at least the column \code{"PRICE"}, containing the transaction price.
#' @param qData a \code{data.table} or \code{xts} object containing the time series data with at least the columns \code{"BID"} and \code{"OFR"}, containing the bid and ask prices.
#' @param lagQuotes numeric, number of seconds the quotes are registered faster than
#' the trades (should be round and positive). Default is 0. For older datasets, i.e. before 2010, it may be a good idea to set this to e.g. 2. See Vergote (2005)
#' @param BFM a logical determining whether to conduct 'Backwards - Forwards matching' of trades and quotes.
#' The algorithm tries to match trades that fall outside the bid - ask and first tries to match a small window forwards and if this fails, it tries to match backwards in a bigger window.
#' The small window is a tolerance for inaccuracies in the timestamps of bids and asks. The backwards window allow for matching of late reported trades, i.e. block trades.
#' @param backwardsWindow a numeric denoting the length of the backwards window. Default is 3600, corresponding to one hour.
#' @param forwardsWindow a numeric denoting the length of the forwards window. Default is 0.5, corresponding to one half second.
#' @param plot a logical denoting whether to visualize the forwards, backwards, and unmatched trades in a plot.
#' @param ... used internally
#' @details Note: in order to work correctly, the input data of this function should be
#' cleaned trade (tData) and quote (qData) data respectively.
#' In older high frequency datasets the trades frequently lag the quotes. In newer datasets this tends to happen 
#' only during extreme market activity when exchange networks are at maximum capacity.
#' 
#' @return \code{xts} or \code{data.table} object depending on input.
#' 
#' @references  
#' 
#' Vergote, O. (2005). How to match trades and quotes for NYSE stocks? K.U.Leuven working paper.
#' 
#' Christensen, K., Oomen, R. C. A., Podolskij, M. (2014): Fact or Friction: Jumps at ultra high frequency. \emph{Journal of Financial Economics}, 144, 576-599
#' 
#' @author Jonathan Cornelissen, Kris Boudt, Onno Kleen, and Emil Sjoerup.
#' @keywords cleaning
#' @importFrom data.table setkey set
#' @export
rmTradeOutliersUsingQuotes <- function(tData, qData, lagQuotes = 0, BFM = FALSE, backwardsWindow = 3600, forwardsWindow = 0.5, plot = FALSE, ...) {
  if(length(lagQuotes) != 1){
    lagQuotes <- lagQuotes[1]
  }
  DATE <- SIZE <- SYMBOL <- PRICE <- DT <- SPREAD <- BID <- OFR <- NULL
  tData <- checkColumnNames(tData)
  qData <- checkColumnNames(qData)
  checkqData(qData)
  checktData(tData)
  
  if (any(class(tData) != class(qData))) {
    stop("tData and qData should be of the same data type, either xts or data.table.")
  }
  
  inputWasXts <- FALSE
  if (!is.data.table(tData)) {
    if (is.xts(tData)) {
      tData <- as.data.table(tData)
      setnames(tData , old = "index", new = "DT")
      # Change the columns to character instead of factor, this means we can roll them in the merge.
      for (col in names(tData)[-1]) {
        set(tData, j = col, value = as.character(tData[[col]]))
      }
      tData[, `:=`(PRICE = as.numeric(PRICE), SIZE = as.numeric(SIZE))]
      
      
      qData <- as.data.table(qData)
      setnames(qData, old = "index", new = "DT")
      # Change the columns to character instead of factor, this means we can roll them in the merge.
      for (col in names(qData)[-1]) {
        set(qData, j = col, value = as.character(qData[[col]]))
      }
      qData[, `:=`(BID = as.numeric(BID), OFR = as.numeric(OFR))]
      inputWasXts <- TRUE
      
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (!("DT" %in% colnames(tData))) {
      stop("tData neeeds DT column.")
    }
  }
  
  tqData <- matchTradesQuotes(tData, qData, lagQuotes = lagQuotes, BFM = BFM, backwardsWindow = backwardsWindow, forwardsWindow = forwardsWindow, plot = plot, ... = ...)
  
  if(!BFM){
    tData <- tqData[, SPREAD := OFR - BID][PRICE <= OFR + SPREAD & PRICE >= BID - SPREAD]
  
    if (inputWasXts) {
      return(xts(as.matrix(tData[, -c("DT", "SPREAD")]), order.by = tData$DT))
    } else {
      return(tData[, -c("SPREAD")][])
    }
  } else {
    
    return(tqData[])
    
  }
  
  
  
}

#' Remove outliers in quotes
#' 
#' @description 
#' Delete entries for which the mid-quote is outlying with respect to surrounding entries.
#' 
#' @param qData a \code{data.table} or \code{xts} object at least containing the columns \code{"BID"} and \code{"OFR"}.
#' @param maxi an integer, indicating the maximum number of median absolute deviations allowed.
#' @param window an integer, indicating the time window for which the "outlyingness" is considered.
#' @param type should be \code{"standard"} or \code{"advanced"} (see details).
#' @param tz fallback time zone used in case we we are unable to identify the timezone of the data, by default: \code{tz = NULL}. 
#' With the non-disk functionality, we attempt to extract the timezone from the \code{DT} column (or index) of the data, which may fail. 
#' In case of failure we use \code{tz} if specified, and if it is not specified, we use \code{"UTC"}. 
#' 
#' @return \code{xts} object or \code{data.table} depending on type of input.
#' 
#' @details
#' 
#' \itemize{
#' \item If \code{type = "standard"}: Function deletes entries for which the mid-quote deviated by more than "maxi"
#' median absolute deviations from a rolling centered median (excluding
#' the observation under consideration) of window observations.
#' \item If \code{type = "advanced"}:  Function deletes entries for which the mid-quote deviates by more than "maxi"
#' median absolute deviations from the value closest to the mid-quote of
#' these three options:
#' \enumerate{
#'  \item Rolling centered median (excluding the observation under consideration)
#'  \item Rolling median of the following window of observations
#'  \item Rolling median of the previous window of observations
#' }
#' The advantage of this procedure compared to the "standard" proposed
#' by Barndorff-Nielsen et al. (2010) is that it will not incorrectly remove
#' large price jumps. Therefore this procedure has been set as the default
#' for removing outliers. 
#' 
#' Note that the median absolute deviation is taken over the entire
#' day. In case it is zero (which can happen if mid-quotes don't change much), 
#' the median absolute deviation is taken over a subsample without constant mid-quotes.
#' }
#' 
#' @references Barndorff-Nielsen, O. E., P. R. Hansen, A. Lunde, and N. Shephard (2009). Realized kernels in practice: Trades and quotes. \emph{Econometrics Journal}, 12, C1-C32.
#' 
#' Brownlees, C.T., and Gallo, G.M. (2006). Financial econometric analysis at ultra-high frequency: Data handling concerns. \emph{Computational Statistics & Data Analysis}, 51, 2232-2245.
#' 
#' @author Jonathan Cornelissen and Kris Boudt.
#' 
#' @keywords cleaning
#' @importFrom stats mad median
#' @importFrom data.table as.data.table is.data.table setnames
#' @importFrom xts is.xts as.xts
#' @export
rmOutliersQuotes <- function (qData, maxi = 10, window = 50, type = "standard", tz = NULL) {
  # NOTE: Median Absolute deviation chosen contrary to Barndorff-Nielsen et al.
  # Setting those variables equal NULL is for suppressing NOTES in devtools::check
  # References inside data.table-operations throw "no visible binding for global variable ..." error
  SYMBOL <- BID <- OFR <- MIDQUOTE <- DATE <- DT <- MADALL <- CRITERION <- NULL
  if ((window %% 2) != 0) {
    stop("Window size can't be odd.")
  }
  
  qData <- checkColumnNames(qData)
  checkqData(qData)
  
  inputWasXts <- FALSE
  if (!is.data.table(qData)) {
    if (is.xts(qData)) {
      qData <- as.data.table(qData)
      setnames(qData , old = "index", new = "DT")
      # Change the columns to character instead of factor, this means we can roll them in the merge.
      for (col in names(qData)[-1]) {
        set(qData, j = col, value = as.character(qData[[col]]))
      }
      qData[, `:=`(BID = as.numeric(BID), OFR = as.numeric(OFR))]
      
      
      inputWasXts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (!("DT" %in% colnames(qData))) {
      stop("Data.table neeeds DT column.")
    }
  }
  
  timeZone <- format(qData$DT[1], format = "%Z")
  if(is.null(timeZone) || timeZone == ""){
    if(is.null(tz)){
      tz <- "UTC"
    }
    if(!("POSIXct" %in% class(qData$DT))){
      qData[, DT := as.POSIXct(format(DT, digits = 20, nsmall = 20), tz = tz)]
    }
  } else {
    tz <- timeZone
  }
  
  
  
  if (!(type %in% c("standard", "advanced"))) {
    stop("type has to be \"standard\" or \"advanced\".")
  }
  
  # weights_med_center_incl <- rep(1, times = window + 1)
  weights_med_center_excl <- c(rep(1, times = window / 2), 0, rep(1, times = window / 2))
  weights_med_follow  <- c(0 , rep(1, times = window))
  weights_med_trail    <- c(rep(1, times = window), 0)
  
  qData <- qData[, `:=`(MIDQUOTE = (BID + OFR) / 2,  DATE = as.Date(DT, tz = tz))][, MADALL := mad(MIDQUOTE), by = list(DATE,SYMBOL)]
  
  if (type == "standard") {
    qData <- qData[ , CRITERION := abs(MIDQUOTE - rollingMedianInclEnds(MIDQUOTE, window = window, weights = weights_med_center_excl)), by = list(DATE,SYMBOL)][
      CRITERION < maxi * MADALL]
    
  }
  if (type == "advanced") {
    qData <- qData[, CRITERION := pmin(abs(MIDQUOTE - rollingMedianInclEnds(MIDQUOTE, window = window, weights = weights_med_center_excl, direction = "center")),
                                       abs(MIDQUOTE - rollingMedianInclEnds(MIDQUOTE, window = window, weights = weights_med_trail, direction = "left")),
                                       abs(MIDQUOTE - rollingMedianInclEnds(MIDQUOTE, window = window, weights = weights_med_follow, direction = "right")),
                                       na.rm = TRUE), by = list(DATE,SYMBOL)][
                                         CRITERION < maxi * MADALL]
  }
  if (inputWasXts) {
    return(xts(as.matrix(qData[, -c("DT", "DATE", "MADALL", "CRITERION", "MIDQUOTE")]), order.by = qData$DT))
  } else {
    return(qData[, -c("MADALL", "CRITERION")][])
  }
}

#' \link{salesCondition} is deprecated. Use \link{tradesCondition} instead.
#' 
#' @description \link{salesCondition} is deprecated. Use \link{tradesCondition} instead.
#' 
#' @param tData \link{salesCondition} is deprecated. Use \link{tradesCondition} instead.
#' @param validConds \link{salesCondition} is deprecated. Use \link{tradesCondition} instead.
#' @keywords cleaning
#' @export
salesCondition <- function(tData, validConds = c('', '@', 'E', '@E', 'F', 'FI', '@F', '@FI', 'I', '@I')) {
  .Deprecated("tradesCondition")
  tradesCondition(tData = tData, validConds = validConds)
}

#' Delete entries with abnormal trades condition.
#' 
#' @description Delete entries with abnormal trades condition
#' 
#' @param tData an \code{xts} or \code{data.table} object containing the time series data, with 
#' one column named \code{"COND"} indicating the Sale Condition.
#' @param validConds a character vector containing valid sales conditions defaults to \cr
#' \code{c('', '@', 'E', '@E', 'F', 'FI', '@F', '@FI', 'I', '@I')}. See details.
#' 
#' @details To get more information on the sales conditions, see the NYSE documentation. Section about Daily TAQ Trades File.
#' The current version (as of May 2020) can be found online at \href{https://www.nyse.com/publicdocs/nyse/data/Daily_TAQ_Client_Spec_v3.3.pdf}{NYSE's webpage}
#' @note Some CSV readers and the WRDS API parses empty strings as NAs. We transform \code{NA} values in COND to \code{""}.
#' 
#' @return \code{xts} or \code{data.table} object depending on input.
#' 
#' @author Jonathan Cornelissen, Kris Boudt, Onno Kleen, and Emil Sjoerup.
#' 
#' @keywords cleaning
#' @export
tradesCondition <- function(tData, validConds = c('', '@', 'E', '@E', 'F', 'FI', '@F', '@FI', 'I', '@I')) {
  COND <- NULL
  tData <- checkColumnNames(tData)
  checktData(tData)
  
  if (!any(colnames(tData) == "COND")) {
    stop("The argument tData should have a column containing sales conditions named COND. Could not find that column.")
  }
  
  inputWasXts <- FALSE
  if (!is.data.table(tData)) {
    if (is.xts(tData)) {
      tData <- setnames(as.data.table(tData), old = "index", new = "DT")
      tData[, COND := as.character(COND)]
      inputWasXts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  }
  tData[is.na(COND), COND := ""]
  
  # setnafill(tData, type = "const", fill = "", cols = "COND") ## For when characters become supported :)
  tData <- tData[gsub("\\s", "", COND) %in% validConds]
  
  if (inputWasXts) {
    return(xts(as.matrix(tData), order.by = tData$DT))
  } else {
    return(tData[])
  }
}

#' Retain only data from a single stock exchange
#' @description Filter raw trade data to only contain specified exchanges
#' 
#' @param data an \code{xts} or \code{data.table} object containing the time series data. 
#' The object should have a column "EX", indicating the exchange by its symbol.
#' @param exch The (vector of) symbol(s) of the stock exchange(s) that should be selected.
#' By default the NYSE is chosen (\code{exch = "N"}). Other exchange symbols are:
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
#' @return \code{xts} or \code{data.table} object depending on input.
#' 
#' @author Jonathan Cornelissen, Kris Boudt, Onno Kleen, and Emil Sjoerup.
#' @importFrom data.table %chin%
#' @keywords cleaning
#' @export
selectExchange <- function(data, exch = "N") { 
  EX  <- NULL
  data <- checkColumnNames(data)
  # checkqData(data)
  
  if (!is.data.table(data)) {
    if (is.xts(data)) {
      filteredts <- data[is.element(data$EX , exch)]
      return(filteredts)
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (!("DT" %in% colnames(data))) {
      stop("Data.table neeeds DT column.")
    }
    return(data[EX %chin% exch])
  }
}

#' Cleans trade data
#' 
#' @description This is a wrapper function for cleaning the trade data of all stock data inside the folder dataSource. 
#' The result is saved in the folder dataDestination. 
#' 
#' In case you supply the argument \code{rawtData}, the on-disk functionality is ignored. The function returns a vector
#' indicating how many trades were removed at each cleaning step in this case.
#' and the function returns an \code{xts} or \code{data.table} object.
#' 
#' The following cleaning functions are performed sequentially:
#' \code{\link{noZeroPrices}}, \code{\link{autoSelectExchangeTrades}} or \code{\link{selectExchange}}, \code{\link{tradesCondition}}, and
#' \code{\link{mergeTradesSameTimestamp}}.
#' 
#' Since the function \code{\link{rmTradeOutliersUsingQuotes}}
#' also requires cleaned quote data as input, it is not incorporated here and
#' there is a separate wrapper called \code{\link{tradesCleanupUsingQuotes}}.
#' 
#' @param dataSource character indicating the folder in which the original data is stored.
#' @param dataDestination character indicating the folder in which the cleaned data is stored.
#' @param exchanges vector of stock exchange symbols for all data in \code{dataSource}, 
#' e.g. \code{exchanges = c("T","N")} retrieves all stock market data from both NYSE and NASDAQ.
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
#' } The default value is \code{"auto"} which automatically selects the exchange for the stocks and days independently using the \code{\link{autoSelectExchangeTrades}}
#' 
#' @param tDataRaw \code{xts} object containing raw trade data. This argument is \code{NULL} by default. Enabling it means the arguments
#' \code{from}, \code{to}, \code{dataSource} and \code{dataDestination} will be ignored (only advisable for small chunks of data).
#' @param report boolean and \code{TRUE} by default. In case it is true the function returns (also) a vector indicating how many trades remained after each cleaning step.
#' @param selection argument to be passed on to the cleaning routine \code{\link{mergeTradesSameTimestamp}}. The default is "median".
#' @param validConds character vector containing valid sales conditions. Passed through to \code{\link{tradesCondition}}.
#' @param marketOpen character in the format of \code{"HH:MM:SS"},
#' specifying the opening time of the exchange(s).
#' @param marketClose character in the format of \code{"HH:MM:SS"},
#' specifying the closing time of the exchange(s).
#' @param printExchange Argument passed to \code{\link{autoSelectExchangeTrades}} indicates whether the chosen exchange is printed on the console, 
#' default is TRUE. This is only used when \code{exchanges} is \code{"auto"}
#' @param saveAsXTS indicates whether data should be saved in \code{xts} format instead of \code{data.table} when using on-disk functionality. FALSE by default.
#' @param tz fallback time zone used in case we we are unable to identify the timezone of the data, by default: \code{tz = NULL}. 
#' With the non-disk functionality, we attempt to extract the timezone from the DT column (or index) of the data, which may fail. 
#' In case of failure we use \code{tz} if specified, and if it is not specified, we use \code{"UTC"}. 
#' In the on-disk functionality, if \code{tz} is not specified, the timezone used will be the system default.
#' 
#' @return For each day an \code{xts} or \code{data.table} object is saved into the folder of that date, containing the cleaned data.
#' This procedure is performed for each stock in \code{"ticker"}.
#' The function returns a vector indicating how many trades remained after each cleaning step.
#' 
#' In case you supply the argument \code{rawtData}, the on-disk functionality is ignored
#' and the function returns a list with the cleaned trades as \code{xts} object (see examples).
#' 
#' @details 
#' Using the on-disk functionality with .csv.zip files from the WRDS database
#' will write temporary files on your machine in order to unzip the files - we try to clean up after it,
#' but cannot guarantee that there won't be files that slip through the crack if the permission settings on your machine does not match 
#' ours.
#' 
#' If the input \code{data.table} does not contain a DT column but it does contain DATE and TIME_M columns, we create the DT column by REFERENCE, altering the \code{data.table} that may be in the user's environment.
#' @examples 
#' # Consider you have raw trade data for 1 stock for 2 days 
#' head(sampleTDataRaw)
#' dim(sampleTDataRaw)
#' tDataAfterFirstCleaning <- tradesCleanup(tDataRaw = sampleTDataRaw, 
#'                                          exchanges = list("N"))
#' tDataAfterFirstCleaning$report
#' dim(tDataAfterFirstCleaning$tData)
#' # In case you have more data it is advised to use the on-disk functionality
#' # via "dataSource" and "dataDestination" arguments
#' 
#' @references
#' Barndorff-Nielsen, O. E., Hansen, P. R., Lunde, A., and Shephard, N. (2009). Realized kernels in practice: Trades and quotes. \emph{Econometrics Journal}, 12, C1-C32.
#' 
#' Brownlees, C.T. and Gallo, G.M. (2006). Financial econometric analysis at ultra-high frequency: Data handling concerns. \emph{Computational Statistics & Data Analysis}, 51, 2232-2245.
#' 
#' @author Jonathan Cornelissen, Kris Boudt, Onno Kleen, and Emil Sjoerup
#' @importFrom data.table fread %chin%
#' @importFrom utils unzip
#' @keywords cleaning
#' @export
tradesCleanup <- function(dataSource = NULL, dataDestination = NULL, exchanges = "auto", tDataRaw = NULL, report = TRUE, selection = "median",
                          validConds = c('', '@', 'E', '@E', 'F', 'FI', '@F', '@FI', 'I', '@I'), marketOpen = "09:30:00", 
                          marketClose = "16:00:00", printExchange = TRUE, saveAsXTS = FALSE, tz = NULL) {
  .SD <- CORR <- SIZE <- SYMBOL <- PRICE <- EX <- DT <- DATE <- TIME_M <- SYM_SUFFIX<- NULL
  
  if (is.null(tDataRaw)) {

    tradesfiles <- list.files(dataSource, recursive = TRUE, full.names = TRUE)[grepl("trades", list.files(dataSource, recursive = TRUE))]
    for (ii in tradesfiles) {
      extension <- strsplit(ii, "\\.")[[1]]
      extension <- extension[length(extension)]
      if(extension == "zip") {
        tmp <- tempdir()
        unzip(ii, exdir = tmp)
        files <- list.files(tmp, full.names = TRUE, pattern = "csv")
        if(length(files) >= 1){
          readdata <- try(rbindlist(lapply(files, fread, tz = tz)), silent = TRUE)
        }
        # Try to Cleanup - we don't force it though!
        unlink(tmp, recursive = TRUE)
      } else if(extension %in% c("csv", "gz", "gzip", "bz2")){
        readdata <- try(fread(ii, tz = tz), silent = TRUE)
      } else if(extension %in% c("rds")){
        readdata <- try(readRDS(ii))
      } else {
        readdata <- try(fread(ii, tz = tz), silent = TRUE)
      }
      if(inherits(readdata, "try-error")){
        stop(paste("Error encountered while opening data, error message is:",readdata))
      }
      if(is.null(tz)) tz <- "UTC"
      if(colnames(readdata)[1] == "index"){ # The data was saved from an xts object
        readdata <- try(readdata[, DT := as.POSIXct(index, tz = tz, format = "%Y-%m-%dT%H:%M:%OS")])
      } else if ("DT" %in% colnames(readdata)){
        readdata <- try(readdata[, DT := as.POSIXct(DT, tz = tz, format = "%Y-%m-%dT%H:%M:%OS")])
      } else {
        readdata <- try(readdata[, `:=`(DT = as.POSIXct(paste(DATE, TIME_M), tz = tz, format = "%Y%m%d %H:%M:%OS"),
                                      DATE = NULL, TIME_M = NULL)], silent = TRUE)
      }
      
      if(inherits(readdata, "try-error")){
        stop(paste("Error encountered while creating a DT column, error message is:",readdata))
      }
      
      tData <- try(tradesCleanup(tDataRaw = readdata,
                                 selection = selection,
                                 exchanges = exchanges,
                                 validConds = validConds, tz = tz))$tData
      tData <- tData[, DATE := as.Date(DT, tz = tz)]
      tData <- split(tData, by = "DATE")
      try(dir.create(paste0(dataDestination, "/", strsplit(ii, "/")[[1]][1]), recursive = TRUE), silent = TRUE)
      for (jj in tData) {
        if (saveAsXTS) {
          df_result <- xts(as.matrix(jj[, -c("DT", "DATE")]), order.by = jj$DT)
        } else {
          df_result <- jj[, -c( "DATE")]
        }
        saveRDS(df_result, paste0(dataDestination, "/", strsplit(ii, "/")[[1]][1], "/", unique(as.Date(jj$DT, tz = tz)), ".rds"))
        # saveRDS(df_result, paste0(dataDestination, "/", strsplit(ii, "/")[[1]][1], "/", strsplit(strsplit(ii, "/")[[1]][2], ".zip")[1], ".rds"))
      }
    }
  }
  
  if (!is.null(tDataRaw)) {
    
    tDataRaw <- checkColumnNames(tDataRaw)
    nm <- toupper(colnames(tDataRaw))
    
    checktData(tDataRaw)
    if(!"DT" %in% nm && c("DATE", "TIME_M") %in% nm){
      if(is.null(tz)) tz <- "UTC"
      tDataRaw[, `:=`(DT = as.POSIXct(paste(DATE, TIME_M), tz = tz, format = "%Y%m%d %H:%M:%OS"),
                      DATE = NULL, TIME_M = NULL)]
    }
    if("SYM_SUFFIX" %in% nm){
      tDataRaw[, `:=`(SYMBOL = fifelse(SYM_SUFFIX == "" | is.na(SYM_SUFFIX), yes = SYMBOL, no = paste0(SYMBOL, "_", SYM_SUFFIX)), SYM_SUFFIX = NULL)]
    }
    
    
    
    inputWasXts <- FALSE
    if (!is.data.table(tDataRaw)) {
      if (is.xts(tDataRaw)) {
        
        tDataRaw <- as.data.table(tDataRaw)
        
        for (col in names(tDataRaw)[-1]) {
          set(tDataRaw, j = col, value = as.character(tDataRaw[[col]]))
        }
        tDataRaw[, `:=`(PRICE = as.numeric(PRICE), SIZE = as.numeric(SIZE))]
        
        setnames(tDataRaw , old = "index", new = "DT")
        inputWasXts <- TRUE
      } else {
        stop("Input has to be data.table or xts.")
      }
    } else {
      if (!("DT" %in% colnames(tDataRaw))) {
        stop("Data.table neeeds DT column.")
      }
    }
    
    
    timeZone <- format(tDataRaw$DT[1], format = "%Z")
    if(is.null(timeZone) || timeZone == ""){
      if(is.null(tz)){
        tz <- "UTC"
      }
      if(!("POSIXct" %in% class(tDataRaw$DT))){
        tDataRaw[, DT := as.POSIXct(format(DT, digits = 20, nsmall = 20), tz = tz)]
      }
    } else {
      tz <- timeZone
    }
    
    REPORT <- c(initialObservations = 0,
                removedFromZeroTrades = 0,
                removedOutsideExchangeHours = 0,
                removedFromSelectingExchange = 0,
                removedFromCorrections = 0,
                removedFromSalesCondition = 0,
                removedFromMergeTimestamp = 0,
                finalObservations = 0)
    
    nm <- toupper(colnames(tDataRaw))
    REPORT[1] <- dim(tDataRaw)[1]
    tDataRaw <- tDataRaw[PRICE != 0]
    REPORT[2] <- dim(tDataRaw)[1] 
    tDataRaw <- exchangeHoursOnly(tDataRaw, marketOpen = marketOpen, marketClose = marketClose, tz = tz)
    REPORT[3] <- dim(tDataRaw)[1]
    if("EX" %in% nm){
      if(all(exchanges != "auto")){
        tDataRaw <- tDataRaw[EX %chin% exchanges]
      } else if (exchanges == "auto"){
        tDataRaw <- tDataRaw[, autoSelectExchangeTrades(.SD, printExchange = printExchange), .SDcols = nm,by = list(SYMBOL, DATE = as.Date(DT, tz = tz))][, nm, with = FALSE]
      }
    }
    REPORT[4] <- dim(tDataRaw)[1]
    if("CORR" %in% nm){
      tDataRaw <- tDataRaw[CORR == 0]
    }
    REPORT[5] <- dim(tDataRaw)[1]
    
    if("COND" %in% nm){
      tDataRaw <- tradesCondition(tDataRaw, validConds)
    }
    REPORT[6] <- dim(tDataRaw)[1]
    
    tDataRaw <- mergeTradesSameTimestamp(tDataRaw, selection = selection)
    REPORT[7] <- dim(tDataRaw)[1]
    REPORT[8]
    if (inputWasXts) {
      df_result <- xts(as.matrix(tDataRaw[, -c("DT")]), order.by = tDataRaw$DT)
    } else {
      df_result <- tDataRaw
    }
    
    if (report) {
      return(list(tData = df_result, report = c(REPORT[1], -diff(REPORT))))
    } else {
      return(df_result[])
    }
  }
}

# #' #' @export
# #' tradesCleanupUsingQuotes <- function(from, to, dataSource, dataDestination, ticker, tData = NULL, qData = NULL) {
# #'   
# #'   ## Deprecated
# #'   warning("Please use tradesCleanupUsingQuotes instead of tradesCleanupFinal.") 
# #'   tradesCleanupFinal(from, to, dataSource, dataDestination, ticker, tData = NULL, qData = NULL)
# #' }

#' Perform a final cleaning procedure on trade data
#' 
#' @description Function performs cleaning procedure \code{\link{rmTradeOutliersUsingQuotes}} 
#' for the trades of all stocks data in "dataDestination". 
#' Note that preferably the input data for this function 
#' is trade and quote data cleaned by respectively e.g. \code{\link{tradesCleanup}}
#' and \code{\link{quotesCleanup}}.
#' 
#' @param tradeDataSource character indicating the folder in which the original trade data is stored.
#' @param quoteDataSource character indicating the folder in which the original quote data is stored.
#' @param dataDestination character indicating the folder in which the cleaned data is stored, folder of \code{dataSource} by default.
#' @param tData \code{data.table} or \code{xts} object containing trade data cleaned by \code{\link{tradesCleanup}}. This argument is \code{NULL} by default. Enabling it, means the arguments
#' \code{from}, \code{to}, \code{dataSource} and \code{dataDestination} will be ignored (only advisable for small chunks of data).
#' @param qData \code{data.table} or \code{xts} object containing cleaned quote data. This argument is NULL by default. Enabling it means the arguments
#' \code{from}, \code{to}, \code{dataSource}, \code{dataDestination} will be ignored (only advisable for small chunks of data).
#' @param lagQuotes numeric, number of seconds the quotes are registered faster than
#' the trades (should be round and positive). Default is 0. For older datasets, i.e. before 2010, it may be a good idea to set this to, e.g., 2 (see, Vergote, 2005).
#' @param BFM a logical determining whether to conduct "Backwards - Forwards matching" of trades and quotes.
#' The algorithm tries to match trades that fall outside the bid - ask and first tries to match a small window forwards and if this fails, it tries to match backwards in a bigger window.
#' The small window is a tolerance for inaccuracies in the timestamps of bids and asks. The backwards window allow for matching of late reported trades, i.e. block trades.
#' @param backwardsWindow a numeric denoting the length of the backwards window used when \code{BFM = TRUE}. Default is 3600, corresponding to one hour.
#' @param forwardsWindow a numeric denoting the length of the forwards window used when \code{BFM = TRUE}. Default is 0.5, corresponding to one half second.
#' @param plot a logical denoting whether to visualize the forwards, backwards, and unmatched trades in a plot. Passed on to \code{\link{rmTradeOutliersUsingQuotes}}
#' @return For each day an \code{xts} object is saved into the folder of that date, containing the cleaned data.
#' 
#' @details 
#' In case you supply the arguments \code{tData} and \code{qData}, the on-disk functionality is ignored
#' and the function returns cleaned trades as a \code{data.table} or \code{xts} object (see examples).
#' 
#' When using the on-disk functionality and tradeDataSource and quoteDataSource are the same, the quote files are all files in the folder that contains 'quote', and the rest are treated as containing trade data.
#' 
#' @references
#' Barndorff-Nielsen, O. E., Hansen, P. R., Lunde, A., and Shephard, N. (2009). Realized kernels in practice: Trades and quotes. \emph{Econometrics Journal}, 12, C1-C32.
#' 
#' Brownlees, C.T., and Gallo, G.M. (2006). Financial econometric analysis at ultra-high frequency: Data handling concerns. \emph{Computational Statistics & Data Analysis}, 51, 2232-2245.
#'
#' Christensen, K., Oomen, R. C. A., Podolskij, M. (2014): Fact or Friction: Jumps at ultra high frequency. \emph{Journal of Financial Economics}, 144, 576-599
#' @author Jonathan Cornelissen, Kris Boudt, Onno Kleen, and Emil Sjoerup.
#' 
#' @examples 
#' # Consider you have raw trade data for 1 stock for 2 days 
#' tDataAfterFirstCleaning <- tradesCleanup(tDataRaw = sampleTDataRaw, 
#'                                           exchanges = "N", report = FALSE)
#' qData <- quotesCleanup(qDataRaw = sampleQDataRaw, 
#'                        exchanges = "N", report = FALSE)
#' dim(tDataAfterFirstCleaning)
#' tDataAfterFinalCleaning <- 
#'   tradesCleanupUsingQuotes(qData = qData[as.Date(DT) == "2018-01-02"],
#'                            tData = tDataAfterFirstCleaning[as.Date(DT) == "2018-01-02"])
#' dim(tDataAfterFinalCleaning)
#' # In case you have more data it is advised to use the on-disk functionality
#' # via the "tradeDataSource", "quoteDataSource", and "dataDestination" arguments
#' @keywords cleaning
#' @export
tradesCleanupUsingQuotes <- function(tradeDataSource = NULL, quoteDataSource = NULL, dataDestination = NULL, tData = NULL, qData = NULL, lagQuotes = 0,
                                     BFM = FALSE, backwardsWindow = 3600, forwardsWindow = 0.5, plot = FALSE) {
  
  if (is.null(dataDestination) && !is.null(tradeDataSource)) {
    dataDestination <- tradeDataSource
    dataDestination <- path.expand(dataDestination)
  }
  
  if ((!is.null(tData)) & (!is.null(qData))) {
    tData <- checkColumnNames(tData)
    qData <- checkColumnNames(qData)
    
    #1 cleaning procedure that needs cleaned trades and quotes
    tData <- rmTradeOutliersUsingQuotes(tData, qData, lagQuotes = lagQuotes, BFM = BFM, backwardsWindow = backwardsWindow, forwardsWindow = forwardsWindow, plot = plot, onlyTQ = TRUE)
    return(tData)
  } else {
    
    if(tradeDataSource == quoteDataSource){
      tradeFiles <- list.files(tradeDataSource, recursive = TRUE, full.names = TRUE)
      quoteFiles <- tradeFiles[grepl("quote", tradeFiles)] # quoteFiles is defined as the files with quote in them
      tradeFiles <- tradeFiles[!grepl("quote", tradeFiles)]
    } else {
      tradeFiles <- list.files(tradeDataSource, recursive = TRUE, full.names = TRUE)
      quoteFiles <- list.files(quoteDataSource, recursive = TRUE, full.names = TRUE)
    }
    
    if(length(quoteFiles) != length(tradeFiles)){
      stop("The number of files in tradeDataSource must be the same as quoteDataSource")
    }
    
    if(!file.exists(dataDestination)){
      dir.create(dataDestination, recursive  = TRUE)
    }
    
    ## Make regular expression to find the tradeDatasource in tradeFiles, so we can create a destination that follows same 
    ## naming conventions for the end files.

    # 
    # finalDestinations <- tradeFiles
    # finalDestinations <- sapply(finalDestinations , strsplit, split = .Platform$file.sep)
    # browser()
    # for (i in 1:length(finalDestinations)) {
    #   ## substitute in tradescleanedbyquotes right before the extension.
    #   finalDestinations[[i]][length(finalDestinations[[i]])] <- 
    #     sub("\\.", "tradescleanedbyquotes.", finalDestinations[[i]][length(finalDestinations[[i]])])
    # }
    
    # finalDestinations <- unlist(lapply(finalDestinations, paste0, collapse = .Platform$file.sep))
    # ## Check if the directories exist
    # if(any(!file.exists(dirname(finalDestinations)))){
    #   sapply(dirname(finalDestinations), dir.create, showWarnings = FALSE)
    # }
    
    for (i in 1:length(tradeFiles)) {
      tradeFile <- tradeFiles[i]
      quoteFile <- quoteFiles[i]
      
      tData <- try(readRDS(tradeFile))
      qData <- try(readRDS(quoteFile))
      saveRDS(rmTradeOutliersUsingQuotes(tData, qData, lagQuotes = lagQuotes, BFM = BFM,
                                         backwardsWindow = backwardsWindow, forwardsWindow = forwardsWindow, plot = FALSE, onlyTQ = TRUE), 
              file = paste0(dataDestination, "/", gsub(".rds", "tradescleanedbyquotes.rds", basename(tradeFile))))
      
    }
    # for (tFile in tradeFiles) {
    #   filesforticker <- list.files(paste0(tradeDataSource, "/", tFile, "/"))
    #   for (jj in filesforticker[!grepl("quotes", filesforticker)]) {
    #     tData <- try(readRDS(paste0(dataSource, "/", ii, "/", jj)))
    #     qData <- try(readRDS(paste0(dataSource, "/", ii, "/", substring(jj, 1, 10), "quotes.rds")))
    #     tData <- checkColumnNames(tData)
    #     qData <- checkColumnNames(qData)
    #     saveRDS(rmTradeOutliersUsingQuotes(tData, qData), paste0(dataDestination, "/", ii, "/", substring(jj, 1, 10), "tradescleanedbyquotes.rds"))
    #   }
    # }
  }
}




#' Synchronize (multiple) irregular timeseries by refresh time
#' 
#' @description This function implements the refresh time synchronization scheme proposed by Harris et al. (1995). 
#' It picks the so-called refresh times at which all assets have traded at least once since the last refresh time point. 
#' For example, the first refresh time corresponds to the first time at which all stocks have traded.
#' The subsequent refresh time is defined as the first time when all stocks have traded again.
#' This process is repeated until the end of one time series is reached.
#' 
#' @param pData a list. Each list-item contains an \code{xts} or a \code{data.table} object (with first column DT (datetime)) containing the original time series (one day only and typically a price series).
#' @param sort logical determining whether to sort the index based on a criterion (will only sort descending; i.e., most liquid first). Default is \code{FALSE}.
#' @param criterion character determining which criterion used. Currently supports \code{"squared duration"} and \code{"duration"}. Default is \code{"squared duration"}.
#' 
#' @return An \code{xts} or \code{data.table} object containing the synchronized time series - depending on the input.
#' 
#' @references Harris, F., T. McInish, Shoesmith, G., and Wood, R. (1995). Cointegration, error correction, and price discovery on informationally linked security markets. \emph{Journal of Financial and Quantitative Analysis}, 30, 563-581.
#' 
#' @examples 
#' # Suppose irregular timepoints:
#' start <- as.POSIXct("2010-01-01 09:30:00")
#' ta <- start + c(1,2,4,5,9)
#' tb <- start + c(1,3,6,7,8,9,10,11)
#' 
#' # Yielding the following timeseries:
#' a <- xts::as.xts(1:length(ta), order.by = ta)
#' b <- xts::as.xts(1:length(tb), order.by = tb)
#' 
#' # Calculate the synchronized timeseries:
#' refreshTime(list(a,b))
#' 
#' @author Jonathan Cornelissen, Kris Boudt, Onno Kleen, and Emil Sjoerup.
#' @keywords data manipulation
#' @importFrom xts xts tzone
#' @importFrom data.table merge.data.table
#' @export
refreshTime <- function (pData, sort = FALSE, criterion = "squared duration") {
  
  if(!is.list(pData)){
    stop("pData must be a list of atleast length one")
  }
  if((!all(as.logical(lapply(pData, is.xts))) & !all(as.logical(lapply(pData, is.data.table))))){
    stop("All the series in pData must be either xts or data.table objects")
  }
  inputWasXts <- is.xts(pData[[1]])
  
  if(inputWasXts){ ## xts case
    if(any(as.logical(lapply(pData, function(x) ndays(x) > 1)))){
      stop("All the series in pData must contain data for a single day")
    }
  } else { 
    if(any(as.logical(lapply(pData, function(x) length(unique(floor(as.numeric(x$DT)/ 86400))) > 1)))){
      stop("All the series in pData must contain data for a single day")
    }
    if(!all(sapply(pData, function(x) any(colnames(x) == "DT") && ncol(x) == 2))){
      stop("DT must be present in all the data.tables in the input, and they should have two columns")
    }
  }
  if((sort && is.null(names(pData)))){
    stop("When using sort, please provide pData as a named list")
  }
  
  if (length(pData) < 1) {
    stop("pData should contain at least two time series")
  }
  if( length(pData) == 1){
    return(pData[[1]])
  }
  
  if(inputWasXts){
    tz_ <- tzone(pData[[1]])
    if(sort){
      
      if(criterion == "squared duration"){
        criterion <- function(x) sum(as.numeric(diff(index(x)))^2)
      } else if( criterion == "duration"){
        criterion <- function(x) sum(as.numeric(diff(index(x))))
      } else {
        stop("Criterion must be either 'squared duration' or 'duration'")
      }
      
      vec <- sort(sapply(pData, criterion), index.return = TRUE)$ix
      nameVec <- names(pData)[vec]
      temp <- pData[[vec[1]]]
      for (i in vec[-1]) {
        temp <- merge(temp, pData[[i]])
      }
      
    } else {
      nameVec <- names(pData)
      temp <- pData[[1]]
      for (i in 2:length(pData)) {
        temp <- merge(temp, pData[[i]])
      }  
    }
    
    
    temp <- refreshTimeMatching(coredata(temp), index(temp))
    temp <- xts(temp[[1]], order.by = as.POSIXct(temp[[2]], tz = tz_, origin = as.POSIXct("1970-01-01", tz = tz_)))
    names(temp) <- nameVec # Set names 
    return(temp)
  } else {
    DT <- NULL
    timeZone <- format(pData[[1]]$DT[1], format = "%Z")
    if(is.null(timeZone) || timeZone == ""){
      tz <- "UTC"
    } else {
      tz <- timeZone
    }
    
    
    if(!all(sapply(pData, function(x) "DT" %in% colnames(x)))){
      stop("DT must be present in all elements of pData")
    }
    
    if(sort){
      if(criterion == "squared duration"){
        criterion <- function(x) sum(diff(as.numeric(x$DT))^2)
      } else if( criterion == "duration"){
        criterion <- function(x) sum(diff(as.numeric(x$DT)))
      } else {
        stop("Criterion must be either 'squared duration' or 'duration'")
      }
      vec <- sort(sapply(pData, criterion), index.return = TRUE)$ix + 1
      
    } else {
      vec <- 2:(length(pData) + 1)
    }
    names <- names(pData)
    
    ## Check if all the column names are the same, e.g. all data.tables in the list
    ## has colnames c("DT", "PRICE") then when we merge, we will get a single column
    ## data.table - which we can't have! We just append a character.
    if(length(unique(unlist(lapply(pData, names)))) == 2){ 
      
      pData <- lapply(pData, copy) #unfortunately we need to copy all the data.tables here
      for (i in 1:length(pData)) {
        setnames(pData[[i]], new = c("DT", paste(names(pData[[i]])[2], i))) 
      }
    }
    
    # # May not be needed
    # # 
    # for (i in 1:length(pData)) {
    #   pData[[i]][,DT := as.numeric(DT, tz = tz)]
    #   # set(pData[[i]], j = "DT",  value= as.numeric(pData[[i]]$DT, tz = tz))
    #   setkey(pData[[i]], "DT")
    # 
    #   # if(flag)  setnames(pData[[i]],  c("DT", paste0(colnames(pData[[i]])[-1] , i)))
    # }
    # # # mergeOverload <- function(x,y) merge.data.table(x, y, all = TRUE, on = "DT")
    # # 
    # # # pData <- Reduce(mergeOverload, pData)
    # # 
    pData <- Reduce(function(x,y) merge.data.table(x, y, all = TRUE, on = "DT"), pData)
    
    # For if the copying is needed
    # pData <- refreshTimeMatching(as.matrix(pData[,-"DT"]), pData$DT)
    
    pData <- refreshTimeMatching(as.matrix(pData[,-"DT"]), as.numeric(pData$DT, tz = tz))
    pData <- data.table(pData[[2]], pData[[1]])
    if(is.null(names)){
      setnames(pData, new = c("DT", paste0("V", 1:(ncol(pData)-1))))
    } else {
      setnames(pData, new =  c("DT", names))
    }
    pData[, DT := as.POSIXct(DT, origin = as.POSIXct("1970-01-01", tz = tz), tz = tz)]
    if(sort) setcolorder(pData, c(1, vec))
    
    
    return(pData[])
  }
}


#' Business time aggregation
#' 
#' Time series aggregation based on `business time` statistics. Instead of equidistant sampling based on time during a trading day, business time sampling creates measures and samples equidistantly using these instead.
#' For example when sampling based on volume, business time aggregation will result in a time series that has an equal amount of volume between each observation (if possible).
#' 
#' @param pData \code{xts} or \code{data.table} containing data to aggregate.
#' @param measure character denoting which measure to use. Valid options are \code{"intensity"}, \code{"vol"}, and \code{"volume"}, 
#' denoting the trade intensity process of Oomen (2005), volatility, and volume, respectively. Default is \code{"volume"}.
#' @param obs integer valued numeric of length 1 denoting how many observations is wanted after the aggregation procedure.
#' @param bandwidth numeric of length one, denoting which bandwidth parameter to use in the trade intensity process estimation of Oomen (2005).
#' @param tz fallback time zone used in case we we are unable to identify the timezone of the data, by default: \code{tz = NULL}. We attempt to extract the timezone from the DT column (or index) of the data, which may fail. 
#' In case of failure we use \code{tz} if specified, and if it is not specified, we use \code{"UTC"}.
#' @param ... extra arguments passed on to \code{\link{spotVol}} when measure is \code{"vol"}.
#' 
#' @return A list containing \code{"pData"} which is the aggregated data and a list containing the intensity process, split up day by day.
#' 
#' @examples
#' pData <- sampleTData[,list(DT, PRICE, SIZE)]
#' # Aggregate based on the trade intensity measure. Getting 390 observations.
#' agged <- businessTimeAggregation(pData, measure = "intensity", obs = 390, bandwidth = 0.075)
#' # Plot the trade intensity measure
#' plot.ts(agged$intensityProcess$`2018-01-02`)
#' rCov(agged$pData[, list(DT, PRICE)], makeReturns = TRUE)
#' rCov(pData[,list(DT, PRICE)], makeReturns = TRUE, alignBy = "minutes", alignPeriod = 1)
#' 
#' # Aggregate based on the volume measure. Getting 78 observations.
#' agged <- businessTimeAggregation(pData, measure = "volume", obs = 78)
#' rCov(agged$pData[,list(DT, PRICE)], makeReturns = TRUE)
#' rCov(pData[,list(DT, PRICE)], makeReturns = TRUE, alignBy = "minutes", alignPeriod = 5)
#' 
#' @references 
#' Dong, Y., and Tse, Y. K. (2017). Business time sampling scheme with applications to testing semi-martingale hypothesis and estimating integrated volatility. \emph{Econometrics}, 5, 51.
#' 
#' Oomen, R. C. A. (2006). Properties of realized variance under alternative sampling schemes. \emph{Journal of Business & Economic Statistics}, 24, 219-237
#' 
#' @importFrom zoo index
#' @importFrom xts is.xts
#' @importFrom data.table copy as.xts.data.table
#' @author Emil Sjoerup.
#' @export
businessTimeAggregation <- function(pData, measure = "volume", obs = 390, bandwidth = 0.075, tz = NULL, ...){
  aggregated <- SIZE <- PRICE <- DT <- intensityProcess <- NULL
  if(length(measure) > 1){
    measures <- measure[1]
  }
  if(! (measure %in% c("intensity", "vol", "volume"))){
    stop("measure not a valid choice, valid choices are: \"intensity\", \"vol\", and \"volume\"")
  }
  
    
  inputWasXTS <- FALSE
  if (!is.data.table(pData)) {
    if (is.xts(pData)) {
      pData <- setnames(as.data.table(pData), old = "index", new = "DT")
      pData[, `:=` (PRICE = as.numeric(PRICE), SIZE = as.numeric(SIZE))]
      inputWasXTS <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (!("DT" %in% colnames(pData))) {
      stop("Data.table needs DT column containing the time-stamps of the trades.")
    }
  }
  
  timeZone <- format(pData$DT[1], format = "%Z")
  if(is.null(timeZone) || timeZone == ""){
    if(is.null(tz)){
      tz <- "UTC"
    }
    if(!("POSIXct" %in% class(pData$DT))){
      pData[, DT := as.POSIXct(format(DT, digits = 20, nsmall = 20), tz = tz)]
    }
  } else {
    tz <- timeZone
  }
  
  
  dates <- as.character(unique(as.Date(pData[,DT], tz = tz)))
  pDataBackcup <- copy(pData)
  ITP <- list() # Container for trade intensity process.
  for (date in dates) {
    pData <- pDataBackcup[as.Date(DT, tz = tz) == date,]
    if(measure == "intensity"){
      time <- as.numeric(pData[, DT], tz = tz)
      bandwidth = bandwidth[1]
      
      
      intensityProcess <- as.numeric(tradeIntensityProcessCpp(time, bandwidth))
      intensityProcess <- intensityProcess / sum(intensityProcess) * obs
      idx <- which(diff(floor(cumsum(intensityProcess))) >= 1)
      pData <- pData[idx,]
      
      if(length(idx) < obs){
        warning(paste("The measure mandated sampling the same point twice, at least once, returning series that is smaller than obs on", date, "\n"))
      }
      
    }
    if(measure == "vol"){
      dat <- as.xts.data.table(pData[, list(DT, PRICE)])
      
      intensityProcess <- spotVol(data = dat, ...)$spot
      intensityProcess <- intensityProcess/sum(intensityProcess) * obs
      idx <- which(diff(floor(cumsum(intensityProcess))) >= 1)
      pData <- pData[DT %in% index(intensityProcess)[idx],]
      if(length(idx) < obs){
        warning(paste("The measure mandated sampling the same point twice, at least once, returning series that is smaller than obs on", date, "\n"))
      }
      
    }
    
    if(measure == "volume"){
      if(!"SIZE" %in% colnames(pData)){
        stop("SIZE must be present in pData in order to aggregate based on volume.")
      }
      
      intensityProcess <- as.numeric(pData$SIZE)
      intensityProcess <- intensityProcess/sum(intensityProcess) * obs
      idx <- which(diff(floor(cumsum(intensityProcess))) >= 1)
      pData <- pData[idx, ]
      if(length(idx) < obs){
        warning(paste("The measure mandated sampling the same point twice, at least once, returning series that is smaller than obs on", date, "\n"))
      }
    }
    ITP[[date]] <- intensityProcess
    aggregated <- rbind(aggregated, pData)
  }
  
  
  
  if(inputWasXTS){
    aggregated <- xts(aggregated[, -"DT"], order.by = aggregated[, DT])
  }
  
  res <- list()
  res[["pData"]] <- aggregated
  res[["intensityProcess"]] <- ITP
  return(res)
  
}



#' Make Open-High-Low-Close-Volume bars
#' 
#' This function makes OHLC-V bars at arbitrary intervals. If the SIZE column is not present in the input, no volume column is created.
#' @param pData \code{data.table} or \code{xts} object to make the bars out of, 
#' containing the intraday price series of possibly multiple stocks for possibly multiple days.
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. 
#' Possible values are: \code{"secs"}, \code{"seconds"}, \code{"mins"}, \code{"minutes"}, \code{"hours"}, and \code{"ticks"}.
#' To aggregate based on a 5 minute frequency, set \code{alignPeriod} to 5 and \code{alignBy} to \code{"minutes"}.
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. For example, to aggregate
#' based on a 5 minute frequency, set \code{alignPeriod} to 5 and \code{alignBy} to \code{"minutes"}.
#' @param tz fallback time zone used in case we we are unable to identify the timezone of the data, by default: \code{tz = NULL}. 
#' With the non-disk functionality, we attempt to extract the timezone from the DT column (or index) of the data, which may fail. 
#' In case of failure we use \code{tz} if specified, and if it is not specified, we use \code{"UTC"}. 
#' @examples 
#' \dontrun{
#' minuteBars <- makeOHLCV(sampleTDataEurope, alignBy = "minutes", alignPeriod = 1)
#' # We can use the quantmod package's chartSeries function to plot the ohlcv data
#' quantmod::chartSeries(minuteBars)
#' 
#' minuteBars <- makeOHLCV(sampleTDataEurope[,], alignBy = "minutes", alignPeriod = 1)
#' # Again we plot the series with chartSeries
#' quantmod::chartSeries(minuteBars)
#' 
#' # We can also handle data across multiple days.
#' fiveMinuteBars <- makeOHLCV(sampleTData)
#' # Again we plot the series with chartSeries
#' quantmod::chartSeries(fiveMinuteBars)
#' 
#' # We can use arbitrary alignPeriod, here we choose pi
#' bars <- makeOHLCV(sampleTDataEurope, alignBy = "seconds", alignPeriod = pi)
#' # Again we plot the series with chartSeries
#' quantmod::chartSeries(bars)
#' }
#' @author Emil Sjoerup
#' @export
makeOHLCV <- function(pData, alignBy = "minutes", alignPeriod = 5, tz = NULL){
  SYMBOL <- .SD <-  DATE <- SIZE <- DT <-  PRICE <- NULL
  pData <- checkColumnNames(pData)
  if (!is.xts(pData) & !is.data.table(pData)) {
    stop("The argument pData should be a data.table or xts object.")
  }
  alignPeriod <- alignPeriod[1]
  if (alignBy == "milliseconds") {
    alignBy <- "secs"
    alignPeriod <- alignPeriod / 1000
  }
  if(alignBy == "secs" | alignBy == "seconds"){
    scaleFactor <- alignPeriod
  }
  if(alignBy == "mins" | alignBy == "minutes"){
    scaleFactor <- alignPeriod * 60
  }
  if(alignBy == "hours"){
    scaleFactor <- alignPeriod * 60 * 60
  }
  
  inputWasXts <- FALSE
  if (!is.data.table(pData)) {
    if (is.xts(pData)) {
      pData <- as.data.table(pData)
      pData <- setnames(pData , old = "index", new = "DT")
      for (col in names(pData)[-1]) {
        set(pData, j = col, value = as.character(pData[[col]]))
      }
      pData[, `:=` (SIZE = as.numeric(SIZE), PRICE = as.numeric(PRICE))]
      inputWasXts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (any(!(c("DT","SYMBOL") %in% colnames(pData)))) {
      stop("Data.table neeeds DT (date-time) and SYMBOL columns.")
    }
    pData <- pData[, list(DT, PRICE, SIZE, SYMBOL)]
  }
  
  timeZone <- format(pData$DT[1], format = "%Z")
  if(is.null(timeZone) || timeZone == ""){
    if(is.null(tz)){
      tz <- "UTC"
    }
    if(!("POSIXct" %in% class(pData$DT))) pData[, DT := as.POSIXct(format(DT, digits = 20, nsmall = 20), tz = tz)]
  } else {
    tz <- timeZone
  }
  
  setkey(pData, SYMBOL, DT)
  nm <- toupper(colnames(pData))
  nm <- nm[nm != "SYMBOL"]
  pData <- pData[, lapply(.SD, nafill, type = "locf"), .SDcols = nm, by = list(SYMBOL = SYMBOL, DATE = as.Date(DT, tz = tz))]
  pData <- pData[, lapply(.SD, nafill, type = "nocb"), .SDcols = nm, by = list(SYMBOL = SYMBOL, DATE = DATE)]
  pData[, DT := DT + (scaleFactor - as.numeric(DT, tz = tz) %% scaleFactor)]
  if(!("SIZE" %in% colnames(pData))){
    pData <- pData[, list(OPEN = first(PRICE), HIGH = max(PRICE), LOW = min(PRICE), CLOSE = last(PRICE)), by = list(SYMBOL, DT)]
  } else{
    pData <- pData[, list(OPEN = first(PRICE), HIGH = max(PRICE), LOW = min(PRICE), CLOSE = last(PRICE), VOLUME = sum(SIZE)), by = list(DT,SYMBOL)]
  }
 
  if (inputWasXts) {
    return(xts(as.matrix(pData[, -c("DT")]), order.by = pData$DT, tzone = tz))
  } else {
    return(pData[])
  }
}

#' DEPRECATED 
#' use \code{\link{spreadPrices}}
#' @param data DEPRECATED
#' @export
makeRMFormat <- function(data){
  .Deprecated(new = "makeRMFormat has been renamed to spreadPrices")
  return(spreadPrices(data))
}




#' Convert to format for realized measures
#' 
#' Convenience function to split data from one \code{xts} or \code{data.table} 
#' with at least \code{"DT"}, \code{"SYMBOL"}, and \code{"PRICE"} columns to a format 
#' that can be used in the functions for calculation of realized measures. 
#' This is the opposite of \code{\link{gatherPrices}}.
#' 
#' @param data An \code{xts} or a \code{data.table} object with at least \code{"DT"}, 
#' \code{"SYMBOL"}, and \code{"PRICE"} columns. This data should already be cleaned.
#' 
#' @return An \code{xts} or a \code{data.table} object with columns \code{"DT"} and 
#' a column named after each unique entrance in the \code{"SYMBOL"} column of the input. 
#' These columns contain the price of the associated symbol. We drop all other columns, e.g. \code{SIZE}. 
#' 
#' @examples
#' \dontrun{
#' library(data.table)
#' data1 <- copy(sampleTData)[,  `:=`(PRICE = PRICE * runif(.N, min = 0.99, max = 1.01),
#'                                                DT = DT + runif(.N, 0.01, 0.02))]
#' data2 <- copy(sampleTData)[, SYMBOL := 'XYZ']
#' 
#' dat <- rbind(data1, data2)
#' setkey(dat, "DT")
#' dat <- spreadPrices(dat)
#' 
#' rCov(dat, alignBy = 'minutes', alignPeriod = 5, makeReturns = TRUE, cor = TRUE) 
#' }
#' @seealso \code{\link{gatherPrices}}
#' @author Emil Sjoerup.
#' @importFrom data.table merge.data.table setkey
#' @importFrom xts is.xts
#' @export
spreadPrices <- function(data){
  SYMBOL <- PRICE <- DT <- NULL
  if(any(!(c("SYMBOL", "PRICE") %in% colnames(data)))){
    stop(paste("Could not find column(s)", 
               paste(c("SYMBOL", "PRICE")[!(c("SYMBOL", "PRICE") %in% colnames(data))], collapse = ", ")), 
         "in data, these columns must be present")
  }
  inputWasXts <- FALSE
  if (!is.data.table(data)) {
    if (is.xts(data)) {
      data <- as.data.table(data)
      data <- setnames(data , old = "index", new = "DT")
      inputWasXts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (!("DT" %in% colnames(data))) {
      stop("Data.table neeeds DT column.")
    }
  }
  splitted <- split(data[,list(DT, PRICE, SYMBOL)], by = 'SYMBOL')
  
  collected <- Reduce(function(x,y) merge.data.table(x,y, by = "DT", all = TRUE), lapply(splitted, function(x){
    name <- as.character(x[1, SYMBOL])
    x <- x[, list(DT,PRICE)]
    setnames(x, old = "PRICE", new = name)
    return(x)
  }))
  
  if (inputWasXts) {
    collected <- as.xts(collected)
    storage.mode(collected) <- 'numeric'
    return(collected)
  } else {
    setkey(collected, "DT")
    return(collected[])
  }
  
}


#' Make TAQ format
#' 
#' Convenience function to gather data from one \code{xts} or \code{data.table} 
#' with at least \code{"DT"}, and d columns containing price data to a \code{"DT"}, \code{"SYMBOL"}, and \code{"PRICE"}
#' column. This function the opposite of \code{\link{spreadPrices}}.
#' 
#' @param data An \code{xts} or a \code{data.table} object with at least \code{"DT"} and d columns with price data with their names corresponding to the respective symbols.
#' 
#' @return a \code{data.table} with columns \code{DT}, \code{SYMBOL}, and \code{PRICE}
#' 
#' @examples
#' \dontrun{
#' library(data.table)
#' data1 <- copy(sampleTData)[,  `:=`(PRICE = PRICE * runif(.N, min = 0.99, max = 1.01),
#'                                                DT = DT + runif(.N, 0.01, 0.02))]
#' data2 <- copy(sampleTData)[, SYMBOL := 'XYZ']
#' dat1 <- rbind(data1[, list(DT, SYMBOL, PRICE)], data2[, list(DT, SYMBOL, PRICE)])
#' setkeyv(dat1, c("DT", "SYMBOL"))
#' dat1
#' dat <- spreadPrices(dat1) # Easy to use for realized measures
#' dat
#' dat <- gatherPrices(dat)
#' dat
#' all.equal(dat1, dat) # We have changed to RM format and back.
#' }
#' @seealso \code{\link{spreadPrices}}
#' @author Emil Sjoerup
#' @importFrom data.table melt
#' @export
gatherPrices <- function(data){
  
  inputWasXts <- FALSE
  if (!is.data.table(data)) {
    if (is.xts(data)) {
      data <- as.data.table(data)
      data <- setnames(data , old = "index", new = "DT")
      inputWasXts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (!("DT" %in% colnames(data))) {
      stop("Data.table neeeds DT column.")
    }
  }
  data <- melt(data, 1, na.rm = TRUE, variable.factor = FALSE)
  setnames(data, old = c("variable", "value"), new = c("SYMBOL", "PRICE"))
  
  if (inputWasXts) {
    data <- as.xts(data)
    storage.mode(data) <- 'numeric'
    return(data)
  } else {
    setkeyv(data, c("DT", "SYMBOL"))
    return(data[])
  }
}
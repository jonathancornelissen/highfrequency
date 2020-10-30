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
#' @param on character, indicating the time scale in which "k" is expressed. Possible values are: "secs", "seconds", "mins", "minutes", "hours", "days", "weeks", "ticks".
#' @param k positive integer, indicating the number of periods to aggregate over. For example, to aggregate an 
#' xts object to the five-minute frequency set k = 5 and on = "minutes".
#' @param weights By default, no weighting scheme is used. 
#' When you assign an xts object with wheights to this argument, a weighted mean is taken over each interval. 
#' Of course, the weights should have the same timestamps as the supplied time series.
#' @param dropna boolean, which determines whether empty intervals should be dropped.
#' By default, an NA is returned in case an interval is empty, except when the user opts
#' for previous tick aggregation, by setting FUN = "previoustick" (default).
#' @param tz character denoting which timezone the output should be in. Defaults to "GMT"
#' @details The timestamps of the new time series are the closing times and/or days of the intervals. 
#' E.g. for a weekly aggregation the new timestamp is the last day in that particular week (namely sunday).
#' 
#' In case of previous tick aggregation, 
#' for on = "seconds"/"minutes"/"hours",
#' the element of the returned series with e.g. timestamp 09:35:00 contains 
#' the last observation up to that point, including the value at 09:35:00 itself.
#' 
#' Please note: In case an interval is empty, by default an NA is returned.. In case e.g. previous 
#' tick aggregation it makes sense to fill these NA's by the function \code{na.locf}
#' (last observation carried forward) from the zoo package.
#' 
#' In case on = "ticks", the sampling is done such the sampling starts on the first tick, and the last tick is always included
#' For example, if 14 observations are made on one day, and these are 1, 2, 3, ... 14.
#' Then, with on = "ticks" and k = 3, the output will be 1, 4, 7, 10, 13, 14.
#' 
#' @return An xts object containing the aggregated time series.
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' @keywords data manipulation
#' 
#' @examples 
#' #load sample price data
#' ts <- sampleTData$PRICE
#' 
#' #Previous tick aggregation to the 5-minute sampling frequency:
#' tsagg5min <- aggregateTS(ts, on = "minutes", k = 5)
#' head(tsagg5min)
#' #Previous tick aggregation to the 30-second sampling frequency:
#' tsagg30sec <- aggregateTS(ts, on = "seconds", k = 30)
#' tail(tsagg30sec)
#' 
#' tsagg3ticks <- aggregateTS(ts, on = "ticks", k = 3)
#' 
#' @importFrom zoo zoo na.locf
#' @importFrom stats start end
#' @importFrom xts period.apply tzone	
#' @export
aggregateTS <- function (ts, FUN = "previoustick", on = "minutes", k = 1, weights = NULL, dropna = FALSE, tz = NULL) {
  
  makethispartbetter <- ((!is.null(weights))| on=="days"| on=="weeks" | (FUN!="previoustick") | dropna)
  if(length(k) > 1){
    k <- k[1]
    warning("K must be of length one. Longer object provided. Using only first entry.")
  }
  
  if (FUN == "previoustick") {
    FUN <- previoustick 
  } else {
    FUN <- match.fun(FUN)
  }
  
  if(is.null(tz)){
    tz <- tzone(ts)
  }
  
  if(on == "ticks"){ ## Special case for on = "ticks"
    if(k < 1 | k%%1 != 0){
      stop("When on is `ticks`, must be a positive integer valued numeric")
    }
    idx <- seq(1, nrow(ts), by = k)
    if(k %% nrow(ts) != 0){
      idx <- c(idx, nrow(ts))
    }
    ts <- ts[idx,]
    return(ts)
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
      ts3 <- xts(ts2, a, tzone = tz)
    }
    if (on == "hours") {
      secs = 3600
      a <- index(ts2) + (secs - as.numeric(index(ts2)) %% secs)
      ts3 <- xts(ts2, a, tzone = tz)
    }
    if (on == "days") {
      secs = 24 * 3600
      a   <- index(ts2) + (secs - as.numeric(index(ts2)) %% secs) - (24 * 3600)
      ts3 <- xts(ts2, a, tzone = tz)
    }
    if (on == "weeks") {
      secs = 24 * 3600 * 7
      a <- (index(ts2) + (secs - (index(ts2) + (3L * 86400L)) %% secs)) - 
        (24 * 3600)
      ts3 <- xts(ts2, a, tzone = tz)
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
    
    ts3 <- xts(ts3, as.POSIXct(index(ts3)), tzone = tz)
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
    
    
    g <- as.POSIXct(newg, origin = "1970-01-01", tz = "GMT")
    firstObs <- ts[1,]
    ts <- na.locf(merge(ts, zoo(NULL, g)))[as.POSIXct(g, tz = tz)]
    if(index(ts[1]) > index(firstObs)){
      index(firstObs) <- index(ts[1]) - secs
      ts <- c(firstObs, ts)
    }
    
    ts <- ts[!duplicated(index(ts), fromLast = TRUE)]
    
    return(ts) 
  }
}


#' Aggregate a time series but keep first and last observation
#' @description Function returns new time series as xts object where first observation is always the opening price
#' and subsequent observations are the closing prices over the interval with as endpoint the timestamp
#' of the result.
#'
#' @param pData data.table or xts object to be aggregated containing the intraday price series, possibly across multiple days.
#' @param on character, indicating the time scale in which "k" is expressed. Possible values are: "milliseconds", "secs", "seconds", "mins", "minutes","hours", "ticks".
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
#' In case on = "ticks", the sampling is done such the sampling starts on the first tick, and the last tick is always included
#' For example, if 14 observations are made on one day, and these are 1, 2, 3, ... 14.
#' Then, with on = "ticks" and k = 3, the output will be 1, 4, 7, 10, 13, 14.
#'
#' @return A data.table or xts object containing the aggregated time series.
#'
#' @author Jonathan Cornelissen, Kris Boudt and Onno Kleen.
#' @keywords data manipulation
#' @examples
#' # aggregate price data to the 30 second frequency
#' aggregatePrice(sampleTDataMicroseconds, on = "secs", k = 30)
#' # aggregate price data to the 30 second frequency including zero return price changes
#' aggregatePrice(sampleTDataMicroseconds, on = "secs", k = 30)
#'
#' # aggregate price data to half a second frequency including zero return price changes
#' aggregatePrice(sampleTDataMicroseconds, on = "milliseconds", k = 500, fill = TRUE)
#' @keywords internal
#' @importFrom xts last tzone
#' @importFrom data.table fifelse
#' @export
aggregatePrice <- function(pData, on = "minutes", k = 1, marketOpen = "09:30:00", marketClose = "16:00:00" , fill = FALSE, tz = NULL) {
  ## checking
  pData <- checkColumnNames(pData)
  .N <- .I <- N <- DATE <- DT <- FIRST_DT <- DT_ROUND <- LAST_DT <- SYMBOL <- PRICE <- NULL

  on_true <- NULL

  if ("PRICE" %in% colnames(pData) == FALSE) {
    stop("data.table or xts needs column named PRICE.")
  }
  if (on == "milliseconds") {
    on_true <- "milliseconds"
    on <- "secs"
    k <- k / 1000
  }
  if(on == "secs" | on == "seconds"){
    scaleFactor <- k
  }
  if(on == "mins" | on == "minutes"){
    scaleFactor <- k * 60
  }
  if(on == "hours"){
    scaleFactor <- k * 60 * 60
  }

  if(! (on %in% c("milliseconds", "secs", "seconds", "mins", "minutes", "hours", "ticks"))){
    stop("on not valid value. Valid values are: \"milliseconds\", \"secs\", \"seconds\", \"mins\", \"minutes\", \"hours\", and \"ticks\".")
  }

  inputWasXts <- FALSE
  if (!is.data.table(pData)) {
    if (is.xts(pData)) {
      inputWasXts <- TRUE
      pData <- setnames(as.data.table(pData)[, PRICE := as.numeric(as.character(PRICE))],
                        old = "index", new = "DT")
      
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    pData <- data.table::copy(pData) # copy
    if (!("DT" %in% colnames(pData))) {
      stop("Data.table neeeds DT column (date-time ).")
    }
    if (!("PRICE" %in% colnames(pData))) {
      stop("Data.table neeeds PRICE column.")
    }
  }
  timeZone <- attr(pData$DT, "tzone")
  if(timeZone == ""){
    if(is.null(tz)){
      tz <- "UTC"
    }
    if(!("POSIXct" %in% class(pData$DT))){
      pData[, DT := as.POSIXct(format(DT, digits = 20, nsmall = 20), tz = tz)]
    }
  } else {
    tz <- timeZone
  }
  setkey(pData, DT) # The below code MAY fail with data with unordered DT column. Also setkey inceases speed of grouping
  ## Checking ends
  # Convert DT to numeric. This is much faster than dealing with the strings (POSIXct)
  pData[, DT := as.numeric(DT, tz = tz)]
  # Calculate the date in the data
  pData[, DATE := as.Date(floor(as.numeric(DT, tz = tz) / 86400), origin = "1970-01-01", tz = tz)]
  # extract a vector of dates
  dates <- unique(pData[,DATE])
  ## Find the opening times of each of the days as numerics.
  marketOpenNumeric <- as.numeric(as.POSIXct(paste(dates, marketOpen), format = "%Y-%m-%d %H:%M:%OS", tz = tz), tz = tz)
  marketCloseNumeric <- as.numeric(as.POSIXct(paste(dates, marketClose), format = "%Y-%m-%d %H:%M:%OS", tz = tz), tz =tz)
  # Find observations per day
  obsPerDay <- pData[, .N, by = DATE][,N]

  ## Here we make sure that we can correctly handle times that happen before midnight in the corrected timestamps from the flag if statements
  if(length(marketOpenNumeric) != length(obsPerDay)){
    if(length(marketOpenNumeric) < length(obsPerDay)){ ## Here we add entries
      marketOpenNumeric <- rep(marketOpenNumeric, length(obsPerDay))[1:length(obsPerDay)]
      marketCloseNumeric <- rep(marketCloseNumeric, length(obsPerDay))[1:length(obsPerDay)]
    } else {
      stop("unknown error occured in aggregatePrice")
    }

  }

  # Subset observations that does not fall between their respective market opening and market closing times.
  pData <- pData[between(DT, rep(marketOpenNumeric, obsPerDay), rep(marketCloseNumeric, obsPerDay))]
  # Find observations per day again
  obsPerDay <- pData[, .N, by = DATE][,N]

  ## Here (AGAIN!) we make sure that we can correctly handle if we deleted a day in the subsetting step above
  if(length(marketOpenNumeric) != length(obsPerDay)){
    if(length(marketOpenNumeric) > length(obsPerDay)){ ## Here we delete entries
      marketOpenNumeric <- rep(marketOpenNumeric, length(obsPerDay))[1:length(obsPerDay)]
      marketCloseNumeric <- rep(marketCloseNumeric, length(obsPerDay))[1:length(obsPerDay)]
    } else {
      stop("unknown error occured in aggregatePrice")
    }


  }

  if(on == "ticks"){ ## Special case for on = "ticks"
    if(k < 1 | k%%1 != 0){
      stop("When on is `ticks`, must be a positive integer valued numeric")
    }
    if(length(unique(as.Date(pData[,DT]))) > 1){
      stop("Multiday support for aggregatePrice with on = \"ticks\" is not implemented yet.")
    }
    idx <- seq(1, nrow(pData), by = k)
    if(k %% nrow(pData) != 0){
      idx <- c(idx, nrow(pData))
    }
    pData <- pData[idx,]
    return(pData[])
  }

  # Find the first observation per day.
  pData[, FIRST_DT := min(DT), by = "DATE"]
  # Use Dirks answer here: https://stackoverflow.com/a/42498175 to round the timestamps to the latest scaleFactor
  pData[, DT_ROUND := fifelse(DT == FIRST_DT,
                             floor(DT/scaleFactor) * scaleFactor,
                             ceiling(DT/scaleFactor) * scaleFactor)]

  pData[, LAST_DT := max(DT), by = "DT_ROUND"]

  # Create the first observation each day.
  pData_open <- pData[pData[DT == FIRST_DT, .I[1], DATE]$V1, c("DT", "PRICE")]
  pData_open[, DT := floor(DT/86400) * 86400 + marketOpenNumeric %% 86400]

  # Take the last observation of each group of LAST_DT
  pData <- pData[pData[DT == LAST_DT, .I[.N], LAST_DT]$V1][, DT := DT_ROUND][, c("DT", "PRICE")] ## Make sure we only take the last observation

  # due to rounding there may be an observation that is refered to the opening time
  pData <- pData[!(DT %in% pData_open$DT)]
  # Merge the opening observation onto the rest of the observations.
  pData <- merge(pData, pData_open, all = TRUE)


  if (fill) {
    # Construct timestamps that go from marketOpenNumeric to marketClose numeric each day with step of scaleFactor e.g. 1 min (60)
    dt_full_index <- data.table(DT = as.numeric(mSeq(marketOpenNumeric, marketCloseNumeric, as.double(scaleFactor))))
    setkey(dt_full_index, DT)
    # Merge the construct the NA.LOCF filled in data.
    pData <- unique(pData[dt_full_index, roll = TRUE, on = "DT"])
  }

  pData[, DT := as.POSIXct(DT, origin = as.POSIXct("1970-01-01", tz = "UTC"), tz = tz)]
  if (inputWasXts) {
    return(xts(as.matrix(pData[, -c("DT")]), order.by = pData$DT, tzone = tz))
  } else {
    return(pData[])
  }
}




#' Aggregate a data.table or xts object containing quote data
#' 
#' @description Function returns a data.table or xts object containing the aggregated quote data with columns "SYMBOL", "EX", "BID","BIDSIZ","OFR","OFRSIZ". 
#' See \code{\link{sampleQData}} for an example of the argument qData.
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
#' qDataAggregated <- aggregateQuotes(sampleQData, on = "seconds", k = 30)
#' head(qDataAggregated)
#' @export
aggregateQuotes <- function(qData, on = "minutes", k = 5, marketOpen = "09:30:00", marketClose = "16:00:00", tz = "GMT") {
  .I <- .N <- N <- DATE <- BID <- OFR <- BIDSIZ <- OFRSIZ <- DT <- FIRST_DT <- DT_ROUND <-LAST_DT <- SYMBOL <- NULL
  
  qData <- checkColumnNames(qData)
  checkqData(qData)
  if (on == "milliseconds") {
    on_true <- "milliseconds"
    on <- "secs"
    k <- k / 1000
  }
  if(on == "secs" | on == "seconds"){
    scaleFactor <- k
  }
  if(on == "mins" | on == "minutes"){
    scaleFactor <- k * 60
  }
  if(on == "hours"){
    scaleFactor <- k * 60 * 60
  }
  inputWasXts <- FALSE
  if (is.data.table(qData) == FALSE) {
    if (is.xts(qData) == TRUE) {
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
    qData <- data.table::copy(qData) #Copy the data so we don't change it in the users environment
    if (("DT" %in% colnames(qData)) == FALSE) {
      stop("Data.table neeeds DT column.")
    }
  }
  
  if ("SYMBOL" %in% colnames(qData)) {
    if (length(unique(qData$SYMBOL)) > 1) {
      stop("Please provide only one symbol at a time.")
    }
  }
  
  timeZone <- attr(qData$DT, "tzone")
  if(timeZone == ""){
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
  dates <- unique(qData[,DATE])
  ## Find the opening times of each of the days as numerics.
  marketOpenNumeric <- as.numeric(as.POSIXct(paste(dates, marketOpen), format = "%Y-%m-%d %H:%M:%OS", tz = tz), tz = tz) 
  marketCloseNumeric <- as.numeric(as.POSIXct(paste(dates, marketClose), format = "%Y-%m-%d %H:%M:%OS", tz = tz), tz =tz)
  # Find observations per day
  obsPerDay <- qData[, .N, by = DATE][,N]
  
  ## Here we make sure that we can correctly handle times that happen before midnight in the corrected timestamps from the flag if statements
  if(length(marketOpenNumeric) != length(obsPerDay)){
    if(length(marketOpenNumeric) < length(obsPerDay)){ ## Here we add entries
      marketOpenNumeric <- rep(marketOpenNumeric, length(obsPerDay))[1:length(obsPerDay)]
      marketCloseNumeric <- rep(marketCloseNumeric, length(obsPerDay))[1:length(obsPerDay)]
    } else {
      stop("unknown error occured in aggregateQuotes")
    }
    
  }
  
  # Subset observations that does not fall between their respective market opening and market closing times.
  qData <- qData[between(DT, rep(marketOpenNumeric, obsPerDay), rep(marketCloseNumeric, obsPerDay))]
  # Find observations per day again
  obsPerDay <- qData[, .N, by = DATE][,N]
  
  ## Here (AGAIN!) we make sure that we can correctly handle if we deleted a day in the subsetting step above
  if(length(marketOpenNumeric) != length(obsPerDay)){
    if(length(marketOpenNumeric) > length(obsPerDay)){ ## Here we delete entries
      marketOpenNumeric <- rep(marketOpenNumeric, length(obsPerDay))[1:length(obsPerDay)]
      marketCloseNumeric <- rep(marketCloseNumeric, length(obsPerDay))[1:length(obsPerDay)]
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
  
  qData[, LAST_DT := max(DT), by = "DT_ROUND"]
  qData[, OFRSIZ := sum(OFRSIZ), by = "DT_ROUND"]
  qData[, BIDSIZ := sum(BIDSIZ), by = "DT_ROUND"]
  
  # Create the first observation each day.
  qData_open <- qData[qData[DT == FIRST_DT, .I[1], DATE]$V1, c("DT", "SYMBOL", "BID", "BIDSIZ", "OFR", "OFRSIZ")]
  qData_open[, DT := floor(DT/86400) * 86400 + marketOpenNumeric %% 86400]
  
  # Take the last observation of each group of LAST_DT 
  qData <- qData[qData[DT == LAST_DT, .I[.N], LAST_DT]$V1][, DT := DT_ROUND][, c("DT", "SYMBOL", "BID", "BIDSIZ", "OFR", "OFRSIZ")] ## Make sure we only take the last observation
  
  # due to rounding there may be an observation that is refered to the opening time
  qData <- qData[!(DT %in% qData_open$DT)]
  # Merge the opening observation onto the rest of the observations.
  qData <- merge(qData, qData_open, all = TRUE)
  
  qData[, DT := as.POSIXct(DT, origin = as.POSIXct("1970-01-01", tz = "UTC"), tz = tz)]
  
  
  if (inputWasXts == TRUE) {
    return(xts(as.matrix(qData[, -c("DT")]), order.by = qData$DT, tzone = tz))
  } else {
    return(qData[])
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
#' tDataAggregated <- aggregateTrades(sampleTData, on = "minutes", k = 5)
#' head(tDataAggregated)
#' @export
aggregateTrades <- function(tData, on = "minutes", k = 5, marketOpen = "09:30:00", marketClose = "16:00:00", tz = "GMT") {
  .I <- .N <- N <- DATE <- SIZE <- DT <- FIRST_DT <- DT_ROUND <- LAST_DT <- SYMBOL <- PRICE <- VWPRICE <- SIZETPRICE <- SIZESUM <- NULL
  tData <- checkColumnNames(tData)
  checktData(tData)
  
  if (on == "milliseconds") {
    on_true <- "milliseconds"
    on <- "secs"
    k <- k / 1000
  }
  if(on == "secs" | on == "seconds"){
    scaleFactor <- k
  }
  if(on == "mins" | on == "minutes"){
    scaleFactor <- k * 60
  }
  if(on == "hours"){
    scaleFactor <- k * 60 * 60
  }
  
  inputWasXts <- FALSE
  if (!is.data.table(tData)) {
    if (is.xts(tData)) {
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
    if (("DT" %in% colnames(tData)) == FALSE) {
      stop("Data.table neeeds DT column (date-time).")
    }
  }
  
  timeZone <- attr(tData$DT, "tzone")
  if(timeZone == ""){
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
  tData[, DATE := as.Date(floor(as.numeric(DT, tz = tz) / 86400), origin = "1970-01-01", tz = tz)]
  # extract a vector of dates
  dates <- unique(tData[,DATE])
  ## Find the opening times of each of the days as numerics.
  marketOpenNumeric <- as.numeric(as.POSIXct(paste(dates, marketOpen), format = "%Y-%m-%d %H:%M:%OS", tz = tz), tz = tz) 
  marketCloseNumeric <- as.numeric(as.POSIXct(paste(dates, marketClose), format = "%Y-%m-%d %H:%M:%OS", tz = tz), tz =tz)
  # Find observations per day
  obsPerDay <- tData[, .N, by = DATE][,N]
  
  ## Here we make sure that we can correctly handle times that happen before midnight in the corrected timestamps from the flag if statements
  if(length(marketOpenNumeric) != length(obsPerDay)){
    if(length(marketOpenNumeric) < length(obsPerDay)){ ## Here we add entries
      marketOpenNumeric <- rep(marketOpenNumeric, length(obsPerDay))[1:length(obsPerDay)]
      marketCloseNumeric <- rep(marketCloseNumeric, length(obsPerDay))[1:length(obsPerDay)]
    } else {
      stop("unknown error occured in aggregateQuotes")
    }
    
  }
  
  # Subset observations that does not fall between their respective market opening and market closing times.
  tData <- tData[between(DT, rep(marketOpenNumeric, obsPerDay), rep(marketCloseNumeric, obsPerDay))]
  # Find observations per day again
  obsPerDay <- tData[, .N, by = DATE][,N]
  
  ## Here (AGAIN!) we make sure that we can correctly handle if we deleted a day in the subsetting step above
  if(length(marketOpenNumeric) != length(obsPerDay)){
    if(length(marketOpenNumeric) > length(obsPerDay)){ ## Here we delete entries
      marketOpenNumeric <- rep(marketOpenNumeric, length(obsPerDay))[1:length(obsPerDay)]
      marketCloseNumeric <- rep(marketCloseNumeric, length(obsPerDay))[1:length(obsPerDay)]
    } else {
      stop("unknown error occured in aggregateQuotes")
    }
  }
  
  # Find the first observation per day.  
  tData[, FIRST_DT := min(DT), by = "DATE"]
  # Use Dirks answer here: https://stackoverflow.com/a/42498175 to round the timestamps to the latest scaleFactor
  tData[, DT_ROUND := fifelse(DT == FIRST_DT,
                             floor(DT/scaleFactor) * scaleFactor,
                             ceiling(DT/scaleFactor) * scaleFactor)]
  tData[, LAST_DT := max(DT), by = "DT_ROUND"]
  tData[, SIZETPRICE := SIZE * PRICE]
  tData[, SIZESUM := sum(SIZE), by = "DT_ROUND"]
  tData[, VWPRICE := sum(SIZETPRICE/SIZESUM), by = "DT_ROUND"]
  tData[, SIZE := SIZESUM]
  
  # Create the first observation each day.
  tData_open <- tData[tData[DT == FIRST_DT, .I[1], DATE]$V1, c("DT", "SYMBOL", "PRICE", "SIZE", "VWPRICE")]
  tData_open[, DT := floor(DT/86400) * 86400 + marketOpenNumeric %% 86400]
  
  # Take the last observation of each group of LAST_DT 
  tData <- tData[tData[DT == LAST_DT, .I[.N], LAST_DT]$V1][, DT := DT_ROUND][, c("DT", "SYMBOL", "PRICE", "SIZE", "VWPRICE")] ## Make sure we only take the last observation
  
  # due to rounding there may be an observation that is refered to the opening time
  tData <- tData[!(DT %in% tData_open$DT)]
  # Merge the opening observation onto the rest of the observations.
  tData <- merge(tData, tData_open, all = TRUE)
  
  tData[, DT := as.POSIXct(DT, origin = as.POSIXct("1970-01-01", tz = "UTC"), tz = tz)]
  
  
  
  if (inputWasXts) {
    return(xts(as.matrix(tData[, -c("DT")]), order.by = tData$DT, tzone = tz))
  } else {
    return(tData[])
  }
}

#' Retain only data from the stock exchange with the highest trading volume
#' 
#' @description Function returns a data.table or xts object containing only observations of the 
#' exchange with the highest value for the variable "SIZE", 
#' i.e. the highest trade volume.
#' @param tData an xts object with at least a column "EX", 
#' indicating the exchange symbol and "SIZE", 
#' indicating the trade volume. 
#' @param printExchange indicates whether the chosen exchange is printed on the console, default is TRUE.
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
#' @return data.table or xts object depending on input
#' 
#' @examples autoSelectExchangeTrades(sampleTDataRawMicroseconds)
#' 
#' @author Jonathan Cornelissen, Kris Boudt and Onno Kleen
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
  if (is.data.table(tData) == FALSE) {
    if (is.xts(tData) == TRUE) {
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
    if (("DT" %in% colnames(tData)) == FALSE) {
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
#' @description Function returns an xts object containing only observations 
#' of the exchange with highest
#' value for the sum of "BIDSIZ" and "OFRSIZ", i.e. the highest quote volume.
#' 
#' @param qData a data.table or xts object with at least a column "EX", indicating the exchange symbol 
#' and columns "BIDSIZ" and "OFRSIZ", indicating 
#' the volume available at the bid and ask respectively.
#' @param printExchange indicates whether the chosen exchange is printed on the console, default is TRUE.
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
#' autoSelectExchangeQuotes(sampleQDataRawMicroseconds)
#'
#' @author Jonathan Cornelissen, Kris Boudt and Onno Kleen
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
  if (is.data.table(qData) == FALSE) {
    if (is.xts(qData) == TRUE) {
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
    if (("DT" %in% colnames(qData)) == FALSE) {
      stop("Data.table neeeds DT column.")
    }
  }
  
  if (length(unique(qData$SYMBOL)) > 1) {
    stop("Please provide only one symbol at a time.")
  }
  
  qData[, SUMVOL := sum(BIDSIZ + OFRSIZ), by = "EX"]
  qData <- qData[SUMVOL == max(SUMVOL)][, -c("SUMVOL")]
  
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


#' Extract data from an xts object for the Exchange Hours Only
#' 
#' @description The function returns data within exchange trading hours
#' "dayBegin" and "dayEnd". By default, dayBegin and dayEnd
#' are set to "09:30:00" and "16:00:00" respectively (see Brownlees and Gallo (2006) for more information on good choices for these arguments).
#' 
#' @param data a data.table or xts object containing the time series data. 
#' Multiple days of input are allowed.
#' @param marketOpen character in the format of \"HH:MM:SS\",
#' specifying the starting hour, minute and second of an exchange
#' trading day.
#' @param marketClose character in the format of \"HH:MM:SS\",
#' specifying the closing hour, minute and second of an exchange
#' trading day.
#' @param tz time zone used, by default: tz = "GMT".
#' 
#' @return xts or data.table object depending on input
#'
#' @references Brownlees, C.T. and Gallo, G.M. (2006). Financial econometric analysis at ultra-high frequency: Data handling concerns. Computational Statistics & Data Analysis, 51, pages 2232-2245.
#' @author Jonathan Cornelissen, Kris Boudt and Onno Kleen.
#' @examples 
#' exchangeHoursOnly(sampleTDataRawMicroseconds)
#' @keywords cleaning
#' @importFrom xts tzone
#' @export
exchangeHoursOnly <- function(data, marketOpen = "09:30:00", marketClose = "16:00:00", tz = NULL) {
  .N <- N <- DATE <- DT <- NULL # needed for data table (otherwise notes pop up in check())
  data <- checkColumnNames(data)
  
  inputWasXts <- FALSE
  if (is.data.table(data) == FALSE) {
    if (is.xts(data) == TRUE) {
      data <- setnames(as.data.table(data), old = "index", new = "DT")
      inputWasXts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(data)) == FALSE) {
      stop("Data.table neeeds DT column.")
    }
  }
  
  timeZone <- attr(data$DT, "tzone")
  if(timeZone == ""){
    if(is.null(tz)){
      tz <- "UTC"
    }
    if(!("POSIXct" %in% class(data$DT))) data[, DT := as.POSIXct(format(DT, digits = 20, nsmall = 20), tz = tz)]
  } else {
    tz <- timeZone
  }
  data <- copy(data) # We need to copy the data so as to not change the user's data
  setkey(data, DT) # The below code MAY fail with data with unordered DT column. Also setkey inceases speed of grouping
  data[, DATE := as.Date(floor(as.numeric(DT, tz = tz) / 86400), origin = "1970-01-01", tz = tz)]
  dates <- unique(data[,DATE])
  # data <- data[DT >= ymd_hms(paste(as.Date(data$DT), dayBegin), tz = tzone(data$DT))]
  # data <- data[DT <= ymd_hms(paste(as.Date(data$DT), dayEnd), tz = tzone(data$DT))]
  marketOpenNumeric <- as.numeric(as.POSIXct(paste(dates, marketOpen), format = "%Y-%m-%d %H:%M:%OS", tz = tz), tz = tz)
  marketCloseNumeric <- as.numeric(as.POSIXct(paste(dates, marketClose), format = "%Y-%m-%d %H:%M:%OS", tz = tz), tz =tz)
  
  obsPerDay <- data[, .N, by = DATE][,N]
  
  ## Here we make sure that we can correctly handle times that happen before midnight in the corrected timestamps from the flag if statements
  if(length(marketOpenNumeric) != length(obsPerDay)){
    if(length(marketOpenNumeric) < length(obsPerDay)){ ## Here we add entries
      marketOpenNumeric <- rep(marketOpenNumeric, length(obsPerDay))[1:length(obsPerDay)]
      marketCloseNumeric <- rep(marketCloseNumeric, length(obsPerDay))[1:length(obsPerDay)]
    } else {
      stop("unknown error occured in aggregatePrice")
    }
    
  }
  
  # Subset observations that does not fall between their respective market opening and market closing times.
  data <- data[between(DT, rep(marketOpenNumeric, obsPerDay), rep(marketCloseNumeric, obsPerDay))]
  
  if (inputWasXts) {
    return(xts(as.matrix(data[, -c("DT")]), order.by = data$DT, tzone = tzone(data$DT)))
  } else {
    return(data[])
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
  inputWasXts <- is.xts(ts)
  l <- dim(ts)[1]
  if(is.null(l)) { ## Special case for ts is numeric vector
    l <- length(ts)
    D <- 1
  } else {
    D <- dim(ts)[2]
  }
  
  col_names <- colnames(ts)
  x <- matrix(as.numeric(ts), nrow = l)
  
  x[(2:l), ] <- log(x[(2:l), ]) - log(x[(1:(l - 1)), ])
  x[1, ] <- rep(0, D)
  if(inputWasXts){
    x <- xts(x, order.by = index(ts))
  }
  colnames(x) <- col_names
  return(x[])
}

#' Match trade and quote data
#' @description Function matches the trades and quotes and returns an xts-object containing both.
#' 
#' @param tData data.table or xts-object containing the trade data (multiple days possible).
#' @param qData data.table or xts-object containing the quote data (multiple days possible).
#' @param lagQuotes numeric, number of seconds the quotes are registered faster than
#' the trades (should be round and positive). Based on the research of
#' Vergote (2005), we set 2 seconds as the default.
#' @param BFM a logical determining whether to conduct 'Backwards - Forwards matching' of trades and quotes.
#' The algorithm tries to match trades that fall outside the bid - ask and first tries to match a small window forwards and if this fails, it tries to match backwards in a bigger window.
#' The small window is a tolerance for inaccuracies in the timestamps of bids and asks. The backwards window allow for matching of late reported trades. I.e. block trades.
#' @param backwardsWindow a numeric denoting the length of the backwards window. Default is 3600, corresponding to one hour.
#' @param forwardsWindow a numeric denoting the length of the forwards window. Default is 0.5, dorresponding to one half second.
#' @param plot a logical denoting whether to visualize the forwards, backwards, and unmatched trades in a plot.
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
#' tqData <- matchTradesQuotes(sampleTData, sampleQData)
#' head(tqData)
#' # multi-day input allowed
#' tqData <- matchTradesQuotes(sampleTDataMicroseconds, sampleQDataMicroseconds)
#' @importFrom xts tzone<- tzone
#' @export
matchTradesQuotes <- function(tData, qData, lagQuotes = 2, BFM = FALSE, backwardsWindow = 3600, forwardsWindow = 0.5, plot = FALSE) {
  
  PRICE <- BID <- OFR <- DATE <- DT <- FIRST_DT <- DT_ROUND <- SYMBOL <- NULL
  
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
    if (("DT" %in% colnames(tData)) == FALSE) {
      stop("tData neeeds DT column.")
    }
  }
  
  if(tzone(tData$DT) != tzone(qData$DT)){
    stop("timezone of the trade data is not the same as the timezone of the quote data")
  }
  tz <- tzone(tData$DT)
  
  setnames(qData, old = "EX", new = "QUOTEEX", skip_absent = TRUE)
  
  if(!BFM){ ## We DONT conduct a backwards- forwards matching search
    
    qData <- copy(qData)[, DT := as.numeric(DT, tz = tz)]
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
    
    qData <- copy(qData)[, DT := as.numeric(DT, tz = tz)]
    qData[, DT := fifelse(DT == min(DT), DT, DT + lagQuotes), by = floor(DT / 86400)]
    tData <- copy(tData)[, DT := as.numeric(DT, tz = tz)]
    out <- BFMalgorithm(tData, qData, backwardsWindow = backwardsWindow, forwardsWindow = forwardsWindow, plot = plot, tz = tz)
    
    return(out[])
    
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
  BID <- OFR <- DT <- SYMBOL <- .SD <- BIDSIZ <- OFRSIZ <- MAXBID <- MAXOFR <- NULL 
  qData <- checkColumnNames(qData)
  checkqData(qData)
  
  condition <- selection == "median" | selection == "max.volume" | selection == "weighted.average"
  if (condition == FALSE) {
    stop(paste("Selection has to be \"median\", \"max.volume\" or \"weighted.average\" "))
  }
  
  inputWasXts <- FALSE
  if (is.data.table(qData) == FALSE) {
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
    if (("DT" %in% colnames(qData)) == FALSE) {
      stop("Data.table neeeds DT column (date-time).")
    }
  }
  # qData <- sampleQDataRawMicroseconds
  # qData <- checkColumnNames(qData)
  # keep summed size columns
  keepCols <- colnames(qData)[!(colnames(qData) %in% c("DT", "SYMBOL","BID", "OFR","BIDSIZ", "OFRSIZ"))]
  keptData <- qData[, lapply(.SD, last), by = list(DT, SYMBOL)][, keepCols, with = FALSE]
  qData_size <- qData[, lapply(.SD, sum), by = list(DT, SYMBOL), .SDcols = c("BIDSIZ", "OFRSIZ")]
  if (selection == "median") {
    qData <- qData[, lapply(.SD, median), by = list(DT, SYMBOL), .SDcols = c("BID", "OFR")]
    
  }
  
  if (selection == "max.volume") {
    qData <- qData[, MAXBID := max(BIDSIZ), by = list(DT, SYMBOL)][, MAXOFR := max(OFRSIZ), by = "DT"][
      , BIDSIZ := fifelse(BIDSIZ == MAXBID, 1, 0)][
      , OFRSIZ := fifelse(OFRSIZ == MAXOFR, 1, 0)][
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
  qData <- cbind(qData, keptData)
  qData <- merge(qData, qData_size, by = c("DT", "SYMBOL"))
  
  if (inputWasXts) {
    return(xts(as.matrix(qData[, -c("DT")]), order.by = qData$DT))
  } else {
    return(qData[])
  }
}
# microbenchmark::microbenchmark(quotesCleanup(qDataRaw = sampleQDataRaw, exchanges = "N", selection = "max.volume"), times = 10, unit = "s")
# microbenchmark::microbenchmark(quotesCleanup(qDataRaw = sampleQDataRaw, exchanges = "N", selection = "maxvolume"), times = 10, unit = "s")

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
#' @note previously this function returned the mean of the size of the merged trades (pre version 0.7 and when not using max.volume as the criterion), now it returns the sum.
#' @return data.table or xts object depending on input
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' @keywords cleaning
#' @export
mergeTradesSameTimestamp <- function(tData, selection = "median") {
  .N <- SIZE <- MAXSIZE <- PRICE <- DT <- SYMBOL <- .SD <- SIZE_WEIGHT <- NULL
  
  tData <- checkColumnNames(tData)
  checktData(tData)
  if (any(colnames(tData) == "SIZE") == FALSE) {
    stop("The argument tData should have a column SIZE indicating the number of shares traded. Could not find that column.")
  }
  
  condition <- selection == "median" | selection == "max.volume" | selection == "weighted.average"
  if (condition == FALSE) {
    stop(paste("Selection has to be \"median\", \"max.volume\" or \"weighted.average\" "))
  }
  
  inputWasXts <- FALSE
  if (is.data.table(tData) == FALSE) {
    if (is.xts(tData) == TRUE) {
      tData <- setnames(as.data.table(tData)[, SIZE := as.numeric(as.character(SIZE))][, PRICE := as.numeric(as.character(PRICE))], 
                        old = "index", new = "DT")
      inputWasXts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(tData)) == FALSE) {
      tData <- tData[, SIZE := as.numeric(as.character(SIZE))][, PRICE := as.numeric(as.character(PRICE))]
      stop("Data.table neeeds DT column (date-time ).")
    }
  }
  
  keepCols <- colnames(tData)[!(colnames(tData) %in% c("DT", "SYMBOL", "PRICE", "SIZE"))]
  keptData <- tData[, lapply(.SD, last), by = list(DT, SYMBOL)][, keepCols, with = FALSE]
  if (selection == "median") {
    tData[, `:=` (PRICE = median(PRICE), NUMTRADES = .N), by = list(DT, SYMBOL)]
    #If there is more than one observation at median price, take the average volume.
    tData[, SIZE := as.numeric(as.character(SIZE))]
    tData[, SIZE := sum(SIZE), by = list(DT, SYMBOL)]
    tData <- unique(tData[, c("DT", "SYMBOL", "PRICE", "SIZE", "NUMTRADES")])
  }
  
  if (selection == "max.volume") {
    tData[, SIZE := as.numeric(as.character(SIZE))]
    tData <- tData[, `:=` (MAXSIZE = max(SIZE), NUMTRADES = .N), by = list(DT, SYMBOL)]
    tData[, SIZE := fifelse(SIZE == MAXSIZE, 1, 0)]
    tData[, PRICE := PRICE * SIZE]
    tData[, PRICE := max(PRICE), by = "DT"]
    tData[, SIZE := MAXSIZE]
    tData[, -c("MAXSIZE")]
    tData <- unique(tData[, c("DT", "SYMBOL", "PRICE", "SIZE", "NUMTRADES")])
  }
  if (selection == "weighted.average") {
    tData[, SIZE := as.numeric(as.character(SIZE))]
    tData <- tData[, `:=` (SIZE_WEIGHT = SIZE / sum(SIZE), NUMTRADES = .N), by = list(DT, SYMBOL)]
    tData[, `:=` (PRICE = sum(PRICE * SIZE_WEIGHT)), by = list(DT, SYMBOL)]
    tData[, SIZE := sum(SIZE), by = list(DT, SYMBOL)]
    tData <- unique(tData[, c("DT", "SYMBOL", "PRICE", "SIZE", "NUMTRADES")])
  }
  tData <- cbind(tData, keptData)
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
  
  inputWasXts <- FALSE
  if (is.data.table(tData) == FALSE) {
    if (is.xts(tData) == TRUE) {
      tData <- setnames(as.data.table(tData)[, PRICE := as.numeric(as.character(PRICE))], old = "index", new = "DT")
      inputWasXts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(tData)) == FALSE) {
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
#' @param qData an xts or data.table object at least containing the columns "BID" and "OFR".
#' 
#' @return xts object or data.table depending on type of input
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' @keywords cleaning
#' @export
noZeroQuotes <- function(qData) {
  BID <- OFR <- DT <- NULL
  qData <- checkColumnNames(qData)
  checkqData(qData)
  
  inputWasXts <- FALSE
  if (is.data.table(qData) == FALSE) {
    if (is.xts(qData) == TRUE) {
      qData <- setnames(as.data.table(qData)[, BID := as.numeric(as.character(BID))][, OFR := as.numeric(as.character(OFR))], old = "index", new = "DT")
      inputWasXts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (("DT" %in% colnames(qData)) == FALSE) {
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
#' @description This is a wrapper function for cleaning the quote data in the entire folder dataSource. 
#' The result is saved in the folder dataDestination. 
#' 
#' In case you supply the argument "qDataRaw", the on-disk functionality is ignored
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
#' @param qDataRaw xts or data.table object containing (ONE stock only) raw quote data. This argument is NULL by default. Enabling it means the arguments
#' from, to, dataSource and dataDestination will be ignored. (only advisable for small chunks of data)
#' @param report boolean and TRUE by default. In case it is true the function returns (also) a vector indicating how many quotes remained after each cleaning step.
#' @param selection argument to be passed on to the cleaning routine \code{\link{mergeQuotesSameTimestamp}}. The default is "median".
#' @param maxi spreads which are greater than median(spreads of day) times maxi are excluded.
#' @param window argument to be passed on to the cleaning routine \code{\link{rmOutliersQuotes}}. 
#' @param type argument to be passed on to the cleaning routine \code{\link{rmOutliersQuotes}}.
#' @param rmoutliersmaxi argument to be passed on to the cleaning routine \code{\link{rmOutliersQuotes}}.
#' @param saveAsXTS indicates whether data should be saved in xts format instead of data.table when using on-disk functionality. TRUE by default.
#' @param tz timezone to use
#' @return The function converts every csv file in dataSource into multiple xts or data.table files.
#' In dataDestination, there will be one folder for each symbol containing .rds files with cleaned data stored either in data.table or xts format.
#' 
#' In case you supply the argument "qDataRaw", the on-disk functionality is ignored
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
#' head(sampleQDataRawMicroseconds)
#' dim(sampleQDataRawMicroseconds)
#' qDataAfterCleaning <- quotesCleanup(qDataRaw = sampleQDataRawMicroseconds, exchanges = "N")
#' qDataAfterCleaning$report
#' dim(qDataAfterCleaning$qData)
#' 
#' # In case you have more data it is advised to use the on-disk functionality
#' #via "dataSource" and "dataDestination" arguments
#' 
#' @importFrom data.table fread
#' @keywords cleaning
#' @export
quotesCleanup <- function(dataSource = NULL, dataDestination = NULL, exchanges = NULL, qDataRaw = NULL, report = TRUE, 
                          selection = "median", maxi = 50, window = 50, type = "advanced", rmoutliersmaxi = 10, saveAsXTS = TRUE, tz = "EST") {
  
  BID <- OFR <- DT <- SPREAD <- SPREAD_MEDIAN <- EX <- DATE <- BIDSIZ <- OFRSIZ <- TIME_M <- SYMBOL <- NULL
  nresult <- c(initial_number = 0,
               no_zero_quotes = 0,
               select_exchange = 0,
               remove_negative_spread = 0,
               remove_large_spread = 0,
               merge_same_timestamp = 0,
               remove_outliers = 0)
  
  if (is.null(qDataRaw) == TRUE) {
    try(dir.create(dataDestination), silent = TRUE)
    
    quotesfiles <- list.files(dataSource, recursive = TRUE)[grepl("quotes", list.files(dataSource, recursive = TRUE))]
    for (ii in quotesfiles) {
      readdata <- try(fread(paste0(dataSource, "/", ii)), silent = TRUE)
      if(colnames(readdata)[1] == "index"){ # The data was saved from an xts object
        readdata <- try(readdata[, DT := as.POSIXct(index, tz = tz, format = "%Y-%m-%dT%H:%M:%OS")])
      } else if ("DT" %in% colnames(readdata)){
        readdata <- try(readdata[, DT := as.POSIXct(DT, tz = tz, format = "%Y-%m-%dT%H:%M:%OS")])
      } else {
        readdata <- try(readdata[, DT := as.POSIXct(substring(paste(as.character(DATE), TIME_M, sep = " "), 1, 20), tz = tz, format = "%Y%m%d %H:%M:%OS")], silent = TRUE)
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
      
      try(dir.create(paste0(dataDestination, "/", strsplit(ii, "/")[[1]][1])), silent = TRUE)
      for (jj in qData) {
        if (saveAsXTS == TRUE) {
          df_result <- xts(as.matrix(jj[, -c("DT", "DATE")]), order.by = jj$DT)
        } else {
          df_result <- jj[, -c( "DATE")]
        }
        saveRDS(df_result, paste0(dataDestination, "/", strsplit(ii, "/")[[1]][1], "/", unique(as.Date(jj$DT, tz = tz)), "quotes.rds"))
        # saveRDS(df_result, paste0(dataDestination, "/", strsplit(ii, "/")[[1]][1], "/", strsplit(strsplit(ii, "/")[[1]][2], ".zip")[1], ".rds"))
      }
    }
  }
  
  if (is.null(qDataRaw) == FALSE) {
    
    qDataRaw <- checkColumnNames(qDataRaw)
    checkqData(qDataRaw)
    
    inputWasXts <- FALSE
    if (is.data.table(qDataRaw) == FALSE) {
      if (is.xts(qDataRaw) == TRUE) {
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
      if (("DT" %in% colnames(qDataRaw)) == FALSE) {
        stop("Data.table neeeds DT column.")
      }
    }
    
    nresult[1] <- dim(qDataRaw)[1] 
    qDataRaw <- qDataRaw[BID != 0 & OFR != 0]
    nresult[2] <- dim(qDataRaw)[1] 
    qDataRaw <- qDataRaw[EX %in% exchanges]
    nresult[3] <- dim(qDataRaw)[1] 
    qDataRaw[OFR > BID, `:=`(SPREAD = OFR - BID, DATE = as.Date(DT))][, SPREAD_MEDIAN := median(SPREAD), by = "DATE"]
    nresult[4] <- dim(qDataRaw)[1] 
    qDataRaw <- qDataRaw[SPREAD < (SPREAD_MEDIAN * maxi)][, -c("SPREAD","SPREAD_MEDIAN")]
    nresult[5] <- dim(qDataRaw)[1]
    qDataRaw <- mergeQuotesSameTimestamp(qData = qDataRaw, selection = selection)
    nresult[6] <- dim(qDataRaw)[1]
    
    qDataRaw <- rmOutliersQuotes(qDataRaw, window = window, type = type, maxi = rmoutliersmaxi)
    nresult[7] <- dim(qDataRaw)[1]
    if (inputWasXts) {
      df_result <- xts(as.matrix(qDataRaw[, -c("DT",  "DATE")]), order.by = qDataRaw$DT)
    } else {
      df_result <- qDataRaw[, -c( "DATE")]
    }
    
    if (report) {
      return(list(qData = df_result, report = nresult))
    } else {
      return(df_result[])
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
#' @export
rmLargeSpread <- function(qData, maxi = 50) {
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
  
  qData <- qData[, DATE := as.Date(DT)][
    , SPREAD := OFR - BID][
    , SPREAD_MEDIAN := median(SPREAD), by = "DATE"][SPREAD < (SPREAD_MEDIAN * maxi)]
  
  if (inputWasXts) {
    return(xts(as.matrix(qData[, -c("DT", "DATE", "SPREAD", "SPREAD_MEDIAN")]), order.by = qData$DT))
  } else {
    return(qData[, -c("DATE", "SPREAD", "SPREAD_MEDIAN")][])
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
#' rmNegativeSpread(sampleQDataRawMicroseconds)
#' 
#' @keywords cleaning
#' @export
rmNegativeSpread <- function(qData) {
  BID <- OFR <- DATE <- DT <- NULL
  qData <- checkColumnNames(qData)
  checkqData(qData)
  inputWasXts <- FALSE
  if (is.data.table(qData) == FALSE) {
    if (is.xts(qData) == TRUE) {
      qData <- setnames(as.data.table(qData), old = "index", new = "DT")
      qData[, BID := as.numeric(as.character(BID))]
      qData[, OFR := as.numeric(as.character(OFR))]
      inputWasXts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } 
  
  qData <- qData[OFR > BID]
  
  if (inputWasXts == TRUE) {
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
#' @param tData a data.table or xts object containing the time series data, with at least the column "PRICE", containing the transaction price.
#' @param qData a data.table or xts object containing the time series data with at least the columns "BID" and "OFR", containing the bid and ask prices.
#' @param lagQuotes a numeric of length 1 that denotes how many seconds to lag the quotes. Default is 2 seconds. See Details.
#' @param BFM a logical determining whether to conduct 'Backwards - Forwards matching' of trades and quotes.
#' The algorithm tries to match trades that fall outside the bid - ask and first tries to match a small window forwards and if this fails, it tries to match backwards in a bigger window.
#' The small window is a tolerance for inaccuracies in the timestamps of bids and asks. The backwards window allow for matching of late reported trades. I.e. block trades.
#' @param backwardsWindow a numeric denoting the length of the backwards window. Default is 3600, corresponding to one hour.
#' @param forwardsWindow a numeric denoting the length of the forwards window. Default is 0.5, dorresponding to one half second.
#' @param plot a logical denoting whether to visualize the forwards, backwards, and unmatched trades in a plot.
#' @details Note: in order to work correctly, the input data of this function should be
#' cleaned trade (tData) and quote (qData) data respectively.
#' In older high frequency datasets the trades frequently lag the quotes. In newer datasets this tends to happen 
#' only during extreme market activity when exchange networks are at maximum capacity.
#' 
#' @return xts or data.table object depending on input
#' 
#' @references  Vergote, O. (2005). How to match trades and quotes for NYSE stocks?
#' K.U.Leuven working paper.
#' 
#' @author Jonathan Cornelissen, Kris Boudt and Onno Kleen
#' @keywords cleaning
#' @importFrom data.table setkey set
#' @export
rmTradeOutliersUsingQuotes <- function(tData, qData, lagQuotes = 2, BFM = FALSE, backwardsWindow = 3600, forwardsWindow = 0.5, plot = FALSE) {
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
    if (("DT" %in% colnames(tData)) == FALSE) {
      stop("tData neeeds DT column.")
    }
  }
  
  
  
  
  tqData <- matchTradesQuotes(tData, qData, lagQuotes = lagQuotes, BFM = BFM, backwardsWindow = backwardsWindow, forwardsWindow = forwardsWindow, plot = plot)
  
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
  BID <- OFR <- MIDQUOTE <- DATE <- DT <- MADALL <- CRITERION <- NULL
  if ((window %% 2) != 0) {
    stop("Window size can't be odd.")
  }
  
  qData <- checkColumnNames(qData)
  checkqData(qData)
  
  inputWasXts <- FALSE
  if (is.data.table(qData) == FALSE) {
    if (is.xts(qData) == TRUE) {
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
    if (("DT" %in% colnames(qData)) == FALSE) {
      stop("Data.table neeeds DT column.")
    }
  }
  
  if (length(unique(qData$SYMBOL)) > 1) {
    stop("Please provide only one symbol at a time.") # This may actually not be needed!
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
  if (inputWasXts) {
    return(xts(as.matrix(qData[, -c("DT", "DATE", "MADALL", "CRITERION", "MIDQUOTE")]), order.by = qData$DT))
  } else {
    return(qData[, -c("MADALL", "CRITERION")][])
  }
}

#' \link{salesCondition} is deprecated. Use \link{tradesCondition} instead.
#' 
#' @description Function deletes entries with abnormal trades condition
#' 
#' @param tData an xts or data.table object containing the time series data, with 
#' one column named "COND" indicating the Sale Condition.
#' @param validConds a character vector containing valid sales conditions defaults to \code{c('', '@', 'E', '@E', 'F', 'FI', '@F', '@FI', 'I', '@I')}. See details.
#' @keywords leaning
#' @export
salesCondition <- function(tData, validConds = c('', '@', 'E', '@E', 'F', 'FI', '@F', '@FI', 'I', '@I')) {
  .Deprecated("tradesCondition")
  tradesCondition(tData = tData, validConds = validConds)
}

#' Delete entries with abnormal trades condition.
#' 
#' @description Function deletes entries with abnormal trades condition
#' 
#' @param tData an xts or data.table object containing the time series data, with 
#' one column named "COND" indicating the Sale Condition.
#' @param validConds a character vector containing valid sales conditions defaults to \code{c('', '@', 'E', '@E', 'F', 'FI', '@F', '@FI', 'I', '@I')}. See details.
#' 
#' @details To get more information on the sales conditions, see the NYSE documentation. Section about Daily TAQ Trades File.
#' The current version (as of May 2020) can be found online at \href{https://www.nyse.com/publicdocs/nyse/data/Daily_TAQ_Client_Spec_v3.3.pdf}{NYSE's webpage}
#' @note Some CSV readers and the WRDS API parses empty strings as NA's. We transform \code{NA} values in COND to \code{""}.
#' @return xts or data.table object depending on input
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' 
#' @keywords cleaning
#' @export
tradesCondition <- function(tData, validConds = c('', '@', 'E', '@E', 'F', 'FI', '@F', '@FI', 'I', '@I')) {
  COND <- NULL
  tData <- checkColumnNames(tData)
  checktData(tData)
  
  if (any(colnames(tData) == "COND") == FALSE) {
    stop("The argument tData should have a column containing sales conditions named COND. Could not find that column.")
  }
  
  inputWasXts <- FALSE
  if (is.data.table(tData) == FALSE) {
    if (is.xts(tData) == TRUE) {
      tData <- setnames(as.data.table(tData), old = "index", new = "DT")
      tData[, COND := as.character(COND)]
      inputWasXts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  }
  tData[is.na(COND), COND := ""]
  
  # setnafill(tData, type = "const", fill = "", cols = "COND") ## For when characters become supported :)
  tData <- tData[trimws(COND) %in% validConds]
  
  if (inputWasXts) {
    return(xts(as.matrix(tData), order.by = tData$DT))
  } else {
    return(tData[])
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
  EX  <- NULL
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
#' @param tDataRaw xts object containing (for ONE stock only) raw trade data. This argument is NULL by default. Enabling it means the arguments
#' from, to, dataSource and dataDestination will be ignored. (only advisable for small chunks of data)
#' @param report boolean and TRUE by default. In case it is true the function returns (also) a vector indicating how many trades remained after each cleaning step.
#' @param selection argument to be passed on to the cleaning routine \code{\link{mergeTradesSameTimestamp}}. The default is "median".
#' @param validConds character vector containing valid sales conditions. Passed through to \code{\link{salesCondition}}.
#' @param saveAsXTS indicates whether data should be saved in xts format instead of data.table when using on-disk functionality. TRUE by default.
#' @param tz timezone to use
#' @return For each day an xts or data.table object is saved into the folder of that date, containing the cleaned data.
#' This procedure is performed for each stock in "ticker".
#' The function returns a vector indicating how many trades remained after each cleaning step.
#' 
#' In case you supply the argument "rawtData", the on-disk functionality is ignored
#' and the function returns a list with the cleaned trades as xts object (see examples).
#' 
#' @examples 
#' # Consider you have raw trade data for 1 stock for 2 days 
#' head(sampleTDataRawMicroseconds)
#' dim(sampleTDataRawMicroseconds)
#' tDataAfterFirstCleaning <- tradesCleanup(tDataRaw = sampleTDataRaw, exchanges = list("N"))
#' tDataAfterFirstCleaning$report
#' dim(tDataAfterFirstCleaning$tData)
#' 
#' #In case you have more data it is advised to use the on-disk functionality
#' #via "dataSource" and "dataDestination" arguments
#' 
#' @references Barndorff-Nielsen, O. E., P. R. Hansen, A. Lunde, and N. Shephard (2009). Realized kernels in practice: Trades and quotes. Econometrics Journal 12, C1-C32.
#' 
#' Brownlees, C.T. and Gallo, G.M. (2006). Financial econometric analysis at ultra-high frequency: Data handling concerns. Computational Statistics & Data Analysis, 51, pp. 2232-2245.
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' @importFrom data.table fread
#' @keywords cleaning
#' @export
tradesCleanup <- function(dataSource = NULL, dataDestination = NULL, exchanges, tDataRaw = NULL, report = TRUE, selection = "median",
                          validConds = c('', '@', 'E', '@E', 'F', 'FI', '@F', '@FI', 'I', '@I'), saveAsXTS = TRUE, tz = "EST") {
  SIZE <- SYMBOL <- PRICE <- EX <- COND <- DT <- DATE <- TIME_M <- NULL
  
  if (is.null(tDataRaw) == TRUE) {
    try(dir.create(dataDestination), silent = TRUE)
    tradesfiles <- list.files(dataSource, recursive = TRUE)[grepl("trades", list.files(dataSource, recursive = TRUE))]
    for (ii in tradesfiles) {
      readdata <- try(fread(paste0(dataSource, "/", ii)), silent = TRUE)
      if(colnames(readdata)[1] == "index"){ # The data was saved from an xts object
        readdata <- try(readdata[, DT := as.POSIXct(index, tz = tz, format = "%Y-%m-%dT%H:%M:%OS")])
      } else if ("DT" %in% colnames(readdata)){
        readdata <- try(readdata[, DT := as.POSIXct(DT, tz = tz, format = "%Y-%m-%dT%H:%M:%OS")])
      } else {
        readdata <- try(readdata[, DT := as.POSIXct(substring(paste(as.character(DATE), TIME_M, sep = " "), 1, 20), tz = tz, format = "%Y%m%d %H:%M:%OS")], silent = TRUE)
      }
      tData <- try(tradesCleanup(tDataRaw = readdata,
                                 selection = selection,
                                 exchanges = exchanges,
                                 validConds = validConds, tz = tz))$tData
      tData <- tData[, DATE := as.Date(DT, tz = tz)]
      tData <- split(tData, by = "DATE")
      
      try(dir.create(paste0(dataDestination, "/", strsplit(ii, "/")[[1]][1])), silent = TRUE)
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
  
  if (is.null(tDataRaw) == FALSE) {
    nresult <- c(initial_number = 0,
                 no_zero_trades = 0,
                 select_exchange = 0,
                 remove_sales_condition = 0,
                 merge_same_timestamp = 0)
    
    tDataRaw <- checkColumnNames(tDataRaw)
    checktData(tDataRaw)
    
    inputWasXts <- FALSE
    if (is.data.table(tDataRaw) == FALSE) {
      if (is.xts(tDataRaw) == TRUE) {
        
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
      if (("DT" %in% colnames(tDataRaw)) == FALSE) {
        stop("Data.table neeeds DT column.")
      }
    }
    nresult[1] <- dim(tDataRaw)[1] 
    tDataRaw <- tDataRaw[PRICE != 0]
    nresult[2] <- dim(tDataRaw)[1] 
    tDataRaw <- tDataRaw[EX %in% exchanges]
    nresult[3] <- dim(tDataRaw)[1] 
    tDataRaw <- tradesCondition(tDataRaw, validConds)
    nresult[4] <- dim(tDataRaw)[1] 
    tDataRaw <- mergeTradesSameTimestamp(tDataRaw, selection = selection)
    nresult[5] <- dim(tDataRaw)[1] 
    
    if (inputWasXts) {
      df_result <- xts(as.matrix(tDataRaw[, -c("DT")]), order.by = tDataRaw$DT)
    } else {
      df_result <- tDataRaw
    }
    
    if (report) {
      return(list(tData = df_result, report = nresult))
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
#' @param dataDestination character indicating the folder in which the cleaned data is stored, folder of dataSource by default.
#' @param tData data.table or xts object containing (ONE day and for ONE stock only) trade data cleaned by \code{\link{tradesCleanup}}. This argument is NULL by default. Enabling it, means the arguments
#' from, to, dataSource and dataDestination will be ignored. (only advisable for small chunks of data)
#' @param qData data.table or xts object containing (ONE day and for ONE stock only) cleaned quote data. This argument is NULL by default. Enabling it means the arguments
#' from, to, dataSource, dataDestination will be ignored. (only advisable for small chunks of data)
#' @param lagQuotes passed through to rmTradeOutliersUsingQuotes. \code{\link{rmTradeOutliersUsingQuotes}}
#' 
#' @return For each day an xts object is saved into the folder of that date, containing the cleaned data.
#' 
#' @details 
#' In case you supply the arguments "tData" and "qData", the on-disk functionality is ignored
#' and the function returns cleaned trades as a data.table or xts object (see examples).
#' 
#' When using the on-disk functionality and tradeDataSource and quoteDataSource are the same, the quote files are all files in the folder that contains 'quote', and the rest are treated as containing trade data.
#' 
#' @references Barndorff-Nielsen, O. E., P. R. Hansen, A. Lunde, and N. Shephard (2009). Realized kernels in practice: Trades and quotes. Econometrics Journal 12, C1-C32.
#' 
#' Brownlees, C.T. and Gallo, G.M. (2006). Financial econometric analysis at ultra-high frequency: Data handling concerns. Computational Statistics & Data Analysis, 51, pages 2232-2245.
#' 
#' @author Jonathan Cornelissen, Kris Boudt and Onno Kleen.
#' 
#' @examples 
#' # Consider you have raw trade data for 1 stock for 2 days 
#' tDataAfterFirstCleaning <- tradesCleanup(tDataRaw = sampleTDataRawMicroseconds, 
#'                                           exchanges = "N", report = FALSE)
#' # 
#' qData <- quotesCleanup(qDataRaw = sampleQDataRawMicroseconds, 
#'                        exchanges = "N", report = FALSE)
#' dim(tDataAfterFirstCleaning)
#' tDataAfterFinalCleaning <- 
#'   tradesCleanupUsingQuotes(qData = qData[as.Date(DT) == "2018-01-02"],
#'                            tData = tDataAfterFirstCleaning[as.Date(DT) == "2018-01-02"])
#' dim(tDataAfterFinalCleaning)
#' #In case you have more data it is advised to use the on-disk functionality
#' #via the "tradeDataSource", "quoteDataSource", and "dataDestination" arguments
#' @keywords cleaning
#' @export
tradesCleanupUsingQuotes <- function(tradeDataSource = NULL, quoteDataSource = NULL, dataDestination = NULL, tData = NULL, qData = NULL, lagQuotes = 2) {
  
  if (is.null(dataDestination) == TRUE) {
    dataDestination <- tradeDataSource
  }
  
  if ((!is.null(tData)) & (!is.null(qData))) {
    tData <- checkColumnNames(tData)
    qData <- checkColumnNames(qData)
    
    #1 cleaning procedure that needs cleaned trades and quotes
    tData <- rmTradeOutliersUsingQuotes(tData, qData, lagQuotes = lagQuotes)
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
      dir.create(dataDestination)
    }
    
    ## Make regular expression to find the tradeDatasource in tradeFiles, so we can create a destination that follows same 
    ## naming conventions for the end files.
    finalDestinations <- tradeFiles
    finalDestinations <- sapply(finalDestinations , strsplit, split = .Platform$file.sep)
    
    for (i in 1:length(finalDestinations)) {
      
      ## We change the directory to the dataDestination directory
      finalDestinations[[i]][grepl(tradeDataSource, finalDestinations[[i]],)] <- dataDestination
      
      ## substitute in tradescleanedbyquotes right before the extension.
      finalDestinations[[i]][length(finalDestinations[[i]])] <- 
        sub("\\.", "tradescleanedbyquotes.", finalDestinations[[i]][length(finalDestinations[[i]])])
    }
    
    finalDestinations <- unlist(lapply(finalDestinations, paste0, collapse = .Platform$file.sep))
    ## Check if the directories exist
    if(any(!file.exists(dirname(finalDestinations)))){
      sapply(dirname(finalDestinations), dir.create, showWarnings = FALSE)
    }
    
    for (i in 1:length(tradeFiles)) {
      
      tradeFile <- tradeFiles[i]
      quoteFile <- quoteFiles[i]
      
      tData <- try(readRDS(tradeFile))
      qData <- try(readRDS(quoteFile))
      saveRDS(rmTradeOutliersUsingQuotes(tData, qData, lagQuotes = lagQuotes), file = finalDestinations[i])
      
      
      
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
#' @param pData a list. Each list-item contains an xts or a data.table object (with first column DT (datetime)) containing the original time series (one day only and typically a price series).
#' @param sort logical determining whether to sort the index based on a criterion (will only sort descending (i.e. most liquid first)). Default is FALSE
#' @param criterion character determining which criterion used. Currently supports "squared duration" and "duration". Default is "squared duration".
#' 
#' @return An xts or data.table object containing the synchronized time series - depending on the input.
#' 
#' @references Harris, F., T. McInish, G. Shoesmith, and R. Wood (1995). Cointegration, error correction, and price discovery on infomationally linked security markets. Journal of Financial and Quantitative Analysis 30, 563-581.
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
#' @author Jonathan Cornelissen and Kris Boudt
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
    if(!all(sapply(pData, function(x) c("DT", "PRICE") %in% colnames(x)))){
      stop("DT and PRICE must be present in the data")
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
    
    
    temp <- refreshTimeMathing(coredata(temp), index(temp))
    temp <- xts(temp[[1]], order.by = as.POSIXct(temp[[2]], tz = tz_, origin = as.POSIXct("1970-01-01", tz = tz_)))
    names(temp) <- nameVec # Set names 
    return(temp)
  } else {
    DT <- NULL
    
    timeZone <- attr(pData[[1]]$DT, "tzone")
    
    if(timeZone == ""){
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
    pData <- lapply(pData, copy)
    
    for (i in 1:length(pData)) {
      pData[[i]][,DT := as.numeric(DT, tz = tz)]
      setkey(pData[[i]], "DT")
      
      # if(flag)  setnames(pData[[i]],  c("DT", paste0(colnames(pData[[i]])[-1] , i)))
    }
    # mergeOverload <- function(x,y) merge.data.table(x, y, all = TRUE, on = "DT")
    
    # pData <- Reduce(mergeOverload, pData)
    
    pData <- Reduce(function(x,y) merge.data.table(x, y, all = TRUE, on = "DT"), pData)
    
    pData <- refreshTimeMathing(as.matrix(pData[,-"DT"]), pData$DT)
    pData <- data.table(pData[[2]], pData[[1]])
    colnames(pData) <- c("DT", names)
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
#' @param pData xts or data.table containing data to aggregate.
#' @param measure character denoting which measure to use. Valid options are "intensity", "vol", and "volume", denoting the trade intensity process of Oomen (2005),
#' volatility, and volume, respectively. Default is "volume"
#' @param obs integer valued numeric of length 1 denoting how many observations is wanted after the aggregation procedure.
#' @param bandwidth numeric of length one, denoting which bandwidth parameter to use in the trade intensity process estimation of Oomen (2005.)
#' @param ... extra arguments passed on to \code{\link{spotVol}} when measure is "vol"
#' 
#' @return A list containing "pData" which is the aggregated data and a list containing the intensity process, split up day by day.
#' 
#' @examples
#' pData <- sampleTData[,c("PRICE", "SIZE")]
#' storage.mode(pData) <- "numeric"
#' # Aggregate based on the trade intensity measure. Getting 390 observations.
#' agged <- businessTimeAggregation(pData, measure = "intensity", obs = 390, bandwidth = 0.075)
#' # Plot the trade intensity measure
#' plot.ts(agged$intensityProcess$`2008-01-04`)
#' rCov(agged$pData[,"PRICE"], makeReturns = TRUE)
#' rCov(pData[,"PRICE"], makeReturns = TRUE, alignBy = "minutes", alignPeriod = 1)
#' 
#' # Aggregate based on the volume measure. Getting 78 observations.
#' agged <- businessTimeAggregation(pData, measure = "volume", obs = 78)
#' rCov(agged$pData[,"PRICE"], makeReturns = TRUE)
#' rCov(pData[,"PRICE"], makeReturns = TRUE, alignBy = "minutes", alignPeriod = 5)
#' 
#' @references Roel C. A. Oomen Properties of realized variance under alternative sampling schemes. (2006) Journal of Business & Economic Statistics 24, pages 219-237
#' 
#' Yingjie Dong and Yiu Kuen Tse. (2017) Business time sampling scheme with applications to testing semi-martingale hypothesis and estimating integrated volatility. Econometrics, 5
#' 
#' @importFrom zoo index
#' @importFrom xts is.xts
#' @importFrom data.table copy as.xts.data.table
#' @author Emil Sjoerup
#' @export
businessTimeAggregation <- function(pData, measure = "volume", obs = 390, bandwidth = 0.075, ...){
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
  
  
  dates <- as.character(unique(as.Date(pData[,DT])))
  pDataBackcup <- copy(pData)
  ITP <- list() # Container for trade intensity process.
  for (date in dates) {
    pData <- pDataBackcup[as.Date(DT) == date,]
    if(measure == "intensity"){
      time <- as.numeric(pData[, DT])
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


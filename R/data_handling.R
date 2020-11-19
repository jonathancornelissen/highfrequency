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
#' @param alignBy character, indicating the time scale in which "alignPeriod" is expressed. Possible values are: "secs", "seconds", "mins", "minutes", "hours", "days", "weeks", "ticks".
#' @param alignPeriod positive integer, indicating the number of periods to aggregate over. For example, to aggregate an 
#' xts object to the five-minute frequency set alignPeriod = 5 and alignBy = "minutes".
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
#' for alignBy = "seconds"/"minutes"/"hours",
#' the element of the returned series with e.g. timestamp 09:35:00 contains 
#' the last observation up to that point, including the value at 09:35:00 itself.
#' 
#' Please note: In case an interval is empty, by default an NA is returned.. In case e.g. previous 
#' tick aggregation it makes sense to fill these NA's by the function \code{na.locf}
#' (last observation carried forward) from the zoo package.
#' 
#' In case alignBy = "ticks", the sampling is done such the sampling starts on the first tick, and the last tick is always included
#' For example, if 14 observations are made on one day, and these are 1, 2, 3, ... 14.
#' Then, with alignBy = "ticks" and alignPeriod = 3, the output will be 1, 4, 7, 10, 13, 14.
#' 
#' @return An xts object containing the aggregated time series.
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' @keywords data manipulation
#' 
#' @examples 
#' #load sample price data
#' \dontrun{
#' library(xts)
#' ts <- as.xts(sampleTDataMicroseconds[, list(DT, PRICE, SIZE)])
#' 
#' #Previous tick aggregation to the 5-minute sampling frequency:
#' tsagg5min <- aggregateTS(ts, alignBy = "minutes", alignPeriod = 5)
#' head(tsagg5min)
#' #Previous tick aggregation to the 30-second sampling frequency:
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
aggregateTS <- function (ts, FUN = "previoustick", alignBy = "minutes", alignPeriod = 1, weights = NULL, dropna = FALSE, tz = NULL) {
  
  makethispartbetter <- ((!is.null(weights))| alignBy=="days"| alignBy=="weeks" | (FUN!="previoustick") | dropna)
  if(length(alignPeriod) > 1){
    alignPeriod <- alignPeriod[1]
    warning("alignPeriod must be of length one. Longer object provided. Using only first entry.")
  }
  
  if (FUN == "previoustick") {
    FUN <- previoustick 
  } else {
    FUN <- match.fun(FUN)
  }
  
  if(is.null(tz)){
    tz <- tzone(ts)
  }
  
  if(alignBy == "ticks"){ ## Special case for alignBy = "ticks"
    if(alignPeriod < 1 | alignPeriod%%1 != 0){
      stop("When alignBy is `ticks`, must be a positive integer valued numeric")
    }
    idx <- seq(1, nrow(ts), by = alignPeriod)
    if(alignPeriod %% nrow(ts) != 0){
      idx <- c(idx, nrow(ts))
    }
    ts <- ts[idx,]
    return(ts)
  }
  
  if (makethispartbetter)  {
    
    if (is.null(weights)) {
      ep <- endpoints(ts, alignBy, alignPeriod)
      if (dim(ts)[2] == 1) { 
        ts2 <- period.apply(ts, ep, FUN) 
      }
      if (dim(ts)[2] > 1) {  
        ts2 <- xts(apply(ts, 2, FUN = periodApply2, FUN2 = FUN, INDEX = ep), order.by = index(ts)[ep],)
      }
    } else {
      tsb <- cbind(ts, weights)
      ep  <- endpoints(tsb, alignBy, alignPeriod)
      ts2 <- period.apply(tsb, ep, FUN = match.fun(weightedaverage))
    }
    if (alignBy == "minutes" | alignBy == "mins" | alignBy == "secs" | alignBy == "seconds") {
      if (alignBy == "minutes" | alignBy == "mins") {
        secs = alignPeriod * 60
      }
      if (alignBy == "secs" | alignBy == "seconds") {
        secs <- alignPeriod
      }
      a <- index(ts2) + (secs - as.numeric(index(ts2)) %% secs)
      ts3 <- xts(ts2, a, tzone = tz)
    }
    if (alignBy == "hours") {
      secs = 3600
      a <- index(ts2) + (secs - as.numeric(index(ts2)) %% secs)
      ts3 <- xts(ts2, a, tzone = tz)
    }
    if (alignBy == "days") {
      secs = 24 * 3600
      a   <- index(ts2) + (secs - as.numeric(index(ts2)) %% secs) - (24 * 3600)
      ts3 <- xts(ts2, a, tzone = tz)
    }
    if (alignBy == "weeks") {
      secs = 24 * 3600 * 7
      a <- (index(ts2) + (secs - (index(ts2) + (3L * 86400L)) %% secs)) - 
        (24 * 3600)
      ts3 <- xts(ts2, a, tzone = tz)
    }
    
    if (!dropna) {
      if (alignBy != "weeks" & alignBy != "days") {
        if (alignBy == "secs" | alignBy == "seconds") {
          tby <- "s"
        }
        if (alignBy == "mins" | alignBy == "minutes") {
          tby <- "min"
        }
        if (alignBy == "hours") {
          tby <- "h"
        }
        by <- paste(alignPeriod, tby, sep = " ")
        allindex <- as.POSIXct(seq(start(ts3), end(ts3), by = by))
        xx <- xts(rep("1", length(allindex)), order.by = allindex)
        ts3 <- merge(ts3, xx)[, (1:dim(ts)[2])]
      }
    }
    
    ts3 <- xts(ts3, as.POSIXct(index(ts3)), tzone = tz)
    return(ts3)
  }
  
  if(!makethispartbetter){
    if (alignBy == "secs" | alignBy == "seconds") { 
      secs <- alignPeriod 
      tby <- paste(alignPeriod, "sec", sep = " ")
    }
    if (alignBy == "mins" | alignBy == "minutes") { 
      secs <- 60*alignPeriod
      tby = paste(60*alignPeriod,"sec",sep=" ")
    }
    if (alignBy == "hours") { 
      secs <- 3600 * alignPeriod 
      tby <- paste(3600 * alignPeriod, "sec", sep=" ")
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
#' @param alignBy character, indicating the time scale in which "alignPeriod" is expressed. Possible values are: "milliseconds", "secs", "seconds", "mins", "minutes","hours", "ticks".
#' @param alignPeriod positive integer, indicating the number of periods to aggregate over; e.g. to aggregate a
#' xts object to the 5 minute frequency set alignPeriod = 5 and alignBy = "minutes".
#' @param marketOpen the market opening time, by default: marketOpen = "09:30:00".
#' @param marketClose the market closing time, by default: marketClose = "16:00:00".
#' @param fill indicates whether rows without trades should be added with the most recent value, FALSE by default.
#' @param tz time zone used, by default: tz = timezone of DT column/index of xts.
#'
#' @details
#' The timestamps of the new time series are the closing times and/or days of the intervals.
#'
#' In case of previous tick aggregation or alignBy = "seconds"/"minutes"/"hours",
#' the element of the returned series with e.g. timestamp 09:35:00 contains
#' the last observation up to that point, including the value at 09:35:00 itself.
#'
#' In case alignBy = "ticks", the sampling is done such the sampling starts on the first tick, and the last tick is always included
#' For example, if 14 observations are made on one day, and these are 1, 2, 3, ... 14.
#' Then, with alignBy = "ticks" and alignPeriod = 3, the output will be 1, 4, 7, 10, 13, 14.
#'
#' @return A data.table or xts object containing the aggregated time series.
#'
#' @author Jonathan Cornelissen, Kris Boudt and Onno Kleen.
#' @keywords data manipulation
#' @examples
#' # aggregate price data to the 30 second frequency
#' aggregatePrice(sampleTDataMicroseconds, alignBy = "secs", alignPeriod = 30)
#' # aggregate price data to the 30 second frequency including zero return price changes
#' aggregatePrice(sampleTDataMicroseconds, alignBy = "secs", alignPeriod = 30)
#'
#' # aggregate price data to half a second frequency including zero return price changes
#' aggregatePrice(sampleTDataMicroseconds, alignBy = "milliseconds", alignPeriod = 500, fill = TRUE)
#' @keywords internal
#' @importFrom xts last tzone
#' @importFrom data.table fifelse
#' @export
aggregatePrice <- function(pData, alignBy = "minutes", alignPeriod = 1, marketOpen = "09:30:00", marketClose = "16:00:00" , fill = FALSE, tz = NULL) {
  ## checking
  pData <- checkColumnNames(pData)
  .N <- .I <- N <- DATE <- DT <- FIRST_DT <- DT_ROUND <- LAST_DT <- SYMBOL <- PRICE <- NULL


  if (!("PRICE" %in% colnames(pData))) {
    stop("data.table or xts needs column named PRICE.")
  }
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
      inputWasXts <- TRUE
      pData <- setnames(as.data.table(pData)[, PRICE := as.numeric(as.character(PRICE))],
                        old = "index", new = "DT")
      
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
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

  if(alignBy == "ticks"){ ## Special case for alignBy = "ticks"
    if(alignPeriod < 1 | alignPeriod%%1 != 0){
      stop("When alignBy is `ticks`, must be a positive integer valued numeric")
    }
    if(length(unique(as.Date(pData[,DT]))) > 1){
      stop("Multiday support for aggregatePrice with alignBy = \"ticks\" is not implemented yet.")
    }
    idx <- seq(1, nrow(pData), by = alignPeriod)
    if(alignPeriod %% nrow(pData) != 0){
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
#' See \code{\link{sampleQDataMicroseconds}} for an example of the argument qData.
#' 
#' @param qData data.table or xts object to be aggregated, containing the intraday quote data of a stock for one day.
#' @param alignBy character, indicating the time scale in which "alignPeriod" is expressed. Possible values are: "secs", "seconds", "mins", "minutes","hours".
#' xts object to the 5 minute frequency, set alignPeriod=5 and alignBy = "minutes".
#' @param alignPeriod positive integer, indicating the number of periods to aggregate over. E.g. to aggregate an
#' object to the 5 minute frequency set alignPeriod = 5 and alignBy = "minutes".
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
#' qDataAggregated <- aggregateQuotes(sampleQDataMicroseconds, alignBy = "seconds", alignPeriod = 30)
#' qDataAggregated # Show the aggregated data
#' @export
aggregateQuotes <- function(qData, alignBy = "minutes", alignPeriod = 5, marketOpen = "09:30:00", marketClose = "16:00:00", tz = "GMT") {
  .I <- .N <- N <- DATE <- BID <- OFR <- BIDSIZ <- OFRSIZ <- DT <- FIRST_DT <- DT_ROUND <-LAST_DT <- SYMBOL <- NULL
  
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
  
  
  if (inputWasXts) {
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
#' @param alignBy character, indicating the time scale in which "alignPeriod" is expressed. Possible values are: "secs", "seconds", "mins", "minutes", "hours".
#' @param alignPeriod positive integer, indicating the number of periods to aggregate over. E.g. to aggregate an
#' object to the 5 minute frequency set alignPeriod = 5 and alignBy = "minutes".
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
#' In case of previous tick aggregation or alignBy = "seconds"/"minutes"/"hours",
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
#' tDataAggregated <- aggregateTrades(sampleTDataMicroseconds, alignBy = "minutes", alignPeriod = 5)
#' tDataAggregated
#' @export
aggregateTrades <- function(tData, alignBy = "minutes", alignPeriod = 5, marketOpen = "09:30:00", marketClose = "16:00:00", tz = "GMT") {
  .I <- .N <- N <- DATE <- SIZE <- DT <- FIRST_DT <- DT_ROUND <- LAST_DT <- SYMBOL <- PRICE <- VWPRICE <- SIZETPRICE <- SIZESUM <- NULL
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
  
  timeZone <- attr(data$DT, "tzone")
  if(timeZone == ""){
    if(is.null(tz)){
      tz <- "UTC"
    }
    if(!("POSIXct" %in% class(data$DT))) data[, DT := as.POSIXct(format(DT, digits = 20, nsmall = 20), tz = tz)]
  } else {
    tz <- timeZone
  }
  setkey(data, "DT") # The below code MAY fail with data with unordered DT column. Also setkey inceases speed of grouping
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
  if (!is.null(symbol)) {
    loc <- grep(symbol, colnames(x))
    if (!identical(loc, integer(0))) {
      x <- x[, loc]
    } else {
      stop(paste("Subscript out of bounds: no column name containing ",symbol,"."))
    }
  }
  if (is.null(prefer)) {
    # default to trying Price, then Trade, then Close
    if(has.Price(x)) prefer = 'price'
    else if(has.Trade(x)) prefer = 'trade'
    else if(has.Cl(x))    prefer = 'close'
    else stop("Subscript out of bounds, no price was discernible from the data.")
  }else {
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
    if (!identical(loc, integer(0))) {
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
#' @param ... used internally
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
#' # multi-day input allowed
#' tqData <- matchTradesQuotes(sampleTDataMicroseconds, sampleQDataMicroseconds)
#' # Show output
#' tqData
#' @importFrom xts tzone<- tzone
#' @export
matchTradesQuotes <- function(tData, qData, lagQuotes = 2, BFM = FALSE, backwardsWindow = 3600, forwardsWindow = 0.5, plot = FALSE, ...) {
  
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
  
  if(tzone(tData$DT) != tzone(qData$DT)){
    stop("timezone of the trade data is not the same as the timezone of the quote data")
  }
  tz <- tzone(tData$DT)
  
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
    qData <- qData[, MAXBID := max(BIDSIZ), by = list(DT, SYMBOL)][, MAXOFR := max(OFRSIZ), by = list(DT, SYMBOL)][
      , BIDSIZ := fifelse(BIDSIZ == MAXBID, 1, 0)][
      , OFRSIZ := fifelse(OFRSIZ == MAXOFR, 1, 0)][
      , BID := BID * BIDSIZ][
      , OFR := OFR * OFRSIZ][
      , BID := max(BID), by = list(DT,SYMBOL)][, OFR := max(OFR), by = list(DT, SYMBOL)][, -c("MAXBID", "MAXOFR", "BIDSIZ", "OFRSIZ")][
      , lapply(.SD, unique), by = list(DT, SYMBOL), .SDcols = c("BID", "OFR")]
  }
  if (selection == "weighted.average") {
    qData[, `:=`(BIDSIZ = as.numeric(BIDSIZ), OFRSIZ = as.numeric(OFRSIZ))]
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
#' }. The default value is \code{"auto"} which automatically selects the exchange for the stocks and days independently using the \code{\link{autoSelectExchangeQuotes}}
#' @param qDataRaw xts or data.table object containing (ONE stock only) raw quote data. This argument is NULL by default. Enabling it means the arguments
#' from, to, dataSource and dataDestination will be ignored. (only advisable for small chunks of data)
#' @param report boolean and TRUE by default. In case it is true the function returns (also) a vector indicating how many quotes remained after each cleaning step.
#' @param selection argument to be passed on to the cleaning routine \code{\link{mergeQuotesSameTimestamp}}. The default is "median".
#' @param maxi spreads which are greater than median(spreads of day) times maxi are excluded.
#' @param window argument to be passed on to the cleaning routine \code{\link{rmOutliersQuotes}}. 
#' @param type argument to be passed on to the cleaning routine \code{\link{rmOutliersQuotes}}.
#' @param rmoutliersmaxi argument to be passed on to the cleaning routine \code{\link{rmOutliersQuotes}}.
#' @param marketOpen passed to \code{\link{exchangeHoursOnly}}. A character in the format of \"HH:MM:SS\",
#' specifying the starting hour, minute and second of an exchange
#' trading day.
#' @param marketClose passed to \code{\link{exchangeHoursOnly}}. A character in the format of \"HH:MM:SS\",
#' specifying the closing hour, minute and second of an exchange
#' trading day.
#' @param printExchange Argument passed to \code{\link{autoSelectExchangeQuotes}} indicates whether the chosen exchange is printed on the console, 
#' default is TRUE. This is only used when \code{exchanges} is \code{"auto"}
#' @param saveAsXTS indicates whether data should be saved in xts format instead of data.table when using on-disk functionality. FALSE by default.
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
#' @details 
#' Using the on-disk functionality with .csv.zip files which is the standard from the WRDS database
#' will write temporary files on your machine - we try to clean up after it, but cannot guarantee that 
#' there won't be files that slip through the crack if the permission settings on your machine does not match 
#' ours
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
#' @importFrom utils unzip
#' @keywords cleaning
#' @export
quotesCleanup <- function(dataSource = NULL, dataDestination = NULL, exchanges = "auto", qDataRaw = NULL, report = TRUE, 
                          selection = "median", maxi = 50, window = 50, type = "advanced", marketOpen = "09:30:00", 
                          marketClose = "16:00:00", rmoutliersmaxi = 10, printExchange = TRUE, saveAsXTS = FALSE, tz = "EST") {
  
  .SD <- BID <- OFR <- DT <- SPREAD <- SPREAD_MEDIAN <- EX <- DATE <- BIDSIZ <- OFRSIZ <- TIME_M <- SYMBOL <- NULL

  
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
      } else if(extension %in% c("csv", "gz", "bz2")){
        readdata <- try(fread(ii), silent = TRUE)
      } else if(extension %in% c("rds")){
        readdata <- try(readRDS(ii))
      } else {
        readdata <- try(fread(ii), silent = TRUE)
      }
      
      if(inherits(readdata, "try-error")){
        stop(paste("Error encountered while opening data, error message is:",readdata))
      }
      if(colnames(readdata)[1] == "index"){ # The data was saved from an xts object
        readdata <- try(readdata[, DT := as.POSIXct(index, tz = tz, format = "%Y-%m-%dT%H:%M:%OS")])
      } else if ("DT" %in% colnames(readdata)){
        readdata <- try(readdata[, DT := as.POSIXct(DT, tz = tz, format = "%Y-%m-%dT%H:%M:%OS")])
      } else {
        readdata <- try(copy(readdata)[,`:=`(DT = as.POSIXct(paste(DATE, TIME_M), tz = "UTC", format = "%Y%m%d %H:%M:%OS"),
                                      DATE = NULL, TIME_M = NULL, SYM_SUFFIX = NULL)], silent = TRUE)
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
    checkqData(qDataRaw)
    
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
    
    timeZone <- attr(qDataRaw$DT, "tzone")
    if(timeZone == ""){
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
    
    nm <- colnames(qDataRaw)
    
    REPORT[1] <- dim(qDataRaw)[1] 
    qDataRaw <- qDataRaw[BID != 0 & OFR != 0]
    REPORT[2] <- dim(qDataRaw)[1] 
    qDataRaw <- exchangeHoursOnly(qDataRaw, marketOpen = marketOpen, marketClose = marketClose, tz = tz)
    REPORT[3] <- dim(qDataRaw)[1] 
    if("EX" %in% nm){
      if(exchanges != "auto"){
        qDataRaw <- qDataRaw[EX %in% exchanges]
      } else if (exchanges == "auto"){
        qDataRaw <- qDataRaw[, autoSelectExchangeQuotes(.SD, printExchange = printExchange), .SDcols = nm,by = list(SYMBOL, DATE = as.Date(DT, tz = tz))][, nm, with = FALSE]
      }
    }
    REPORT[4] <- dim(qDataRaw)[1]
    qDataRaw[OFR > BID, `:=`(SPREAD = OFR - BID, DATE = as.Date(DT, tz = tz))][, SPREAD_MEDIAN := median(SPREAD), by = "DATE"]
    REPORT[5] <- dim(qDataRaw)[1] 
    qDataRaw <- qDataRaw[SPREAD < (SPREAD_MEDIAN * maxi)][, -c("SPREAD","SPREAD_MEDIAN")]
    REPORT[6] <- dim(qDataRaw)[1]
    qDataRaw <- mergeQuotesSameTimestamp(qData = qDataRaw, selection = selection)
    REPORT[7] <- dim(qDataRaw)[1]
    
    qDataRaw <- rmOutliersQuotes(qDataRaw, window = window, type = type, maxi = rmoutliersmaxi)
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
#' @param tData a data.table or xts object containing the time series data, with at least the column "PRICE", containing the transaction price.
#' @param qData a data.table or xts object containing the time series data with at least the columns "BID" and "OFR", containing the bid and ask prices.
#' @param lagQuotes a numeric of length 1 that denotes how many seconds to lag the quotes. Default is 2 seconds. See Details.
#' @param BFM a logical determining whether to conduct 'Backwards - Forwards matching' of trades and quotes.
#' The algorithm tries to match trades that fall outside the bid - ask and first tries to match a small window forwards and if this fails, it tries to match backwards in a bigger window.
#' The small window is a tolerance for inaccuracies in the timestamps of bids and asks. The backwards window allow for matching of late reported trades. I.e. block trades.
#' @param backwardsWindow a numeric denoting the length of the backwards window. Default is 3600, corresponding to one hour.
#' @param forwardsWindow a numeric denoting the length of the forwards window. Default is 0.5, dorresponding to one half second.
#' @param plot a logical denoting whether to visualize the forwards, backwards, and unmatched trades in a plot.
#' @param ... used internally
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
rmTradeOutliersUsingQuotes <- function(tData, qData, lagQuotes = 2, BFM = FALSE, backwardsWindow = 3600, forwardsWindow = 0.5, plot = FALSE, ...) {
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
#' @param tz timezone to use
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
rmOutliersQuotes <- function (qData, maxi = 10, window = 50, type = "advanced", tz = "EST") {
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
#' @description Function deletes entries with abnormal trades condition
#' 
#' @param tData an xts or data.table object containing the time series data, with 
#' one column named "COND" indicating the Sale Condition.
#' @param validConds a character vector containing valid sales conditions defaults to \cr
#' \code{c('', '@', 'E', '@E', 'F', 'FI', '@F', '@FI', 'I', '@I')}. See \link{tradesCondition}.
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
#' @param validConds a character vector containing valid sales conditions defaults to \cr
#' \code{c('', '@', 'E', '@E', 'F', 'FI', '@F', '@FI', 'I', '@I')}. See details.
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
#' \code{\link{noZeroPrices}}, \code{\link{selectExchange}}, \code{\link{tradesCondition}},
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
#' } The default value is \code{"auto"} which automatically selects the exchange for the stocks and days independently using the \code{\link{autoSelectExchangeTrades}}
#' 
#' @param tDataRaw xts object containing (for ONE stock only) raw trade data. This argument is NULL by default. Enabling it means the arguments
#' from, to, dataSource and dataDestination will be ignored. (only advisable for small chunks of data)
#' @param report boolean and TRUE by default. In case it is true the function returns (also) a vector indicating how many trades remained after each cleaning step.
#' @param selection argument to be passed on to the cleaning routine \code{\link{mergeTradesSameTimestamp}}. The default is "median".
#' @param validConds character vector containing valid sales conditions. Passed through to \code{\link{tradesCondition}}.
#' @param marketOpen passed to \code{\link{exchangeHoursOnly}}. A character in the format of \"HH:MM:SS\",
#' specifying the starting hour, minute and second of an exchange
#' trading day.
#' @param marketClose passed to \code{\link{exchangeHoursOnly}}. A character in the format of \"HH:MM:SS\",
#' specifying the closing hour, minute and second of an exchange
#' trading day.
#' @param printExchange Argument passed to \code{\link{autoSelectExchangeTrades}} indicates whether the chosen exchange is printed on the console, 
#' default is TRUE. This is only used when \code{exchanges} is \code{"auto"}
#' @param saveAsXTS indicates whether data should be saved in xts format instead of data.table when using on-disk functionality. FALSE by default.
#' @param tz timezone to use
#' @return For each day an xts or data.table object is saved into the folder of that date, containing the cleaned data.
#' This procedure is performed for each stock in "ticker".
#' The function returns a vector indicating how many trades remained after each cleaning step.
#' 
#' In case you supply the argument "rawtData", the on-disk functionality is ignored
#' and the function returns a list with the cleaned trades as xts object (see examples).
#' 
#' @details 
#' Using the on-disk functionality with .csv.zip files which is the standard from the WRDS database
#' will write temporary files on your machine in order to unzip the files - we try to clean up after it,
#' but cannot guarantee that there won't be files that slip through the crack if the permission settings on your machine does not match 
#' ours
#' 
#' @examples 
#' # Consider you have raw trade data for 1 stock for 2 days 
#' head(sampleTDataRawMicroseconds)
#' dim(sampleTDataRawMicroseconds)
#' tDataAfterFirstCleaning <- tradesCleanup(tDataRaw = sampleTDataRawMicroseconds, 
#'                                          exchanges = list("N"))
#' tDataAfterFirstCleaning$report
#' dim(tDataAfterFirstCleaning$tData)
#' #In case you have more data it is advised to use the on-disk functionality
#' #via "dataSource" and "dataDestination" arguments
#' 
#' @references Barndorff-Nielsen, O. E., P. R. Hansen, A. Lunde, and N. Shephard (2009). Realized kernels in practice: Trades and quotes. Econometrics Journal 12, C1-C32.
#' 
#' Brownlees, C.T. and Gallo, G.M. (2006). Financial econometric analysis at ultra-high frequency: Data handling concerns. Computational Statistics & Data Analysis, 51, pp. 2232-2245.
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' @importFrom data.table fread
#' @importFrom utils unzip
#' @keywords cleaning
#' @export
tradesCleanup <- function(dataSource = NULL, dataDestination = NULL, exchanges = "auto", tDataRaw = NULL, report = TRUE, selection = "median",
                          validConds = c('', '@', 'E', '@E', 'F', 'FI', '@F', '@FI', 'I', '@I'), marketOpen = "09:30:00", 
                          marketClose = "16:00:00", printExchange = TRUE, saveAsXTS = FALSE, tz = "EST") {
  .SD <- CORR <- SIZE <- SYMBOL <- PRICE <- EX <- COND <- DT <- DATE <- TIME_M <- NULL
  
  if (is.null(tDataRaw)) {

    tradesfiles <- list.files(dataSource, recursive = TRUE, full.names = TRUE)[grepl("trades", list.files(dataSource, recursive = TRUE))]
    for (ii in tradesfiles) {
      extension <- strsplit(ii, "\\.")[[1]]
      extension <- extension[length(extension)]
      if(extension == "zip") {
        tmp <- tempdir()
        unzip(ii, exdir = tmp, )
        files <- list.files(tmp, full.names = TRUE, pattern = "csv")
        if(length(files) >= 1){
          readdata <- try(rbindlist(lapply(files, fread, tz = tz)), silent = TRUE)
        }
        # Try to Cleanup - we don't force it though!
        unlink(tmp, recursive = TRUE)
      } else if(extension %in% c("csv", "gz", "bz2")){
        readdata <- try(fread(ii, tz = tz), silent = TRUE)
      } else if(extension %in% c("rds")){
        readdata <- try(readRDS(ii))
      } else {
        readdata <- try(fread(ii, tz = tz), silent = TRUE)
      }
      if(inherits(readdata, "try-error")){
        stop(paste("Error encountered while opening data, error message is:",readdata))
      }
      if(colnames(readdata)[1] == "index"){ # The data was saved from an xts object
        readdata <- try(readdata[, DT := as.POSIXct(index, tz = tz, format = "%Y-%m-%dT%H:%M:%OS")])
      } else if ("DT" %in% colnames(readdata)){
        readdata <- try(readdata[, DT := as.POSIXct(DT, tz = tz, format = "%Y-%m-%dT%H:%M:%OS")])
      } else {
        readdata <- try(readdata[, `:=`(DT = as.POSIXct(paste(DATE, TIME_M), tz = "UTC", format = "%Y%m%d %H:%M:%OS"),
                                      DATE = NULL, TIME_M = NULL, SYM_SUFFIX = NULL)], silent = TRUE)
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
    checktData(tDataRaw)
    
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
    
    
    timeZone <- attr(tDataRaw$DT, "tzone")
    if(timeZone == ""){
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
    
    nm <- colnames(tDataRaw)
    REPORT[1] <- dim(tDataRaw)[1]
    tDataRaw <- tDataRaw[PRICE != 0]
    REPORT[2] <- dim(tDataRaw)[1] 
    tDataRaw <- exchangeHoursOnly(tDataRaw, marketOpen = marketOpen, marketClose = marketClose, tz = tz)
    REPORT[3] <- dim(tDataRaw)[1]
    if("EX" %in% nm){
      if(exchanges != "auto"){
        tDataRaw <- tDataRaw[EX %in% exchanges]
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
#' @param dataDestination character indicating the folder in which the cleaned data is stored, folder of dataSource by default.
#' @param tData data.table or xts object containing (ONE day and for ONE stock only) trade data cleaned by \code{\link{tradesCleanup}}. This argument is NULL by default. Enabling it, means the arguments
#' from, to, dataSource and dataDestination will be ignored. (only advisable for small chunks of data)
#' @param qData data.table or xts object containing (ONE day and for ONE stock only) cleaned quote data. This argument is NULL by default. Enabling it means the arguments
#' from, to, dataSource, dataDestination will be ignored. (only advisable for small chunks of data)
#' @param lagQuotes passed through to \code{\link{rmTradeOutliersUsingQuotes}}. A numeric of length 1 that denotes how many seconds to lag the quotes. Default is 2 seconds. See Details.
#' @param BFM passed through to \code{\link{rmTradeOutliersUsingQuotes}}. A logical determining whether to conduct 'Backwards - Forwards matching' of trades and quotes.
#' The algorithm tries to match trades that fall outside the bid - ask and first tries to match a small window forwards and if this fails, it tries to match backwards in a bigger window.
#' The small window is a tolerance for inaccuracies in the timestamps of bids and asks. The backwards window allow for matching of late reported trades. I.e. block trades.
#' @param backwardsWindow passed through to \code{\link{rmTradeOutliersUsingQuotes}}. A numeric denoting the length of the backwards window. Default is 3600, corresponding to one hour.
#' @param forwardsWindow passed through to \code{\link{rmTradeOutliersUsingQuotes}}. A numeric denoting the length of the forwards window. Default is 0.5, dorresponding to one half second.
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
tradesCleanupUsingQuotes <- function(tradeDataSource = NULL, quoteDataSource = NULL, dataDestination = NULL, tData = NULL, qData = NULL, lagQuotes = 2,
                                     BFM = FALSE, backwardsWindow = 3600, forwardsWindow = 0.5) {
  
  if (is.null(dataDestination) && !is.null(tradeDataSource)) {
    dataDestination <- tradeDataSource
    dataDestination <- path.expand(dataDestination)
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
#' pData <- sampleTDataMicroseconds[,list(DT, PRICE, SIZE)]
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



#' Make Open-High-Low-Close-Volume bars
#' 
#' This function makes OHLC-V bars at arbitrary intevals. If the SIZE column is not present in the input, no volume column is created.
#' @param pData data.table or xts object to make the bars out of, containing the intraday price series of a stock for possibly multiple days.
#' @param alignBy character, indicating the time scale in which "alignPeriod" is expressed. Possible values are: "secs", "seconds", "mins", "minutes", "hours".
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. E.g. to aggregate an
#' object to the 5 minute frequency set alignPeriod = 5 and alignBy = "minutes".
#' @param tz time zone used, by default: tz = "GMT".
#' @examples 
#' \dontrun{
#' minuteBars <- makeOHLCV(sampleTDataEurope, alignBy = "minutes", alignPeriod = 1)
#' ## We can use the quantmod package's chartSeries function to plot the ohlcv data
#' quantmod::chartSeries(minuteBars)
#' 
#' minuteBars <- makeOHLCV(sampleTDataEurope[,], alignBy = "minutes", alignPeriod = 1)
#' ## Again we plot the series with chartSeries
#' quantmod::chartSeries(minuteBars)
#' 
#' ## We can also handle data across multiple days.
#' fiveMinuteBars <- makeOHLCV(sampleTDataMicroseconds)
#' ## Again we plot the series with chartSeries
#' quantmod::chartSeries(fiveMinuteBars)
#' 
#' ## We can use arbitrary alignPeriod, here we choose pi
#' bars <- makeOHLCV(sampleTDataEurope, alignBy = "seconds", alignPeriod = pi)
#' ## Again we plot the series with chartSeries
#' quantmod::chartSeries(bars)
#' }
#' 
#' @export
makeOHLCV <- function(pData, alignBy = "minutes", alignPeriod = 5, tz = NULL){
  .SD <-  DATE <- SIZE <- DT <-  PRICE <- NULL
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
    if (!("DT" %in% colnames(pData))) {
      stop("Data.table neeeds DT column (date-time).")
    }
    pData <- pData[, list(DT, PRICE, SIZE)]
  }
  
  
  timeZone <- attr(pData$DT, "tzone")
  if(timeZone == ""){
    if(is.null(tz)){
      tz <- "UTC"
    }
    if(!("POSIXct" %in% class(pData$DT))) pData[, DT := as.POSIXct(format(DT, digits = 20, nsmall = 20), tz = tz)]
  } else {
    tz <- timeZone
  }
  
  setkey(pData, DT)
  pData <- pData[, lapply(.SD, nafill, type = "locf"), .SDcols = colnames(pData), by = list(DATE = as.Date(DT, tz = tz))]
  pData <- pData[, lapply(.SD, nafill, type = "nocb"), by = DATE]
  pData[, DT := DT + (scaleFactor - as.numeric(DT, tz = tz) %% scaleFactor)]
  if(!("SIZE" %in% colnames(pData))){
    pData <- pData[, list(OPEN = first(PRICE), HIGH = max(PRICE), LOW = min(PRICE), CLOSE = last(PRICE)), by = DT]
  } else{
    pData <- pData[, list(OPEN = first(PRICE), HIGH = max(PRICE), LOW = min(PRICE), CLOSE = last(PRICE), VOLUME = sum(SIZE)), by = DT]
  }
 
  if (inputWasXts) {
    return(xts(as.matrix(pData[, -c("DT")]), order.by = pData$DT, tzone = tz))
  } else {
    return(pData[])
  }
}





#' Convert to format for realized measures
#' 
#' Convenience function to split data from one xts or data.table with atleast DT, SYMBOL, and PRICE columns to a format that can be used in the 
#' r* functions for calculation of realized measures.
#' 
#' @param data An xts or a data.table object with atleast DT, SYMBOL, and PRICE columns. This data should already be cleaned.
#' 
#' @return An xts or a data.table object with columns DT and a column named after each unique entrance in the SYMBOL column of the input. 
#' These columns contain the price of the associated symbol. 
#' 
#' @examples
#' \dontrun{
#' library(data.table)
#' data1 <- copy(sampleTDataMicroseconds)[,  `:=`(PRICE = PRICE * runif(.N, min = 0.99, max = 1.01),
#'                                                DT = DT + runif(.N, 0.01, 0.02))]
#' data2 <- copy(sampleTDataMicroseconds)[, SYMBOL := 'XYZ']
#' 
#' dat <- rbind(data1, data2)
#' setkey(dat, "DT")
#' dat <- makeRMFormat(dat)
#' 
#' rCov(dat, alignBy = 'minutes', alignPeriod = 5, makeReturns = TRUE, cor = TRUE) 
#' }
#' 
#' @importFrom data.table merge.data.table setkey
#' @importFrom xts is.xts
#' @export
makeRMFormat <- function(data){
  SYMBOL <- PRICE <- DT <- NULL
  if(any(!(c("SYMBOL", "PRICE") %in% colnames(data)))){
    stop(paste("Could not find column(s)", c("SYMBOL", "PRICE")[!(c("SYMBOL", "PRICE") %in% colnames(data))]), "these columns must be present")
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
    name <- x[1, SYMBOL]
    x <- x[, list(DT,PRICE)]
    setnames(x, old = "PRICE", new = name)
    return(x)
  }))
  
  
  if(inputWasXts){
    collected <- as.xts(collected)
    storage.mode(collected) <- 'numeric'
    return(collected)
  } else {
    setkey(collected, "DT")
    return(collected[])
  }
  
}


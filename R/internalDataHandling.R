#' @importFrom zoo zoo na.locf `index<-`
#' @importFrom stats start end
#' @keywords internal
fastTickAgregation <- function (ts, alignBy = "minutes", alignPeriod = 1, tz = "GMT") {
  
  if (alignBy == "secs" | alignBy == "seconds") {
    secs <- alignPeriod
    tby <- paste(alignPeriod, "sec", sep = " ")
  } 
  if (alignBy == "mins" | alignBy == "minutes") {
    secs <- 60 * alignPeriod
    tby <- paste(60 * alignPeriod, "sec", sep = " ")
  } 
  if (alignBy == "hours"){
    secs <- 3600 * alignPeriod
    tby <- paste(3600 * alignPeriod, "sec", sep = " ")
  }
  g <- base::seq(start(ts), end(ts), by = tby)
  rawg <- as.numeric(as.POSIXct(g, tz = tz))
  newg <- rawg + (secs - rawg %% secs)
  if(as.numeric(end(ts)) == newg[length(newg)-1]){
    newg  <- newg[-length(newg)]
  }
  g    <- as.POSIXct(newg, origin = "1970-01-01", tz = tz)
  
  firstObs <- ts[1,]
  ts <- na.locf(merge(ts, zoo(NULL, g)))[as.POSIXct(g, tz = tz)]
  if(index(ts[1]) > index(firstObs)){
    index(firstObs) <- index(ts[1]) - secs
    ts <- c(firstObs, ts)
  }
  return(ts)
}


#' @importFrom data.table rbindlist setkey nafill set
#' @keywords internal
fastTickAgregation_DATA.TABLE <- function(dat, alignBy = "minutes", alignPeriod = 1, tz = "GMT"){
  MAXDT <- .SD <- firstDT <- DT <- PRICE <- DATE <- NULL
  if (alignBy == "secs" | alignBy == "seconds") {
    secs <- alignPeriod
  } 
  if (alignBy == "mins" | alignBy == "minutes") {
    secs <- 60 * alignPeriod
  } 
  if (alignBy == "hours"){
    secs <- 3600 * alignPeriod
  }
  #n
  timeZone <- format(dat$DT[1], format = "%Z")
  if(is.null(timeZone) || timeZone == ""){
    if(is.null(tz)){
      tz <- "UTC"
    }
    if(!("POSIXct" %in% class(dat$DT))) dat[, DT := as.POSIXct(format(DT, digits = 20, nsmall = 20), tz = tz)]
  } else {
    tz <- timeZone
  }
  ## These are used to make sure we dont have na's back and forth and to recude the chance we erroneously produce NA's
  dat <- dat[, lapply(.SD, nafill, type = "locf"), .SDcols = colnames(dat), by = list(DATE = as.Date(DT, tz = tz))]
  dat <- dat[, lapply(.SD, nafill, type = "nocb"), by = DATE]
  g <- dat[, list(DT = seq(first(DT), last(DT), by = secs, tz = tz), MAXDT = max(DT)), by = DATE]
  g$DT <- g$DT + (secs - as.numeric(g$DT, tz = tz) %% secs)
  out <- dat[g, roll = TRUE, on = "DT"][DT < MAXDT + secs]
  
  prependingCheck <- cbind(out[, list(firstDT = first(DT)), by = DATE],
        dat[, lapply(.SD, first), by = list(DATE = as.Date(DT, tz = tz)), .SDcols = names(dat)][, "DATE" := NULL]
        )[, "DATE" := NULL]
  
  ## Hacky fix - need further investigation later.
  if("DATE" %in% colnames(prependingCheck)){
    set(prependingCheck, j = "DATE", value = NULL)
  }
  if("i.DATE" %in% colnames(out)){ 
    set(out, j = "i.DATE", value = NULL)
  }
  out <- rbindlist(list(out[, c("DATE", "MAXDT") := NULL], prependingCheck[DT < firstDT, DT := firstDT - secs][, c("firstDT") := NULL]),
                   use.names = TRUE)

  setkey(out, "DT")
  
  numericColumns <- colnames(out)[colnames(out) != "DT"]
  
  for(col in numericColumns){
    set(out, j = col, value = as.numeric(out[[col]]))
  }
  #setnafill(out, type = "locf", cols = colnames(out)[colnames(out) != "DT"])
  return(out)
  
}


#' @importFrom zoo zoo na.locf `index<-`
#' @importFrom stats start end
#' @keywords internal
fastTickAgregation_RETURNS <- function (ts, alignBy = "minutes", alignPeriod = 1, tz = "GMT") {
  
  if (alignBy == "secs" | alignBy == "seconds") {
    secs <- alignPeriod
    tby <- paste(alignPeriod, "sec", sep = " ")
  } 
  if (alignBy == "mins" | alignBy == "minutes") {
    secs <- 60 * alignPeriod
    tby <- paste(60 * alignPeriod, "sec", sep = " ")
  } 
  if (alignBy == "hours"){
    secs <- 3600 * alignPeriod
    tby <- paste(3600 * alignPeriod, "sec", sep = " ")
  }
  g <- base::seq(start(ts), end(ts), by = tby)
  rawg <- as.numeric(as.POSIXct(g, tz = tz))
  newg <- rawg + (secs - rawg %% secs)
  if(as.numeric(end(ts)) == newg[length(newg)-1]){
    newg  <- newg[-length(newg)]
  }
  
  firstObs <- ts[1,]
  firstObs[] <- 0 ## We should have 0 return! We use [] to not just overwrite it as a numeric with 0
  ts <- period.apply(ts, INDEX = merge.data.table(data.table(DT = index(ts), 1:nrow(ts)), data.table(DT = newg), by = "DT")$V2, 
                     FUN = colSums, na.rm = TRUE)
  ts <- na.locf(ts)
  if(index(ts[1]) > index(firstObs)){
    index(firstObs) <- index(ts[1]) - secs
    ts <- c(firstObs, ts)
  }
  
  return(ts)
}


#' @importFrom data.table rbindlist setkey nafill set
#' @keywords internal
fastTickAgregation_DATA.TABLE_RETURNS <- function(dat, alignBy = "minutes", alignPeriod = 1, tz = "GMT"){
  DT_ROUND <- FIRST_DT <- .SD <- FIRST_DT <- DT <- DATE <- NULL
  if (alignBy == "secs" | alignBy == "seconds") {
    secs <- alignPeriod
  } 
  if (alignBy == "mins" | alignBy == "minutes") {
    secs <- 60 * alignPeriod
  } 
  if (alignBy == "hours"){
    secs <- 3600 * alignPeriod
  }
  #n
  timeZone <- format(dat$DT[1], format = "%Z")
  if(is.null(timeZone) || timeZone == ""){
    if(is.null(tz)){
      tz <- "UTC"
    }
    if(!("POSIXct" %in% class(dat$DT))) dat[, DT := as.POSIXct(format(DT, digits = 20, nsmall = 20), tz = tz)]
  } else {
    tz <- timeZone
  }
  ## These are used to make sure we dont have na's back and forth and to recude the chance we erroneously produce NA's
  dat <- dat[, lapply(.SD, nafill, type = "locf"), .SDcols = colnames(dat), by = list(DATE = as.Date(DT, tz = tz))]
  dat <- dat[, lapply(.SD, nafill, type = "nocb"), by = DATE]
  dat[, DT := as.numeric(DT, tz = tz)]
  dat[, FIRST_DT := min(DT), by = "DATE"]
  dat[, DT_ROUND := fifelse(DT == FIRST_DT,
                              floor(DT/secs) * secs,
                              ceiling(DT/secs) * secs)]
  cols = colnames(dat)
  cols <- cols[!(cols %in% c("DATE", "DT", "FIRST_DT", "DT_ROUND"))]
  dat <- dat[, lapply(.SD, sum), by = "DT_ROUND", .SDcols = cols]
  setnames(dat, old = "DT_ROUND", new = "DT")
  dat[, DT := as.POSIXct(DT, origin = as.POSIXct("1970-01-01", tz = "UTC"), tz = tz)]
  return(dat[])
  
}



# Necessary for check-package not throwing errors
#' @keywords internal
`:=` <- function(...) {
  NULL
}

#' @importFrom xts xtsAttributes
#' @importFrom data.table copy
#' @keywords internal
checkColumnNames <- function(data) { 
  # FUNCTION sets column names according to RTAQ format using quantmod conventions, 
  # such that all the other functions find the correct information.
  # First step: assign the xts attributes:
  
  
  if(is.xts(data)){
    colnames(data) <-  toupper(colnames(data)) # Make sure we have upper case column names
    # Change column names to previous RTAQ format! 
    # Adjust Ask col naming:    
    try((colnames(data)[xtsAttributes(data)[['Ask']]] = 'OFR'))
    # Adjust SYMBOL col naming:    
    try((colnames(data)[xtsAttributes(data)[['SYM_ROOT']]] = 'SYMBOL'))
    # Adjust Ask size col naming:
    try((colnames(data)[xtsAttributes(data)[['BIDSIZE']]] = 'BIDSIZ'))
    # Adjust Bid size col naming:    
    try((colnames(data)[xtsAttributes(data)[['ASKSIZE']]] = 'OFRSIZ'))
    try((colnames(data)[xtsAttributes(data)[['ASKSIZ']]] = 'OFRSIZ'))
    # Adjust correction column, if necessary:
    if (any(colnames(data) == "CR")) {
      colnames(data)[colnames(data) == "CR"] <- "CORR"
    }
    
  }
  
  if(is.data.table(data)){
    data <- copy(data)
    
    setnames(data, colnames(data), toupper(colnames(data))) # Make sure we have upper case column names
    # Change column names to previous RTAQ format! 

    # Adjust Ask col naming:    
    setnames(data, "ASK", "OFR", skip_absent = TRUE)
    # Adjust SYMBOL col naming:    
    setnames(data, "SYM_ROOT", "SYMBOL", skip_absent = TRUE)
    
    # Adjust Ask size col naming:
    setnames(data, "BIDSIZE", "BIDSIZ", skip_absent = TRUE)
    
    # Adjust Bid size col naming:    
    setnames(data, "ASKSIZE", "OFRSIZ", skip_absent = TRUE)
    # Adjust Bid size col naming:    
    setnames(data, "ASKSIZ", "OFRSIZ", skip_absent = TRUE)
    
    setnames(data, "TR_SCOND", "COND", skip_absent = TRUE)
    setnames(data, "CR", "CORR", skip_absent = TRUE)
    setnames(data, "TR_CORR", "CORR", skip_absent = TRUE)
    
  }
  
  
  return(data)
} 

#' @importFrom data.table is.data.table
#' @importFrom xts is.xts
#' @keywords internal
checktData <- function(tData) {
  if (!is.xts(tData) & !is.data.table(tData)) {
    stop("The argument tData should be a data.table or xts object.")
  }
  if (!any(colnames(tData) == "PRICE")) {
    stop("The argument tData should have a column containing the PRICE. Could not find that column.")
  }
  if (!any(c("SYMBOL", "SYM_ROOT") %chin% colnames(tData))) {
    warning("The argument tData should have a column containing SYMBOL (or SYM_ROOT). Could not find that column.")
  }
  
  if (is.data.table(tData)) {
    if (typeof(tData$PRICE) != "double") {
      stop("Column PRICE should be of type double.")
    }
    if("SIZE" %chin% colnames(tData)){
      if (typeof(tData$SIZE) != "integer" & typeof(tData$SIZE) != "double") {
        stop("Column SIZE should be of type integer or double (after cleaning).")
      }
    }
  }
}

#' @importFrom data.table is.data.table
#' @importFrom xts is.xts
#' @keywords internal
checkqData <- function(qData) {
  if (!is.xts(qData) & !is.data.table(qData)) {
    stop("The argument qData should be a data.table or xts object.")
  }
  if (!any(colnames(qData) == "BID")) {
    stop("The argument qData should have a column containing the BID. Could not find that column.")
  }
  if (!any(colnames(qData) == "OFR")) {
    stop("The argument qData should have a column containing the ASK / OFR. Could not find that column.")
  }
  if (!any(c("SYMBOL", "SYM_ROOT") %chin% colnames(qData))) {
    warning("The argument tData should have a column containing SYMBOL (or SYM_ROOT). Could not find that column.")
  }
  if (is.data.table(qData)) {
    if (typeof(qData$BID) != "double") {
      stop("Column BID should be of type double.")
    }
    if (typeof(qData$OFR) != "double") {
      stop("Column OFR should be of type double.")
    }
  }
}

# Function for calculating three different measures:
# 1. Rolling centered median (excluding the observation under consideration)
# 2. Rolling median of the following "window" observations
# 3. Rolling median of the previous "window" observations
#' @importFrom RcppRoll roll_median
#' @keywords internal
rollingMedianInclEnds <- function(x, weights, window, direction = "center") {
  length_median_vec <- length(x)
  median_vec <- rep(NA, times = length_median_vec)
  halfwindow <- window / 2
  
  if (direction == "center") {
    median_vec[(halfwindow + 1):(length(x) - halfwindow)] <- roll_median(x, weights = weights, fill = numeric(0))
    
    # We have to add the "end"-values manually as currently roll_median does not support the increasing windows specified 
    # at the ends of the time series
    for (ii in 1:halfwindow) {
      median_vec[ii] <- median(c(x[0:(ii - 1)], x[(ii + 1):(ii + halfwindow)]))
      median_vec[length_median_vec - ii + 1] <- 
        median(c(x[(length_median_vec - ii + 1 - halfwindow):(length_median_vec - ii)], 
                 x[(length_median_vec - ii + 2):(length_median_vec + 1)]), na.rm = TRUE)
    }
  }
  if (direction == "left") {
    median_vec[(window + 1):length(x)] <- roll_median(x, weights = weights, fill = numeric(0))
    for (ii in 2:window) {
      median_vec[ii] <- median(x[0:(ii - 1)])
    }
  }
  if (direction == "right") {
    median_vec[1:(length(x) - window)] <- roll_median(x, weights = weights, fill = numeric(0))
    for (ii in 2:window) {
      median_vec[length_median_vec - ii + 1] <-
        median(x[(length_median_vec - ii + 1 - window):length_median_vec])
    }
  }
  
  median_vec
}


#' @keywords internal
#' @importFrom data.table copy data.table setnafill fifelse
BFMalgorithm <- function(tData, qData, backwardsWindow, forwardsWindow, plot, tz){
  N <- .N <- MIDQUOTE <- SPREAD <- DT <- BID <- OFR <- PRICE <- NULL
  qData <- qData[, list(DT, BID, OFR)]
  # Filter quotes to only places where they actually change.
  qData <- copy(qData[c(TRUE, diff(BID)|diff(OFR))])[, `:=`(BID = pmin(BID,OFR), OFR = pmax(BID, OFR), N = 1:.N)]
  # Locate outliers
  idxOutliers <- qData[, `:=`(SPREAD = pmax(OFR-BID, median(OFR-BID)), MIDQUOTE = (BID + OFR)/2)][
    tData, roll = TRUE, on = "DT"][,N := 1:.N][PRICE > MIDQUOTE + 2.01 * SPREAD | PRICE < MIDQUOTE - 2.01 * SPREAD]$N
  
  # Insert quotes 
  bid <- qData$BID
  ask <- qData$OFR
  qTS <- qData$DT
  idxUp <- which(bid[-1] > ask[-length(ask)]) + 1
  idxDown <- which(ask[-1] < bid[-length(bid)]) + 1
  bid <- c(bid, ask[idxUp - 1], ask[idxDown])
  ask <- c(ask, bid[idxUp], bid[idxDown + 1])
  qTS <- c(qTS, pmax(qTS[idxUp - 1], qTS[idxUp]-1), pmax(qTS[idxDown-1], qTS[idxDown]-1))
  idx <- sort(qTS, index.return = TRUE)$ix
  qTS <- qTS[idx]
  bid <- bid[idx]
  ask <- ask[idx]
  idx <- qData[, N := 1:.N][roll = TRUE, copy(tData), on = "DT"]$N
  idxForward <- qData[, N := 1:.N][roll = TRUE, copy(tData)[, DT := DT + forwardsWindow], on = "DT"]$N
  idxBackward <- copy(qData)[, `:=`(DT = DT + backwardsWindow)][roll = TRUE, tData, on = "DT"]$N + 1# Compensate for the forwards window 
  idxBackward[is.na(idxBackward)] <- 2
  tqData <- qData[roll = TRUE, tData, on = "DT"]
  qData <- data.table(DT = qTS, BID = bid, OFR = ask)
  
  forwardsMatches <- rep(NA, (nrow(tData)))
  for(i in idxOutliers){
    startIdx <- idx[i]
    endIdx <- idxForward[i]
    mask <- tData[i, PRICE] > qData[startIdx:endIdx, BID - 1e-8] & tData[i, PRICE] < qData[startIdx:endIdx, OFR + 1e-8]
    if(sum(mask)){
      forwardsMatches[i] <- qData[startIdx + which(mask)[1] - 1,DT]
    }
  }
  
  forwardsMatches <- as.numeric(forwardsMatches - tData[,DT])
  
  backwardsMatches <- rep(NA, nrow(tData))
  idxOutliers2 <- idxOutliers[!idxOutliers %in% which(!is.na(forwardsMatches))] ## remove already forward matched values.
  for (i in idxOutliers2) {
    
    startIdx <- idxBackward[i]
    endIdx <- idx[i]
    mask <- tData[i, PRICE] > qData[startIdx:endIdx, BID - 1e-8] & tData[i, PRICE] < qData[startIdx:endIdx, OFR + 1e-8]
    if(sum(mask)){
      foo <- which(mask)
      backwardsMatches[i] <- qData[startIdx + foo[length(foo)] - 1,DT]
    }
    
  }
  
  
  
  
  backwardsMatches <- as.numeric(tData[,DT] - backwardsMatches)
  remaining <- idxOutliers2[!idxOutliers2 %in% which(!is.na(backwardsMatches))]
  whichBackwards <- which(!is.na(backwardsMatches))
  backwardsMatches <- backwardsMatches[whichBackwards]
  whichForwards <- which(!is.na(forwardsMatches))
  forwardsMatches <- forwardsMatches[whichForwards]
  
  tqData[, DT := as.POSIXct(DT, tz = tz, origin = "1970-01-01")]
  
  
  if(plot){
    yLim <- range(tqData[, list(PRICE, BID, OFR)], na.rm = TRUE)
    plot(tqData$DT, tqData$PRICE, xaxs = 'i', pch = 20, ylim = yLim, col = fifelse(!(1:nrow(tqData) %in% remaining),
                                                                                   1 + 1:nrow(tqData) %in% idxOutliers + 1:nrow(tqData) %in% idxOutliers2,
                                                                                   0))
    lines(tqData$DT, tqData$OFR, type = "l", col = "blue", ylim = yLim)
    lines(tqData$DT, tqData$BID, type = "l", col = "green", ylim = yLim)
    points(tqData[remaining, DT], tqData[remaining, PRICE], pch = 4, col = "red")
    tzone(tqData) <- tz
    
  }
  
  tqData[c(whichBackwards, whichForwards), `:=`(DT = DT - c(na.omit(backwardsMatches), na.omit(forwardsMatches)))]
  tqData[c(whichBackwards, whichForwards), `:=`(BID = NA, OFR = NA)]
  
  
  
  tqData <- tqData[!remaining]
  setkey(tqData, DT)
  setnafill(tqData, "locf", cols = c("BID", "OFR"))
  
  
  
  return(list("tqData" = tqData, "forwardMatched" = cbind(forwardsMatches, whichForwards), 
              "backwardsMatched" = cbind(backwardsMatches, whichBackwards), "unmatched" = remaining))
}


seqInclEnds <- function(start, end, by){
  val <- seq(1, end, by = by)
  if(val[length(val)] != end){
    val <- c(val, end)
  }
  return(val)
}

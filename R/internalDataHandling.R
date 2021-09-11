#' @importFrom zoo zoo na.locf `index<-`
#' @importFrom stats start end
#' @keywords internal
fastTickAgregation <- function (ts, alignBy = "minutes", alignPeriod = 1, tz = "GMT") {
  
  if(alignBy == "ticks"){
    return(na.locf(ts)[seqInclEnds(1, length(ts), alignPeriod)])
  } else  if (alignBy == "secs" | alignBy == "seconds") {
    secs <- alignPeriod
    tby <- paste(alignPeriod, "sec", sep = " ")
  } else  if (alignBy == "mins" | alignBy == "minutes") {
    secs <- 60 * alignPeriod
    tby <- paste(60 * alignPeriod, "sec", sep = " ")
  } else if (alignBy == "hours"){
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
  .N <- MAXDT <- .SD <- firstDT <- DT <- DATE <- NULL
  dat <- copy(dat)
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
  
  if(alignBy == "ticks"){ ## Special case for alignBy = "ticks"
    if(alignPeriod == 1) return(dat[,!"DATE"])
    if(alignPeriod < 1 | alignPeriod%%1 != 0){
      stop("When alignBy is `ticks`, must be a positive integer valued numeric")
    }
    # if(length(unique(as.Date(pData[,DT]))) > 1){
    #   stop("Multiday support for aggregatePrice with alignBy = \"ticks\" is not implemented yet.")
    # }
    return(dat[seqInclEnds(1, .N, alignPeriod), .SD, by = list(DATE = as.Date(DT)), .SDcols = 1:ncol(dat)][,!"DATE"])
  }
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
  DT <- NULL
  agged <- fastTickAgregation_DATA.TABLE_RETURNS(as.data.table(ts)[, DT := index][, !"index"], alignBy = alignBy, alignPeriod = alignPeriod, tz = tz)
  return(as.xts(agged))
  # 
  # 
  # if (alignBy == "secs" | alignBy == "seconds") {
  #   secs <- alignPeriod
  #   tby <- paste(alignPeriod, "sec", sep = " ")
  # } 
  # if (alignBy == "mins" | alignBy == "minutes") {
  #   secs <- 60 * alignPeriod
  #   tby <- paste(60 * alignPeriod, "sec", sep = " ")
  # } 
  # if (alignBy == "hours"){
  #   secs <- 3600 * alignPeriod
  #   tby <- paste(3600 * alignPeriod, "sec", sep = " ")
  # }
  # if(alignBy != "ticks"){
  #   g <- base::seq(start(ts), end(ts), by = tby)
  #   rawg <- as.numeric(as.POSIXct(g, tz = tz))
  #   newg <- rawg + (secs - rawg %% secs)
  # } else {
  #   newg <- index(ts)[seqInclEnds(1, NROW(ts), alignPeriod)]
  # }
  # if(as.numeric(end(ts)) == newg[length(newg)-1]){
  #   newg  <- newg[-length(newg)]
  # }
  # 
  # 
  # firstObs <- ts[1,]
  # firstObs[] <- 0 ## We should have 0 return! We use [] to not just overwrite it as a numeric with 0
  # ts <- period.apply(ts, INDEX = merge.data.table(data.table(DT = index(ts), 1:nrow(ts)), data.table(DT = newg), by = "DT")$V2, 
  #                    FUN = colSums, na.rm = TRUE)
  # ts <- na.locf(ts)
  # if(index(ts[1]) > index(firstObs)){
  #   index(firstObs) <- index(ts[1]) - secs
  #   ts <- c(firstObs, ts)
  # }
  # 
  # return(ts)
}


#' @importFrom data.table rbindlist setkey nafill set
#' @keywords internal
fastTickAgregation_DATA.TABLE_RETURNS <- function(dat, alignBy = "minutes", alignPeriod = 1, tz = "GMT"){
  ....GRP.... <- .N <- DT_ROUND <- FIRST_DT <- .SD <- FIRST_DT <- DT <- DATE <- NULL
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
  if(alignBy == "ticks"){ ## Special case for alignBy = "ticks"
    if(alignPeriod == 1) return(dat[,!"DATE"])
    if(alignPeriod < 1 | alignPeriod%%1 != 0){
      stop("When alignBy is `ticks`, must be a positive integer valued numeric")
    }
    dat[, ....GRP.... := tickGrouping_RETURNS(.N, alignPeriod), by = list(DATE = as.Date(DT))]
    
    expr <- setdiff(colnames(dat), c("DT","DATE", "....GRP...."))
    expr <- paste0(sprintf("%s = sum(%s)", expr, expr), collapse = ", ")
    expr <- sprintf("dat[, list(DT = DT[.N], %s), by = list(DATE = DATE, ....GRP....)]", expr)
    dat <- eval(parse(text = expr))[, !c("DATE", "....GRP....")]
    
    return(dat[])
  }
  
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
    try((colnames(data)[xtsAttributes(data)[['ASK']]] = 'OFR'))
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
    setnames(data, "OFRSIZE", "OFRSIZ", skip_absent = TRUE)
    
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
  if (!any(colnames(qData) %chin% c("OFR", "ASK"))) {
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



internalAggregateTSXTS <- function(ts, FUN = "previoustick", alignBy = "minutes", alignPeriod = 1, weights = NULL, dropna = FALSE, tz = NULL, ...){
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
    idx <- seqInclEnds(1, NROW(ts), alignPeriod)
    ts <- ts[idx,]
    return(ts)
  }
  
  if (makethispartbetter)  {
    
    if (is.null(weights)) {
      ep <- endpoints(ts, alignBy, alignPeriod)
      if (dim(ts)[2] == 1) { 
        ts2 <- period.apply(ts, ep, FUN, ...) 
      }
      if (dim(ts)[2] > 1) {  
        ts2 <- xts(apply(ts, 2, FUN = periodApply2, FUN2 = FUN, INDEX = ep, ...), order.by = index(ts)[ep],)
      }
    } else {
      tsb <- cbind(ts, weights)
      ep  <- endpoints(tsb, alignBy, alignPeriod)
      ts2 <- period.apply(tsb, ep, FUN = match.fun(weightedaverage), ...)
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


internalAggregateTSDT <- function(ts, FUN = "previoustick", alignBy = "minutes", alignPeriod = 1, weights = NULL, dropna = FALSE, tz = NULL, ...){
  .SD <- .N <- .I <- N <- DATE <- DT <- FIRST_DT <- DT_ROUND <- LAST_DT <- SYMBOL <- PRICE <- NULL
  
  ts <- copy(ts)
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
  
  if (!("DT" %in% colnames(ts))) {
    stop("Data.table neeeds DT column (date-time).")
  }

  if(alignBy == "ticks"){ ## Special case for alignBy = "ticks"
    if(alignPeriod == 1) return(ts[])
    
    if(FUN != "previoustick") stop("Only previoustick function is supported when alignBy is `ticks`")
    
    if(alignPeriod < 1 | alignPeriod%%1 != 0){
      stop("When alignBy is `ticks`, must be a positive integer valued numeric")
    }
    # if(length(unique(as.Date(pData[,DT]))) > 1){
    #   stop("Multiday support for aggregatePrice with alignBy = \"ticks\" is not implemented yet.")
    # }
    return(ts[seqInclEnds(1, .N, alignPeriod), .SD, by = list(DATE = as.Date(DT)), .SDcols = 1:ncol(ts)][])
  }
  
  timeZone <- format(ts$DT[1], format = "%Z")
  if(is.null(timeZone) || timeZone == ""){
    if(is.null(tz)){
      tz <- "UTC"
    }
    if(!("POSIXct" %in% class(ts$DT))){
      ts[, DT := as.POSIXct(format(DT, digits = 20, nsmall = 20), tz = tz)]
    }
  } else {
    tz <- timeZone
  }
  setkeyv(ts, c("DT")) # The below code MAY fail with data with unordered DT column. Also setkey inceases speed of grouping
  ## Checking ends
  # Convert DT to numeric. This is much faster than dealing with the strings (POSIXct)
  ts[, DT := as.numeric(DT, tz = tz)]
  # Calculate the date in the data
  ts[, DATE := as.Date(floor(DT / 86400), origin = "1970-01-01", tz = tz)]
  
  # Find the first observation per day.
  ts[, FIRST_DT := min(DT), by = list(DATE)]
  # Use Dirks answer here: https://stackoverflow.com/a/42498175 to round the timestamps to the latest scaleFactor
  ts[, DT_ROUND := fifelse(DT == FIRST_DT,
                              floor(DT/scaleFactor) * scaleFactor,
                              ceiling(DT/scaleFactor) * scaleFactor)]
  
  ts[, LAST_DT := max(DT), by = list(DT_ROUND)]
  
  if(FUN == "previoustick"){
    startsAndEnds <- ts[, list(START = first(DT_ROUND), END = last(DT_ROUND)), by = DATE]
    dt_full_index <- data.table(DT_ROUND = as.numeric(mSeq(startsAndEnds[["START"]], startsAndEnds[["END"]], as.double(scaleFactor))))
    
    setkey(dt_full_index, "DT_ROUND")
    
    setkey(ts, DT)
    
    ts <- unique(ts[dt_full_index, roll = TRUE, on = "DT_ROUND"])
    
  }
  
  FUN <- match.fun(FUN)
  if(!is.null(weights)){
    if(NROW(weights) != NROW(ts) & NROW(weights) != 1){
      stop("number of observbations in weights must match number of observations in ts")
    }
    for (col in setdiff(colnames(ts), c("DT", "DT_ROUND", "FIRST_DT", "LAST_DT", "DATE"))) {
      set(ts, j = col, value = ts[[col]] * weights)
    }
  }
  ts <- ts[, lapply(.SD, FUN, ...), by = list(DT_ROUND), .SDcols = setdiff(colnames(ts), c("DT", "DT_ROUND", "FIRST_DT", "LAST_DT", "DATE"))]
  
  
  setnames(ts, "DT_ROUND", "DT")
  set(ts, j = "DT", value = as.POSIXct(ts$DT, tz = tz, origin = "1970-01-01"))
  
  
  return(ts[])
  
}
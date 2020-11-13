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
  timeZone <- attr(dat$DT, "tzone")
  if(is.null(timeZone) | timeZone == ""){
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
  # g$DT <- as.POSIXct(as.numeric(g$DT, tz = tz) + (secs - as.numeric(g$DT, tz = tz) %% secs), origin = as.POSIXct("1970-01-01", tz = tz), tz = tz)
  # dropDATE <- ifelse("DATE" %in% colnames(dat), "i.DATE", character(0))
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
  ts <- period.apply(ts, INDEX = merge.data.table(data.table(DT = index(ts), 1:nrow(ts)), data.table(DT = newg), by = "DT")$V2, FUN = colSums, na.rm = TRUE)
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
  timeZone <- attr(dat$DT, "tzone")
  if(is.null(timeZone) | timeZone == ""){
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




# # Necessary for check-package not throwing errors
# #' @keywords internal
# ..keepCols <- NULL
# Necessary for check-package not throwing errors
#' @keywords internal
`:=` <- function(...) {
  NULL
}

#' @importFrom xts xtsAttributes
#' @keywords internal
checkColumnNames <- function(data) { 
  # FUNCTION sets column names according to RTAQ format using quantmod conventions, 
  # such that all the other functions find the correct information.
  # First step: assign the xts attributes:
  
  
  if(is.xts(data)){
    data <- set.AllColumns(data)
    # Change column names to previous RTAQ format! 
    # Adjust price col naming:  
    try((colnames(data)[xtsAttributes(data)[['Price']]] = 'PRICE'), silent = TRUE)
    # Adjust Bid col naming:    
    try((colnames(data)[xtsAttributes(data)[['Bid']]] = 'BID'))  
    # Adjust Ask col naming:    
    try((colnames(data)[xtsAttributes(data)[['Ask']]] = 'OFR'))
    # Adjust SYMBOL col naming:    
    try((colnames(data)[xtsAttributes(data)[['SYM_ROOT']]] = 'SYMBOL'))
    # Adjust Ask size col naming:
    try((colnames(data)[xtsAttributes(data)[['BidSize']]] = 'BIDSIZ'))
    # Adjust Bid size col naming:    
    try((colnames(data)[xtsAttributes(data)[['AskSize']]] = 'OFRSIZ'))
    # Adjust correction column, if necessary:
    if (any(colnames(data) == "CR")) {
      colnames(data)[colnames(data) == "CR"] <- "CORR"
    }
    
  }
  
  if(is.data.table(data)){
    # Change column names to previous RTAQ format! 
    # Adjust price col naming:  
    try(setnames(data, "Price", "PRICE", skip_absent = TRUE), silent = TRUE)
    
    # Adjust Bid col naming:    
    try(setnames(data, "Bid", "BID", skip_absent = TRUE), silent = TRUE)
    # Adjust Ask col naming:    
    try(setnames(data, "Ask", "OFR", skip_absent = TRUE), silent = TRUE)
    try(setnames(data, "ASK", "OFR", skip_absent = TRUE), silent = TRUE)
    # Adjust SYMBOL col naming:    
    try(setnames(data, "SYM_ROOT", "SYMBOL", skip_absent = TRUE), silent = TRUE)
    
    # Adjust Ask size col naming:
    try(setnames(data, "BidSize", "BIDSIZ", skip_absent = TRUE), silent = TRUE)
    
    # Adjust Bid size col naming:    
    try(setnames(data, "AskSize", "OFRSIZ", skip_absent = TRUE), silent = TRUE)
    # Adjust Bid size col naming:    
    try(setnames(data, "ASKSIZ", "OFRSIZ", skip_absent = TRUE), silent = TRUE)
    
    
    try(setnames(data, "TR_SCOND", "COND", skip_absent = TRUE), silent = TRUE)
    try(setnames(data, "CR", "CORR", skip_absent = TRUE), silent = TRUE)  
  }
  
  
  return(data)
} 

#' @keywords internal
is.BBO <- function (x) {
  if (all(has.Bid(x), has.Ask(x))) {
    TRUE
  } else {
    FALSE
  }
}

#' @keywords internal
is.TBBO <- function (x) {
  if (all(has.Trade(x), hasQty(x), has.Bid(x), has.Ask(x))) {
    TRUE
  }
  else FALSE
}

#' @keywords internal
is.BAM <- function(x) {
  if (all(has.Bid(x), has.Ask(x), has.Mid(x))) {
    TRUE
  }
  else FALSE
}

#' @keywords internal
is.BATM <- function(x) {
  if (all(has.Bid(x), has.Ask(x), has.Trade(x), has.Mid(x))) {
    TRUE
  }
  else FALSE
}

#' @keywords internal
has.Bid <- function(x, which = FALSE) {
  colAttr <- attr(x, "Bid")
  if(!is.null(colAttr))
    return(if(which) colAttr else TRUE)
  #first try with "price" for data that has both bid.size and bid.price
  loc <- grep("bid.*price", colnames(x), ignore.case=TRUE)
  if (identical(loc, integer(0))) #If no column named bid.price
    loc <- grep("bid", colnames(x), ignore.case=TRUE) #look for bid
  if (!identical(loc, integer(0))) {
    return(if(which) loc else TRUE)
  } else FALSE
}

#' @keywords internal
has.BidSize <- function(x, which = FALSE) {
  colAttr <- attr(x, "BidSize")
  if(!is.null(colAttr))
    return(if(which) colAttr else TRUE)
  
  loc <- grep("bid.*(size|qty|quantity)", colnames(x), ignore.case=TRUE)
  if (!identical(loc, integer(0))) {
    return(if(which) loc else TRUE)
  }
  loc <- grep("(bidsize|bidsiz)", colnames(x), ignore.case=TRUE)
  if (!identical(loc, integer(0))) {
    return(if(which) loc else TRUE)
  }
  else FALSE
}

#' @keywords internal
has.Ask <- function(x, which = FALSE) {
  colAttr <- attr(x, "Ask") #case sensitive; doesn't work for SYMBOL.Ask :-(
  if(!is.null(colAttr))
    return(if(which) colAttr else TRUE)
  #first try with "price" for data that has both ask.size and ask.price
  loc <- grep("(ask|offer).*price", colnames(x), ignore.case=TRUE)
  if (identical(loc, integer(0))) #if that failed, try to find just "ask|offer"
    loc <- grep("(ask|offer|ofr)", colnames(x), ignore.case=TRUE)
  if (!identical(loc, integer(0))) {
    return(if(which) loc else TRUE)
  } else FALSE
}

#' @keywords internal
has.AskSize <- function(x, which = FALSE) {
  colAttr <- attr(x, "AskSize")
  if(!is.null(colAttr))
    return(if(which) colAttr else TRUE)
  
  loc <- grep("(ask|offer).*(size|qty|quantity)", colnames(x), ignore.case=TRUE)
  if (!identical(loc, integer(0))) {
    return(if(which) loc else TRUE)
  }
  loc <- grep("(ofrsize|ofrsiz|offersize|offersiz|asksiz)", colnames(x), ignore.case=TRUE)
  if (!identical(loc, integer(0))) {
    return(if(which) loc else TRUE)
  }
  else FALSE
}

#' @keywords internal
has.Price <- function(x, which = FALSE) {
  colAttr <- attr(x, "Price")
  if(!is.null(colAttr))
    return(if(which) colAttr else TRUE)
  
  locBidAsk <- c(has.Bid(x, which=TRUE),has.Ask(x, which=TRUE))
  loc <- grep("price", colnames(x), ignore.case=TRUE)
  loc <- loc[!(loc %in% locBidAsk)]
  if (!identical(loc, integer(0))) {
    return(if(which) loc else TRUE)
  } else FALSE
}

#' @keywords internal
has.Trade <- function(x, which = FALSE) {
  colAttr <- attr(x, "Trade")
  if(!is.null(colAttr)) {
    return(if(which) colAttr else TRUE)
  }
  loc <- grep("trade", colnames(x), ignore.case=TRUE)
  if (!identical(loc, integer(0))) {
    return(if(which) loc else TRUE)
  } else FALSE
}

#' @keywords internal
has.Mid <- function(x, which=FALSE) {
  colAttr <- attr(x, "Mid")
  if(!is.null(colAttr))
    return(if(which) colAttr else TRUE)
  
  loc <- grep("Mid", colnames(x), ignore.case = TRUE)
  if (!identical(loc, integer(0)))
    return(ifelse(which, loc, TRUE))
  ifelse(which, loc, FALSE)
}

#' @keywords internal
has.Chg <- function(x, which=FALSE) {
  colAttr <- attr(x, "Chg")
  if(!is.null(colAttr))
    return(if(which) colAttr else TRUE)
  loc <- grep("(chg|change)", colnames(x), ignore.case=TRUE)
  if (!identical(loc, integer(0)))
    return(ifelse(which, loc, TRUE))
  ifelse(which, loc, FALSE)
}

#' @keywords internal
has.Cl <- function (x, which = FALSE){
  colAttr <- attr(x, "Cl")
  if (!is.null(colAttr))
    return(if (which) colAttr else TRUE)
  loc <- grep("Close", colnames(x), ignore.case = TRUE)
  if (!identical(loc, integer(0))) {
    return(if (which) loc else TRUE)
  }
  else FALSE
}

#' @keywords internal
has.Ad<-function (x, which = FALSE){
  colAttr <- attr(x, "Ad")
  if (!is.null(colAttr))
    return(if (which) colAttr else TRUE)
  loc <- grep("Adjusted", colnames(x), ignore.case = TRUE)
  if (!identical(loc, integer(0))) {
    return(if (which) loc else TRUE)
  }
  else FALSE
}

#' @keywords internal
has.Hi <- function (x, which = FALSE) {
  colAttr <- attr(x, "Hi")
  if (!is.null(colAttr))
    return(if (which) colAttr else TRUE)
  loc <- grep("High", colnames(x), ignore.case = TRUE)
  if (!identical(loc, integer(0))) {
    return(if (which) loc else TRUE)
  }
  else FALSE
}

#' @keywords internal
has.Lo <- function (x, which = FALSE) {
  colAttr <- attr(x, "Lo")
  if (!is.null(colAttr))
    return(if (which) colAttr else TRUE)
  loc <- grep("Low", colnames(x), ignore.case = TRUE)
  if (!identical(loc, integer(0))) {
    return(if (which) loc else TRUE)
  }
  else FALSE
}

#' @keywords internal
has.Op<-function (x, which = FALSE) {
  colAttr <- attr(x, "Op")
  if (!is.null(colAttr)) {
    return(if (which) colAttr else TRUE)
  }
  loc <- grep("Open", colnames(x), ignore.case = TRUE)
  if (!identical(loc, integer(0))) {
    return(if (which) loc else TRUE)
  }
  else FALSE
}

#' @keywords internal
has.Vo<-function (x, which = FALSE){
  colAttr <- attr(x, "Vo")
  if (!is.null(colAttr)) {
    return(if (which) colAttr else TRUE)
  }
  loc <- grep("Volume", colnames(x), ignore.case = TRUE)
  if (!identical(loc, integer(0))) {
    return(if (which) loc else TRUE)
  }
  else FALSE
}

#' Check for Trade, Bid, and Ask/Offer (BBO/TBBO), Quantity, and Price data
#' @description A set of functions to check for appropriate TBBO/BBO and price column
#' names within a data object, as well as the availability and
#' position of those columns.
#' 
#' @param x data object
#' @param which display position of match
#' 
#' @export
hasQty <- function(x, which = FALSE) {
  colAttr <- attr(x, "Qty")
  if(!is.null(colAttr)) {
    return(if (which) colAttr else TRUE)
  }
  
  locBidAsk <- c(has.Bid(x, which = TRUE), has.Ask(x, which = TRUE))
  loc <- grep("qty", colnames(x), ignore.case=TRUE)
  loc <- loc[!(loc %in% locBidAsk)]
  if (!identical(loc, integer(0))) {
    return(if(which) loc else TRUE)
  } else FALSE
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
  if (!any(colnames(tData) == "SYMBOL")) {
    stop("The argument tData should have a column containing SYMBOL. Could not find that column.")
  }
  
  if (is.data.table(tData)) {
    if (typeof(tData$PRICE) != "double") {
      stop("Column PRICE should be of type double.")
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
  if (!any(colnames(qData) == "SYMBOL")) {
    stop("The argument qData should have a column containing SYMBOL. Could not find that column.")
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

# Column setting functions
#' @keywords internal
set.AllColumns <- function(x) {
  cols <- c("Op","Hi","Lo","Cl","Vo","Ad","Price","Trade","Qty",
            "Bid","BidSize","Ask", "AskSize", "Mid", "Chg")
  for(col in cols) {
    try(x <- do.call(paste("set", col, sep = "."), list(x)), silent = TRUE)
  }
  return(x)
}

#' @keywords internal
set.Chg <- function(x, error = TRUE) {
  if (has.Chg(x))
    attr(x,"Chg") <- has.Chg(x, which = TRUE)
  return(x)
}

#' @keywords internal
set.Mid <- function(x, error = TRUE) {
  if(has.Mid(x))
    attr(x,"Mid") <- has.Mid(x, which=TRUE)
  return(x)
}

#' @keywords internal
set.Ad <- function(x, error = TRUE) {
  if(has.Ad(x))
    attr(x,"Ad") <- has.Ad(x, which=TRUE)
  return(x)
}

#' @keywords internal
set.Bid <- function(x, error=TRUE) {
  if(has.Bid(x))
    attr(x,"Bid") <- has.Bid(x, which=TRUE)
  return(x)
}

#' @keywords internal
set.BidSize <- function(x, error=TRUE) {
  if(has.BidSize(x))
    attr(x,"BidSize") <- has.BidSize(x, which=TRUE)
  return(x)
}

#' @keywords internal
set.Hi <- function(x, error=TRUE) {
  if(has.Hi(x))
    attr(x,"Hi") <- has.Hi(x, which=TRUE)
  return(x)
}

#' @keywords internal
set.Lo <- function(x, error=TRUE) {
  if(has.Lo(x))
    attr(x,"Lo") <- has.Lo(x, which=TRUE)
  return(x)
}

#' @keywords internal
set.Op <- function(x, error=TRUE) {
  if(has.Op(x))
    attr(x,"Op") <- has.Op(x, which=TRUE)
  return(x)
}

#' @keywords internal
set.Qty <- function(x, error=TRUE) {
  if(hasQty(x))
    attr(x,"Qty") <- hasQty(x, which=TRUE)
  return(x)
}

#' @keywords internal
set.Vo <- function(x, error=TRUE) {
  if(has.Vo(x))
    attr(x,"Vo") <- has.Vo(x, which=TRUE)
  return(x)
}

#' @keywords internal
set.Ask <- function(x, error=TRUE) {
  if(has.Ask(x))
    attr(x,"Ask") <- has.Ask(x, which=TRUE)
  return(x)
}

#' @keywords internal
set.AskSize <- function(x, error=TRUE) {
  if(has.AskSize(x))
    attr(x,"AskSize") <- has.AskSize(x, which=TRUE)
  return(x)
}

#' @keywords internal
set.Cl <- function(x, error = TRUE) {
  if(has.Cl(x))
    attr(x,"Cl") <- has.Cl(x, which=TRUE)
  return(x)
}

#' @keywords internal
set.Price <- function(x, error = TRUE) {
  if(has.Price(x))
    attr(x,"Price") <- has.Price(x, which = TRUE)
  return(x)
}

#' @keywords internal
set.Trade <- function(x, error = TRUE) {
  if(has.Trade(x))
    attr(x,"Trade") <- has.Trade(x, which = TRUE)
  return(x)
}


## #' @keywords internal
## #' @importFrom data.table fread
## readDataset <- function(path){
##   extension <- regexpr("*.[a-z]{0,}$", path) # Extract the extension of the file.
##   extension <- substr(path, start = extension[[1]], stop = extension[[1]] + attr(extension, "match.length"))
##   
##   if(extension == ".rds"){
##     return(try(readRDS(path)))
##   } else if( extension == ".csv"){
##     return(try(fread(path)))
##   }else {
##      stop("Extension not recognized")
##    }
##   
## }


#' This function is not exported, but the documentation is here to help users
#' @keywords internal
#' @importFrom data.table copy data.table setnafill
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
    # axis(1, at = seq.POSIXt(as.POSIXct(range(tqData$DT), origin = as.POSIXct("1970-01-01", tz = "GMT"), tz = "GMT")[1], as.POSIXct(range(tqData$DT), origin = as.POSIXct("1970-01-01", tz = "GMT"), tz = "GMT")[2], length.out = 3, tz = "GMT"),
    #      labels = format(seq.POSIXt(as.POSIXct(range(tqData$DT), origin = as.POSIXct("1970-01-01", tz = "GMT"), tz = "GMT")[1], as.POSIXct(range(tqData$DT), origin =as.POSIXct("1970-01-01", tz = "GMT"))[2], length.out = 3, tz = "GMT"),
    #      format = "%H:%M"))
    lines(tqData$DT, tqData$OFR, type = "l", col = "blue", ylim = yLim)
    lines(tqData$DT, tqData$BID, type = "l", col = "green", ylim = yLim)
    points(tqData[remaining, DT], tqData[remaining, PRICE], pch = 4, col = "red")
    tzone(tqData) <- tz
    
  }
  
  tqData[c(whichBackwards, whichForwards), `:=`(DT = DT - c(na.omit(backwardsMatches), na.omit(forwardsMatches)))]
  tqData[c(whichBackwards, whichForwards), `:=`(BID = NA)]
  tqData[c(whichBackwards, whichForwards), `:=`(OFR = NA)]
  
  
  
  tqData <- tqData[!remaining]
  setkey(tqData, DT)
  setnafill(tqData, "locf", cols = c("BID", "OFR"))
  
  
  
  return(list("tqData" = tqData, "forwardMatched" = cbind(forwardsMatches, whichForwards), 
              "backwardsMatched" = cbind(backwardsMatches, whichBackwards), "unmatched" = remaining))
}

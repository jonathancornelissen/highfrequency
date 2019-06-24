
# Necessary for check-package not throwing errors
#' @keywords internal
`:=` <- function(...) {
  NULL
}

#' @importFrom xts xtsAttributes
#' @keywords internal
checkColumnNames <- function(data) { 
  # FUNCTION sets column names according to RTAQ format using quantmod conventions, such that all the other functions find the correct information.
  # First step: assign the xts attributes:
  data <- set.AllColumns(data)
  
  # Change column names to previous RTAQ format! 
  # Adjust price col naming:  
  try((colnames(data)[xtsAttributes(data)[['Price']]] = 'PRICE'))
  try(setnames(data, "Price", "PRICE", skip_absent = TRUE), silent = TRUE)
  # Adjust Bid col naming:    
  try((colnames(data)[xtsAttributes(data)[['Bid']]] = 'BID'))
  try(setnames(data, "Bid", "BID", skip_absent = TRUE), silent = TRUE)
  # Adjust Ask col naming:    
  try((colnames(data)[xtsAttributes(data)[['Ask']]] = 'OFR'))
  try(setnames(data, "Ask", "OFR", skip_absent = TRUE), silent = TRUE)
  # Adjust SYMBOL col naming:    
  try((colnames(data)[xtsAttributes(data)[['SYM_ROOT']]] = 'SYMBOL'))
  try(setnames(data, "SYM_ROOT", "SYMBOL", skip_absent = TRUE), silent = TRUE)
  
  # Adjust Ask size col naming:
  try((colnames(data)[xtsAttributes(data)[['BidSize']]] = 'BIDSIZ'))
  try(setnames(data, "BidSize", "BIDSIZ", skip_absent = TRUE), silent = TRUE)
  
  # Adjust Bid size col naming:    
  try((colnames(data)[xtsAttributes(data)[['AskSize']]] = 'OFRSIZ'))
  try(setnames(data, "AskSize", "OFRSIZ", skip_absent = TRUE), silent = TRUE)
  
  try(setnames(data, "TR_SCOND", "COND", skip_absent = TRUE), silent = TRUE)
  
  
  # Adjust correction column, if necessary:
  if (any(colnames(data) == "CR")) {
    colnames(data)[colnames(data) == "CR"] = "CORR"
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
  if (all(has.Trade(x), has.Qty(x), has.Bid(x), has.Ask(x))) {
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
  loc <- grep("(ofrsize|ofrsiz|offersize|offersiz)", colnames(x), ignore.case=TRUE)
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
has.Lo <- function (x, which = FALSE){
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

#' @keywords internal
has.Qty <- function(x, which = FALSE)
{
  colAttr <- attr(x, "Qty")
  if(!is.null(colAttr)) {
    return(if(which) colAttr else TRUE)
  }
  
  locBidAsk <- c(has.Bid(x, which=TRUE),has.Ask(x, which=TRUE))
  loc <- grep("qty", colnames(x), ignore.case=TRUE)
  loc <- loc[!(loc %in% locBidAsk)]
  if (!identical(loc, integer(0))) {
    return(if(which) loc else TRUE)
  } else FALSE
}

#' @importFrom data.table is.data.table
#' @importFrom xts is.xts
#' @keywords internal
checktdata <- function(tdata) {
  if (is.xts(tdata) == FALSE & is.data.table(tdata) == FALSE) {
    stop("The argument tdata should be an data.table or xts object.")
  }
  if (any(colnames(tdata) == "PRICE") == FALSE) {
    stop("The argument tdata should have a column containing the PRICE. Could not find that column.")
  }
  if (any(colnames(tdata) == "SYMBOL") == FALSE) {
    stop("The argument tdata should have a column containing SYMBOL. Could not find that column.")
  }
  
  if (is.data.table(tdata) == TRUE) {
    if (typeof(tdata$PRICE) != "double") {
      stop("Column PRICE should be of type double.")
    }
  }
}

#' @importFrom data.table is.data.table
#' @importFrom xts is.xts
#' @keywords internal
checkqdata <- function(qdata) {
  if (is.xts(qdata) == FALSE & is.data.table(qdata) == FALSE) {
    stop("The argument qdata should be an data.table or xts object.")
  }
  if (any(colnames(qdata) == "BID") == FALSE) {
    stop("The argument qdata should have a column containing the BID. Could not find that column.")
  }
  if (any(colnames(qdata) == "OFR") == FALSE) {
    stop("The argument qdata should have a column containing the ASK / OFR. Could not find that column.")
  }
  if (any(colnames(qdata) == "SYMBOL") == FALSE) {
    stop("The argument qdata should have a column containing SYMBOL. Could not find that column.")
  }
  if (is.data.table(qdata) == TRUE) {
    if (typeof(qdata$BID) != "double") {
      stop("Column BID should be of type double.")
    }
    if (typeof(qdata$OFR) != "double") {
      stop("Column OFR should be of type double.")
    }
  }
}

# Function for calculating three different measures:
# 1. Rolling centered median (excluding the observation under consideration)
# 2. Rolling median of the following "window" observations
# 3. Rolling median of the previous "window" observations
#' @keywords internal
rolling_median_incl_ends <- function(x, weights, window, direction = "center") {
  
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
            "Bid","BidSize","Ask","AskSize","Mid","Chg")
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
  if(has.Qty(x))
    attr(x,"Qty") <- has.Qty(x, which=TRUE)
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
set.Cl <- function(x, error=TRUE) {
  if(has.Cl(x))
    attr(x,"Cl") <- has.Cl(x, which=TRUE)
  return(x)
}

#' @keywords internal
set.Price <- function(x, error=TRUE) {
  if(has.Price(x))
    attr(x,"Price") <- has.Price(x, which=TRUE)
  return(x)
}

#' @keywords internal
set.Trade <- function(x, error=TRUE) {
  if(has.Trade(x))
    attr(x,"Trade") <- has.Trade(x, which=TRUE)
  return(x)
}

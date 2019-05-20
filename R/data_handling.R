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
      stop(paste("subscript out of bounds: no column name containing ",symbol,"."))
    }
  }
  if (is.null(prefer) == TRUE) {
    # default to trying Price, then Trade, then Close
    if(has.Price(x)) prefer = 'price'
    else if(has.Trade(x)) prefer = 'trade'
    else if(has.Cl(x))    prefer = 'close'
    else stop("subscript out of bounds, no price was discernible from the data.")
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
    if (!identical(loc, integer(0))) {
      return(x[, loc])
    } else {
      stop("subscript out of bounds, no price was discernible from the data.")
    }
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
#' sample. In case it is zero (which can happen if mid-quotes don't change much), 
#' the median absolute deviation is taken over a subsample without constant mid-quotes.
#' 
#' @param qdata an xts object at least containing the columns "BID" and "OFR".
#' @param maxi an integer, indicating the maximum number of median absolute deviations allowed.
#' @param window an integer, indicating the time window for which the "outlyingness" is considered.
#' @param type should be "standard" or "advanced" (see description).
#' 
#' @details NOTE: This function works only correct if supplied input data consists of 1 day.
#' 
#' @return xts object
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
rmOutliers <- function (qdata, maxi = 10, window = 50, type = "advanced") {
  # NOTE: Median Absolute deviation chosen contrary to Barndorff-Nielsen et al.
  # Setting those variables equal NULL is for suppressing NOTES in devtools::check
  # References inside data.table-operations throw "no visible binding for global variable ..." error
  BID = OFR = MIDQUOTE = DATE = DT = MADALL = CRITERION = NULL
  if ((window %% 2) != 0) {
    stop("Window size can't be odd.")
  }
  
  checkQdata(qdata)
  qdata <- checkColumnNames(qdata)
  
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
    stop("Please only one symbol at a time.")
  }
  
  if ((type %in% c("standard", "advanced")) == FALSE) {
    stop("type has to be \"standard\" or \"advanced\".")
  }
  
  # weights_med_center_incl <- rep(1, times = window + 1)
  weights_med_center_excl <- c(rep(1, times = window / 2), 0, rep(1, times = window / 2))
  weights_med_follow  <- c(0 , rep(1, times = window))
  weights_med_trail    <- c(rep(1, times = window), 0)
  
  halfwindow <- window / 2
  
  # Function for calculating three different measures:
  # 1. Rolling centered median (excluding the observation under consideration)
  # 2. Rolling median of the following "window" observations
  # 3. Rolling median of the previous "window" observations
  rolling_median_incl_ends <- function(x, weights, direction = "center") {
    
    length_median_vec <- length(x)
    median_vec <- rep(NA, times = length_median_vec)
    
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
  
  qdata <- qdata[, MIDQUOTE := (BID + OFR) / 2][, DATE := as.Date(DT)][, MADALL := mad(MIDQUOTE), by = "DATE"]
  
  if (type == "standard") {
    qdata <- qdata[ , CRITERION := abs(MIDQUOTE - rolling_median_incl_ends(MIDQUOTE, weights = weights_med_center_excl))][
      CRITERION < maxi * MADALL]
    
  }
  if (type == "advanced") {
    qdata <- qdata[, CRITERION := pmin(abs(MIDQUOTE - rolling_median_incl_ends(MIDQUOTE, weights = weights_med_center_excl, direction = "center")),
                                       abs(MIDQUOTE - rolling_median_incl_ends(MIDQUOTE, weights = weights_med_trail, direction = "left")),
                                       abs(MIDQUOTE - rolling_median_incl_ends(MIDQUOTE, weights = weights_med_follow, direction = "right")),
                                       na.rm = TRUE)][
                                         CRITERION < maxi * MADALL]
  }
  
  if (dummy_was_xts == TRUE) {
    return(xts(as.matrix(qdata[, -c("DT", "DATE", "MADALL", "CRITERION", "MIDQUOTE")]), order.by = qdata$DT))
  } else {
    qdata[, -c("MADALL", "CRITERION")]
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


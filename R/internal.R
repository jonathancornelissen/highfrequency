#' Aggregate a time series
#' 
#' @description Function returns aggregated time series as xts object. 
#' It can handle irregularly spaced timeseries and returns a regularly spaced one.
#' Use univariate timeseries as input for this function, and check out \code{\link{aggregateTrades}}
#' and \code{\link{aggregateQuotes}} to aggregate Trade or Quote data objects.
#' 
#' @param ts xts object to aggregate.
#' @param on character, indicating the time scale in which "k" is expressed. Possible values are: "secs", "seconds", "mins", "minutes", "hours", "days", "weeks".
#' @param k positive integer, indicating the number of periods to aggregate over. E.g. to aggregate a 
#' xts object to the 5 minute frequency set k=5 and on="minutes".
#' @param tz time zone used
#' 
#' @details The timestamps of the new time series are the closing times and/or days of the intervals. 
#' E.g. for a weekly aggregation the new timestamp is the last day in that particular week (namely sunday).
#' 
#' In case of previous tick aggregation, 
#' for on = "seconds"/"minutes"/"hours",
#' the element of the returned series with e.g. timestamp 09:35:00 contains 
#' the last observation up to that point, excluding the value at 09:35:00 itself.
#' 
#' Please Note:
#' In case an interval is empty these NA's are filled by the function \code{na.locf}
#' (last observation carried forward) from the zoo package.
#' 
#' @return An xts object containing the aggregated time series.
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' @keywords data manipulation
#' 
#' @examples 
#' #load sample price data
#' ts <- sample_tdata$PRICE
#' 
#' #Previous tick aggregation to the 5-minute sampling frequency:
#' tsagg5min <- aggregatets(ts, on = "minutes", k = 5)
#' head(tsagg5min)
#' #Previous tick aggregation to the 30-second sampling frequency:
#' tsagg30sec <- aggregatets(ts, on = "seconds", k = 30)
#' tail(tsagg30sec)
#' 
#' @importFrom zoo zoo na.locf
#' @importFrom stats start end
#' @export
aggregatets <- function (ts, on = "minutes", k = 1, tz = "GMT") {
  if (on == "secs" | on == "seconds") {
    secs <- k
    tby <- paste(k, "sec", sep = " ")
  } 
  if (on == "mins" | on == "minutes") {
    secs <- 60 * k
    tby <- paste(60 * k, "sec", sep = " ")
  } 
  if (on == "hours"){
    secs <- 3600 * k
    tby <- paste(3600 * k, "sec", sep = " ")
  } 
  g <- base::seq(start(ts), end(ts), by = tby)
  rawg <- as.numeric(as.POSIXct(g, tz = tz))
  newg <- rawg + (secs - rawg%%secs)
  g    <- as.POSIXct(newg, origin = "1970-01-01", tz = tz)
  ts <- na.locf(merge(ts, zoo(NULL, g)))[as.POSIXct(g, tz = tz)]
  return(ts)
}

# system.time(aggregatets(sample_tdata$PRICE, k = 5))
# # 
# system.time({blub <- sample_tdata$PRICE[endpoints(index(sample_tdata$PRICE), on = "minutes", k = 5), ]
# blub <- xts(blub, order.by = ceiling_date(index(blub), unit = paste(5, "minutes")))})
# blub[1]
# 
# 
# if (index(blub[dim(blub)[1] - 1, ]) == index(blub[dim(blub)[1], ])) {
#   blub <- blub[-(dim(blub)[1] - 1), ]
# }
# 
# as.xts(index = )

### Do a daily apply but with list as output:
#' @importFrom xts try.xts
#' @importFrom xts endpoints
#' @keywords internal
applyGetList <- function(x, FUN, cor = FALSE, align.by = NULL, align.period = NULL, makeReturns = FALSE, makePsd = NULL,...){
  on <- "days" 
  k <- 1
  x <- try.xts(x, error = FALSE)
  INDEX <- endpoints(x, on = on, k = k)
  D <- length(INDEX)-1
  result <- list()
  FUN <- match.fun(FUN)
  for(i in 1:(length(INDEX)-1)){
    if (is.null(makePsd) == TRUE) {
      result[[i]] <- FUN(x[(INDEX[i] + 1):INDEX[i + 1]], cor, align.by, align.period, makeReturns)
    } else {
      result[[i]] <- FUN(x[(INDEX[i] + 1):INDEX[i + 1]], cor, align.by, align.period, makeReturns, makePsd)
    }
    
  }
  return(result)
}

#' Returns the positive semidinite projection of a symmetric matrix using the eigenvalue method
#' 
#' @description Function returns the positive semidinite projection of a symmetric matrix using the eigenvalue method.
#' 
#' @param S matrix.
#' @param method character, indicating whether the negative eigenvalues of the correlation or covariance should be replaced by zero. Possible values are "covariance" and "correlation".
#' 
#' @details We use the eigenvalue method to transform \eqn{S} into a positive
#' semidefinite covariance matrix (see e.g. Barndorff-Nielsen and Shephard, 2004, and Rousseeuw and Molenberghs, 1993).  Let \eqn{\Gamma} be the
#' orthogonal matrix consisting of the \eqn{p} eigenvectors of \eqn{S}. Denote
#' \eqn{\lambda_1^+,\ldots,\lambda_p^+} its \eqn{p} eigenvalues, whereby the negative eigenvalues have been replaced by zeroes.
#' Under this approach, the positive semi-definite
#' projection of \eqn{S} is \eqn{ S^+ = \Gamma' \mbox{diag}(\lambda_1^+,\ldots,\lambda_p^+) \Gamma}. 
#' 
#' If method = "correlation", the eigenvalues of the correlation matrix corresponding to the matrix \eqn{S} are 
#' transformed. See Fan et al (2010).  
#' 
#' @return An xts object containing the aggregated trade data.
#'
#' @references 
#' Barndorff-Nielsen, O. and N. Shephard (2004). Measuring the impact of jumps in multivariate price processes using bipower covariation. Discussion paper, Nuffield College, Oxford University.
#' Fan, J., Y. Li, and K. Yu (2010). Vast volatility matrix estimation using high frequency data for portfolio selection. Working paper.
#' Rousseeuw, P. and G. Molenberghs (1993). Transformation of non positive semidefinite correlation matrices. Communications in Statistics - Theory and Methods 22, 965-984.
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' @keywords data manipulation
#' @export
makePsd <- function(S, method = "covariance") {
  if (method == "correlation" & !any(diag(S) <= 0) ) {
    # Fan, J., Y. Li, and K. Yu (2010). Vast volatility matrix estimation using high frequency data for portfolio selection.
    D <- matrix(diag(S)^(1/2), ncol=1)
    R <- S / (D %*% t(D))
    out <- eigen(x = R , symmetric = TRUE)
    mGamma <- t(out$vectors)
    vLambda <- out$values
    vLambda[vLambda<0] <- 0
    Apsd  <- t(mGamma)%*%diag(vLambda)%*%mGamma
    dApsd <- matrix(diag(Apsd)^(1/2),ncol=1)
    Apsd  <- Apsd/(dApsd%*%t(dApsd))
    D     <- diag( as.numeric(D)  , ncol = length(D) )
    Spos  <- D %*% Apsd %*% D
    return(Spos)
  } else {
    # Rousseeuw, P. and G. Molenberghs (1993). Transformation of non positive semidefinite correlation matrices. Communications in Statistics - Theory and Methods 22, 965-984.
    out     <- eigen(x = S , symmetric = TRUE)
    mGamma  <- t(out$vectors)
    vLambda <- out$values
    vLambda[vLambda<0] <- 0
    Apsd    <- t(mGamma) %*% diag(vLambda) %*% mGamma
  }
}

#' @importFrom xts is.xts
#' @importFrom xts ndays
#' @keywords internal
multixts <- function(x, y = NULL) { 
  if (is.null(y) == TRUE) {
    test <- is.xts(x) && (ndays(x)!=1)
    return(test)
  } else {
    test <- (is.xts(x) && (ndays(x)!=1)) || (ndays(y)!=1 && is.xts(y))
    if (test == TRUE) {
      equal_dimension <- (dim(y) == dim(x))
      if (equal_dimension == FALSE) { 
        warning("Please make sure x and y have the same dimensions")
        } else {
          test <- list(TRUE, cbind(x,y))
        return(test) 
      }
    } 
  } 
} 

#' @importFrom xts is.xts
#' @importFrom xts ndays
#' @keywords internal
checkMultiDays <- function(x) { 
  
  if (is.matrix(x) != is.xts(x)) {
    return(FALSE)
  }
  if (is.xts(x) == FALSE) {
    stop("Please provide xts-object.")
  }
  if (is.xts(x) && (ndays(x)!=1)) {
    TRUE
  } else {
    FALSE
  }
} 

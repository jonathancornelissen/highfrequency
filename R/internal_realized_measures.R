#' @importFrom stats qchisq
#' @importFrom stats pchisq
#' @keywords internal
conHR <- function(di, alpha = 0.05) {
# consistency factor ROWQCov based on hard rejection weight function
  return((1 - alpha) / pchisq(qchisq(1 - alpha, df = di), df = di + 2))
}

#' @importFrom stats qchisq
#' @importFrom stats integrate
#' @keywords internal
conhuber <- function(di, alpha = 0.05) {   
  c <- qchisq(p = 1 - alpha, df = di)
  fw2 <- function(t) {
    z <- t^2
    return(huberweight(z,c) * (t^(di - 1)) * exp(-z/2)) 
  }
  fw1 <- function(t) {
    z <- t^2
    return(huberweight(z,c) * (t^(di + 1)) * exp(-z/2))
  }
  c2 <- integrate(fw2, 0, Inf)$value
  c1 <- integrate(fw1, 0, Inf)$value
  return(di * c2/c1)
}

#' @keywords internal
huberweight <- function(d,k) {
  # Huber or soft rejection weight function
  w <- apply(cbind(rep(1, length(d)), (k/d)), 1, 'min')
  return(w)
}

#' @keywords internal
multixts <- function(x, y = NULL) { 
  if (is.null(y)) {
    test <- is.xts(x) && (ndays(x)!=1)
    return(test)
  }
  if(!is.null(y)){
    test <- (is.xts(x) && (ndays(x)!=1)) || ( ndays(y)!=1 && is.xts(y) );
    if (test) {
      test1 <- (dim(y) == dim(x))
      if (!test1) { 
        warning("Please make sure x and y have the same dimensions")
      }
      if (test1) {
        test <- list(TRUE, cbind(x,y))
        return(test) 
      }
    }
  }
}      

# Check data:
#' @keywords internal
rdatacheck <- function (rdata, multi = FALSE) {
  if ((dim(rdata)[2] < 2) & (multi)) {
    stop("Your rdata object should have at least 2 columns")
  }
}

#' Synchronize (multiple) irregular timeseries by refresh time
#' 
#' @description This function implements the refresh time synchronization scheme proposed by Harris et al. (1995). 
#' It picks the so-called refresh times at which all assets have traded at least once since the last refresh time point. 
#' For example, the first refresh time corresponds to the first time at which all stocks have traded.
#' The subsequent refresh time is defined as the first time when all stocks have again traded.
#' This process is repeated untill the end of one time series is reached.
#' 
#' @param pdata a list. Each list-item contains an xts object  
#' containing the original time series (one day only and typically a price series).
#' 
#' @return An xts object containing the synchronized time series.
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
#' @importFrom xts as.xts
#' @export
refreshTime <- function (pdata) {
  dim <- length(pdata)
  lengths <- rep(0, dim + 1)
  for (i in 1:dim) {
    lengths[i + 1] <- length(pdata[[i]])
  }
  minl <- min(lengths[(2:(dim + 1))])
  lengths <- cumsum(lengths)
  alltimes <- rep(0, lengths[dim + 1])
  for (i in 1:dim) {
    alltimes[(lengths[i] + 1):lengths[i + 1]] <- as.numeric(as.POSIXct(index(pdata[[i]]), tz = "GMT"))
  }
  x <- .C("refreshpoints", as.integer(alltimes), as.integer(lengths),
         as.integer(rep(0, minl)), as.integer(dim), as.integer(0),
         as.integer(rep(0, minl * dim)), as.integer(minl), PACKAGE = "highfrequency")
  
  newlength <- x[[5]]
  pmatrix <- matrix(ncol = dim, nrow = newlength)
  for (i in 1:dim) {
    selection    <- x[[6]][((i - 1) * minl + 1):(i * minl)]
    pmatrix[, i] <- pdata[[i]][selection[1:newlength]]
  }
  time <- as.POSIXct(x[[3]][1:newlength], origin = "1970-01-01", tz = "GMT")
  resmatrix <- xts(pmatrix, order.by = time)
  return(resmatrix)
}

#' @keywords internal
RBPCov_bi <- function(ts1, ts2) {
  n <- length(ts1)
  a <- abs(ts1 + ts2)
  b <- abs(ts1 - ts2)
  first <- as.numeric(a[1:(n-1)]) * as.numeric(a[2:n])
  last <- as.numeric(b[1:(n-1)]) * as.numeric(b[2:n])
  result <-  (pi/8)*sum(first-last)
  return(result)
}

#' @keywords internal
RBPVar <- function(rdata) {
  
  returns <- as.vector(as.numeric(rdata))
  n <- length(returns)
  rbpvar <- (pi/2) * sum(abs(returns[1:(n-1)]) * abs(returns[2:n]))
  return(rbpvar)
}

#' @importFrom robustbase covMcd
#' @keywords internal
ROWVar <- function(rdata, seasadjR = NULL, wfunction = "HR" , alphaMCD = 0.75, alpha = 0.001) {
  
  if (is.null(seasadjR) == TRUE) {
    seasadjR <- rdata
  }
  
  rdata <- as.vector(rdata)
  seasadjR <- as.vector(seasadjR)
  intraT <- length(rdata); N=1
  MCDcov <- as.vector(covMcd( rdata , use.correction = FALSE )$raw.cov)
  outlyingness <- seasadjR^2/MCDcov    
  k <- qchisq(p = 1 - alpha, df = N)
  outlierindic <- outlyingness > k
  weights <- rep(1, intraT)
  if (wfunction == "HR") {
    weights[outlierindic] <- 0
    wR <- sqrt(weights) * rdata
    return((conHR(di = N, alpha = alpha) * sum(wR^2)) / mean(weights))
  }
  if (wfunction == "SR") {
    weights[outlierindic] <- k/outlyingness[outlierindic]
    wR <- sqrt(weights) * rdata
    return((conhuber(di = N, alpha = alpha) * sum(wR^2)) / mean(weights))
  }
  
}

#' @importFrom xts first
#' @keywords internal
TSCov_bi <- function (pdata1, pdata2, K = 300, J = 1) {
  x <- refreshTime(list(pdata1, pdata2))
  newprice1 <- x[, 1]
  newprice2 <- x[, 2]
  logprices1 <- log(as.numeric(newprice1))
  logprices2 <- log(as.numeric(newprice2))
  seconds <- as.numeric(as.POSIXct(index(newprice1)))
  secday <- last(seconds) - first(seconds)
  n <- length(logprices1)
  nbarK <- (n - K + 1) / K
  nbarJ <- (n - J + 1) / J
  adj <- n / ((K - J) * nbarK)
  
  logreturns_K1 = logreturns_K2 = logreturns_J1 = logreturns_J2 = c()
  vdelta_K = vdelta_J = c()
  
  for (k in 1:K) {
    sel.avg <- seq(k, n, K)
    logreturns_K1 <- c(logreturns_K1, diff(logprices1[sel.avg]))
    logreturns_K2 <- c(logreturns_K2, diff(logprices2[sel.avg]))
    vdelta_K <- c(vdelta_K, diff(seconds[sel.avg]) / secday)
  }
  
  for (j in 1:J) {
    sel.avg <- seq(j, n, J)
    logreturns_J1 <- c(logreturns_J1, diff(logprices1[sel.avg]))
    logreturns_J2 <- c(logreturns_J2, diff(logprices2[sel.avg]))
    vdelta_J <- c(vdelta_J, diff(seconds[sel.avg])/secday)
  }
  
  TSCOV <- adj * ((1/K) * sum(logreturns_K1 * logreturns_K2) - 
                   ((nbarK/nbarJ) * (1/J) * sum(logreturns_J1 * logreturns_J2)))
  return(TSCOV)
}

#' @keywords internal
TSRV <- function(pdata , K = 300 , J = 1) {
  # based on rv.timescale
  logprices <- log(as.numeric(pdata))
  n <- length(logprices) 
  nbarK <- (n - K + 1)/(K) # average number of obs in 1 K-grid
  nbarJ <- (n - J + 1)/(J)
  adj <- (1 - (nbarK/nbarJ))^-1 
  logreturns_K = logreturns_J = c()
  for (k in 1:K) {
    sel <- seq(k,n,K)  
    logreturns_K <- c(logreturns_K, diff( logprices[sel]))
  }
  for (j in 1:J) {
    sel <-  seq(j,n,J)
    logreturns_J <- c(logreturns_J, diff( logprices[sel]))
  }
  TSRV <- adj * ( (1/K) * sum(logreturns_K^2) - ((nbarK/nbarJ) * (1/J) * sum(logreturns_J^2)))
  return(TSRV)
}

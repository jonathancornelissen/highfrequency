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
rdatacheck = function (rdata, multi = FALSE) {
  if ((dim(rdata)[2] < 2) & (multi)) {
    stop("Your rdata object should have at least 2 columns")
  }
}

#' @keywords internal
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
    selection <- x[[6]][((i - 1) * minl + 1):(i * minl)]
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

#' @keywords internal
ROWVar <- function(rdata, seasadjR = NULL, wfunction = "HR" , alphaMCD = 0.75, alpha = 0.001,...) {
  
  if (is.null(seasadjR)) {
    seasadjR = rdata
  }
  
  rdata <- as.vector(rdata)
  seasadjR <- as.vector(seasadjR)
  intraT <- length(rdata); N=1
  MCDcov <- as.vector(robustbase::covMcd( rdata , use.correction = FALSE )$raw.cov)
  outlyingness <- seasadjR^2/MCDcov    
  k <- qchisq(p = 1 - alpha, df = N)
  outlierindic <- outlyingness > k
  weights <- rep(1, intraT)
  if( wfunction == "HR" ){
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


#' @keywords internal
alignReturns <- function(x, period, ...) {
  .C("rv", 
     as.double(x), #a
     as.double(x), #b
     as.integer(length(x)), #na
     as.integer(period), #period 
     tmpa = as.double(rep(0,as.integer(length(x) / period +1))), #tmp
     as.double(rep(0,as.integer(length(x) / period +1))), #tmp
     as.integer(length(x) / period), #tmpn
     ans = double(1), 
     COPY = c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE), 
     PACKAGE = "highfrequency")$tmpa     
}

#' @importFrom stats pchisq
#' @keywords internal
cfactor_RTSCV <- function(eta = 9) {
  # rho = 1
  c1 <- pchisq(eta, df = 1) / pchisq(eta, df = 3) 
  # 
  rho <- 0.001
  R <- matrix( c(1,rho,rho,1) , ncol = 2 ) 
  int1 <- function(x) {    
    mvtnorm::dmvnorm(x, sigma = R) 
  }
  num = cubature::adaptIntegrate(int1, c(-3,-3), c(3,3), tol=1e-4)$integral
  int2 <- function(x) {  
    x[1] * x[2] * mvtnorm::dmvnorm(x, sigma = R) 
  }
  denom <- cubature::adaptIntegrate(int2, c(-3,-3), c(3,3), tol=1e-4)$integral
  c2 <- rho * num / denom   
  return((c1 + c2) / 2)
}

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



rcKernel <- function(x,                             # Tick Data for first asset
                     y,                             # Tick Data for second asset
                     kernel.type = "rectangular",   # Kernel name (or number)
                     kernel.param = 1,              # Kernel parameter (usually lags)
                     kernel.dofadj = TRUE,          # Kernel Degree of freedom adjustment
                     align.by = "seconds",            # Align the tick data to [seconds|minutes|hours]
                     align.period = 1,              # Align the tick data to this many [seconds|minutes|hours]
                     cts = TRUE,                    # Calendar Time Sampling is used
                     makeReturns = FALSE) {           # Convert to Returns
  #
  # Handle deprication
  #
  if(!is.null(type)){
    warning("type is deprecated, use kernel.type")
    kernel.type <- type
  }
  if(!is.null(q)){
    warning("q is deprecated, use kernel.param")
    kernel.param <- q
  }
  if(!is.null(adj)){
    warning("adj is deprecated, use kernel.dofadj")
    kernel.dofadj <- adj
  }
  
  align.period <- .getAlignPeriod(align.period, align.by)   
  cdata <- .convertData(x, cts = cts, makeReturns = makeReturns)
  
  x <- cdata$data
  x <- .alignReturns(x, align.period)
  cdatay <- .convertData(y, cts = cts, makeReturns = makeReturns)
  y <- cdatay$data
  y <- .alignReturns(y, align.period)
  type <- kernelCharToInt(kernel.type)
  .C("kernelEstimator", as.double(x), as.double(y), as.integer(length(x)),
     as.integer(kernel.param), as.integer(ifelse(kernel.dofadj, 1, 0)),
     as.integer(type), ab=double(kernel.param + 1),
     ab2 = double(kernel.param + 1),
     ans = double(1), PACKAGE = "highfrequency")$ans
}



# Hayashi-Yoshida helper function:
rcHY <- function(x, y, period = 1, align.by = "seconds", align.period = 1, makeReturns = FALSE) {
  align.period = .getAlignPeriod(align.period, align.by)
  cdata <- .convertData(x, cts=cts, makeReturns=makeReturns)
  x <- cdata$data
  x.t <- cdata$milliseconds
  
  cdatay <- .convertData(y, cts=cts, makeReturns=makeReturns)
  y <- cdatay$data
  y.t <- cdatay$milliseconds
  
  
  errorCheck <- c(is.null(x.t),is.na(x.t), is.null(y.t), is.na(y.t))
  if(any(errorCheck))
    stop("ERROR: Time data is not in x or y.")
  
  
  sum(.C("pcovcc", 
         as.double(x), #a
         as.double(rep(0,length(x)/(period*align.period)+1)),
         as.double(y), #b
         as.double(x.t), #a
         as.double(rep(0,length(x)/(period*align.period)+1)), #a
         as.double(y.t), #b
         as.integer(length(x)), #na
         as.integer(length(x)/(period*align.period)),
         as.integer(length(y)), #na
         as.integer(period*align.period),
         ans = double(length(x)/(period*align.period)+1), 
         COPY=c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE), 
         PACKAGE="highfrequency")$ans)
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

#' @keywords internal
kernelCharToInt <- function(type) {
  if (is.character(type) == TRUE) {
    ans <- switch(casefold(type), 
                  rectangular = 0,
                  bartlett = 1,
                  second = 2,
                  epanechnikov = 3,
                  cubic = 4,
                  fifth = 5,
                  sixth = 6,
                  seventh = 7,
                  eighth = 8,
                  parzen = 9,
                  th = 10,
                  mth = 11,
                  tukeyhanning = 10,
                  modifiedtukeyhanning = 11,
                  -99)
    
    if (ans == -99) { 
      warning("Invalid Kernel, using Bartlet")
      1
    } else {
      ans     
    }
  } else {
    type
  }
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

#' @keywords internal
RTSCov_bi <- function (pdata1, pdata2, startIV1 = NULL, startIV2 = NULL, noisevar1 = NULL, 
                       noisevar2 = NULL, K = 300, J = 1,
                       K_cov = NULL, J_cov = NULL,
                       K_var1 = NULL, K_var2 = NULL,
                       J_var1 = NULL, J_var2 = NULL, 
                       eta = 9) {
  
  if (is.null(K_cov)){ 
    K_cov <- K 
  }
  if (is.null(J_cov)) { 
    J_cov <- J 
  }
  if (is.null(K_var1)) { 
    K_var1 <- K 
  }
  if (is.null(K_var2)) { 
    K_var2 <- K 
  }   
  if (is.null(J_var1)) { 
    J_var1 <- J 
  } 
  if (is.null(J_var2)){ 
    J_var2 <- J 
  }
  
  # Calculation of the noise variance and TSRV for the truncation
  if (is.null(noisevar1) == TRUE) {
    logprices1 <- log(as.numeric(pdata1))
    n_var1     <- length(logprices1)
    nbarK_var1 <- (n_var1 - K_var1 + 1)/(K_var1)
    nbarJ_var1 <- (n_var1 - J_var1 + 1)/(J_var1)
    adj_var1   <- n_var1/((K_var1 - J_var1) * nbarK_var1) 
    
    logreturns_K1 = logreturns_J1 = c()
    for (k in 1:K_var1) {
      sel.avg <- seq(k, n_var1, K_var1)
      logreturns_K1 <- c(logreturns_K1, diff(logprices1[sel.avg]))
    }
    for (j in 1:J_var1) {
      sel.avg <- seq(j, n_var1, J_var1)
      logreturns_J1 <- c(logreturns_J1, diff(logprices1[sel.avg]))
    }   
    if (is.null(noisevar1)) {
      noisevar1 <- max(0,1/(2 * nbarJ_var1) * (sum(logreturns_J1^2)/J_var1 - TSRV(pdata1,K=K_var1,J=J_var1)))
    }
  }
  if (is.null(noisevar2)) {
    logprices2 = log(as.numeric(pdata2))
    n_var2 = length(logprices2)
    nbarK_var2 = (n_var2 - K_var2 + 1)/(K_var2)
    nbarJ_var2 = (n_var2 - J_var2 + 1)/(J_var2)
    adj_var2 = n_var2/((K_var2 - J_var2) * nbarK_var2)   
    
    logreturns_K2 = logreturns_J2 = c()
    for (k in 1:K_var2) {
      sel.avg = seq(k, n_var2, K_var2)
      logreturns_K2 = c(logreturns_K2, diff(logprices2[sel.avg]))
    }
    for (j in 1:J_var2) {
      sel.avg = seq(j, n_var2, J_var2)
      logreturns_J2 = c(logreturns_J2, diff(logprices2[sel.avg]))
    }        
    noisevar2 = max(0,1/(2 * nbarJ_var2) * (sum(logreturns_J2^2)/J_var2 - TSRV(pdata2,K=K_var2,J=J_var2)))
  }    
  
  if (!is.null(startIV1)) {
    RTSRV1 = startIV1
  } else {
    RTSRV1 <- RTSRV(pdata=pdata1, noisevar = noisevar1, K = K_var1, J = J_var1, eta = eta)      
  }
  if (is.null(startIV2) == FALSE) {
    RTSRV2 <- startIV2
  }else{
    RTSRV2 <- RTSRV(pdata = pdata2, noisevar = noisevar2, K = K_var2, J = J_var2, eta = eta)      
  }
  
  # Refresh time is for the covariance calculation
  
  x <- refreshTime(list(pdata1, pdata2))
  newprice1 <- x[, 1]
  newprice2 <- x[, 2]
  logprices1 <- log(as.numeric(newprice1))
  logprices2 <- log(as.numeric(newprice2))
  seconds <- as.numeric(as.POSIXct(index(newprice1)))
  secday <- last(seconds) - first(seconds)        
  K <- K_cov
  J <- J_cov     
  
  n <- length(logprices1)
  nbarK_cov <- (n - K_cov + 1)/(K_cov)
  nbarJ_cov <- (n - J_cov + 1)/(J_cov)
  adj_cov   <- n/((K_cov - J_cov) * nbarK_cov)    
  
  logreturns_K1 = logreturns_K2 = vdelta_K = c()
  for (k in 1:K_cov) {
    sel.avg <- seq(k, n, K_cov)
    logreturns_K1 <- c(logreturns_K1, diff(logprices1[sel.avg]))
    logreturns_K2 <- c(logreturns_K2, diff(logprices2[sel.avg]))
    vdelta_K <- c(vdelta_K, diff(seconds[sel.avg]) / secday)
  }
  
  logreturns_J1 = logreturns_J2 = vdelta_J = c()      
  for (j in 1:J_cov) {
    sel.avg <- seq(j, n, J_cov)
    logreturns_J1 <- c(logreturns_J1, diff(logprices1[sel.avg]))
    logreturns_J2 <- c(logreturns_J2, diff(logprices2[sel.avg]))
    vdelta_J <- c(vdelta_J, diff(seconds[sel.avg])/secday)
  }
  
  I_K1 <- 1 * (logreturns_K1^2 <= eta * (RTSRV1 * vdelta_K + 2 * noisevar1))
  I_K2 <- 1 * (logreturns_K2^2 <= eta * (RTSRV2 * vdelta_K + 2 * noisevar2))
  I_J1 <- 1 * (logreturns_J1^2 <= eta * (RTSRV1 * vdelta_J + 2 * noisevar1))
  I_J2 <- 1 * (logreturns_J2^2 <= eta * (RTSRV2 * vdelta_J + 2 * noisevar2))
  if (eta == 9) {
    ccc <- 1.0415
  } else {
    ccc <- cfactor_RTSCV(eta = eta)
  }
  RTSCV <- adj_cov * 
    (ccc * (1/K_cov) * 
       sum(logreturns_K1 * I_K1 * 
             logreturns_K2 * I_K2)/mean(I_K1 * I_K2) - 
       ((nbarK_cov/nbarJ_cov) * 
          ccc * (1/J_cov) * sum(logreturns_J1 * logreturns_J2 * I_J1 * 
                                  I_J2)/mean(I_J1 * I_J2)))
  return(RTSCV)
}

#' @keywords internal
RTSRV <- function(pdata, startIV = NULL, noisevar = NULL, K = 300, J = 1, eta = 9) {
  logprices <- log(as.numeric(pdata))
  n <- length(logprices)
  nbarK <- (n - K + 1)/(K)
  nbarJ <- (n - J + 1)/(J)
  adj <- (1 - (nbarK/nbarJ))^-1
  zeta <- 1/pchisq(eta, 3)
  seconds <- as.numeric(as.POSIXct(index(pdata)))
  secday <- last(seconds) - first(seconds)
  logreturns_K = vdelta_K = logreturns_J = vdelta_J = c()
  for (k in 1:K) {
    sel <- seq(k, n, K)
    logreturns_K <- c(logreturns_K, diff(logprices[sel]))
    vdelta_K <- c(vdelta_K, diff(seconds[sel])/secday)
  }
  for (j in 1:J) {
    sel <- seq(j, n, J)
    logreturns_J <- c(logreturns_J, diff(logprices[sel]))
    vdelta_J <- c(vdelta_J, diff(seconds[sel])/secday)
  }
  if (is.null(noisevar)) {
    noisevar <- max(0,1/(2 * nbarJ) * (sum(logreturns_J^2)/J - TSRV(pdata=pdata,K=K,J=J)))        
  }
  if (!is.null(startIV)) {
    RTSRV <- startIV
  }
  if (is.null(startIV)) {
    sel <- seq(1, n, K)
    RTSRV <- medRV(diff(logprices[sel]))
  }
  iter <- 1
  while (iter <= 20) {
    I_K <- 1 * (logreturns_K^2 <= eta * (RTSRV * vdelta_K + 
                                          2 * noisevar))
    I_J <- 1 * (logreturns_J^2 <= eta * (RTSRV * vdelta_J + 
                                          2 * noisevar))
    if (sum(I_J) == 0) {
      I_J <- rep(1, length(logreturns_J))
    }
    if (sum(I_K) == 0) {
      I_K <- rep(1, length(logreturns_K))
    }
    RTSRV <- adj * (zeta * (1/K) * sum(logreturns_K^2 * I_K)/mean(I_K) - 
                     ((nbarK/nbarJ) * zeta * (1/J) * sum(logreturns_J^2 * 
                                                           I_J)/mean(I_J)))
    iter <- iter + 1
  }
  return(RTSRV)
}

#' @keywords internal
rvKernel <- function(x,                             # Tick Data
                     kernel.type = "rectangular",   # Kernel name (or number)
                     kernel.param = 1,              # Kernel parameter (usually lags)
                     kernel.dofadj = TRUE,          # Kernel Degree of freedom adjustment
                     align.by = "seconds",          # Align the tick data to [seconds|minutes|hours]
                     align.period = 1) {            # Align the tick data to this many [seconds|minutes|hours]            
  # Multiday adjustment: 
  multixts <- multixts(x)
  if (multixts == TRUE) {
    result <- apply.daily(x, rv.kernel,kernel.type,kernel.param,kernel.dofadj,
                          align.by, align.period, cts, makeReturns)
    return(result)
  } else { #Daily estimation:
    align.period <- .getAlignPeriod(align.period, align.by)         
    cdata <- .convertData(x, cts = cts, makeReturns = makeReturns)
    x <- cdata$data
    x <- .alignReturns(x, align.period)
    type <- kernelCharToInt(kernel.type)
    .C("kernelEstimator", as.double(x), as.double(x), as.integer(length(x)),
       as.integer(kernel.param), as.integer(ifelse(kernel.dofadj, 1, 0)),
       as.integer(type), ab = double(kernel.param + 1),
       ab2 = double(kernel.param + 1),
       ans = double(1), PACKAGE = "highfrequency")$ans
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

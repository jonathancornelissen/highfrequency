#### Standard error and confidence band of RV measures.####
# User can choose integrated variance (IV) estimators RV, BV, minRV or medRV; 
#and integrated quarticity (IQ) estimators: rQuar, TP, QP, minRQ or medRQ.
# Output: 1)value of IVestimator; 
#2) standard error of IVestimator;
#3) confidence band of IVestimator. 

## hatIV
#' @keywords internal
hatIV <- function(rdata, IVestimator, startV = NULL) {
  switch(IVestimator,
         RV = RV(rdata),
         BV = RBPVar(rdata),
         TV = rTPVar(rdata),
         minRV = minRV(rdata),
         medRV = medRV(rdata),
         ROWvar = rOWCov(rdata),
         CTBV = .ctBV(rdata, startV = startV))
}

### ivInference help functions:
##IQ estimator:
#' @keywords internal
hatIQ <- function (rdata, IQestimator) {
  switch(IQestimator,
         rQuar = rQuar(rdata),
         QP = rQPVar(rdata),
         TP = rTPVar(rdata),
         minRQ = minRQ(rdata),
         medRQ = medRQ(rdata),
         CTTPV = .ctTPV(rdata))
}


##Standard error of IVestimator:
# Reference can be found at: Andersen, T. G., D. Dobrev, and E. Schaumburg (2012).
#Jump-robust volatility estimation using nearest neighbor truncation. Journal of Econometrics, 169(1), 75- 93.

#' @keywords internal
IV <- function(IVestimator, iq) {
  switch(IVestimator,
         RV = sqrt(2*iq),
         BV = sqrt(2.61*iq),
         TV = sqrt(3.06*iq),
         minRV = sqrt(3.81*iq),
         medRV = sqrt(2.96*iq))
}

ivInference <- function(rdata, IVestimator = "RV", IQestimator = "rQuar", confidence = 0.95, align.by = NULL, align.period = NULL, makeReturns = FALSE, ...) {
  
  if (checkMultiDays(rdata) == TRUE) { 
    result <- apply.daily(rdata, ivInference, align.by, align.period, makeReturns)
    return(result)
  } else {
    if((!is.null(align.by)) && (!is.null(align.period))){
      rdata <- aggregatets(rdata, on = align.by, k = align.period)
    }
    
    if (makeReturns == TRUE) { 
      rdata <- makeReturns(rdata)  
    }
    
    N <- length(rdata)
    p <- as.numeric(confidence)
    
    iq <- hatIQ(rdata,IQestimator)
    iv <- IV(IVestimator, iq)
    
    hatIV  <- hatIV(rdata, IVestimator, N)
    stderr <- 1 / sqrt(N) * iv
    
    ##confidence band
    lowband  <- as.numeric(hatIV - stderr * qnorm(p))
    highband <- as.numeric(hatIV + stderr * qnorm(p))
    cb <- c(lowband, highband)
    
    ## reports: 
    out       <- {}
    out$hativ <- hatIV
    out$se    <- stderr
    out$cb    <- cb
    
    return(out)
  }
}

#' @keywords internal
tt <- function(IVestimator, ...) {
  switch(IVestimator,
         BV = pi^2/4+pi-3,
         minRV = 3.81,
         medRV = 2.96,
         CTBV = pi^2/4+pi-3,
         ROWVar = .thetaROWVar(...))
}
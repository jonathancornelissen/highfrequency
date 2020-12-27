# User can choose integrated variance (IV) estimators RV, BV, rMinRV or rMedRV; 
#and integrated quarticity (IQ) estimators: rQuar, TP, QP, rMinRQ or rMedRQ.
# Output: 1)value of IVestimator; 
#2) standard error of IVestimator;
#3) confidence band of IVestimator. 

## hatIV
#' @keywords internal
hatIV <- function(rData, IVestimator, startV = NULL) {
  switch(IVestimator,
         RV = RV(rData),
         BV = RBPVar(rData),
         rMinRV = rMinRV(rData),
         rMedRV = rMedRV(rData),
         ROWvar = rOWCov(rData),
         CTBV = ctBV(rData, startV = startV))
}

### IVinference help functions:
##IQ estimator:
#' @keywords internal
hatIQ <- function (rData, IQestimator) {
  switch(IQestimator,
         rQuar = rQuar(rData),
         QP = rQPVar(rData),
         TP = rTPQuar(rData),
         rMinRQ = rMinRQ(rData),
         rMedRQ = rMedRQ(rData),
         CTTPV = ctTPV(rData))
}


##Standard error of IVestimator:
# Reference can be found at: Andersen, T. G., D. Dobrev, and E. Schaumburg (2012).
#Jump-robust volatility estimation using nearest neighbor truncation. Journal of Econometrics, 169(1), 75- 93.
#' @keywords internal
IV <- function(IVestimator, iq) {
  
  switch(IVestimator, 
         RV = sqrt(2 * iq), 
         BV = sqrt(2.61 * iq),
         TV = sqrt(3.06 * iq), 
         rMinRV = sqrt(3.81 * iq), 
         rMedRV = sqrt(2.96 * iq))
}

#' Function returns the value, the standard error and the confidence band of the integrated variance (IV) estimator. 
#' 
#' @description This function supplies information about standard error and confidence band of integrated variance (IV) estimators under Brownian semimartingales model such as: bipower variation, rMinRV, rMedRV. 
#' Depending on users' choices of estimator (integrated variance (IVestimator), integrated quarticity (IQestimator)) and confidence level, the function returns the result.(Barndorff (2002))
#' Function returns three outcomes: 1.value of IV estimator 2.standard error of IV estimator and 3.confidence band of IV estimator. 
#' 
#' Assume there is \eqn{N} equispaced returns in period \eqn{t}.
#' 
#' Then the IVinference is given by: 
#' \deqn{
#' \mbox{standard error}= \frac{1}{\sqrt{N}} *sd
#' }
#' \deqn{
#' \mbox{confidence band}= \hat{IV} \pm cv*se
#' }
#' in which,
#' \deqn{
#' \mbox{sd}= \sqrt{\theta \times \hat{IQ}} 
#' }
#' 
#' \eqn{cv:} critical value. 
#' 
#' \eqn{se:} standard error.
#' 
#' \eqn{\theta:} depending on IQestimator, \eqn{\theta} can take different value (Andersen et al. (2012)). 
#' 
#' \eqn{\hat{IQ}} integrated quarticity estimator.
#' 
#' @param rData \code{xts} object containing all returns in period t for one asset.
#' @param IVestimator can be chosen among integrated variance estimators: RV, BV, rMinRV or rMedRV. RV by default.
#' @param IQestimator can be chosen among integrated quarticity estimators: rQuar, realized tri-power quarticity (TPQ), quad-power quarticity (QPQ), rMinRQ or rMedRQ. TPQ by default.
#' @param confidence confidence level set by users. 0.95 by default. 
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. Possible values are: "secs", "seconds", "mins", "minutes","hours".
#' To aggregate based on a 5 minute frequency, set \code{alignPeriod} to 5 and \code{alignBy} to \code{"minutes"}.
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. E.g. to aggregate
#' based on a 5 minute frequency, set \code{alignPeriod} to 5 and \code{alignBy} to \code{"minutes"}.
#' @param makeReturns boolean, should be \code{TRUE} when \code{rData} contains prices instead of returns. \code{FALSE} by default.
#' @param ... additional arguments.
#' 
#' @return list
#' 
#' @details The theoretical framework is the logarithmic price process \eqn{X_t} belongs to the class of Brownian semimartingales, which can be written as:
#' \deqn{
#' \mbox{X}_{t}=  \int_{0}^{t} a_udu + \int_{0}^{t}\sigma_{u}dW_{u}
#' }
#' where \eqn{a} is the drift term, \eqn{\sigma} denotes the spot vivInferenceolatility process, \eqn{W} is a standard Brownian motion (assume that there are no jumps). 
#' 
#' @references
#' Andersen, T. G., Dobrev, D., and Schaumburg, E. (2012). Jump-robust volatility estimation using nearest neighbor truncation. \emph{Journal of Econometrics}, 169, 75-93.
#' 
#' Barndorff-Nielsen, O. E. (2002). Econometric analysis of realized volatility and its use in estimating stochastic volatility models. \emph{Journal of the Royal Statistical Society: Series B (Statistical Methodology)}, 64, 253-280.
#' 
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#' 
#' @examples 
#' \dontrun{
#' library("xts") # This function only accepts xts data currently
#' ivInf <- IVinference(as.xts(sampleTData[, list(DT, PRICE)]), IVestimator= "rMinRV",
#'                      IQestimator = "rMedRQ", confidence = 0.95, makeReturns = TRUE)
#' ivInf
#' }
#'             
#' @keywords highfrequency IVinference
#' @export 
IVinference <- function(rData, IVestimator = "RV", IQestimator = "rQuar", confidence = 0.95, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE, ...) {
  
  if (checkMultiDays(rData)) { 
    
    result <- applyGetList(rData, IVinference, IVestimator = IVestimator, IQestimator = IQestimator, confidence = confidence, alignBy = alignBy, 
                          alignPeriod = alignPeriod, makeReturns = makeReturns)
    names(result) <- unique(as.Date(index(rData)))
    return(result)
  } else {
    if((!is.null(alignBy)) && (!is.null(alignPeriod))){
      rData <- fastTickAgregation(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    
    if (makeReturns) { 
      rData <- makeReturns(rData)  
    }
    
    N <- length(rData)
    p <- as.numeric(confidence)
    
    iq <- hatIQ(rData,IQestimator)
    iv <- IV(IVestimator, iq)
    
    hatIV  <- hatIV(rData, IVestimator, N)
    stderr <- 1 / sqrt(N) * iv
    
    ##confidence band
    lowband  <- as.numeric(hatIV - stderr * qnorm(p))
    highband <- as.numeric(hatIV + stderr * qnorm(p))
    cb <- c(lowband, highband)
    
    ## reports: 
    out       <- list()
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
         rMinRV = 3.81,
         rMedRV = 2.96,
         CTBV = pi^2/4+pi-3,
         ROWVar = thetaROWVar(...))
}


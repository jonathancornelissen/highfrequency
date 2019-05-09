#' Realized beta: a tool in measuring risk with respect to the market. 
#' 
#' @description Depending on users' choices of estimator (realized covariance (RCOVestimator) and realized variance (RVestimator)), the function returns the realized beta, defined as the ratio between both.
#' 
#' The realized beta is given by
#' \deqn{
#' \beta_{jm} = \frac {RCOVestimator_{jm}}{RVestimator_{m}}
#' }
#' 
#' in which
#' 
#' \eqn{RCOVestimator:} Realized covariance of asset j and market index m.
#' 
#' \eqn{RVestimator:} Realized variance of market index m. 
#' 
#' @param rdata a zoo/xts object containing all returns in period t for one asset.
#' @param rindex a zoo/xts object containing return in period t for an index.
#' @param RCOVestimator can be chosen among realized covariance estimators: rCov, rAVGCov, rBPCov, rHYCov, rKernelCov, rOWCov, rRTSCov, rThresholdCov and rTSCov. rCov by default.}
#' @param RVestimator can be chosen among realized variance estimators: RV, minRV and medRV. RV by default. In case of missing RVestimator, RCOVestimator function applying for rindex will be used.}
#' @param makeReturns boolean, should be TRUE when rdata contains prices instead of returns. FALSE by  default.
#' 
#' @return numeric
#' 
#' @details 
#' Suppose there are \eqn{N} equispaced returns on day \eqn{t} for the asset j and the index m. Denote \eqn{r_{(j)i,t}}, \eqn{r_{(m)i,t}} as the \eqn{i}th return on day \eqn{t} for asset \eqn{j} and index \eqn{m} (with \eqn{i=1, \ldots,N}).
#' 
#' By default, the RCov is used and the realized beta coefficient is computed as:  
#' \deqn{
#' \hat{\beta}_{(jm)t}= \frac{\sum_{i=1}^{N} r_{(j)i,t} r_{(m)i,t}}{\sum_{i=1}^{N} r_{(m)i,t}^2}
#' }
#'  
#' (Barndorff & Shephard (2004)).
#' 
#' Note: It is worth to note that the function does not support to calculate for data of multiple days. 
#' 
#' @references 
#' Barndorff-Nielsen, O. E., & Shephard, N. (2004). Econometric analysis of realized covariation: High frequency based covariance, regression, and correlation in 
#' #' financial economics. Econometrica, 72(3), 885-925.
#' 
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#' 
#' @examples 
#' data(sample_5minprices_jumps)
#' a <- sample_5minprices_jumps['2010-01-04',1]
#' b <- sample_5minprices_jumps['2010-01-04',2]
#' rBeta(a,b,RCOVestimator="rBPCov",RVestimator="minRV",makeReturns=TRUE)
#' 
#' @keywords highfrequency rBeta
#' @export
rBeta <- function(rdata, rindex, RCOVestimator = "rCov", RVestimator = "RV", makeReturns = FALSE) {
  if (hasArg(data)) {
    rdata <- data
  }
  
  if (RCOVestimator != "rRTSCov" & RCOVestimator != "rTSCov" &  makeReturns) {
    rdata <- makeReturns(rdata);
    rindex <- makeReturns(rindex);  
  }
  
  if(!makeReturns) {
    if (RCOVestimator == "rRTSCov" || RCOVestimator == "rTSCov"){
      if (min(rdata) < 0) {
        print("when using rRTSCov, rTSCov, introduce price data - transformation to price data done")
        rdata <- exp(cumsum(rdata))
      }
      if( min(rindex) <0 ){
        print("when using rRTSCov, rTSCov, introduce price data - transformation to price data done")
        rindex <- exp(cumsum(rindex))
      }       
    }
  }
  
  multixts <- .multixts(rdata)
  
  if (multixts) {
    print("No support for multiple days")
  }
  
  if (!multixts) {
    rcovfun <- function(rdata, rindex, RCOVestimator) {
      
      switch(RCOVestimator,
             rCov = rCov(cbind(rdata,rindex) ),
             rAVGCov = rAVGCov(list(rdata, rindex) ),
             rBPCov = rBPCov(cbind(rdata, rindex) ),
             rHYCov = rHYCov(list(rdata, rindex) ),
             rKernelCov = rKernelCov(list(rdata, rindex) ),
             rOWCov = rOWCov(cbind(rdata, rindex) ),
             rRTSCov = rRTSCov(list(rdata, rindex)),
             rThresholdCov = rThresholdCov(cbind(rdata, rindex) ),
             rTSCov = rTSCov(list(rdata, rindex)))
      
    }
    
    rcov <- rcovfun(rdata,rindex,RCOVestimator)
    
    if (RVestimator == RCOVestimator || is.null(RVestimator)) {
      rbeta <- rcov[1,2] / rcov[2,2]
    } else {
      rvfun <- function(rindex, RVestimator) {
        
        switch(RVestimator,
               rCov = rCov(rindex ) ,
               RV = RV(rindex),
               BV = RBPVar(rindex),
               minRV = minRV(rindex ),
               medRV = medRV(rindex ),
               rAVGCov = rAVGCov(rindex ) ,
               rBPCov = rBPCov(rindex ) ,
               rHYCov = rHYCov(rindex ) ,
               rKernelCov = rKernelCov(rindex ) ,
               rOWCov = rOWCov(rindex ) ,
               rRTSCov = rRTSCov(rindex) ,
               rThresholdCov = rThresholdCov(rindex ) ,
               rTSCov = rTSCov(rindex))
      }
      rv <- rvfun(rindex,RVestimator)
      rbeta <- rcov[1,2] / rv
    }
    return(rbeta)
  }
}










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

####Realized quadpower variation of highfrequency return series####
rQPVar = function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE) {
  if (hasArg(data)) {
    rdata = data
  }
  multixts = .multixts(rdata)
  if (multixts) 
  {
    result = apply.daily(rdata, rQPVar, align.by, align.period,  ##check FUN
                         makeReturns)
    return(result)
  }
  if (!multixts) 
  {
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata =.aggregatets(rdata, on = align.by, k = align.period)
    }
    if (makeReturns) 
    {
      rdata = makeReturns(rdata)
    }
    
    q      = as.numeric(rdata)
    q      = abs(rollapply(q,width=4,FUN=prod,align="left"))
    N      = length(q)+3
    rQPVar = N/(N-3)*pi^2/4*sum(q)
    return(rQPVar)
  }
}

####Realized tripower variation of highfrequency return series.####

rTPVar <- function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE) {
  if (hasArg(data)) 
  {
    rdata = data
  }
  multixts = .multixts(rdata)
  if (multixts) 
  {
    result = apply.daily(rdata, rTPVar, align.by, align.period,
                         makeReturns)
    return(result)
  }
  if (!multixts) 
  {
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata = .aggregatets(rdata, on = align.by, k = align.period)
    }
    if (makeReturns) 
    {
      rdata = makeReturns(rdata)
    }
    
    q      = as.numeric(rdata)
    q      = abs(rollapply(q,width = 3, FUN = prod, align = "left"))
    N      = length(q)+2
    rTPVar = N/(N-2)*gamma(1/2)^2/(4*gamma(7/6)^2)*sum(q^(4/3))
    
#    (N^2)/(N - 2) * 2^(2/3)_gamma(7/6)_gamma(1/2)^(-1) * sum(q^(4/3))
    return(rTPVar)
  }
}


####Realized Multipower Variation (MPV)####

rMPV = function(rdata, m= 2, p=2, align.by= NULL, align.period= NULL, makeReturns= FALSE,...)
{
  if (hasArg(data)) 
  {
    rdata = data
  }
  
  multixts = .multixts(rdata)
  
  if (multixts) 
  {
    result = apply.daily(rdata, rMPV, align.by, align.period, makeReturns)
    return(result)
  }
  
  if (!multixts) 
  {
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata =.aggregatets(rdata, on = align.by, k = align.period)
    }
    if (makeReturns) 
    {
      rdata = makeReturns(rdata)
    }
    
    
    if(m>p/2)
    { m= as.numeric(m) ##m> p/2
      p= as.numeric(p)
      q=as.numeric(rdata)
      q=abs(rollapply(q,width=m,FUN=prod,align="left"))
      N = length(rdata)
      
      dmp= (2^((p/m)/2)*gamma((p/m+1)/2)/gamma(1/2))^(-m)
      
      rmpv = dmp* N^(p/2)/(N-m+1)*sum(q^(p/m))
      return(rmpv)
    }
    else{warning("Please supply m>p/2 for the arguments m and p")}
    
  }
}  


####Preaveraging estimators (matrix)####
##Preaveraging
MRC <- function(pdata, pairwise = FALSE , makePsd= FALSE,...) {
  
  if (!is.list(pdata)) {
    n = 1
  } else {
    n = length(pdata)
  }
  if (n == 1) {
    multixts = .multixts(pdata); 
    if(multixts){ stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input"); }
    mrc = .crv(pdata)
  }  
  
  if (n > 1) {
    multixts = .multixts(pdata[[1]]); 
    if(multixts){ stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input"); }
    
    if(pairwise){
      cov = matrix(rep(0, n * n), ncol = n)
      diagonal = c()
      for (i in 1:n) {
        diagonal[i] = .crv(pdata[[i]])
      }
      diag(cov) = diagonal
      
      for (i in 2:n) {
        for (j in 1:(i - 1)) {
          cov[i, j] = cov[j, i] = .preav_bi(pdata[[i]], pdata[[j]])
        }
      }
      
      mrc = cov
      
      if(makePsd)
      {
        mrc=makePsd(mrc)
      }
      
    }else{
      x     = refreshTime(pdata)
      N     = nrow(x)
      theta = 0.8 ##recommendation by Hautsch and Podolskij
      kn    = floor(theta*sqrt(N))  
      
      ##psi:
      psi1 = 1
      psi2 = 1/12
      
      psi1kn = kn* sum((.gfunction((1:kn)/kn) - .gfunction(( (1:kn) - 1 )/kn ) )^2 )
      psi2kn = 1/kn*sum(.gfunction((1:kn)/kn)^2)   
      
      preavreturn = c()
      for( i in 1:ncol(x)){
        preavreturn = cbind( preavreturn , .hatreturn(x[,i],kn) )
      }       
      
      S = rCov(preavreturn)
      
      mrc = N/(N-kn+2)*1/(psi2*kn)*S
      
      if(makePsd)
      {
        mrc = makePsd(mrc)
      }
      
    }
  }
  return(mrc) 
} 


####Realized beta####




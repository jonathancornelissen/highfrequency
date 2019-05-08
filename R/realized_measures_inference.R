#### Standard error and confidence band of RV measures.####
# User can choose integrated variance (IV) estimators RV, BV, minRV or medRV; 
#and integrated quarticity (IQ) estimators: rQuar, TP, QP, minRQ or medRQ.
# Output: 1)value of IVestimator; 
#2) standard error of IVestimator;
#3) confidence band of IVestimator. 

ivInference <- function(rdata, IVestimator = "RV", IQestimator = "rQuar", confidence = 0.95, align.by = NULL, align.period = NULL, makeReturns = FALSE, ...) {
  if (hasArg(data)){ rdata = data  }
  
  multixts = .multixts(rdata)
  if (multixts) 
  {
    result = apply.daily(rdata, ivInference, align.by, align.period,
                         makeReturns)
    return(result)
  }
  else{
    if((!is.null(align.by)) && (!is.null(align.period))){
      rdata = .aggregatets(rdata, on = align.by, k = align.period)
    }
    
    if(makeReturns){  rdata=makeReturns(rdata)  }
    
    N      = length(rdata)
    p      = as.numeric(confidence)
    
    iq     = .hatiq(rdata,IQestimator)
    iv     = .IV(IVestimator,iq)
    
    hatIV  = .hativ(rdata, IVestimator, N,...)
    stderr = 1/sqrt(N)*iv
    
    ##confidence band
    lowband  = as.numeric(hatIV-stderr*qnorm(p))
    highband = as.numeric(hatIV+stderr*qnorm(p))
    cb <- c(lowband,highband)
    
    ## reports: 
    out       = {}
    out$hativ = hatIV
    out$se    = stderr
    out$cb    = cb
    
    return(out)
  }
}
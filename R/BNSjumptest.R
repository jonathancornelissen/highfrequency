

####BNSjump-test: Barndorff- Nielsen and Shephard tests for the presence of jumps 
#in the price series.
# It includes option of corrected threshold bipower variation (CTBV).

BNSjumptest = function(rdata, IVestimator= "BV", IQestimator= "TP", type= "linear", logtransform= FALSE, 
                       max= FALSE, align.by= NULL, align.period= NULL, makeReturns = FALSE, ...)
{
  if (hasArg(data)){  rdata = data  }
  
  multixts = .multixts(rdata)
  
  if (multixts)
  {
    result = apply.daily(rdata, BNSjumptest, align.by, align.period, makeReturns)
    return(result)
    
  }else{
    if((!is.null(align.by)) && (!is.null(align.period))) {
      rdata = .aggregatets(rdata, on = align.by, k = align.period)
    }
    
    if(makeReturns){  rdata = makeReturns(rdata) }
    
    N=length(rdata)
    
    ## hatQV
    hatQV = RV(rdata)
    
    ## hatIV        
    hatIV = .hativ( rdata, IVestimator, N=N, ... )
    
    ##theta
    theta = .tt(IVestimator,...)    
    
    ##hatIQ
    hatIQ = .hatiq(rdata, IQestimator)
    
    ## linear or ratio
    if(type=="linear")
    {
      ##logtransform
      if(logtransform)
      {
        hatQV = log(RV(rdata))
        hatIV = log(.hativ(rdata,IVestimator, N, ...))
      }
      
      if(!logtransform)
      {
        hatQV = RV(rdata)
        hatIV = .hativ(rdata,IVestimator, N, ...)
      }
      
      ## max argument
      if(max)
      {
        product = max(1,.hatiq(rdata,IQestimator)/.hativ(rdata,IVestimator, N, ...)^2)
      }
      
      if(!max)
      {
        product = .hatiq(rdata,IQestimator)
      }
      
      a = sqrt(N)*(hatQV-hatIV)/sqrt((theta-2)*product)
      
      out                = {}
      out$ztest          = a
      out$critical.value = qnorm(c(0.025,0.975))
      out$pvalue         = 2*pnorm(-abs(a))
      return(out)
    }   
    
    if(type=="ratio")
    {
      ## max argument
      if(max)
      {
        product = max(1,.hatiq(rdata,IQestimator)/.hativ(rdata,IVestimator, N, ...)^2)
      }
      if(!max)
      {
        product = .hatiq(rdata,IQestimator)/.hativ(rdata,IVestimator, N, ...)^2
      }
      a = sqrt(N)*(1-.hativ(rdata,IVestimator,  N, ...)/RV(rdata))/sqrt((theta-2)*product)
      out                = {}
      out$ztest          = a
      out$critical.value = qnorm(c(0.025,0.975))
      out$pvalue         = 2*pnorm(-abs(a))
      return(out)
    }       
  }
}


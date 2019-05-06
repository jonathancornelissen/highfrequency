
####minRQ: An estimator of integrated quarticity 
#from applying the minimum operator on blocks of two returns.

minRQ = function(rdata,align.by=NULL,align.period = NULL, makeReturns = FALSE,...)
{
  if (hasArg(data)) 
  {
    rdata = data
  }
  multixts = .multixts(rdata)
  if (multixts) 
  {
    result = apply.daily(rdata, minRQ, align.by, align.period, makeReturns)
    return(result)
  }
  if (!multixts) 
  {
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata = .aggregatets(rdata, on = align.by, k = align.period)
    }
    if(makeReturns)
    {
      rdata = makeReturns(rdata)
    }
    q     = as.zoo(abs(as.numeric(rdata)))
    q     = as.numeric(rollapply(q, width = 2, FUN = min, by = 1, align = "left"))
    N     = length(q)+1
    minRQ = pi*N/(3*pi-8)*(N/(N-1))*sum(q^4)
    return(minRQ)
  }
}




####Realized quarticity of highfrequency return series.####

rQuar = function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE,...)
{
  if (hasArg(data)) 
  {
    rdata = data
  }
  multixts = .multixts(rdata)
  if (multixts) 
  {
    result = apply.daily(rdata, rQuar, align.by, align.period,
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
    
    q     = as.numeric(rdata)
    N     = length(q)+1
    rQuar = N/3*sum(q^4)
    return(rQuar)
  }
}

####Realized quadpower variation of highfrequency return series####

rQPVar = function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE,...)
{
  if (hasArg(data)) 
  {
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

rTPVar = function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE,...)
{
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



#### Standard error and confidence band of RV measures.####
# User can choose integrated variance (IV) estimators RV, BV, minRV or medRV; 
#and integrated quarticity (IQ) estimators: rQuar, TP, QP, minRQ or medRQ.
# Output: 1)value of IVestimator; 
#2) standard error of IVestimator;
#3) confidence band of IVestimator. 

ivInference = function(rdata, IVestimator = "RV", IQestimator = "rQuar", confidence = 0.95, align.by = NULL, align.period = NULL, makeReturns = FALSE, ...)
{
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




####Realized semivariance####

rSV= function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE,...)
{
  if (hasArg(data)) 
  {
    rdata = data
  }
  
  multixts =  .multixts(rdata)
  
  if (multixts) 
  {
    result = apply.daily(rdata, rSV, align.by, align.period,  
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
    
    q = as.numeric(rdata)
    select.down <-rdata <0
    select.up <- rdata >0
    
    rSVd = sum(q[select.down]^2)
    rSVu = sum(q[select.up]^2)
    
    out={}
    out$rSVdownside = rSVd
    out$rSVupside = rSVu
    
    return(out)
    
  }
}


####Realized skewness####

rSkew = function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE,...)
{
  if (hasArg(data)) 
  {
    rdata = data
  }
  
  multixts = .multixts(rdata)
  
  if (multixts) 
  {
    result = apply.daily(rdata, rSkew, align.by, align.period,
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
    
    q=as.numeric(rdata)
    N= length(q)
    
    rv= RV(rdata)
    
    rSkew= sqrt(N)*sum(q^3)/rv^(3/2)
    
    return(rSkew)
    
  }
}


####Realized kurtosis####

rKurt = function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE,...)
{
  if (hasArg(data)) 
  {
    rdata = data
  }
  
  multixts = .multixts(rdata)
  
  if (multixts) 
  {
    result = apply.daily(rdata, rKurt, align.by, align.period,
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
    
    q=as.numeric(rdata)
    N= length(q)
    
    rv= RV(rdata)
    
    rkurt= N*sum(q^4)/rv^(2)
    
    return(rkurt)
    
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
MRC= function(pdata, pairwise = FALSE , makePsd= FALSE,...)
{
  
  if (!is.list(pdata)) {
    n = 1
  }else {
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

rBeta = function(rdata, rindex, RCOVestimator= "rCov", RVestimator= "RV", makeReturns= FALSE,...)
{
  if (hasArg(data)) 
  {
    rdata = data
  }
  
  if(RCOVestimator!="rRTSCov" & RCOVestimator!="rTSCov" &  makeReturns  ){
    rdata = makeReturns(rdata);
    rindex= makeReturns(rindex);  
  }
  
  if(!makeReturns)
  {
    if(RCOVestimator=="rRTSCov" || RCOVestimator=="rTSCov"){
      if( min(rdata) <0 ){
        print("when using rRTSCov, rTSCov, introduce price data - transformation to price data done")
        rdata = exp(cumsum(rdata))
      }
      if( min(rindex) <0 ){
        print("when using rRTSCov, rTSCov, introduce price data - transformation to price data done")
        rindex = exp(cumsum(rindex))
      }       
    }
  }
  
  multixts = .multixts(rdata)
  
  if (multixts) 
  {
    print("No support for multiple days")
  }
  if (!multixts) 
  {
    
    rcovfun= function(rdata, rindex, RCOVestimator)
    {
      switch(RCOVestimator,
             rCov= rCov(cbind(rdata,rindex) ),
             rAVGCov= rAVGCov(list(rdata, rindex) ),
             rBPCov= rBPCov(cbind(rdata, rindex) ),
             rHYCov= rHYCov(list(rdata, rindex) ),
             rKernelCov= rKernelCov(list(rdata, rindex) ),
             rOWCov= rOWCov(cbind(rdata, rindex) ),
             rRTSCov= rRTSCov(list(rdata, rindex)),
             rThresholdCov= rThresholdCov(cbind(rdata, rindex) ),
             rTSCov= rTSCov(list(rdata, rindex))
      )
      
    }
    rcov= rcovfun(rdata,rindex,RCOVestimator);
    
    if(RVestimator == RCOVestimator || is.null(RVestimator))
    {
      rbeta = rcov[1,2]/rcov[2,2];
    }else{
      rvfun= function(rindex, RVestimator)
      {
        
        switch(RVestimator,
               RV= RV(rindex),
               BV= RBPVar(rindex),
               minRV= minRV(rindex ),
               medRV= medRV(rindex ),
               rCov= rCov(rindex ) ,
               rAVGCov= rAVGCov(rindex ) ,
               rBPCov= rBPCov(rindex ) ,
               rHYCov= rHYCov(rindex ) ,
               rKernelCov= rKernelCov(rindex ) ,
               rOWCov= rOWCov(rindex ) ,
               rRTSCov= rRTSCov(rindex) ,
               rThresholdCov= rThresholdCov(rindex ) ,
               rTSCov= rTSCov(rindex)
        )             
        
      }
      rv=rvfun(rindex,RVestimator)
      
      rbeta = rcov[1,2]/rv
    }
    
    return(rbeta)
  }
}


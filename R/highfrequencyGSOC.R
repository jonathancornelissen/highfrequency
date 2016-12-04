
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


####medRQ: An estimator of integrated quarticity 
#from applying the median operator on blocks of three returns. 

medRQ = function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE,...)
{
  if (hasArg(data)) 
  {
    rdata = data
  }
  multixts = .multixts(rdata)
  if (multixts) 
  {
    result = apply.daily(rdata, medRQ, align.by, align.period, makeReturns) 
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
    q = abs(as.numeric(rdata))
    q = as.numeric(rollmedian(q, k = 3,align="center"))
    N = length(q)+2
    medRQ = 3*pi*N/(9*pi+72-53*sqrt(3))*(N/(N-2))*sum(q^4)
    return(medRQ)
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


####JOjumptest:Jiang and Oomen tests for the presence of jumps in the price series.####

JOjumptest= function(pdata, power=4,...)
{
  R  = as.zoo(.simre(pdata));
  r  = as.zoo(makeReturns(pdata));
  N  = length(pdata)-1
  bv = RBPVar(r)
  rv = RV(r)
  
  SwV = 2*sum(R-r,na.rm = TRUE)
  mu1 = 2^(6/2)*gamma(1/2*(6+1))/gamma(1/2)
  
  ##mupower:
  if(power==4)
  {
    q      = abs(rollapply(r, width = 4, FUN = prod, align = "left",na.rm = TRUE))
    mu2    = 2^((6/4)/2)*gamma(1/2*(6/4+1))/gamma(1/2)
    av     = mu1/9 * N^3*(mu2)^(-4)/(N-4-1)*sum(q^(6/4),na.rm= TRUE)   ##check formula
    JOtest = N*bv/sqrt(av)*(1- rv/SwV)
    
    out                = {}
    out$ztest          = JOtest
    out$critical.value = qnorm(c(0.025,0.975))
    out$pvalue         = 2*pnorm(-abs(JOtest))
    return(out)
  }     
  
  if(power==6)
  {
    q=abs(rollapply(r, width = 6, FUN = prod, align = "left",na.rm = TRUE))
    mu2= 2^((6/6)/2)*gamma(1/2*(6/6+1))/gamma(1/2)
    av=mu1/9 * N^3*(mu2)^(-6)/(N-6-1)*sum(q^(6/6),na.rm= TRUE)   ##check formula
    JOtest= N*bv/sqrt(av)*(1- rv/SwV)
    
    out                = {}
    out$ztest          = JOtest
    out$critical.value = qnorm(c(0.025,0.975))
    out$pvalue         = 2*pnorm(-abs(JOtest))
    return(out)
  }     
}  

####AJjumptest:Ait- Sahalia and Jacod tests for the presence of jumps in the price series.####

AJjumptest = function(pdata, p=4 , k=2, align.by= NULL, align.period = NULL,alpha_multiplier=4, makeReturns= FALSE, ...)
{
  if (hasArg(data)) {  pdata = data  }
  
  multixts = .multixts(pdata)
  
  if (multixts) 
  {
    result = apply.daily(pdata, AJjumptest, align.by, align.period, makeReturns)
    return(result)
  }else{
    pdata = .aggregatets(pdata, on = "seconds", k = 1)
  }
  
  N = length(pdata)-1;
  p = as.numeric(p);
  k = as.numeric(k);
  
  alpha = alpha_multiplier * sqrt(rCov(pdata, align.by = align.by, align.period = align.period, makeReturns = makeReturns))
  w = 0.47;
  cvalue = alpha*(1/N)^w;
  
  h = align.period * .scale(align.by)
  hk= h*k
  
  seq1 = seq(1, N, h);
  seq2 = seq(1, N, hk);
  
  # return data
  pdata1 = pdata[seq1];
  pdata2 = pdata[seq2];
  
  r  = abs(makeReturns(pdata));
  r1 = abs(makeReturns(pdata1));
  r2 = abs(makeReturns(pdata2));
  
  pv1 = sum(r1^p);
  pv2 = sum(r2^p);
  
  S = pv1/pv2;
  
  ## selection return:
  selection <- abs(r) < cvalue
  rse <- abs(makeReturns(pdata[selection]))
  
  ## AJ test: 
  AJtest = (S-k^(p/2-1))/sqrt(.V(rse,p,k,N))
  
  out = {};
  out$ztest = AJtest;
  out$critical.value = qnorm(c(0.025,0.975));
  out$pvalue = 2*pnorm(-abs(AJtest));
  return(out)  
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


############### STANDARD ERROR OF HEAVY MODEL IMPLEMENTATION ##########
###########
# Likelihood for HEAVY volatility model of Shephard and Sheppard 
# Code is R-translation by Jonathan Cornelissen of matlab code of http://www.kevinsheppard.com/wiki/MFE_Toolbox
# by: kevin.sheppard@economics.ox.ac.uk

# INPUTS: 
#   PARAMETERS  - A vector with K+sum(sum(P))+sum(sum(Q)) elements. 
#    DATA       - A T by K vector of non-negative data.  Returns should be squared before using
#    P          - A K by K matrix containing the lag length of model innovations.  Position (i,j)
#                   indicates the number of lags of series j in the model for series i
#    Q          - A K by K matrix containing the lag length of conditional variances.  Position (i,j)
#                   indicates the number of lags of series j in the model for series i
#    BACKCAST   - A 1 by K matrix of values to use fo rback casting
#    LB         - A 1 by K matrix of volatility lower bounds to use in estimation
#    UB         - A 1 by K matrix of volatility upper bounds to use in estimation
# 
# OUTPUTS:
#    LL          - The log likelihood evaluated at the PARAMETERS
#    LLS         - A T by 1 vector of log-likelihoods
#    HT          - A T by K matrix of conditional variances


.get_param_names = function( estparams, p, q){
  K = dim(p)[2];
  nAlpha =  sum(p);
  nBeta  =  sum(q);
  omegas = paste("omega",1:K,sep="");
  alphas = paste("alpha",1:nAlpha,sep="");
  betas  = paste("beta", 1:nBeta,sep="");
  names  = c(omegas,alphas,betas);
  
}


############### INTERNAL HELP FUNCTIONS ############### 
### thetaROWVar help functions:
.IF_MCD = function( x, alpha )
{
  N = 1
  q = qchisq( 1-alpha , df = N )
  calpha = (1-alpha)/pchisq( q , df = N+2 )
  out = ( (x^2-q)/(1-alpha) )*( abs(x) <= sqrt(q) ) 
  
  return( -1+q*calpha + calpha*out ) 
}

# .int = function(x){ 
#   return( .IF_MCD(x)^2*dnorm(x) )
# }

.avar_MCD = function(alpha){
  N = 1
  q_alpha = qchisq( 1-alpha , df = N )
  c_alpha = (1-alpha)/pchisq( q_alpha , df = N+2 )
  a_alpha = -2*sqrt(q_alpha)*dnorm(sqrt(q_alpha))+1-alpha
  b_alpha = -2*q_alpha^(3/2)*dnorm(sqrt(q_alpha))+3*a_alpha
  
  avar = c_alpha^2*q_alpha^2+1-2*c_alpha*q_alpha
  avar = avar + c_alpha^2/(1-alpha)^2*(b_alpha+q_alpha^2*(1-alpha)-2*q_alpha*a_alpha)
  avar = avar + 2*( c_alpha*q_alpha - 1)*c_alpha*(1/(1-alpha))*(-q_alpha*(1-alpha)+a_alpha) 
  
  return(avar)
}


### ivInference help functions:
##IQ estimator: 
.hatiq = function(rdata,IQestimator)
{
  switch(IQestimator,
         rQuar  = rQuar (rdata),
         QP     = rQPVar(rdata),
         TP     = rTPVar(rdata),
         minRQ  = minRQ (rdata),
         medRQ  = medRQ (rdata))
}


##Standard error of IVestimator: 
# Reference can be found at: Andersen, T. G., D. Dobrev, and E. Schaumburg (2012). 
#Jump-robust volatility estimation using nearest neighbor truncation. Journal of Econometrics, 169(1), 75- 93.

.IV=function(IVestimator,iq)
{
  switch(IVestimator,
         RV= sqrt(2*iq),
         BV= sqrt(2.61*iq),
         TV= sqrt(3.06*iq),
         minRV= sqrt(3.81*iq),
         medRV= sqrt(2.96*iq))
}


### AJjumptest help functions:
## scale
.scale = function(align.by){
  switch(align.by,
         "seconds"= as.numeric(1),
         "minutes"= as.numeric(60),
         "hours"= as.numeric(3600))
}


## mukp: to use when p,k different from range [4,6]
.mukp=function(p,k, T=1000000)
{
  p = as.numeric(p)
  k = as.numeric(k)
  
  U = rnorm(T)
  Y = rnorm(T)
  absU = abs(U)
  mukp = mean((absU^p)*(abs(U +sqrt(k-1)*Y))^p) 
  return(mukp)
}

##fmupk: to use to calculate mupk in the function of the author. 
.fmupk = function(p,k){
  mupk = NULL;
  if(p==2){
    mupk = switch(as.character(k),
                  "2" = 4.00,
                  "3" = 5.00,
                  "4" = 6.00)
  }
  if(p==3){
    mupk = switch(as.character(k),
                  "2" = 24.07,
                  "3" = 33.63,
                  "4" = 43.74)
  }
  if(p==4){
    mupk = switch(as.character(k),
                  "2" = 204.04,
                  "3" = 320.26,
                  "4" = 455.67)
  }
  if(is.null(mupk)){
    # reduce simulation error by taking large T and large nrep
    nrep = 100;
    vmupk = rep(NA,times= nrep)
    
    for( i in 1:nrep){
      vmupk[i] = .mukp(p,k,T=1000000)  
    }
    mupk = round(mean(vmupk),2)    
  }
  return(mupk)
}

.Npk = function(p,k){
  mup= 2^(p/2)*gamma(1/2*(p+1))/gamma(1/2)
  mu2p= 2^((2*p)/2)*gamma(1/2*((2*p)+1))/gamma(1/2)      
  #npk= (1/mup^2)*(k^(p-2)*(1+k))*mu2p + k^(p-2)*(k-1)*mup^2-2*k^(p/2-1)*.fmupk(p,k)
  npk= (1/mup^2)*(k^(p-2)*(1+k)*mu2p + k^(p-2)*(k-1)*mup^2-2*k^(p/2-1)*.fmupk(p,k))
  return(npk)
}


.V = function(rse,p,k,N){
  mup = 2^(p/2)*gamma(1/2*(p+1))/gamma(1/2)
  mu2p = 2^(p)*gamma(1/2*(2*p+1))/gamma(1/2)
  Ap = (1/N)^(1-p/2)/mup*sum(rse^p)
  A2p = (1/N)^(1-p)/mu2p*sum(rse^(2*p))   
  
  V = .Npk(p,k) *A2p/(N*Ap^2) 
  return(V)
}

## BNSJumptest help functions:

## zgamma function: 
.zgamma = function(x,y){
  if(x^2<y)
  {
    out = abs(x)
  }
  else(out=1.094*sqrt(y))  ##1.094: estimated by the article
  return(out)
}

##corrected threshold bipower variation: 
.ctBV = function(rdata, startV = NULL, N){
  q = as.numeric(rdata)
  
  ## hatV
  if(is.null(startV))
  {
    hatV = medRV(rdata)
  }else{ hatV = startV }
  
  v  = 3^2*hatV
  for (i in 2:N)
  {z1 = rep(0,N-1)
   z1[i] =  .zgamma(rdata[i],v)  ##check formula TODO
  }
  
  for (j in 1:(N-1))
  {z2 = rep(0,N-1)
   z2[i] =  .zgamma(rdata[j],v) ##check formula TODO 
  }
  ctbv= (pi/2)*sum(z1*z2)
  
  return(ctbv)
}

## hatIV
.hativ = function( rdata, IVestimator, startV = NULL, N,...)
{
  switch(IVestimator,
         RV     = RV(rdata),
         BV     = RBPVar(rdata),
         TV     = rTPVar(rdata),         
         minRV  = minRV(rdata),
         medRV  = medRV(rdata),
         ROWvar = rOWCov(rdata,...),
         CTBV   = .ctBV(rdata, N,...))
}


.tt = function(IVestimator,...)
{
  switch(IVestimator,
         BV= 2.61,
         minRV= 3.81,
         medRV= 2.96,
         ROWVar = .thetaROWVar(...))
}

.thetaROWVar = function( alpha = 0.001 , alphaMCD = 0.5 )
{
  N = 1;  
  q_alpha = qchisq( 1-alpha , df = N );  
  c_alpha = (1-alpha)/pchisq( q_alpha , df = N+2 );  
  a_alpha = -2*sqrt(q_alpha)*dnorm(sqrt(q_alpha))+1-alpha;  
  b_alpha = -2*q_alpha^(3/2)*dnorm(sqrt(q_alpha))+3*a_alpha;
  
  k = qchisq(1-alpha, df= 1); #TODO GIANG ## suggestion in the article.
  halfk = sqrt(k); halfq = sqrt(q_alpha) 
  
  Ewu2   = 2*pnorm(halfk)-1;
  Ewu2u2 = -2*halfk*dnorm(halfk)+Ewu2;
  Ewu2u4 = -2*(k^(3/2))*dnorm(halfk)+3*Ewu2u2;
  
  Ewu2u2IF = (-1+c_alpha*q_alpha-(c_alpha*q_alpha)/(1-alpha))*a_alpha+c_alpha*b_alpha/(1-alpha)
  Ewu2u2IF = Ewu2u2IF + 2*(1-c_alpha*q_alpha)*(
    halfk*dnorm(halfk)-halfq*dnorm(halfq) + 1 - alpha/2 - pnorm(halfk)   )
  Ewu2IF = (alpha-1-c_alpha*q_alpha*alpha) + c_alpha*a_alpha/(1-alpha) + 2*(c_alpha*q_alpha-1)*( pnorm(halfk)-(1-alpha/2))
  Ederwu2u4 = -k^(3/2)*dnorm(halfk);
  Ederwu2u2 = -halfk*dnorm(halfk);
  c1 = 1/Ewu2u2;   c2 = 1/Ewu2;   c3 = c2*Ederwu2u2-c1*Ederwu2u4
  Avar0 = .avar_MCD(alpha)
  theta = c3^2*Avar0 + c1^2*Ewu2u4 + c2^2*Ewu2 - 2*c1*c2*Ewu2u2;
  theta = theta + 2*c3*( c1*Ewu2u2IF-c2*Ewu2IF );
  
  return( theta );
}

#Function to calculate simple returns#
.simre=function (pdata) 
{
  l = dim(pdata)[1]
  x = matrix(as.numeric(pdata), nrow = l)
  x[(2:l), ] = x[(2:l), ]/x[(1:(l - 1)), ]-1
  x[1, ] = rep(0, dim(pdata)[2])
  x = xts(x, order.by = index(pdata))
  return(x)
}


####INTERNAL FUNCTIONS OF PREAVERAGING####
##Preaverage return: 
.hatreturn= function(pdata,kn)
{
  rdata= as.zoo(makeReturns(pdata));
  kn=as.numeric(kn)
  if(kn == 1){ hatre = rdata}
  else{
    x = (1:(kn-1) )/kn
    x[x > (1-x)] = (1-x)[x > (1-x)]   
    weightedsum = function(series){ return( sum(x*series) )}
    hatre = rollapply(rdata,width = kn-1, FUN = weightedsum, align = "left")   
    hatre[is.na(hatre)] = rdata[is.na(hatre)]
  }  
  return(hatre)  
}

##gfunction:
.gfunction = function(x)
{
  # returns the minimum of x and 1-x
  # whenevr x > 1-x , replace with 1-x
  x[x > (1-x)] = (1-x)[x > (1-x)] 
  return(x)
  
}

#r#Univariate estimation:
.crv=function(pdata)
{
  N= nrow(pdata)
  theta= 0.8 ##recommendation by Hautsch and Podolskij
  kn= floor(theta*sqrt(N))  
  
  ##psi:
  psi1 = 1
  psi2 = 1/12
  
  psi1kn = kn* sum((.gfunction((1:kn)/kn) - .gfunction(( (1:kn) - 1 )/kn ) )^2 )
  
  psi2kn = 1/kn*sum(.gfunction((1:kn)/kn)^2) 
  
  r1 = .hatreturn(pdata,kn=kn)
  rdata = makeReturns(pdata)
  crv = 1/(sqrt(N)*theta*psi2kn)*sum(r1^2,na.rm=TRUE) - psi1kn*(1/N)/(2*theta^2*psi2kn)*sum(rdata^2,na.rm=TRUE)
  return(crv)
}

##preav_bi
.preav_bi= function(pdata1, pdata2)
{
  x = refreshTime(list(pdata1, pdata2))
  newprice1 = x[, 1]
  newprice2 = x[, 2]  
  
  N = nrow(x)
  theta = 0.8 ##recommendation by Hautsch and Podolskij
  kn = floor(theta*sqrt(N))  
  
  ##psi:
  psi1 = 1
  psi2 = 1/12
  
  psi1kn = kn* sum((.gfunction((1:kn)/kn) - .gfunction(( (1:kn) - 1 )/kn ) )^2 )
  
  psi2kn = 1/kn*sum(.gfunction((1:kn)/kn)^2)   
  
  r1 = .hatreturn(newprice1,kn=kn)
  r2 = .hatreturn(newprice2,kn=kn)
  
  mrc = N/(N-kn+2)*1/(psi2*kn)*(sum(r1*r2,na.rm=TRUE))
  
  return(mrc)
}

### Standard error of HEAVY model: 
## INPUT: 
# - rpara, rmpara: vectors of parameters: omega, alpha and beta of demeaned returns and realized measures from heavyModel. 
# - lR, lRM: function of log-likelihood from heavyModel;
# Note: numDeriv package is required. 
## OUTPUT: standard error of parameters calculated from log-likelihood heavyModel.


.transformparams = function( p, q, par ){
  K = dim(p)[1]; 
  pmax = max(p); qmax = max(q); # Max number of lags for innovations and cond vars
  
  O = matrix( par[1:K], ncol=1);
  A = B = list();
  start = (K+1); 
  
  for(i in 1:pmax){    # A will contain a list-item per innovation lag
    end =          start + sum(p>=i) - 1; # How many non-zero params in this loop?
    A[[i]] =       matrix(rep(0,K^2),ncol=K); 
    A[[i]][p>=i] = par[start:end];
    start  = end + 1;   
  }#end loop over number of lags for innovations
  
  for(i in 1:qmax){   # B will contain a list-item per cond var lag
    end   = start + sum(q>=i) -1; # How many non-zero params in this loop?
    B[[i]] = matrix(rep(0,K^2),ncol=K); 
    B[[i]][q >= i] = par[start:end];
    start  = end + 1;   
  }#End loop over number of lags for cond variances
  
  return( list(O,A,B) ) 
}  

.transtosplit = function(paramsvector,p,q){
  # K is the number of equations in the heavy model
  
  # paramsvector is the vector of paramaters ordered as follows:
  # First the estimates for omega then the estimates for the non-zero alpha's with the most recent lags first in case max(p) > 1, 
  # then the estimates for the non-zero beta's with the most recent lag first in case max(q) > 1.
  
  # splittedparams is the vector of parameters ordered by equation
  # first the omega, alphas and betas of the first equation
  # then the second equation 
  # and so on
  
  # determine a list with two outputs: list element one is the splittesparamsvectors 
  # and list element two is vk such that vk[i] is the number of parameters of equation i in the heavy model
  
  # clarify omega, alpha and beta: 
  K = nrow(p)[1]
  
  # intercept paramaters
  vo     = paramsvector[1:K]
  # data paramaters
  pmax = max(p); qmax = max(q); # Max number of lags for innovations and cond vars
  mA = mB = c();
  start = (K+1); 
  
  for(i in 1:pmax){    # A will contain a list-item per innovation lag
    end      = start + sum(p>=i) - 1; # How many non-zero params in this loop?
    Ai       = matrix(rep(NA,K^2),ncol=K); 
    Ai[p>=i] = paramsvector[start:end];
    mA       = cbind(mA,Ai)
    start    = end + 1;   
  }#end loop over number of lags for innovations
  
  
  # autoregressive term parameters
  for(i in 1:qmax){   # B will contain a list-item per cond var lag
    end      = start + sum(q>=i) -1; # How many non-zero params in this loop?
    Bi       = matrix(rep(NA,K^2),ncol = K); 
    Bi[q>=i] = paramsvector[start:end];
    mB       = cbind(mB,Bi)
    start    = end + 1;   
  }#End loop over number of lags for cond variances  
  
  all = cbind(vo , mA , mB)
  tall = t(all) 
  theta = tall[!is.na(tall)]
  
  vk = rep(0, K)
  for (i in 1: K)
  {
    vk[i] = 1 + sum(p[i,]) + sum(q[i,])
  }
  
  return(list(theta,vk))
}

.transtopar = function(theta, p, q){
  
  K = nrow(p);
  maxp = max(p);
  maxq = max(q);
  
  # Determine vk: 
  vk = rep(0, K);
  
  for (i in 1: K)
  {
    vk[i] = 1 + sum(p[i, ]) + sum(q[i, ])
  } 
  
  # matrix O
  vo = matrix(rep(1,K),ncol=1)
  
  # matrix A, B
  
  mA = mB = c();
  
  for(i in 1:maxp){    # A will contain a list-item per innovation lag
    Ai       = matrix(rep(0,K^2),ncol=K); 
    Ai[p>=i] = 1;
    mA       = cbind(mA,Ai)
  }#end loop over number of lags for innovations
  
  
  # autoregressive term parameters
  for(i in 1:maxq){   # B will contain a list-item per cond var lag
    Bi       = matrix(rep(0,K^2),ncol = K); 
    Bi[q>=i] = 1;
    mB       = cbind(mB,Bi)
  }#End loop over number of lags for cond variances  
  
  # Merge vO,mA, mB
  all = matrix(cbind(vo , mA , mB),nrow=K)
  
  nma= ncol(all);
  start = 1;
  
  for (i in 1:K)
  {
    for (j in 1:nma)
    {
      if(all[i,j] == 1) {all[i,j] = theta[start]} 
      else {
        all[i,j] = NA;
        next};
      start = start+1
    }
  }
  
  params = all[!is.na(all)]
  
  return(params)
  
}

.heavy_likelihood = function( par, data, p, q, backcast, LB, UB, foroptim=TRUE, compconst=FALSE ){ 
  # Get the required variables
  # p is Max number of lags for innovations 
  # q is Max number of lags for conditional variances
  
  K    = dim(data)[2];  #Number of series to model
  T    = dim(data)[1];  #Number of time periods
  lls  = rep(NA,T);     #Vector containing the likelihoods
  h    = matrix(nrow=K,ncol=T); #Matrix to containing conditional variances
  maxp = max(p); maxq=max(q);
  
  if(!(class(data) == "matrix")){data = matrix(data,ncol=K)}
  
  # Get the parameters:
  x = .transformparams( par, p=p, q=q );
  if( compconst ){ O = x[[1]]; } 
  A = x[[2]]; B = x[[3]]; 
  # Compute constant in case it needn't to be optimized:
  if( !compconst ){ # Don't compute the omega's but do (1-alpha-beta)*unconditional
    totalA = totalB = matrix( rep(0,K) ,ncol=1,nrow=K);
    for(j in 1:length(A) ){ totalA = totalA + t(t(rowSums(A[[j]]))); } # Sum over alphas for all models
    for(j in 1:length(B) ){ totalB = totalB + t(t(rowSums(B[[j]]))); } # Sum over betas for all models
    O = 1 - totalA - totalB; # The remaing weight after substracting A & B
    # Calculate the unconditionals
    uncond = t(t(colMeans(data)));
    O = O*uncond;
  } #End if close for not optimizing over omega  
  
  if( sum(O) < 0 ){ O[O<0] = 10^(-10)} #params here are shouldn't be smaller than zero
  
  likConst = K*log(2*pi); #constant for loglikelihood
  
  for(t in 1:T){ # Start loop over time periods
    h[,t] = O;    #Add constant to h
    
    for(j in 1:maxp){# Loop over innovation lags
      if( (t-j) > 0 ){ 
        h[,t] = h[,t] + t( A[[j]] %*% t(t(data[(t-j),])) ); #Adding innovations to h        
      }else{ 
        h[,t] = h[,t] + t( A[[j]] %*% backcast ); #Adding innovations to h          
      }
    } #end loop over innovation lags # CHECK: error caution????
    
    for(j in 1:maxq){# Loop over cond variances lags
      if( (t-j) > 0 ){ 
        h[,t] = h[,t] + t( B[[j]] %*% t(t(h[,(t-j)])) ); #Adding cond vars to h 
      }else{ 
        h[,t] = h[,t] + t( B[[j]] %*% backcast ); #Adding cond vars to h          
      }
    }#End loop over innovation lags
    
    if( any( h[,t]>1e3 )){ break ;}
    # Check whether h's are between LB and UB:      
    for(j in 1:K){ #Loop over 
      if( h[j,t] < LB[j] ){  h[j,t] = LB[j]/(1- (h[j,t] - LB[j]) );}
      if( h[j,t] > UB[j] ){  h[j,t] = UB[j] + log( h[j,t] - UB[j]);}
    }#end loop over series
    
    lls[t] = 0.5*( likConst + sum( log(h[,t]) ) + sum( data[t,] / h[,t] ) );            
  } #End loop over days
  ll = sum(lls);
  
  if(is.na(ll) || is.infinite(ll) ){  ll = 10^7 } 
  
  if(foroptim){   output = ll; return(output); }
  if(!foroptim){
    output = list( loglikelihood=ll, likelihoods=lls, condvar = h );
    return(output)
    # Output list:
    # (i) total loglikelihood
    # (ii) likelihood parts per time period
    # (iii) matrix with conditional variances    
  } #end output in case you want params
}



.heavy_likelihood_ll  = function( splittedparams, data, p, q, backcast, LB, UB, compconst=FALSE, ... ){ 
  K = ncol(data);
  TT = nrow(data);
  means = c(colMeans(data));
  maxp  = max(p); 
  maxq = max(q);
  
  par = .transtopar( splittedparams,  p, q );
  
  
  out = .C("heavy_likelihoodR", 
           parameters = as.double(par), 
           data = as.double(t(data)), 
           TT = as.integer(TT), 
           K = as.integer(K), 
           means = as.double(means),
           p = as.integer(p),
           q = as.integer(q),
           pMax = as.integer(maxp),
           qMax = as.integer(maxq),
           backcast = as.double(t(backcast)),
           LB = as.double(LB), 
           UB = as.double(UB), 
           compconst = as.integer(compconst),
           h = as.double(matrix(rep(0,K*TT),nrow=K,ncol=TT)),
           lls = as.double( rep(0, TT) ),
           llRM = as.double( rep(0,K ) ),
           ll = as.double(0),
           PACKAGE="highfrequency");
  
  return((-1)*out$ll)
} 

.heavy_likelihood_llC  = function( splittedparams, data, p, q, backcast, LB, UB, compconst=FALSE, ... ){ 
  K = ncol(data);
  TT = nrow(data);
  means = c(colMeans(data));
  maxp  = max(p); 
  maxq = max(q);
  
  par = .transtopar( splittedparams,  p, q );
  
  out = .C("heavy_likelihoodR", 
           parameters = as.double(par), 
           data = as.double(t(data)), 
           TT = as.integer(TT), 
           K = as.integer(K), 
           means = as.double(means),
           p = as.integer(p),
           q = as.integer(q),
           pMax = as.integer(maxp),
           qMax = as.integer(maxq),
           backcast = as.double(t(backcast)),
           LB = as.double(LB), 
           UB = as.double(UB), 
           compconst = as.integer(compconst),
           h = as.double(matrix(rep(0,K*TT),nrow=K,ncol=TT)),
           lls = as.double( rep(0, TT) ),
           llRM = as.double( rep(0,K ) ),
           ll = as.double(0),
           PACKAGE="highfrequency");
  
  return(out$ll)
} 

.heavy_likelihood_lls  = function( splittedparams , data, p, q, backcast, LB, UB, compconst=FALSE,... ){ 
  K = ncol(data);
  TT = nrow(data);
  means = c(colMeans(data));
  maxp  = max(p); 
  maxq = max(q);
  
  par = .transtopar( splittedparams,  p, q );
  
  
  out = .C("heavy_likelihoodR", 
           parameters = as.double(par), 
           data = as.double(t(data)), 
           TT = as.integer(TT), 
           K = as.integer(K), 
           means = as.double(means),
           p = as.integer(p),
           q = as.integer(q),
           pMax = as.integer(maxp),
           qMax = as.integer(maxq),
           backcast = as.double(t(backcast)),
           LB = as.double(LB), 
           UB = as.double(UB), 
           compconst = as.integer(compconst),
           h = as.double(matrix(rep(0,K*TT),nrow=K,ncol=TT)),
           lls = as.double( rep(0, TT) ),
           llRM = as.double( rep(0,K ) ),
           ll = as.double(0),
           PACKAGE="highfrequency");
  
  return((-1)*out$lls)
} 

.get_param_names = function( estparams, p, q){
  K = dim(p)[2];
  nAlpha =  sum(p);
  nBeta  =  sum(q);
  omegas = paste("omega",1:K,sep="");
  alphas = paste("alpha",1:nAlpha,sep="");
  betas  = paste("beta", 1:nBeta,sep="");
  names  = c(omegas,alphas,betas);
  
}


.SEheavyModel = function( paramsvector, data, p, q, backcast, LB, UB, compconst=FALSE, ...)
{
  require(numDeriv)
  K    = ncol(data);  #Number of series to model
  
  # Set lower and upper-bound if not specified:
  if( is.null(LB) ){ LB = rep(0,K)   }
  if( is.null(UB) ){ UB = rep(Inf,K) }
  
  if(!compconst){
    # based on p and q, map heavy paramsvector into splitted params vector called theta 
    
    out = .transtosplit ( paramsvector=paramsvector,  p=p, q=q)
    
    # vk[i] is the number of parameters of equation i in the heavy model
    vk = out[[2]];
    
    splittedparams = out[[1]];
    
    # compute the asymptotic covariance matrix of splittedparamsvector
    
    mH = numDeriv::hessian(.heavy_likelihood_ll, x= splittedparams, data=data, p=p, q=q, backcast=backcast, LB=LB, UB=UB, compconst=compconst)
    
    T        = nrow(data) 
    nm       = length(paramsvector)
    Jmatrix  = matrix (rep(0, nm^2), ncol = nm)
    end = 0
    
    
    for( k in 1:K){
      start = end + 1
      end   = start + vk[k] - 1
      Jmatrix[start:end, start:end] =  mH[start:end, start:end]
    } 
    
    ## Define It
    # jacobian will be T x length of theta 
    m  = numDeriv::jacobian(.heavy_likelihood_lls, x = splittedparams, data=data, p=p, q=q, backcast=backcast, LB=LB, UB=UB, compconst=compconst) # returns a vector?
    It = cov(m)
    
  }else{
    
    # Change value of mu: 
    paramsvector[1:K]  = colMeans(data)
    
    
    # based on p and q, map heavy paramsvector into splitted params vector called theta 
    
    out = .transtosplit ( paramsvector=paramsvector,  p=p, q=q)
    
    # vk[i] is the number of parameters of equation i in the heavy model
    vk = out[[2]];
    
    splittedparams = out[[1]];
    
    # compute the asymptotic covariance matrix of splittedparamsvector
    
    mH = numDeriv::hessian(.heavy_likelihood_ll, x= splittedparams, data=data, p=p, q=q, backcast=backcast, LB=LB, UB=UB, compconst=compconst)
    
    T        = nrow(data) 
    nm       = length(paramsvector)
    Jmatrix  = matrix (rep(0, nm^2), ncol = nm)
    end = 0
    
    
    for( k in 1:K){
      start = end + 1
      end   = start + vk[k] - 1
      Jmatrix[start:end, start:end] =  mH[start:end, start:end]
    } 
    
    # Change value of matrix Jt:
    
    # Change value of 0: 
    end = 0;
    
    for (j in 1:K )
    {
      if(j>1){col= col+ vk[j-1]} 
      else{col=1};
      
      start = end + 1;
      end = start + vk[j] - 1;
      Jmatrix[start:end,col] = 0;
      
    }
    
    # Change value of T:
    start = 1;
    
    for (i in 1:K)
    {
      
      Jmatrix[start,start] = T;
      
      start = start + vk[i]
    }
    
    ## Define It
    # jacobian will be T x length of theta 
    m  = numDeriv::jacobian(.heavy_likelihood_lls, x = splittedparams, data=data, p=p, q=q, backcast=backcast, LB=LB, UB=UB, compconst=compconst) # returns a matrix of T by L (length of theta)
    
    start = 1;
    for (i in 1:K)
    {
      m[,start] = 1/T * (data[,i] - paramsvector[i]);
      start = start + vk[i-1]
      
      if(i<K){start = start + vk[i]}
      
    }
    
    require(sandwich);
    fm = lm(m ~ 0); 
    It = try(sandwich::vcovHAC(fm))
    
    if(class(It) == "try-error")
    {
      print("HAC estimator reports an error. It is replaced by non HAC estimator.")
      It = cov(m)
    }
  }
  
  ## Jt matrix
  Jt = -1/T * Jmatrix
  ## J-1 (inverse matrix of J):
  
  invJ = try(solve(Jt))
  if( class(invJ) == "try-error"){
    require("MASS")
    print("-1*Hessian is not invertible - generalized inverse is used")
    invJ = MASS::ginv(Jt)
  }
  
  
  ## Standard error:
  
  ACOVheavyModel =  (invJ %*% It %*% t(invJ)) #return a matrix
  
  # compute the standard errors of splittedparamsvector
  
  # Standard error:
  SEheavyModel = sqrt(diag(ACOVheavyModel))/sqrt(T)   
  
  # reorder such that they correspond to paramsvector
  reSEheavyModel = .transtopar(SEheavyModel, p, q)
  
  # map it back to paramsvector
  out           = as.matrix(t(rbind(paramsvector, reSEheavyModel)))
  colnames(out) <- c("Parameter", "Standard error")
  rownames(out) = .get_param_names(estparams = paramsvector, p=p, q=q)
  return(out)
  
}

## Function: heavyModel by applying C code: 
heavyModelC = function (data, p = matrix(c(0, 0, 1, 1), ncol = 2), q = matrix(c(1, 0, 0, 1), ncol = 2), 
                        startingvalues = NULL, LB = NULL, UB = NULL, backcast = NULL, compconst = FALSE) 
{
  K = ncol(data);
  TT = nrow(data);
  means = c(colMeans(data));
  maxp  = max(p); 
  maxq = max(q);
  
  if (is.null(LB)) {
    LB = rep(0, K)
  }
  
  if (is.null(UB)) {
    UB = rep(10^6, K)
  }
  
  if (is.null(startingvalues)) {
    startingvalues = rep(NA, K + sum(p) + sum(q))
    startingvalues[1:K] = 0.1
    start = K + 1
    end = K + sum(p)
    startingvalues[start:end] = 0.3
    start = end + 1
    end = start + sum(q) - 1
    startingvalues[start:end] = 0.6
  }
  
  if (is.null(backcast)) {
    backcast = t(t(colMeans(data)))
  }
  
  KKK = length(startingvalues)
  ui = diag(rep(1, KKK))
  ci = rep(0, dim(ui)[2])
  
  splittedparams = .transtosplit(startingvalues, p, q)[[1]];
  
  x = try(optim( par = splittedparams, fn = .heavy_likelihood_llC,  data=data, p=p, q=q, backcast=backcast, LB=LB, UB=UB, compconst=compconst,                      
                 method = "L-BFGS-B")); 
  
  
  if (class(x) == "try-error") {
    print("Error in likelihood optimization")
    print(x)
  }
  else {
    if (x$convergence != 0) {
      print("Possible problem in likelihood optimization. Check convergence")
    }
  }
  
  estparams = x$par
  
  loglikelihood = x$value
  
  xx = .C("heavy_likelihoodR", 
          parameters = as.double(estparams), 
          data = as.double(t(data)), 
          TT = as.integer(TT), 
          K = as.integer(K), 
          means = as.double(means),
          p = as.integer(p),
          q = as.integer(q),
          pMax = as.integer(maxp),
          qMax = as.integer(maxq),
          backcast = as.double(t(backcast)),
          LB = as.double(LB), 
          UB = as.double(UB), 
          compconst = as.integer(compconst),
          h = as.double(matrix(rep(0,K*TT),nrow=K,ncol=TT)),
          lls = as.double( rep(0, TT) ),
          llRM = as.double( rep(0,K ) ),
          ll = as.double(0),
          PACKAGE="highfrequency");
  
  if (!is.null(rownames(data))) {
    xx$condvar = xts(t(matrix(xx$h,K)), order.by = as.POSIXct(rownames(data)))
    xx$likelihoods = xts(t(matrix(xx$lls,1)), order.by = as.POSIXct(rownames(data)))
  }
  xx$estparams = matrix(estparams, ncol = 1)
  rownames(xx$estparams) = .get_param_names(estparams, p, q)
  xx$convergence = x$convergence
  return(xx)
}

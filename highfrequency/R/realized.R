# This file contains all realized measures previously implemented in RTAQ and realized
######################################################## 
## Help functions: (not exported)
######################################################## 
.multixts <- function( x, y=NULL)
{ 
    if(is.null(y)){
        test = is.xts(x) && (ndays(x)!=1);
        return(test);}
    if(!is.null(y)){
        test = (is.xts(x) && (ndays(x)!=1)) || ( ndays(y)!=1 && is.xts(y) );
        if( test ){
            test1 = dim(y) == dim(x);
            if(!test1){ warning("Please make sure x and y have the same dimensions") }
            if(test1){  test = list( TRUE, cbind(x,y) ); return(test) }
        } 
    } 
} 

RV = function(rdata,...){
    if(hasArg(data)){ rdata = data }
    returns=as.numeric(rdata);
    RV = sum(returns*returns);
    return(RV);
}

RBPCov_bi = function(ts1,ts2){
    n = length(ts1);
    a = abs(ts1+ts2);
    b = abs(ts1-ts2);  
    first = as.numeric(a[1:(n-1)])*as.numeric(a[2:n]);
    last = as.numeric(b[1:(n-1)])*as.numeric(b[2:n]);
    result =  (pi/8)*sum(first-last);
    return(result);
}

#Realized BiPower Variation (RBPVar) (RBPVar)
RBPVar = function(rdata,...){
    if(hasArg(data)){ rdata = data }
    
    returns = as.vector(as.numeric(rdata));
    n = length(returns);
    rbpvar = (pi/2)*sum(abs(returns[1:(n-1)])*abs(returns[2:n]));
    return(rbpvar);
}

# Check data:
rdatacheck = function (rdata, multi = FALSE) 
{
    if ((dim(rdata)[2] < 2) & (multi)) {
        stop("Your rdata object should have at least 2 columns")
    }
}

######## rowcov helper functions:
#Realized Outlyingness Weighted Quadratic Covariation (ROWQCov)
conhuber = function(di,alpha=0.05)
{# consistency factor ROWQCov based on Huber weight function
    c = qchisq(p=1-alpha,df=di)
    fw2 = function(t){
        z=t^2; return(  huberweight(z,c)*( t^(di-1) )*exp(-z/2)    ) }
    fw1 = function(t){
        z=t^2; return(  huberweight(z,c)*( t^(di+1) )*exp(-z/2)   )}
    c2 = integrate(fw2,0,Inf)$value;  c1 = integrate(fw1,0,Inf)$value;
    return( di*c2/c1 )
}

conHR = function(di,alpha=0.05)
{
    # consistency factor ROWQCov based on hard rejection weight function
    return( (1-alpha)/pchisq(qchisq(1-alpha,df=di),df=di+2)  )
}

huberweight = function(d,k){
    # Huber or soft rejection weight function
    w = apply( cbind( rep(1,length(d) ) , (k/d) ),1,'min'); return(w);
}

countzeroes = function( series )
{
    return( sum( 1*(series==0) ) )
}

#Realized Outlyingness Weighted Variance (ROWVar):
univariateoutlyingness = function(rdata,...){
    require('robustbase');
    if(hasArg(data)){ rdata = data }
    #computes outlyingness of each obs compared to row location and scale
    location = 0;
    scale = mad(rdata);
    if(scale==0){
        scale = mean(rdata);
    }
    d = ((rdata - location)/scale)^2;
}


ROWVar = function(rdata, seasadjR = NULL, wfunction = "HR" , alphaMCD = 0.75, alpha = 0.001,...) 
{
    require('robustbase');
    if(hasArg(data)){ rdata = data }
    require(robustbase)
    if (is.null(seasadjR)) {
        seasadjR = rdata;
    }
    
    rdata = as.vector(rdata); seasadjR = as.vector(seasadjR);
    intraT = length(rdata); N=1;
    MCDcov = as.vector(covMcd( rdata , use.correction = FALSE )$raw.cov)
    outlyingness = seasadjR^2/MCDcov    
    k = qchisq(p = 1 - alpha, df = N)
    outlierindic = outlyingness > k
    weights = rep(1, intraT)
    if( wfunction == "HR" ){
        weights[outlierindic] = 0
        wR = sqrt(weights) * rdata
        return((conHR(di = N, alpha = alpha) * sum(wR^2))/mean(weights))
    }
    if( wfunction == "SR" ){
        weights[outlierindic] = k/outlyingness[outlierindic]
        wR = sqrt(weights) * rdata
        return((conhuber(di = N, alpha = alpha) * sum(wR^2))/mean(weights))
    }
    
}

#### Two time scale helper functions:
TSRV = function ( pdata , K=300 , J=1 ) 
{
    # based on rv.timescale
    logprices = log(as.numeric(pdata))
    n = length(logprices) ;
    nbarK = (n - K + 1)/(K) # average number of obs in 1 K-grid
    nbarJ = (n - J + 1)/(J)
    adj = (1 - (nbarK/nbarJ))^-1 
    logreturns_K = logreturns_J = c();
    for( k in 1:K){
        sel =  seq(k,n,K)  
        logreturns_K = c( logreturns_K , diff( logprices[sel] ) )
    }
    for( j in 1:J){
        sel =  seq(j,n,J)  
        logreturns_J = c( logreturns_J , diff( logprices[sel] ) )
    }
    TSRV = adj * ( (1/K)*sum(logreturns_K^2) - ((nbarK/nbarJ) *(1/J)*sum(logreturns_J^2)))
    return(TSRV)
}

RTSRV = function (pdata, startIV = NULL, noisevar = NULL, K = 300, J = 1, 
eta = 9){
    logprices = log(as.numeric(pdata))
    n = length(logprices)
    nbarK = (n - K + 1)/(K)
    nbarJ = (n - J + 1)/(J)
    adj = (1 - (nbarK/nbarJ))^-1
    zeta = 1/pchisq(eta, 3)
    seconds = as.numeric(as.POSIXct(index(pdata)))
    secday = last(seconds) - first(seconds)
    logreturns_K = vdelta_K = logreturns_J = vdelta_J = c()
    for (k in 1:K) {
        sel = seq(k, n, K)
        logreturns_K = c(logreturns_K, diff(logprices[sel]))
        vdelta_K = c(vdelta_K, diff(seconds[sel])/secday)
    }
    for (j in 1:J) {
        sel = seq(j, n, J)
        logreturns_J = c(logreturns_J, diff(logprices[sel]))
        vdelta_J = c(vdelta_J, diff(seconds[sel])/secday)
    }
    if (is.null(noisevar)) {
        noisevar = max(0,1/(2 * nbarJ) * (sum(logreturns_J^2)/J - TSRV(pdata=pdata,K=K,J=J)))        
    }
    if (!is.null(startIV)) {
        RTSRV = startIV
    }
    if (is.null(startIV)) {
        sel = seq(1, n, K)
        RTSRV = medRV(diff(logprices[sel]))
    }
    iter = 1
    while (iter <= 20) {
        I_K = 1 * (logreturns_K^2 <= eta * (RTSRV * vdelta_K + 
        2 * noisevar))
        I_J = 1 * (logreturns_J^2 <= eta * (RTSRV * vdelta_J + 
        2 * noisevar))
        if (sum(I_J) == 0) {
            I_J = rep(1, length(logreturns_J))
        }
        if (sum(I_K) == 0) {
            I_K = rep(1, length(logreturns_K))
        }
        RTSRV = adj * (zeta * (1/K) * sum(logreturns_K^2 * I_K)/mean(I_K) - 
        ((nbarK/nbarJ) * zeta * (1/J) * sum(logreturns_J^2 * 
        I_J)/mean(I_J)))
        iter = iter + 1
    }
    return(RTSRV)
}


RTSCov_bi = 
function (pdata1, pdata2, startIV1 = NULL, startIV2 = NULL, noisevar1 = NULL, 
noisevar2 = NULL, K = 300, J = 1, 
K_cov = NULL , J_cov = NULL , 
K_var1 = NULL , K_var2 = NULL , 
J_var1 = NULL , J_var2 = NULL ,       
eta = 9) 
{
    if( is.null(K_cov)){ K_cov = K }   ;   if( is.null(J_cov)){ J_cov = J } 
    if( is.null(K_var1)){ K_var1 = K } ;   if( is.null(K_var2)){ K_var2 = K }   
    if( is.null(J_var1)){ J_var1 = J } ;   if( is.null(J_var2)){ J_var2 = J }
    
    # Calculation of the noise variance and TSRV for the truncation
    
    
    
    if (   is.null(noisevar1)   ) {
        logprices1 = log(as.numeric(pdata1))     
        n_var1 = length(logprices1)
        nbarK_var1 = (n_var1 - K_var1 + 1)/(K_var1) ;
        nbarJ_var1 = (n_var1 - J_var1 + 1)/(J_var1)
        adj_var1 = n_var1/((K_var1 - J_var1) * nbarK_var1) 
        
        logreturns_K1 = logreturns_J1 = c()
        for (k in 1:K_var1) {
            sel.avg = seq(k, n_var1, K_var1)
            logreturns_K1 = c(logreturns_K1, diff(logprices1[sel.avg]))
        }
        for (j in 1:J_var1) {
            sel.avg = seq(j, n_var1, J_var1)
            logreturns_J1 = c(logreturns_J1, diff(logprices1[sel.avg]))
        }   
        if(  is.null(noisevar1)  ){
            noisevar1 = max(0,1/(2 * nbarJ_var1) * (sum(logreturns_J1^2)/J_var1 - TSRV(pdata1,K=K_var1,J=J_var1)))
        }
    }
    if (is.null(noisevar2)) {
        logprices2 = log(as.numeric(pdata2))
        n_var2 = length(logprices2)
        nbarK_var2 = (n_var2 - K_var2 + 1)/(K_var2) ;
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
    }else{
        RTSRV1 = RTSRV(pdata=pdata1, noisevar = noisevar1, K = K_var1, J = J_var1, eta = eta)      
    }
    if (!is.null(startIV2)) {
        RTSRV2 = startIV2
    }else{
        RTSRV2 = RTSRV(pdata=pdata2, noisevar = noisevar2, K = K_var2, J = J_var2, eta = eta)      
    }
    
    # Refresh time is for the covariance calculation
    
    x = refreshTime(list(pdata1, pdata2))
    newprice1 = x[, 1]
    newprice2 = x[, 2]
    logprices1 = log(as.numeric(newprice1))
    logprices2 = log(as.numeric(newprice2))
    seconds = as.numeric(as.POSIXct(index(newprice1)))
    secday = last(seconds) - first(seconds)        
    K = K_cov ; J = J_cov ;    
    
    n = length(logprices1)
    nbarK_cov = (n - K_cov + 1)/(K_cov)
    nbarJ_cov = (n - J_cov + 1)/(J_cov)
    adj_cov = n/((K_cov - J_cov) * nbarK_cov)    
    
    logreturns_K1 = logreturns_K2 = vdelta_K = c()
    for (k in 1:K_cov) {
        sel.avg = seq(k, n, K_cov)
        logreturns_K1 = c(logreturns_K1, diff(logprices1[sel.avg]))
        logreturns_K2 = c(logreturns_K2, diff(logprices2[sel.avg]))
        vdelta_K = c(vdelta_K, diff(seconds[sel.avg])/secday)
    }
    
    logreturns_J1 = logreturns_J2 = vdelta_J = c()      
    for (j in 1:J_cov) {
        sel.avg = seq(j, n, J_cov)
        logreturns_J1 = c(logreturns_J1, diff(logprices1[sel.avg]))
        logreturns_J2 = c(logreturns_J2, diff(logprices2[sel.avg]))
        vdelta_J = c(vdelta_J, diff(seconds[sel.avg])/secday)
    }
    
    
    I_K1 = 1 * (logreturns_K1^2 <= eta * (RTSRV1 * vdelta_K + 2 * noisevar1))
    I_K2 = 1 * (logreturns_K2^2 <= eta * (RTSRV2 * vdelta_K + 2 * noisevar2))
    I_J1 = 1 * (logreturns_J1^2 <= eta * (RTSRV1 * vdelta_J + 2 * noisevar1))
    I_J2 = 1 * (logreturns_J2^2 <= eta * (RTSRV2 * vdelta_J + 2 * noisevar2))
    if (eta == 9) {
        ccc = 1.0415
    } else {
        ccc = cfactor_RTSCV(eta = eta)
    }
    RTSCV = adj_cov * (ccc * (1/K_cov) * sum(logreturns_K1 * I_K1 * 
    logreturns_K2 * I_K2)/mean(I_K1 * I_K2) - ((nbarK_cov/nbarJ_cov) * 
    ccc * (1/J_cov) * sum(logreturns_J1 * logreturns_J2 * I_J1 * 
    I_J2)/mean(I_J1 * I_J2)))
    return(RTSCV)
}

TSCov_bi = function (pdata1, pdata2, K = 300, J = 1) 
{
    x = refreshTime(list(pdata1, pdata2))
    newprice1 = x[, 1]
    newprice2 = x[, 2]
    logprices1 = log(as.numeric(newprice1))
    logprices2 = log(as.numeric(newprice2))
    seconds = as.numeric(as.POSIXct(index(newprice1)))
    secday = last(seconds) - first(seconds)
    n = length(logprices1)
    nbarK = (n - K + 1)/(K)
    nbarJ = (n - J + 1)/(J)
    adj = n/((K - J) * nbarK)
    
    logreturns_K1 = logreturns_K2 = logreturns_J1 = logreturns_J2 = c()
    vdelta_K =  vdelta_J = c();
    
    for (k in 1:K) {
        sel.avg = seq(k, n, K)
        logreturns_K1 = c(logreturns_K1, diff(logprices1[sel.avg]))
        logreturns_K2 = c(logreturns_K2, diff(logprices2[sel.avg]))
        vdelta_K = c(vdelta_K, diff(seconds[sel.avg]) / secday)
    }
    
    for (j in 1:J) {
        sel.avg = seq(j, n, J)
        logreturns_J1 = c(logreturns_J1, diff(logprices1[sel.avg]))
        logreturns_J2 = c(logreturns_J2, diff(logprices2[sel.avg]))
        vdelta_J = c(vdelta_J, diff(seconds[sel.avg])/secday)
    }
    
    TSCOV = adj * ((1/K) * sum(logreturns_K1 * logreturns_K2) - 
    ((nbarK/nbarJ) * (1/J) * sum(logreturns_J1 * logreturns_J2)))
    return(TSCOV)
}

cfactor_RTSCV = function(eta=9){
    require('cubature'); require('mvtnorm')
    # rho = 1
    c1 = pchisq(eta,df=1)/pchisq(eta,df=3) 
    # 
    rho = 0.001
    R = matrix( c(1,rho,rho,1) , ncol = 2 ) 
    int1 <- function(x) {    dmvnorm(x,sigma=R) }
    num = adaptIntegrate(int1, c(-3,-3), c(3,3), tol=1e-4)$integral
    int2 <- function(x) {  x[1]*x[2]*dmvnorm(x,sigma=R) }
    denom = adaptIntegrate(int2, c(-3,-3), c(3,3), tol=1e-4)$integral
    c2 = rho*num/denom   
    return( (c1+c2)/2 )
}

# Hayashi-Yoshida helper function:
rc.hy <- function(x,y, period=1,align.by="seconds", align.period =1, cts = TRUE, makeReturns=FALSE, ...)
{
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
    
    
    sum(     .C("pcovcc", 
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

# 
# Realized variance calculation using a kernel estimator.
#
rv.kernel <- function(x,                             # Tick Data
kernel.type = "rectangular",   # Kernel name (or number)
kernel.param = 1,              # Kernel parameter (usually lags)
kernel.dofadj = TRUE,          # Kernel Degree of freedom adjustment
align.by="seconds",            # Align the tick data to [seconds|minutes|hours]
align.period = 1,              # Align the tick data to this many [seconds|minutes|hours]
cts = TRUE,                    # Calendar Time Sampling is used
makeReturns = FALSE,            # Convert to Returns 
type = NULL,                   # Deprectated
adj = NULL,                    # Deprectated
q = NULL, ...){                     # Deprectated
    # Multiday adjustment: 
    multixts = .multixts(x);
    if(multixts){
        result = apply.daily(x,rv.kernel,kernel.type,kernel.param,kernel.dofadj,
        align.by,align.period,cts,makeReturns,type,adj,q);
        return(result)}
    if(!multixts){ #Daily estimation:
        
        #
        # Handle deprication
        #
        
        
        if(!is.null(type)){
            warning("type is deprecated, use kernel.type")
            kernel.type=type
        }
        if(!is.null(q)){
            warning("q is deprecated, use kernel.param")
            kernel.param=q
        }
        if(!is.null(adj)){
            warning("adj is deprecated, use kernel.dofadj")
            kernel.dofadj=adj
        }          
        align.period = .getAlignPeriod(align.period, align.by)         
        cdata <- .convertData(x, cts=cts, makeReturns=makeReturns)
        x <- cdata$data
        x <- .alignReturns(x, align.period)
        type <- .kernel.chartoint(kernel.type)
        .C("kernelEstimator", as.double(x), as.double(x), as.integer(length(x)),
        as.integer(kernel.param), as.integer(ifelse(kernel.dofadj, 1, 0)),
        as.integer(type), ab=double(kernel.param + 1),
        ab2=double(kernel.param + 1),
        ans=double(1),PACKAGE="highfrequency")$ans
    }
}

rc.kernel <- function(x,                             # Tick Data for first asset
y,                             # Tick Data for second asset
kernel.type = "rectangular",   # Kernel name (or number)
kernel.param = 1,              # Kernel parameter (usually lags)
kernel.dofadj = TRUE,          # Kernel Degree of freedom adjustment
align.by="seconds",            # Align the tick data to [seconds|minutes|hours]
align.period = 1,              # Align the tick data to this many [seconds|minutes|hours]
cts = TRUE,                    # Calendar Time Sampling is used
makeReturns = FALSE,           # Convert to Returns 
type = NULL,                   # Deprectated
adj = NULL,                    # Deprectated
q = NULL,...){                 # Deprectated
    #
    # Handle deprication
    #
    if(!is.null(type)){
        warning("type is deprecated, use kernel.type")
        kernel.type=type
    }
    if(!is.null(q)){
        warning("q is deprecated, use kernel.param")
        kernel.param=q
    }
    if(!is.null(adj)){
        warning("adj is deprecated, use kernel.dofadj")
        kernel.dofadj=adj
    }
    
    align.period = .getAlignPeriod(align.period, align.by)   
    cdata <- .convertData(x, cts=cts, makeReturns=makeReturns)
    
    x <- cdata$data
    x <- .alignReturns(x, align.period)
    cdatay <- .convertData(y, cts=cts, makeReturns=makeReturns)
    y <- cdatay$data
    y <- .alignReturns(y, align.period)
    type <- .kernel.chartoint(kernel.type)
    .C("kernelEstimator", as.double(x), as.double(y), as.integer(length(x)),
    as.integer(kernel.param), as.integer(ifelse(kernel.dofadj, 1, 0)),
    as.integer(type), ab=double(kernel.param + 1),
    ab2=double(kernel.param + 1),
    ans=double(1),PACKAGE="highfrequency")$ans
}

rKernel <- function(x,type=0)
{
    type <- .kernel.chartoint(type)
    .C("justKernel", x=as.double(x),type= as.integer(type), ans=as.double(0),PACKAGE="highfrequency")$ans
}

.kernel.chartoint <- function(type)
{
    if(is.character(type))
    {
        ans <- switch(casefold(type), 
        rectangular=0,
        bartlett=1,
        second=2,
        epanechnikov=3,
        cubic=4,
        fifth=5,
        sixth=6,
        seventh=7,
        eighth=8,
        parzen=9,
        th=10,
        mth=11,
        tukeyhanning=10,
        modifiedtukeyhanning=11,
        -99)
        if(ans==-99)
        { 
            warning("Invalid Kernel, using Bartlet")
            1
        }
        else
        {
            ans     
        }
    }
    else
    {
        type
    }
}

rKernel.available <- function()
{
    c("Rectangular", 
    "Bartlett",
    "Second",
    "Epanechnikov",
    "Cubic",
    "Fifth",
    "Sixth",
    "Seventh",
    "Eighth",
    "Parzen",
    "TukeyHanning",
    "ModifiedTukeyHanning")
}


## REalized Variance: Average subsampled
rv.avg = function(x, period)
{ 
    mean(.rv.subsample(x, period))
}

rc.avg = function( x, y,  period )
{
    mean(.rc.subsample(x, y, period));
}

.rv.subsample <- function(x, period, cts=TRUE, makeReturns=FALSE,...)
{
    cdata <- .convertData(x, cts=cts, makeReturns=makeReturns)
    x <- cdata$data
    
    .C("subsample", 
    
    as.double(x), #a
    as.double(x), #na
    as.integer(length(x)), #na
    as.integer(length(x)/period),       #m
    as.integer(period), #period 
    as.double(rep(0,as.integer(length(x)/period +1))), #tmp
    as.double(rep(0,as.integer(length(x)/period +1))), #tmp
    as.integer(length(x)/period), #tmpn
    ans = double(period), 
    COPY=c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE), 
    PACKAGE="highfrequency")$ans
}


.rc.subsample <- function(x, y, period, cts=TRUE, makeReturns=FALSE, ... )
{
    cdata <- .convertData(x, cts=cts, makeReturns=makeReturns)
    x <- cdata$data
    
    cdatay <- .convertData(y, cts=cts, makeReturns=makeReturns)
    y <- cdatay$data
    
    .C("subsample", 
    as.double(x), #a
    as.double(y), #na
    as.integer(length(x)), #na
    as.integer(length(x)/period),       #m
    as.integer(period), #period 
    as.double(rep(0,as.integer(length(x)/period +1))), #tmp
    as.double(rep(0,as.integer(length(x)/period +1))), #tmp
    as.integer(length(x)/period), #tmpn
    ans = double(period), 
    COPY=c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE), 
    PACKAGE="highfrequency")$ans               
}

#### percentage of zeros calc:
.makeROlist = function(rdata, align.period, align.by,cts,makeReturns){
    align.period = .getAlignPeriod(align.period, align.by); 
    L = list(); 
    for(i in 1:length(rdata)){ 
        L[[i]] = .alignReturns(.convertData(rdata[[i]], cts=cts, makeReturns=makeReturns)$data, align.period);
    }
    return(L);
}

rv.zero = function(x, period)
{  
    ac <- .accum.naive(x=x,y=x,period=period)
    sum((ac*ac)==0)/length(ac)
}  

rc.zero = function(x, y, period)
{  
    acy <- .accum.naive(x=y,y=y,period=period)
    acx <- .accum.naive(x=x,y=x,period=period)
    sum((acx*acy)==0)/length(acy)
}  

#########################################################################
#
# Utility Functions from realized package Scott Payseur
#
#########################################################################
.alignedAccum <- function(x,y, period, cum=TRUE, makeReturns...)
{
    x<-.accum.naive(x,x, period)
    y<-.accum.naive(y,y, period)
    if(cum)
    {
        ans <- cumsum(x*y)
    }
    else
    {
        ans <- x*y     
    }
    ans
}


.accum.naive <- function(x,y, period, ...)
{
    .C("rv", 
    as.double(x), #a
    as.double(y), #b
    as.integer(length(x)), #na
    as.integer(period), #period 
    tmpa = as.double(rep(0,as.integer(length(x)/period +1))), #tmp
    as.double(rep(0,as.integer(length(x)/period +1))), #tmp
    as.integer(length(x)/period), #tmpn
    ans = double(1), 
    COPY=c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE), 
    PACKAGE="highfrequency")$tmpa     
}


.alignReturns <- function(x, period, ...)
{
    .C("rv", 
    as.double(x), #a
    as.double(x), #b
    as.integer(length(x)), #na
    as.integer(period), #period 
    tmpa = as.double(rep(0,as.integer(length(x)/period +1))), #tmp
    as.double(rep(0,as.integer(length(x)/period +1))), #tmp
    as.integer(length(x)/period), #tmpn
    ans = double(1), 
    COPY=c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE), 
    PACKAGE="highfrequency")$tmpa     
}

.getAlignPeriod <- function(align.period, align.by)
{   
    align.by <- gsub("(^ +)|( +$)", "",align.by) # Trim White
    
    if(casefold(align.by)=="min" || casefold(align.by)=="mins" ||casefold(align.by)=="minute"||casefold(align.by)=="minutes"||casefold(align.by)=="m"){
        ans <- align.period * 60
    }
    if(casefold(align.by)=="sec" || casefold(align.by)=="secs" ||casefold(align.by)=="second"||casefold(align.by)=="seconds"||casefold(align.by)=="s"||casefold(align.by)==""){
        ans <- align.period
    }
    if(casefold(align.by)=="hour" || casefold(align.by)=="hours" ||casefold(align.by)=="h"){
        ans <- align.period * 60 * 60
    }
    return(ans)
}


.alignIndices <- function(x, period, ...)
{
    .C("rvperiod", 
    as.double(x), #a
    as.double(x), #b
    as.integer(length(x)), #na
    as.integer(period), #period 
    tmpa = as.double(rep(max(x),as.integer(length(x)/period +1))), #tmp
    as.double(rep(0,as.integer(length(x)/period +1))), #tmp
    as.integer(length(x)/period), #tmpn
    ans = double(1), 
    COPY=c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE), 
    PACKAGE="highfrequency")$tmpa     
}

.multixts <- function( x, y=NULL)
{ 
    if(is.null(y)){
        test = is.xts(x) && (ndays(x)!=1);
        return(test);}
    if(!is.null(y)){
        test = (is.xts(x) && (ndays(x)!=1)) || ( ndays(y)!=1 && is.xts(y) );
        if( test ){
            test1 = dim(y) == dim(x);
            if(!test1){ warning("Please make sure x and y have the same dimensions") }
            if(test1){  test = list( TRUE, cbind(x,y) ); return(test) }
        }
    }
}      

.convertData <- function(x, cts = TRUE, millisstart=NA, millisend=NA, makeReturns=FALSE)
{
    if(is.null(x))
    {
        return(NULL)
    }
    if("realizedObject" %in% class(x))
    {
        return(x)
    }
    if(is.null(version$language)) #splus
    {
        if("timeSeries" %in% class(x))
        {
            x <- x[!is.na(x[,1]),1]
            if(cts)
            {
                return(ts2realized(x, millisstart=millisstart, millisend=millisend, make.returns=makeReturns)$cts)
            }
            else
            {
                return(ts2realized(x, millisstart=millisstart, millisend=millisend, make.returns=makeReturns)$tts)
            }
            #list(milliseconds = positions(x)@.Data[[2]], data = matrix(seriesData(x), ncol=1))
        }
    }
    
    if("xts" %in% class(x))
    {
        xtmp <- x
        x <- list() 
        x$data <- as.numeric(xtmp[,1])
        
        x$milliseconds <- (as.POSIXlt(time(xtmp))$hour*60*60 + as.POSIXlt(time(xtmp))$min*60 + as.POSIXlt(time(xtmp))$sec )*1000
        if(is.na(millisstart))
        {
            millisstart = x$milliseconds[[1]]
        }
        if(is.na(millisend))
        {
            millisend = x$milliseconds[[length(x$milliseconds)]]
        }
        
        cat(paste("xts -> realizedObject [", as.character(time(xtmp[1])), " :: ", as.character(time(xtmp[length(x$milliseconds)])), "]", sep=""),"\n")
    }
    
    if(is.na(millisstart))
    {
        millisstart=34200000
    }
    if(is.na(millisend))
    {
        millisend=57600000
    }    
    if("list" %in% class(x))
    {
        if(sum(names(x) == c("tts", "cts")) == 2) #realized obj  
        {
            if(cts)
            {
                return(x$cts)
            }
            else
            {
                return(x$tts)
            }
        }
        if(sum(names(x) == c("data", "milliseconds")) == 2) 
        {
            if(makeReturns)
            {                                           # only works on non cts prices
                errcheck <- try(.getReturns(.sameTime(x$data, x$milliseconds)))
                if(class(errcheck) != "Error")
                {
                    x$data <- errcheck
                    x$milliseconds <- intersect(x$milliseconds,x$milliseconds)
                }
                else
                {
                    warning("It appears that these are already returns.  Not creating returns")
                }
            }          
            else
            {
                x$data <- .sameTime(x$data, x$milliseconds)
                x$milliseconds <- intersect(x$milliseconds,x$milliseconds)
            }          
            if(cts)
            {
                toret <- list(data=.toCts(x=x$data, millis=intersect(x$milliseconds,x$milliseconds), millisstart=millisstart, millisend=millisend),
                milliseconds=(((millisstart/1000)+1):(millisend/1000))*1000)
                return(toret)
            }
            else
            {
                toret <- list(data=x$data, 
                milliseconds=intersect(x$milliseconds,x$milliseconds))
                return(toret)
            }
        }
    }
    
    
    if("timeSeries" %in% class(x))
    {
        stop("R timeSeries not implmented yet. Convert to realized object")
    }
    return(list(milliseconds = 1:dim(as.matrix(x))[[1]], data = as.matrix(x)))  # not an object, fake the milliseconds and return
}

.getReturns <- function(x)
{
    x <- as.numeric(x)
    n <- length(x)[[1]]
    return(log(x[2:n]) - log(x[1:(n-1)]))
}

.sameTime <- function(x, millis)
{
    .C("sametime", 
    as.double(x), #a
    as.integer(length(x)), #na
    as.integer(millis), #millis
    ans = double(length(union(millis,millis))), #tts
    COPY=c(FALSE,FALSE,FALSE,TRUE), 
    PACKAGE="highfrequency")$ans
}


data.toCts <- function(x, millis, millisstart=34200000, millisend=57600000)
{
    .toCts(x=x, millis=millis, millisstart=millisstart, millisend=millisend)
}

.toCts <- function(x, millis, millisstart=34200000, millisend=57600000)
{
    .C("tocts", 
    as.double(x), #a
    as.integer(length(x)),
    as.integer(millis), #millis
    as.integer(millisstart),
    as.integer(millisend),
    ans = double(((millisend-millisstart)/1000)), #cts
    COPY=c(FALSE,FALSE,FALSE,FALSE,TRUE), 
    PACKAGE="highfrequency")$ans
}

data.toReturns <- function(x)
{
    x <- as.numeric(x)   
    n <- length(x)
    log(x[2:n]) - log(x[1:(n-1)])
}

ts2realized <- function(x, make.returns=TRUE,millisstart=34200000, millisend=57600000)
{
    warning("SPLUS is no longer supported.")
    #     thedata <- data.sameTime(as.numeric(as.matrix(x@data)), .ts2millis(x))
    
    #    if(make.returns)
    #    {
    
    #          thedata <- .getReturns(thedata)
    
    #          tts <- list(data=as.numeric(thedata), milliseconds=intersect(.ts2millis(x),.ts2millis(x))[-1])
    #          cts <- list(data=.toCts(x=as.numeric(thedata), millis=intersect(.ts2millis(x),.ts2millis(x)), millisstart=millisstart, millisend=millisend),
    #               milliseconds=(((millisstart/1000)+1):(millisend/1000))*1000)
    #    }
    #    else
    #    {
    #          tts <- list(data=as.numeric(thedata), milliseconds=intersect(.ts2millis(x),.ts2millis(x)))
    #          cts <- list(data=.toCts(x=as.numeric(thedata), millis=intersect(.ts2millis(x),.ts2millis(x)), millisstart=millisstart, millisend=millisend),
    #               milliseconds=(((millisstart/1000)+1):(millisend/1000))*1000)
    
    
    #    }
    #     ans <- list(tts=tts, cts=cts)     
    #     ans
}


# Make positive definite
makePsd = function(S,method="covariance"){
    if(method=="correlation" & !any(diag(S)<=0) ){
        # Fan, J., Y. Li, and K. Yu (2010). Vast volatility matrix estimation using high frequency data for portfolio selection.
        D = matrix(diag(S)^(1/2),ncol=1)
        R = S/(D%*%t(D))
        out = eigen( x=R , symmetric = TRUE )
        mGamma = t(out$vectors)
        vLambda = out$values
        vLambda[vLambda<0] = 0
        Apsd = t(mGamma)%*%diag(vLambda)%*%mGamma
        dApsd = matrix(diag(Apsd)^(1/2),ncol=1)
        Apsd = Apsd/(dApsd%*%t(dApsd))
        D = diag( as.numeric(D)  , ncol = length(D) )
        Spos = D%*%Apsd%*%D
        return(Spos)
        #check:  eigen(Apsd)$values
    }else{
        # Rousseeuw, P. and G. Molenberghs (1993). Transformation of non positive semidefinite correlation matrices. Communications in Statistics - Theory and Methods 22, 965-984.
        out = eigen( x=S , symmetric = TRUE )
        mGamma = t(out$vectors)
        vLambda = out$values
        vLambda[vLambda<0] = 0
        Apsd = t(mGamma)%*%diag(vLambda)%*%mGamma
    }
}

### Do a daily apply but with list as output:
.applygetlist = function(x, FUN,cor=FALSE,align.by=NULL,align.period=NULL,makeReturns=FALSE,makePsd=FALSE,...){
    on="days";k=1;
    x <- try.xts(x, error = FALSE); 
    INDEX = endpoints(x,on=on,k=k); 
    D = length(INDEX)-1; 
    result = list(); 
    FUN <- match.fun(FUN);
    for(i in 1:(length(INDEX)-1)){
        result[[i]] = FUN(x[(INDEX[i] + 1):INDEX[i + 1]],cor,align.by,align.period,makeReturns,makePsd);
    }
    return(result);
}

# Aggregation function: FAST previous tick aggregation
.aggregatets = function (ts, on = "minutes", k = 1) 
{
    if (on == "secs" | on == "seconds") {
        secs = k
        tby = paste(k, "sec", sep = " ")
    } 
    if (on == "mins" | on == "minutes") {
        secs = 60 * k
        tby = paste(60 * k, "sec", sep = " ")
    } 
    if (on == "hours"){
        secs = 3600 * k;
        tby = paste(3600 * k, "sec", sep = " ");
    } 
    g = base:::seq(start(ts), end(ts), by = tby);
    rawg = as.numeric(as.POSIXct(g, tz = "GMT"));
    newg = rawg + (secs - rawg%%secs);
    g    = as.POSIXct(newg, origin = "1970-01-01",tz = "GMT");
    ts3 = na.locf(merge(ts, zoo(, g)))[as.POSIXct(g, tz = "GMT")];
    return(ts3)
} #Very fast and elegant way to do previous tick aggregation :D!

#Make Returns: 
makeReturns = function (ts) 
{
    l = dim(ts)[1]
    x = matrix(as.numeric(ts), nrow = l)
    x[(2:l), ] = log(x[(2:l), ]) - log(x[(1:(l - 1)), ])
    x[1, ] = rep(0, dim(ts)[2])
    x = xts(x, order.by = index(ts))
    return(x);
}

#Refresh Time:
refreshTime = function (pdata) 
{
    dim = length(pdata)
    lengths = rep(0, dim + 1)
    for (i in 1:dim) {
        lengths[i + 1] = length(pdata[[i]])
    }
    minl = min(lengths[(2:(dim + 1))])
    lengths = cumsum(lengths)
    alltimes = rep(0, lengths[dim + 1])
    for (i in 1:dim) {
        alltimes[(lengths[i] + 1):lengths[i + 1]] = as.numeric(as.POSIXct(index(pdata[[i]]), 
        tz = "GMT"))
    }
    x = .C("refreshpoints", as.integer(alltimes), as.integer(lengths), 
    as.integer(rep(0, minl)), as.integer(dim), as.integer(0), 
    as.integer(rep(0, minl * dim)), as.integer(minl))
    newlength = x[[5]]
    pmatrix = matrix(ncol = dim, nrow = newlength)
    for (i in 1:dim) {
        selection = x[[6]][((i - 1) * minl + 1):(i * minl)]
        pmatrix[, i] = pdata[[i]][selection[1:newlength]]
    }
    time = as.POSIXct(x[[3]][1:newlength], origin = "1970-01-01", 
    tz = "GMT")
    resmatrix = xts(pmatrix, order.by = time)
    return(resmatrix)
}

########################################################
# 1 Univariate measures : 
########################################################
# MinRV : 
minRV <- function(rdata,align.by=NULL,align.period=NULL,makeReturns=FALSE,...){
    if(hasArg(data)){ rdata = data }
    
    # Multiday adjustment: 
    multixts = .multixts(rdata); 
    if(multixts){ 
        result = apply.daily(rdata,minRV,align.by,align.period,makeReturns); 
        return(result)} 
    if(!multixts){
        if((!is.null(align.by))&&(!is.null(align.period))){
            rdata = .aggregatets(rdata, on=align.by, k=align.period);
        } 
        if(makeReturns){  rdata = makeReturns(rdata) }  
        q = as.zoo(abs(as.numeric(rdata))); #absolute value
        q = as.numeric(rollapply(q, width=2, FUN=min,by = 1, align="left"));
        N = length(q)+1; #number of obs
        minrv = (pi/(pi-2))*(N/(N-1))*sum(q^2);
        return(minrv) 
    }  
}  

# MedRV
medRV <- function(rdata,align.by=NULL,align.period=NULL,makeReturns=FALSE,...){
    if(hasArg(data)){ rdata = data }
    
    # Multiday adjustment: 
    multixts = .multixts(rdata); 
    if(multixts){ 
        result = apply.daily(rdata,medRV,align.by,align.period,makeReturns); 
        return(result)} 
    if(!multixts){
        if((!is.null(align.by))&&(!is.null(align.period))){
            rdata = .aggregatets(rdata, on=align.by, k=align.period);
        } 
        if(makeReturns){  rdata = makeReturns(rdata) }  
        q = abs(as.numeric(rdata)); #absolute value
        q = as.numeric(rollmedian(q, k=3, align="center"));
        N = length(q) + 2;
        medrv = (pi/(6-4*sqrt(3)+pi))*(N/(N-2))*sum(q^2);
        return(medrv)
    }
}


########################################################
## 2 Multivariate measures:    
########################################################
# Realized Covariation (RCov): 
rCov = function(rdata, cor=FALSE, align.by=NULL,align.period=NULL, makeReturns = FALSE, ...) 
{
    if (hasArg(data)){ 
        rdata = data; 
    } 
    # Multiday adjustment: 
    multixts = .multixts(rdata); 
    if(multixts){ 
        if(is.null(dim(rdata))){  n = 1
        }else{ n = dim(rdata)[2] }
        if( n==1 ){ result = apply.daily(rdata,rCov,align.by=align.by,align.period=align.period,makeReturns=makeReturns) }
        if( n >1 ){ result = .applygetlist(rdata,rCov,cor=cor,align.by=align.by,align.period=align.period,makeReturns=makeReturns) }    
        return(result)} 
    if(!multixts){ #single day code
        if((!is.null(align.by))&&(!is.null(align.period))){
            rdata = .aggregatets(rdata, on=align.by, k=align.period);
        } 
        if(makeReturns){  rdata = makeReturns(rdata) }  
        if (is.null(dim(rdata))) {  n = 1
        }else { n = dim(rdata)[2]}
        
        if (n == 1) {
            return(RV(rdata))
        }
        if (n > 1) {
            #        rdata = na.locf(rdata, na.rm = FALSE)
            
            rdata = as.matrix(rdata)
            covariance = t(rdata) %*% rdata
            if (cor == FALSE) {
                return(covariance)
            }
            if (cor == TRUE){
                sdmatrix = sqrt(diag(diag(covariance)));
                rcor = solve(sdmatrix) %*% covariance %*% solve(sdmatrix)
                return(rcor)
            }
        }
    }
}

# Realized Bi-power covariance:
# Realized BiPower Covariation (RBPCov)
rBPCov = function( rdata, cor=FALSE, align.by=NULL,align.period=NULL, makeReturns = FALSE, makePsd=FALSE,...) 
{
    if(hasArg(data)){ rdata = data }
    
    # Multiday adjustment: 
    multixts = .multixts(rdata); 
    if(multixts){ 
        if(is.null(dim(rdata))){  n = 1
        }else{ n = dim(rdata)[2] }
        if( n==1 ){ result = apply.daily(rdata,rBPCov,align.by=align.by,align.period=align.period,makeReturns=makeReturns,makePsd) }
        if( n >1 ){ result = .applygetlist(rdata,rBPCov,cor=cor,align.by=align.by,align.period=align.period,makeReturns=makeReturns,makePsd) }    
        return(result)} 
    if(!multixts){ #single day code
        if((!is.null(align.by))&&(!is.null(align.period))){
            rdata = .aggregatets(rdata, on=align.by, k=align.period);
        } 
        if(makeReturns){  rdata = makeReturns(rdata) }  
        if (is.null(dim(rdata))) {  n = 1
        }else { n = dim(rdata)[2]}
        
        if (n == 1) {
            return(RBPVar(rdata))
        }
        
        ## ACTUAL RBPCOV calculation:   
        if( n > 1 ){    
            #    rdatacheck(rdata,multi=TRUE);
            
            rdata  = as.matrix(rdata);
            n = dim(rdata)[2]
            cov = matrix(rep(0, n * n), ncol = n)
            diagonal = c()
            for (i in 1:n) {
                diagonal[i] = RBPVar(rdata[, i])
            }
            diag(cov) = diagonal
            for (i in 2:n) {
                for (j in 1:(i - 1)) {
                    cov[i, j] = cov[j, i] = RBPCov_bi(rdata[, i], rdata[, j])
                }
            }
            
            if(cor==FALSE){
                if(makePsd==TRUE){cov = makePsd(cov);}
                return(cov)
            }
            if(cor==TRUE){
                sdmatrix = sqrt(diag(diag(cov)));
                rcor = solve(sdmatrix)%*%cov%*%solve(sdmatrix);
                if(makePsd==TRUE){rcor = makePsd(rcor);}
                return(rcor)}
        } 
    } 
}

# rOWCov: Realized Outlyingness Covariation : 
# NOT YET IMPLEMENTED: MULTIDAY XTS SUPPORT...
rOWCov = function (rdata, cor=FALSE, align.by=NULL,align.period=NULL, makeReturns = FALSE, seasadjR = NULL, wfunction = "HR" , alphaMCD = 0.75, alpha = 0.001,...){
    if(hasArg(data)){ rdata = data }
    if(is.null(seasadjR)) { seasadjR = rdata }
    multixts = .multixts(rdata); 
    if(multixts){ stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input"); }
    
    # Aggregate:
    if((!is.null(align.by))&&(!is.null(align.period))){
        rdata = .aggregatets(rdata, on=align.by, k=align.period);
        seasadjR = .aggregatets(seasadjR, on=align.by, k=align.period);
    }     
    if( makeReturns ){ rdata = makeReturns(rdata); 
        if( !is.null(seasadjR) ){ seasadjR = makeReturns(seasadjR)} }
    
    if(is.null(dim(rdata))){ n=1 }else{ n = dim(rdata)[2]}        
    
    if( n == 1 ){ return( ROWVar( rdata , seasadjR = seasadjR , wfunction = wfunction , alphaMCD = alphaMCD , alpha = alpha ))}
    
    if( n > 1 ){ 
        rdatacheck(rdata,multi=TRUE);
        
        require(robustbase)
        rdata = as.matrix(rdata); seasadjR = as.matrix(seasadjR);
        intraT = nrow(rdata)
        N = ncol(rdata)
        perczeroes = apply(seasadjR, 2, countzeroes)/intraT
        select = c(1:N)[perczeroes < 0.5]
        seasadjRselect = seasadjR[, select]
        N = ncol(seasadjRselect)
        MCDobject = try(covMcd(x = seasadjRselect, alpha = alphaMCD))
        if (length(MCDobject$raw.mah) > 1) {
            betaMCD = 1-alphaMCD; asycor = betaMCD/pchisq( qchisq(betaMCD,df=N),df=N+2 )
            MCDcov = (asycor*t(seasadjRselect[MCDobject$best,])%*%seasadjRselect[MCDobject$best,])/length(MCDobject$best);  
            invMCDcov = solve(MCDcov) ; outlyingness = rep(0,intraT);
            for( i in 1:intraT ){ 
                outlyingness[i] = matrix(seasadjRselect[i,],ncol=N)%*%invMCDcov%*%matrix(seasadjRselect[i,],nrow=N)    }
        }
        else {
            print(c("MCD cannot be calculated")); stop();
        }
        k = qchisq(p = 1 - alpha, df = N)
        outlierindic = outlyingness > k
        weights = rep(1, intraT)
        if( wfunction == "HR" ){
            weights[outlierindic] = 0
            wR = sqrt(weights) * rdata
            covariance = (conHR(di = N, alpha = alpha) * t(wR) %*% wR)/mean(weights);
            if(cor==FALSE){return(covariance)}
            if(cor==TRUE){
                sdmatrix = sqrt(diag(diag(covariance)));
                rcor = solve(sdmatrix)%*%covariance%*%solve(sdmatrix);
                return(rcor)
            }
        }
        if( wfunction == "SR" ){
            weights[outlierindic] = k/outlyingness[outlierindic]
            wR = sqrt(weights) * rdata
            covariance = (conhuber(di = N, alpha = alpha) * t(wR) %*% wR)/mean(weights);
            if(cor==FALSE){return(covariance)}
            if(cor==TRUE){
                sdmatrix = sqrt(diag(diag(covariance)));
                rcor = solve(sdmatrix)%*%covariance%*%solve(sdmatrix);
                return(rcor)
            } 
        } 
    } 
} 

### Two time scale covariance : 
rTSCov = function (pdata, cor = FALSE, K = 300, J = 1, K_cov = NULL, J_cov = NULL, 
K_var = NULL, J_var = NULL, makePsd = FALSE) 
{
    if (!is.list(pdata)) {
        n = 1
    }
    else {
        n = length(pdata)
        if (n == 1) {
            pdata = pdata[[1]]
        }
    }
    if (n == 1) {
        multixts = .multixts(pdata); 
        if(multixts){ stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input"); }
        return(TSRV(pdata, K = K, J = J))
    }
    if (n > 1) {
        multixts = .multixts(pdata[[1]]); 
        if(multixts){ stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input"); }
        
        
        cov = matrix(rep(0, n * n), ncol = n)
        if( is.null(K_cov)){ K_cov = K }
        if( is.null(J_cov)){ J_cov = J }
        if( is.null(K_var)){ K_var = rep(K,n) }
        if( is.null(J_var)){ J_var = rep(J,n) }
        
        diagonal = c()
        for (i in 1:n) {
            diagonal[i] = TSRV(pdata[[i]], K = K_var[i], J = J_var[i])
        }
        diag(cov) = diagonal
        
        for (i in 2:n) {
            for (j in 1:(i - 1)) {
                cov[i, j] = cov[j, i] = TSCov_bi(pdata[[i]], 
                pdata[[j]], K = K_cov, J = J_cov)
            }
        }
        if (cor == FALSE) {
            if (makePsd == TRUE) {
                cov = makePsd(cov)
            }
            return(cov)
        }
        if (cor == TRUE) {
            invsdmatrix = try(solve(sqrt(diag(diag(cov)))), silent = F)
            if (!inherits(invsdmatrix, "try-error")) {
                rcor = invsdmatrix %*% cov %*% invsdmatrix
                if (makePsd == TRUE) {
                    rcor = makePsd(rcor)
                }
                return(rcor)
            }
        }
    }
}

### ROBUST Two time scale covariance : 
rRTSCov = function (pdata, cor = FALSE, startIV = NULL, noisevar = NULL, 
K = 300, J = 1, 
K_cov = NULL , J_cov = NULL,
K_var = NULL , J_var = NULL , 
eta = 9, makePsd = FALSE){
    if (!is.list(pdata)) {
        n = 1
    }
    else {
        n = length(pdata)
        if (n == 1) {
            pdata = pdata[[1]]
        }
    }
    if (n == 1) {
        multixts = .multixts(pdata); 
        if(multixts){ stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input"); }    
        return(RTSRV(pdata, startIV = startIV, noisevar = noisevar, 
        K = K, J = J, eta = eta))
    }
    if (n > 1) {
        multixts = .multixts(pdata[[1]]); 
        if(multixts){ stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input"); }
        
        cov = matrix(rep(0, n * n), ncol = n)
        diagonal = c()
        if( is.null(K_cov)){ K_cov = K }
        if( is.null(J_cov)){ J_cov = J }  
        if( is.null(K_var)){ K_var = rep(K,n) }
        if( is.null(J_var)){ J_var = rep(J,n) }        
        for (i in 1:n){ 
            diagonal[i] = RTSRV(pdata[[i]], startIV = startIV[i], 
            noisevar = noisevar[i], K = K_var[i], J = J_var[i], 
            eta = eta)
        }
        diag(cov) = diagonal
        if( is.null(K_cov)){ K_cov = K }
        if( is.null(J_cov)){ J_cov = J }                        
        for (i in 2:n) {
            for (j in 1:(i - 1)) {
                cov[i, j] = cov[j, i] = RTSCov_bi(pdata[[i]], 
                pdata[[j]], startIV1 = diagonal[i], startIV2 = diagonal[j], 
                noisevar1 = noisevar[i], noisevar2 = noisevar[j], 
                K = K_cov, J = J_cov, eta = eta)
            }
        }
        if (cor == FALSE) {
            if (makePsd == TRUE) {
                cov = makePsd(cov)
            }
            return(cov)
        }
        if (cor == TRUE) {
            invsdmatrix = try(solve(sqrt(diag(diag(cov)))), silent = F)
            if (!inherits(invsdmatrix, "try-error")) {
                rcor = invsdmatrix %*% cov %*% invsdmatrix
                if (makePsd == TRUE) {
                    rcor = makePsd(rcor)
                }
                return(rcor)
            }
        }
    }
}

## Threshold covariance: 
rThresholdCov = function( rdata, cor=FALSE, align.by=NULL, align.period=NULL, makeReturns=FALSE,...){
    if(hasArg(data)){ rdata = data } 
    rdatacheck(rdata,multi=TRUE); 
    
    # Multiday adjustment: 
    multixts = .multixts(rdata); 
    multixts = .multixts(rdata); 
    if(multixts){ 
        if(is.null(dim(rdata))){  n = 1
        }else{ n = dim(rdata)[2] }
        if( n==1 ){ result = apply.daily(rdata,rThresholdCov,cor=cor,align.by=align.by,align.period=align.period,makeReturns=makeReturns) }
        if( n >1 ){ result = .applygetlist(rdata,rThresholdCov,cor=cor,align.by=align.by,align.period=align.period,makeReturns=makeReturns) }    
        return(result)} 
    if( !multixts ){ #single day code
        if((!is.null(align.by))&&(!is.null(align.period))){
            rdata = .aggregatets(rdata, on=align.by, k=align.period);
        } 
        if(makeReturns){ rdata = makeReturns(rdata) }  
        
        rdata=as.matrix(rdata);
        n=dim(rdata)[1];						                  # number of observations
        delta = 1/n;
        rbpvars = apply(rdata,2,FUN=RBPVar);		      # bipower variation per stock
        tresholds = 3*sqrt(rbpvars)*(delta^(0.49));	  # treshold per stock
        tresmatrix = matrix(rep(tresholds,n),ncol=length(tresholds),nrow=n,byrow=TRUE); 
        condition = abs(rdata) > tresmatrix;
        rdata[condition] = 0;
        covariance = rCov(rdata);
        
        if(cor==FALSE){ return(covariance) }
        if(cor==TRUE){
            sdmatrix = sqrt(diag(diag(covariance)));
            rcor = solve(sdmatrix)%*%covariance%*%solve(sdmatrix);
            return(rcor)}
    } 
} 

## Hayashi Yoshida covariance estimator
rHYCov = function(rdata, cor = FALSE, period = 1, align.by = "seconds", align.period = 1, cts = TRUE, makeReturns = FALSE, makePsd=TRUE, ...) 
{
    if (!is.list(rdata)){
        stop('The rdata input is not a list. Please provide a list as input for this function. Each list-item should contain the series for one asset.')
    }else{
        n = length(rdata)
        if(n == 1){
            stop('Please provide a list with multiple list-items as input. You cannot compute covariance from a single price series.')      
        }
    }  
    multixts = .multixts(rdata[[1]]); 
    if(multixts){ stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")}
    
    cov = matrix(rep(0, n * n), ncol = n);
    diagonal = c();  
    for (i in 1:n){ 
        diagonal[i] = rCov( rdata[[i]], align.by = align.by, align.period = align.period,makeReturns=makeReturns ); 
    } 
    diag(cov) = diagonal;
    for (i in 2:n){
        for (j in 1:(i - 1)){
            cov[i, j] = cov[j, i] = rc.hy( x=rdata[[i]], y=rdata[[j]], period = period,align.by=align.by, 
            align.period = align.period, cts = cts, makeReturns = makeReturns);       
        }
    }
    
    if (cor == FALSE) {
        if (makePsd == TRUE) {
            cov = makePsd(cov)
        }
        return(cov)
    }
    if (cor == TRUE){
        invsdmatrix = try(solve(sqrt(diag(diag(cov)))), silent = F)
        if (!inherits(invsdmatrix, "try-error")) {
            rcor = invsdmatrix %*% cov %*% invsdmatrix
            if (makePsd == TRUE) {
                rcor = makePsd(rcor)
            }
            return(rcor)
        }
    }
}  

## Kernel Covariance Estimator: 
rKernelCov = function( rdata, cor=FALSE, kernel.type = "rectangular", kernel.param = 1, 
kernel.dofadj = TRUE, align.by = "seconds", align.period = 1, 
cts = TRUE, makeReturns = FALSE, type = NULL, adj = NULL, 
q = NULL, ...)
{
    if(!is.list(rdata)){ # In case of only one stock this makes sense
        if(is.null(dim(rdata))){  n = 1
        }else{ n = dim(rdata)[2] }
        if( n == 1 ){ result = rv.kernel(rdata, cor=cor, kernel.type = kernel.type, kernel.param = kernel.param, kernel.dofadj = kernel.dofadj, 
            align.by = align.by, align.period = align.period, cts = cts, makeReturns = makeReturns, 
            type = type, adj = adj, q = q)}
        if( n >  1 ){ stop("Please provide a list with one list-item per stock as input.")  }    
        return(result)    
        #stop('The rdata input is not a list. Please provide a list as input for this function. Each list-item should contain the series for one asset.')
    }else{
        n = length(rdata);
        if(n == 1){
            result = rv.kernel(rdata[[1]], cor=cor,kernel.type = kernel.type, kernel.param = kernel.param, kernel.dofadj = kernel.dofadj, 
            align.by = align.by, align.period = align.period, cts = cts, makeReturns = makeReturns, 
            type = type, adj = adj, q = q); return(result);
        }
        if( n>1 ){
            
            multixts = .multixts(rdata[[1]]); 
            if(multixts){ stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")}
            
            cov = matrix(rep(0, n * n), ncol = n);
            diagonal = c();  
            for( i in 1:n ){ 
                diagonal[i] = rv.kernel(rdata[[i]], cor=cor,kernel.type = kernel.type, kernel.param = kernel.param, kernel.dofadj = kernel.dofadj, 
                align.by = align.by, align.period = align.period, cts = cts, makeReturns = makeReturns, 
                type = type, adj = adj, q = q);        
            } 
            diag(cov) = diagonal;
            for (i in 2:n){
                for (j in 1:(i - 1)){
                    cov[i, j] = cov[j, i] = rc.kernel(x = rdata[[i]], y = rdata[[j]], kernel.type = kernel.type, kernel.param = kernel.param, 
                    kernel.dofadj = kernel.dofadj, align.by = align.by, align.period = align.period, 
                    cts = cts, makeReturns = makeReturns, type = type, adj = adj,q = q);   
                }
            }
            
            if(cor == FALSE){
                cov = makePsd(cov);
                return(cov)
            }
            if(cor == TRUE){
                invsdmatrix = try(solve(sqrt(diag(diag(cov)))), silent = F)
                if (!inherits(invsdmatrix, "try-error")) {
                    rcor = invsdmatrix %*% cov %*% invsdmatrix
                    return(rcor)
                }
            }
        }
    }
}

## Average subsample estimator: 
rAVGCov = function( rdata, cor = FALSE, period = 1, align.by = "seconds", align.period = 1, cts = TRUE, makeReturns = FALSE, ...){
    if (!is.list(rdata)){
        multixts = .multixts(rdata); 
        if(multixts){ stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")}
        
        if(is.null(dim(rdata))){  n = 1
        }else{ n = dim(rdata)[2] }
        if( n == 1 ){ 
            L = .makeROlist( rdata=list(rdata), align.period=align.period, align.by=align.by,cts=cts,makeReturns=makeReturns);#make objects list            
            result = rv.avg( L[[1]], period=period ); 
            return(result)  }
        if( n >  1 ){ stop('The rdata input is not a list. Please provide a list as input for this function. Each list-item should contain the series for one asset.') }
    }
    if(is.list(rdata)){
        n = length(rdata)
        multixts = .multixts(rdata[[1]]); 
        if(multixts){ stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")}
        
        if( n == 1 ){ 
            L = .makeROlist( rdata=rdata, align.period=align.period, align.by=align.by,cts=cts,makeReturns=makeReturns);#make objects list            
            result = rv.avg( L[[1]], period=period ); 
            return(result) 
        }
        if( n > 1){
            
            cov = matrix(rep(0, n * n), ncol = n);
            diagonal = c(); 
            L = .makeROlist(rdata=rdata, align.period=align.period, align.by=align.by,cts=cts,makeReturns=makeReturns);#make objects list     
            
            for(i in 1:n){ 
                diagonal[i] = rv.avg( L[[i]], period=period );
            } 
            diag(cov) = diagonal;
            for(i in 2:n){
                for (j in 1:(i - 1)){
                    cov[i, j] = cov[j, i] = rc.avg( x = L[[i]], y = L[[j]], period=period ); 
                } 
            } 
            
            if (cor == FALSE) {
                cov = makePsd(cov)
                return(cov)
            }
            if (cor == TRUE){
                invsdmatrix = try(solve(sqrt(diag(diag(cov)))), silent = F)
                if (!inherits(invsdmatrix, "try-error")) {
                    rcor = invsdmatrix %*% cov %*% invsdmatrix
                    rcor = makePsd(rcor)
                    return(rcor)
                } 
            } 
        }  #List-length > 1
    }  #If-list condition
}   #end rAVGCov

## Percentage of zero's for given time aggregation calculator:
rZero = function( rdata, period = 1, align.by = "seconds", align.period = 1, cts = TRUE, makeReturns = FALSE, ...){
    if (!is.list(rdata)){
        multixts = .multixts(rdata);  
        if(multixts){ stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")}     
        if(is.null(dim(rdata))){  n = 1
        }else{ n = dim(rdata)[2] }
        if( n == 1 ){ 
            L = .makeROlist(rdata=list(rdata), align.period=align.period, align.by=align.by,cts=cts,makeReturns=makeReturns);#make objects list     
            result = rv.zero( L[[1]], period=period ); 
            return(result)  }
        if( n >  1 ){ stop('The rdata input is not a list. Please provide a list as input for this function. Each list-item should contain the series for one asset.') }
    }
    if(is.list(rdata)){
        multixts = .multixts(rdata[[1]]);  
        if(multixts){ stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")}
        n = length(rdata)
        if( n == 1 ){ 
            L = .makeROlist(rdata=rdata, align.period=align.period, align.by=align.by,cts=cts,makeReturns=makeReturns);#make objects list     
            result = rv.zero(L[[1]], period=period ); 
            return(result) }
        if( n > 1){      
            cov = matrix(rep(0, n * n), ncol = n); 
            diagonal = c(); 
            L = .makeROlist(rdata=rdata, align.period=align.period, align.by=align.by,cts=cts,makeReturns=makeReturns);#make objects list     
            
            for(i in 1:n){ 
                diagonal[i] = rv.zero( L[[i]], period=period );
            } 
            diag(cov) = diagonal;
            for (i in 2:n){
                for (j in 1:(i - 1)){
                    cov[i, j] = cov[j, i] = rc.zero( x=L[[i]], y=L[[j]], period=period);       
                }
            }
            return(cov)
        }  #List-length > 1
    }  #If-list condition
}   #end rAVGCov

# Accumulation:
rAccumulation <- function(x, period=1, y=NULL, align.by="seconds",align.period=1, plotit=FALSE, cts=TRUE, makeReturns=FALSE)
{
  multixts = .multixts(x) || .multixts(y);
  if( multixts ){ stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")}     
    
    align.period = .getAlignPeriod(align.period, align.by)   
    ans <- list(x=NULL, y=NULL)
    ans$y <- cumsum(rMarginal(x=x, y=y, period=period, align.period=align.period, cts=cts, makeReturns=makeReturns)$y)
    #    ans$x <- .alignIndices(1:length(x), align.period)
    #    ans$x <- .alignIndices(ans$x, period)
    ans$x <- rCumSum(x=x, period=period, align.period=align.period, cts=cts, makeReturns=makeReturns)$x
    #ans$x <- ans$x[-length(ans$x)]
    if(plotit)
    {
        plot(ans, xlab="", ylab="Realized Accumulation")
        return(NULL)
    }
    ans
} 

# Marginal distribution:
rMarginal <- function(x, y=NULL, period, align.by="seconds", align.period=1, plotit=FALSE, cts=TRUE, makeReturns=FALSE)
{
  multixts = .multixts(x) || .multixts(y);
  if( multixts ){ stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")}     
  
    align.period = .getAlignPeriod(align.period, align.by)   
    ans <- list(x = NULL, y = NULL)
    ans$x <- .alignIndices(1:length(x), align.period)
    ans$x <- .alignIndices(ans$x, period)
    
    if(is.null(y))
    y <- x   
    x<- .alignReturns(.convertData(x, cts=cts, makeReturns=makeReturns)$data, align.period)
    y<- .alignReturns(.convertData(y, cts=cts, makeReturns=makeReturns)$data, align.period)
    ans$y <- .alignedAccum(x=x, y=y, period=period, cum=FALSE)
    
    if(plotit)
    {
        plot(ans, xlab="", ylab="Realized Marginals")
        return(NULL)
    }
    ans
}

# Cumulative sum of returns:
rCumSum <- function(x, period = 1, align.by="seconds", align.period=1, plotit=FALSE, type='l', cts = TRUE, makeReturns=FALSE)
{
  multixts = .multixts(x);
  if( multixts ){ stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")}     

    align.period = .getAlignPeriod(align.period, align.by)   
    ans <- list(x = NULL, y = NULL)
    ans$x <- .alignIndices(1:length(.convertData(x, cts=cts, makeReturns=makeReturns)$data), align.period)
    ans$x <- .alignIndices(ans$x, period)
    
    x<- .alignReturns(.convertData(x, cts=cts, makeReturns=makeReturns)$data, align.period)
    x<- .alignReturns(.convertData(x, cts=cts, makeReturns=makeReturns)$data, period)
    
    ans$y <- cumsum(x)
    if(plotit)
    {
        plot(cumsum(x), xlab="Time", ylab="Cummulative Returns", type=type)
        return(NULL)
    }
    ans
} 

#Scatter returns:
rScatterReturns <- function(x,y, period, align.by="seconds", align.period=1,numbers=FALSE,xlim= NULL, ylim=NULL, plotit=TRUE, pch=NULL, cts=TRUE, makeReturns=FALSE, scale.size=0, col.change=FALSE,...)
{
  multixts = .multixts(x) || .multixts(y);
  if( multixts ){ stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")}     
  
    align.period = .getAlignPeriod(align.period, align.by) 
    y<- .alignReturns(.convertData(y, cts=cts, makeReturns=makeReturns)$data, align.period)
    x<- .alignReturns(.convertData(x, cts=cts, makeReturns=makeReturns)$data, align.period)
    
    x<-.accum.naive(x, x, period)
    y<-.accum.naive(y, y, period)
    if(is.null(pch))
    pch=1
    
    it <- table(round(x,4),round(y,4))
    xs <- as.numeric(dimnames(it)[[1]])
    ys <- as.numeric(dimnames(it)[[2]])
    
    if(is.null(ylim))
    ylim=c(min(ys), max(ys))
    if(is.null(xlim))
    xlim=c(min(xs), max(xs))
    
    mat <- matrix(it, nrow=length(xs), ncol=length(ys))
    
    if(plotit)
    {
        plot(0,0, xlim=xlim, ylim=ylim , type='n',...)
        lines(c(0,0), c(-1,2), col="grey", lty=3, lwd=2)
        lines(c(-1,2), c(0,0), col="grey", lty=3, lwd=2)
        
        maxed <- max(mat)
        
        for(i in 1:length(xs))
        {
            for(j in 1:length(ys))
            {
                if(mat[i,j]!=0)
                {
                    if(col.change)
                    thecol <- round(runif(1)*100,0)
                    else
                    thecol = 1
                    
                    if(numbers)
                    {
                        
                        if(scale.size ==0)
                        text(xs[i], ys[j],as.character(mat[i,j]), cex=.7, col=thecol)         
                        else
                        text(xs[i], ys[j], as.character(mat[i,j]), cex = (mat[i,j]/maxed) * scale.size, col=thecol)
                    }
                    else
                    {
                        if(scale.size ==0)
                        points(xs[i], ys[j], pch=pch, cex=.7, col=thecol)         
                        else
                        points(xs[i], ys[j], pch=pch, cex = (mat[i,j]/maxed) * scale.size, col=thecol)
                    }
                }
                
            }
        }
        return(NULL)
        
    }     
    mat
}


######################################################################
#  START implementation of paper:
#  ROUGHING IT UP: INCLUDING JUMP COMPONENTS IN THE MEASUREMENT, MODELING, AND FORECASTING OF RETURN VOLATILITY
#  Torben G. Andersen, Tim Bollerslev, and Francis X. Diebold
#  data: a xts object with the intraday data
#  periods: a vector with time periods to aggregate over, expressed in days
#  RVest: estimator for daily realized volatility, 
#  in case a vector is supplied, the first estimator is the unrobust estimator, the second is the robust estimator 
#  type: string defining the type of model
#  "HARRV" from "roughing paper"
#  "HARRVJ" from "roughing paper"
#  "HARRVCJ" from "roughing paper"
#  jumptest: function to calculate the jump test statistic which determines whether the daily jump contribution is significant
#  alpha: a value between zero and one to indicate what
#  h: integer, determining over how many periods the depend variable should be aggregated. The default is 1, i.e. no aggregation is done, just one day. 
#  TODO ADD extra argument: jump-periods??? for aggregated jumps in the model...

# Helpfunctions: 
TQfun = function(rdata){ #Calculate the realized tripower quarticity
    returns = as.vector(as.numeric(rdata));
    n = length(returns);
    mu43 = 0.8308609; #    2^(2/3)*gamma(7/6) *gamma(1/2)^(-1)   
    tq = n * ((mu43)^(-3)) *  sum( abs(returns[1:(n - 2)])^(4/3) *abs(returns[2:(n-1)])^(4/3) *abs(returns[3:n])^(4/3) );
    return(tq);
} 

ABDJumptest = function(RV, BPV, TQ){ # Comput jump detection stat mentioned in roughing paper
    mu1  = sqrt(2/pi);
    n = length(RV);
    zstat = ((1/n)^(-1/2))*((RV-BPV)/RV)*(  (mu1^(-4) + 2*(mu1^(-2))-5) * pmax( 1,TQ*(BPV^(-2)) )   )^(-1/2); 
    return(zstat);
}

harModel = function(data, periods = c(1,5,22), periodsJ = c(1,5,22), leverage=NULL, RVest = c("rCov","rBPCov"), type="HARRV", 
jumptest="ABDJumptest",alpha=0.05,h=1,transform=NULL, ...){  
    nperiods = length(periods); # Number of periods to aggregate over
    nest = length(RVest);       # Number of RV estimators
    if( !is.null(transform) ){ Ftransform = match.fun(transform); }
    if( !(type %in% c("HARRV","HARRVJ","HARRVCJ"))){ warning("Please provide a valid argument for type, see documentation.")  }    
    
    if( sum(data<0) != 0 ){ #If it are returns as input
        # Get the daily RMs (in a non-robust and robust way)
        RV1 = match.fun(  RVest[1]);
        RM1 = apply.daily( data, RV1 );
        # save dates:
        alldates = index(RM1)
        if( nest == 2 ){ 
            RV2 = match.fun( RVest[2]); 
            RM2 = apply.daily( data, RV2 ); }
    } 
    
    if( sum(data<0) == 0 ){ #The input is most likely already realized measures
        dimdata = dim(data)[2]; 
        alldates = index(data); 
        RM1 = data[,1]; 
        if( dimdata > 1 ){ RM2 = data[,2]; } 
        if( type != "HARRV" ){ warning("Please provide returns as input for the type of model you want to estimate. All your returns are positive which is quite unlikely honestly. Only for the HAR-RV model you can input realized measures.") }
    } 
    
    # Get the matrix for estimation of linear model: 
    maxp      = max(periods,periodsJ); # Max number of aggregation levels
    if(!is.null(leverage)){ maxp = max(maxp,leverage) }
    n         = length(RM1);  #Number of Days
    
    # Aggregate RV: 
    RVmatrix1 = aggRV(RM1,periods);
    if( nest==2 ){ RVmatrix2 = aggRV(RM2,periods); }  # In case a jumprobust estimator is supplied
    
    # Aggregate and subselect y:
    y = aggY(RM1,h,maxp);
    
    # Only keep useful parts: 
    x1 = RVmatrix1[(maxp:(n-h)),]; 
    if( nest==2 ){ x2 = RVmatrix2[(maxp:(n-h)),]; } # In case a jumprobust estimator is supplied 
    
    # Jumps:
    if(type!="HARRV"){ # If model type is as such that you need jump component 
        J = pmax( RM1 - RM2,0 ); # Jump contributions should be positive
        J = aggJ(J,periodsJ);         
    }
    
    if( !is.null(leverage) ){ 
        if( sum(data<0) == 0 ){ warning("You cannot use leverage variables in the model in case your input consists of Realized Measures") }
        # Get close-to-close returns
        e = apply.daily(data,sum); #Sum logreturns daily     
        # Get the rmins:
        rmintemp = pmin(e,0);    
        # Aggregate everything:
        rmin = aggRV(rmintemp,periods=leverage,type="Rmin"); 
        # Select:
        rmin = rmin[(maxp:(n-h)),];
    }else{ rmin = matrix(ncol=0,nrow=dim(x1)[1]) }
    
    ###############################
    # Estimate the model parameters, according to type of model : 
    # First model type: traditional HAR-RV: 
    if( type == "HARRV" ){ 
        if(!is.null(transform)){ y = Ftransform(y); x1 = Ftransform(x1) }
        x1 = cbind(x1,rmin);
        model     = estimhar(y=y,x=x1); 
        model$transform = transform; model$h = h; model$type = "HARRV"; model$dates = alldates[(maxp+h):n];
        class(model) = c("harModel","lm"); 
        return( model )
    } #End HAR-RV if cond
    
    if( type == "HARRVJ" ){   
        if(!is.null(transform) && transform=="log"){ J = J + 1; }
        J = J[(maxp:(n-h)),]; 
        x = cbind(x1,J);              # bind jumps to RV data 
        if(!is.null(transform)){ y = Ftransform(y); x = Ftransform(x); }       
        x = cbind(x,rmin);
        model = estimhar(y=y,x=x); 
        model$transform = transform; model$h = h; model$type = "HARRVJ"; model$dates = alldates[(maxp+h):n];
        class(model) = c("harModel","lm"); 
        return( model )    
    }#End HAR-RV-J if cond
    
    if( type == "HARRVCJ" ){ 
        # Are the jumps significant? if not set to zero:
        if( jumptest=="ABDJumptest" ){ 
            TQ = apply.daily(data, TQfun); 
            J = J[,1];
            teststats= ABDJumptest(RV=RM1,BPV=RM2,TQ=TQ ); 
        }else{ jtest = match.fun(jumptest); teststats = jtest(data,...) }  
        Jindicators  = teststats > qnorm(1-alpha); 
        J[!Jindicators] = 0;
        
        # Get continuus components if necessary RV measures if necessary: 
        Cmatrix = matrix( nrow = dim(RVmatrix1)[1], ncol = 1 );
        Cmatrix[Jindicators,]    = RVmatrix2[Jindicators,1];      #Fill with robust one in case of jump
        Cmatrix[(!Jindicators)]  = RVmatrix1[(!Jindicators),1];   #Fill with non-robust one in case of no-jump  
        # Aggregate again:
        Cmatrix <- aggRV(Cmatrix,periods,type="C");
        Jmatrix <- aggJ(J,periodsJ);
        # subset again:
        Cmatrix <- Cmatrix[(maxp:(n-h)),];
        Jmatrix <- Jmatrix[(maxp:(n-h)),];   
        if(!is.null(transform) && transform=="log"){ Jmatrix = Jmatrix + 1 }
        
        x = cbind(Cmatrix,Jmatrix);               # bind jumps to RV data      
        if(!is.null(transform)){ y = Ftransform(y); x = Ftransform(x); }  
        x = cbind(x,rmin);
        model = estimhar( y=y, x=x ); 
        model$transform = transform; model$h = h; model$type = "HARRVCJ"; model$dates = alldates[(maxp+h):n];      
        class(model) = c("harModel","lm");
        return(model)
    } 
} #End function harModel
#################################################################
estimhar = function(y, x){ #Potentially add stuff here
    colnames(y)="y";
    output = lm( formula(y~x), data=cbind(y,x));
}

# Help function to get nicely formatted formula's for print/summary methods..
getHarmodelformula = function(x){
    modelnames = colnames(x$model$x);
    if(!is.null(x$transform)){ 
        
        modelnames = paste(x$transform,"(",modelnames,")",sep=""); } #Added visual tingie for plotting transformed RV
    betas      = paste("beta",(1:length(modelnames)),"",sep="")
    betas2     = paste(" + ",betas,"*")
    rightside  = paste(betas2, modelnames,collapse="");
    h = x$h;
    left = paste("RV",h,sep="");
    if(!is.null(x$transform)){  left = paste(x$transform,"(",left,")",sep="" ) }
    modeldescription = paste(left,"= beta0",rightside);
    return(list(modeldescription,betas))  
}

aggRV <- function(RM1,periods,type="RV"){
    n = length(RM1);
    nperiods = length(periods);
    RVmatrix1 = matrix(nrow=n,ncol=nperiods);
    for(i in 1:nperiods){ 
        if(periods[i]==1){ RVmatrix1[,i] = RM1; 
        }else{ RVmatrix1[(periods[i]:n),i] = rollmean(x=RM1,k=periods[i],align="left")  }
    } #end loop over periods for standard RV estimator
    colnames(RVmatrix1) = paste(type,periods,sep="");
    return(RVmatrix1);
}

aggJ <- function( J, periodsJ ){
    n = length(J);
    nperiods = length(periodsJ);
    JM = matrix(nrow=n,ncol=nperiods);
    for(i in 1:nperiods){ 
        if(periodsJ[i]==1){ JM[,i] = J; 
        }else{ JM[(periodsJ[i]:n),i] = rollmean( x=J, k=periodsJ[i], align="left")  }
    } # End loop over periods for standard RV estimator
    colnames(JM) = paste("J",periodsJ,sep="");
    return(JM)
}

aggY = function(RM1,h,maxp){
    n         = length(RM1);
    if( h == 1 ){  y  = RM1[(maxp+1):n]; }
    if( h != 1 ){ 
        y = matrix( nrow=length(RM1), ncol=1 ); colnames(y) = "y";
        y[(h:n),] = rollmean(x=RM1,k=h,align="left");
        y = matrix(y[((maxp+h):n),],ncol=1); y=as.data.frame(y) }  
    return(y);
}


######################################################################### 
# Print method for harmodel:  
print.harModel = function(x, digits = max(3, getOption("digits") - 3), ...){ 
    formula = getHarmodelformula(x); modeldescription = formula[[1]]; betas = formula[[2]];
    
    cat("\nModel:\n", paste(modeldescription, sep = "\n", collapse = "\n"), 
    "\n\n", sep = "")
    
    coefs = coef(x);
    names(coefs)  = c("beta0",betas)
    
    if (length(coef(x))){
        cat("Coefficients:\n")
        print.default(format(coefs, digits = digits), print.gap = 2,quote = FALSE);
        cat("\n\n");
        Rs = summary(x)[c("r.squared", "adj.r.squared")]
        zz = c(Rs$r.squared,Rs$adj.r.squared);
        names(zz) = c("r.squared","adj.r.squared")
        print.default((format(zz,digits=digits) ),print.gap = 2,quote=FALSE)
    }
    else cat("No coefficients\n")
    cat("\n")
    invisible(x)
} 

summary.harModel = function(object, correlation = FALSE, symbolic.cor = FALSE,...){
    x=object; 
    dd = summary.lm(x);
    formula = getHarmodelformula(x); modeldescription = formula[[1]]; betas = formula[[2]];
    dd$call = modeldescription;
    rownames(dd$coefficients) = c("beta0",betas);
    return(dd)
} 

plot.harModel = function(x, which = c(1L:3L, 5L), caption = list("Residuals vs Fitted", 
"Normal Q-Q", "Scale-Location", "Cook's distance", "Residuals vs Leverage", 
expression("Cook's dist vs Leverage  " * h[ii]/(1 - h[ii]))), 
panel = if (add.smooth) panel.smooth else points, sub.caption = NULL, 
main = "", ask = prod(par("mfcol")) < length(which) && dev.interactive(), 
..., id.n = 3, labels.id = names(residuals(x)), cex.id = 0.75, 
qqline = TRUE, cook.levels = c(0.5, 1), add.smooth = getOption("add.smooth"), 
label.pos = c(4, 2), cex.caption = 1){ 
    observed = x$model$y;
    fitted   = x$fitted.values;
    dates    = x$dates;
    dates    = as.POSIXct(dates);
    observed = xts(observed, order.by=dates);
    fitted   = xts(fitted, order.by=dates);
    type     = x$type;
    
    g_range = range(fitted,observed)
    g_range[1] = 0.95*g_range[1]; g_range[2]= 1.05 * g_range[2]; 
    #ind = seq(1,length(fitted),length.out=5);
    title = paste("Observed and forecasted RV based on HAR Model:",type);
    plot.zoo(observed,col="red",lwd=2,main=title, ylim=g_range,xlab="Time",ylab="Realized Volatility"); 
    #  axis(1,time(b)[ind], format(time(b)[ind],), las=2, cex.axis=0.8); not used anymore
    #  axis(2);
    lines(fitted,col="blue",lwd=2);
    legend("topleft", c("Observed RV","Forecasted RV"), cex=1.1, col=c("red","blue"),lty=1, lwd=2, bty="n"); 
}

##################################################################################################
######################################### FORMER RTAQ FUNCTIONS ##################################
########## HELPFUNCTION ####
readdata = function(path=NULL, extension="txt",header=FALSE,dims=0){
  #extention should either be "txt" or "csv"
  if(!(extension=="txt"|extension=="csv")){print("Please select a supported extension")}
  colnames = rep("x",dims);
  #load txt
  if(extension == "txt"){
    fullpath = paste(path,".txt",sep="");
    data = try(read.delim(fullpath,sep="",header=header,dec=",",col.names=colnames),silent=TRUE);
    
    if(is.null(dim(data))){
      data = try(read.delim(fullpath,sep="",header=header,dec=",",col.names=c(colnames,"EXTRA")),silent=TRUE);
      if(is.null(dim(data))){data=matrix(nrow=0,ncol=9);
      }else{data=data[,(-dim(data)[2])]}
    }
  }
  
  if(extension == "csv"){
    fullpath = paste(path,".csv",sep="");
    data = try(read.delim(fullpath,sep=",",header=header,dec=".",col.names=colnames),silent=TRUE);
    
    if(is.null(dim(data))){
      data = try(read.delim(fullpath,sep=",",header=header,dec=".",col.names=c(colnames,"EXTRA")),silent=TRUE);
      if(is.null(dim(data))){data=matrix(nrow=0,ncol=9);
      }else{data=data[,(-dim(data)[2])]}
    }
  }
  return(data);
}


convert_trades = function (datasource, datadestination, ticker, extension = "txt", 
                           header = FALSE, tradecolnames = NULL, format = "%Y%M%D %H:%M:%S") 
{  
  missingt=matrix(ncol=2,nrow=0);
  
  suppressWarnings(dir.create(datadestination));
  suppressWarnings(dir.create(datasource));
  
  setwd(datasource)
  adjtime = function(z) {
    zz = unlist(strsplit(z, ":"))
    if (nchar(zz[1]) != 2) {
      return(paste(paste(0, zz[1], sep = ""), zz[2], zz[3], 
                   sep = ":"))
    }
    return(z)
  }
  for (i in 1:length(ticker)) {
    tfile_name = paste(datasource, "/", ticker[i], "_trades", 
                       sep = "")
    tdata = try(highfrequency:::readdata(path = tfile_name, extension = extension, 
                                header = header, dims = 9), silent = TRUE)
    
    error = dim(tdata)[1] == 0
    if (error) {
      print(paste("no trades for stock", ticker[i]))
      missingt = rbind(missingt, c(datasource, ticker[i]))
    }
    if (error == FALSE) {
      if (is.null(tradecolnames)) {
        tradecolnames = c("SYMBOL", "DATE", "EX", "TIME", 
                          "PRICE", "SIZE", "COND", "CORR", "G127")
        colnames(tdata) = tradecolnames
      }else {
        colnames(tdata) = tradecolnames
      }
      cond = tdata$COND[is.na(tdata$G127)];
      cr = tdata$CORR[is.na(tdata$G127)];
      
      tdata$COND[is.na(tdata$G127)] = 0
      tdata$CORR[is.na(tdata$G127)] = as.character(cond)
      tdata$G127[is.na(tdata$G127)] = as.character(cr)
      rm(cond, cr)
      oldtime = as.matrix(as.vector(tdata$TIME))
      newtime = apply(oldtime, 1, adjtime)
      tdata$TIME = newtime
      rm(oldtime, newtime); 
      tdobject = as.POSIXct(paste(as.vector(tdata$DATE), as.vector(tdata$TIME)), format=format, tz="GMT")       
      tdata = xts(tdata, order.by = tdobject)
      tdata = tdata[, c("SYMBOL", "EX", "PRICE", "SIZE", 
                        "COND", "CORR", "G127")]
      rm(tdobject)
    }
    xts_name = paste(ticker[i], "_trades.RData", sep = "")
    setwd(datadestination)
    save(tdata, file = xts_name)
  }
}


convert_quotes = function (datasource, datadestination, ticker, extension = "txt", 
                           header = FALSE, quotecolnames = NULL, format = "%Y%M%D %H:%M:%S") 
{
  missingq=matrix(ncol=2,nrow=0);
  
  suppressWarnings(dir.create(datadestination));
  suppressWarnings(dir.create(datasource));
  
  setwd(datasource)
  adjtime = function(z) {
    zz = unlist(strsplit(z, ":"))
    if (nchar(zz[1]) != 2) {
      return(paste(paste(0, zz[1], sep = ""), zz[2], zz[3], 
                   sep = ":"))
    }
    return(z)
  }
  for (i in 1:length(ticker)) {
    qfile_name = paste(datasource, "/", ticker[i], "_quotes", 
                       sep = "")
    qdata = try(readdata(path = qfile_name, extension = extension, 
                         header = header, dims = 9), silent = TRUE)
    error = dim(qdata)[1] == 0
    if (error) {
      print(paste("no quotes for stock", ticker[i]))
      missingq = rbind(missingq, c(datasource, ticker[i]))
    }
    if (error == FALSE) {
      if (is.null(quotecolnames)) {
        quotecolnames = c("SYMBOL", "DATE", "EX", "TIME", 
                          "BID", "BIDSIZ", "OFR", "OFRSIZ", "MODE")
        colnames(qdata) = quotecolnames
      }
      else {
        colnames(qdata) = quotecolnames
      }
      qdata = qdata[qdata$SYMBOL == ticker[i], ]
      oldtime = as.matrix(as.vector(qdata$TIME))
      newtime = apply(oldtime, 1, adjtime)
      qdata$TIME = newtime
      rm(oldtime, newtime)
      test = paste(as.vector(qdata$DATE), as.vector(qdata$TIME))
      tdobject = as.POSIXct(test, format=format, tz="GMT")                       
      qdata = xts(qdata, order.by = tdobject)
      qdata = qdata[, c("SYMBOL", "EX", "BID", "BIDSIZ", 
                        "OFR", "OFRSIZ", "MODE")]
    }
    xts_name = paste(ticker[i], "_quotes.RData", sep = "")
    setwd(datadestination)
    save(qdata, file = xts_name)
  }
}

# NEW CODE GSoC 2012 #
makeXtsTrades = function(tdata,format=format){
  adjtime = function(z) {
    zz = unlist(strsplit(z, ":"))
    if (nchar(zz[1]) != 2) {
      return(paste(paste(0, zz[1], sep = ""), zz[2], zz[3], 
                   sep = ":"))  } 
    return(z) }
  tradecolnames = colnames(tdata)
  if (is.null(tradecolnames)){
    tradecolnames = c("SYMBOL", "DATE", "EX", "TIME", 
                      "PRICE", "SIZE", "COND", "CORR", "G127");
    colnames(tdata) = tradecolnames; }  
  
  cond = tdata$COND[is.na(tdata$G127)];
  cr = tdata$CORR[is.na(tdata$G127)];
  tdata$COND[is.na(tdata$G127)] = 0;
  tdata$CORR[is.na(tdata$G127)] = as.character(cond);
  tdata$G127[is.na(tdata$G127)] = as.character(cr);
  rm(cond, cr);
  oldtime = as.matrix(as.vector(tdata$TIME));
  newtime = apply(oldtime, 1, adjtime);
  tdata$TIME = newtime;
  rm(oldtime, newtime);
  tdobject = as.POSIXct(paste(as.vector(tdata$DATE), as.vector(tdata$TIME)), format=format, tz="GMT")       
  tdata  = xts(tdata, order.by = tdobject);
  tdata  = tdata[, c("SYMBOL", "EX", "PRICE", "SIZE","COND", "CORR", "G127")];
  rm(tdobject)
  return(tdata)  
}  

####
makeXtsQuotes = function( qdata, format = format){ 
  adjtime = function(z) {    zz = unlist(strsplit(z, ":")); if (nchar(zz[1]) != 2) {return(paste(paste(0, zz[1], sep = ""), zz[2], zz[3], sep = ":"))}; return(z) }
  quotecolnames = colnames(qdata);
  
  if (is.null(quotecolnames)) {
    quotecolnames = c("SYMBOL", "DATE", "EX", "TIME", "BID", "BIDSIZ", "OFR", "OFRSIZ", "MODE")
    colnames(qdata) = quotecolnames;
  }else{ colnames(qdata) = quotecolnames }
  
  oldtime = as.matrix(as.vector(qdata$TIME));
  newtime = apply(oldtime, 1, adjtime);
  qdata$TIME = newtime;
  rm(oldtime, newtime);
  test = paste(as.vector(qdata$DATE), as.vector(qdata$TIME))
  tdobject = as.POSIXct(test, format=format, tz="GMT")
  qdata = xts(qdata, order.by = tdobject)
  qdata = qdata[, c("SYMBOL", "EX", "BID", "BIDSIZ","OFR", "OFRSIZ", "MODE")];
  rm(tdobject);
  return(qdata);
}

################ The real conversion starts here ;)

convert = function(from, to, datasource, datadestination, trades = TRUE, 
                   quotes = TRUE, ticker, dir = FALSE, extension = "txt", header = FALSE, 
                   tradecolnames = NULL, quotecolnames = NULL, format = "%Y%m%d %H:%M:%S", onefile=FALSE){  
  require("timeDate")
  
  #############  1.A the data is in the "RTAQ folder" sturcture ##############
  if( onefile == FALSE ){
    
    # Create trading dates:
    dates = timeSequence(from, to, format = "%Y-%m-%d", FinCenter = "GMT")
    dates = dates[isBizday(dates, holidays = holidayNYSE(1950:2030))];
    
    # Create folder structure for saving:
    if (dir) { dir.create(datadestination); for (i in 1:length(dates)) {dirname = paste(datadestination, "/", as.character(dates[i]), sep = ""); dir.create(dirname)    } }
    for (i in 1:length(dates)){ #Loop over days  
      #Get the day-specific path
      datasourcex = paste(datasource, "/", dates[i], sep = "")
      datadestinationx = paste(datadestination, "/", dates[i], sep = "")
      
      if(trades == TRUE){ 
        if(extension=="txt"|extension=="csv"){ convert_trades(datasourcex, datadestinationx, ticker, extension = extension, header = header, tradecolnames = tradecolnames, format = format) }
      }
      
      if (quotes == TRUE) { 
        if(extension=="txt"|extension=="csv"){ convert_quotes(datasourcex, datadestinationx, ticker, extension = extension, header = header, quotecolnames = quotecolnames,format = format)}
      } 
    }#End loop over days
  }#End "not oneday" if
  
  #############  1.B The data is in one file: ###########
  if( onefile == TRUE ){
    # Load the data: ############################ This depends on the data provider
    if(trades == TRUE){ 
      if( extension=="txt"){ dataname = paste(datasource,"/",ticker,"_trades",sep=""); highfrequency:::readdata(path = datasource, extension = "txt", header = FALSE, dims = 0); } 
      if( extension=="csv"){ dataname = paste(datasource,"/",ticker,"_trades.csv",sep=""); data = read.csv(dataname);}
      if( extension=="tickdatacom"){ 
        dataname   = paste(datasource,"/",ticker,"_trades.asc",sep="");
        colnames   = c("DATE","TIME","PRICE","SIZE","EX","COND","CORR","SEQN","SOURCE","TSTOP","G127","EXCL","FPRICE");
        alldata    = read.delim(dataname, header=F, sep=",",dec=".",col.names=colnames); 
        taqnames   = c("DATE","EX","TIME","PRICE","SIZE","COND","CORR","G127"); 
        data = alldata[,taqnames]; 
        data = cbind(rep(ticker,dim(data)[1]),data); colnames(data)[1] = "SYMBOL"; 
      }
      alldata = suppressWarnings(makeXtsTrades(tdata=data,format=format)); 
    }
    if (quotes == TRUE){ 
      if( extension=="txt"){ dataname = paste(datasource,"/",ticker,"_quotes",sep=""); highfrequency:::readdata(path = datasource, extension = "txt", header = FALSE, dims = 0); } 
      if( extension=="csv"){ dataname = paste(datasource,"/",ticker,"_quotes.csv",sep=""); data = read.csv(dataname);}
      if( extension=="tickdatacom"){ 
        dataname   = paste(datasource,"/",ticker,"_quotes.asc",sep=""); 
        colnames   = c("DATE","TIME","EX","BID","OFR","BIDSIZ","OFRSIZ","MODE","MMID","SEQN","EXB", "EXO","NBBOID","NBBOID","CORR","QSO"); 
        alldata    = read.delim(dataname, header=F, sep=",",dec=".",col.names=colnames); 
        taqnames   = c("DATE","TIME","EX","BID","BIDSIZ","OFR","OFRSIZ","MODE"); 
        data = alldata[,taqnames]; 
        data = cbind(rep(ticker,dim(data)[1]),data); colnames(data)[1] = "SYMBOL"; 
        format = "%d/%m/%Y %H:%M:%S"; # Tickdata always has this format
      } 
      alldata = suppressWarnings( makeXtsQuotes( qdata=data, format=format) );
    }
    
    # Save the data: ############################ This is the same irrespective of the data provider
    # Create trading dates: 
    
    dates = unique(as.Date(index(alldata)));
    
    # Create folder structure for saving : 
    suppressWarnings( if (dir){ dir.create(datadestination); for (i in 1:length(dates)) {dirname = paste(datadestination, "/", as.character(dates[i]), sep = ""); dir.create(dirname) } })
    
    for(i in 1:length(dates) ){ # Loop over days
      datadestinationx = paste(datadestination, "/", dates[i], sep = ""); 
      
      if( trades == TRUE ){ 
        tdata        = alldata[as.character(dates[i])];
        xts_name     = paste(ticker, "_trades.RData", sep = "")
        destfullname = paste(datadestinationx,"/",xts_name,sep="");          
        save(tdata, file = destfullname); # Save daily in right folder:
      } 
      
      if( quotes == TRUE ){ 
        qdata        = alldata[as.character(dates[i])]; 
        xts_name     = paste(ticker, "_quotes.RData", sep = ""); 
        destfullname = paste(datadestinationx,"/",xts_name,sep=""); 
        save(qdata, file = destfullname); # Save daily in right folder: 
      }#End quotes if
    } #End save loop over days
  } #End oneday   
} #End convert function

### Manipulation functions:
#MANIPULATION FUNCTIONS:
TAQLoad = function(tickers,from,to,trades=TRUE,quotes=FALSE,datasource=NULL,variables=NULL){ 
  if( is.null(datasource)){print("Please provide the argument 'datasource' to indicate in which folder your data is stored")}
  
  if(!(trades&quotes)){#not both trades and quotes demanded
    for( ticker in tickers ){
      out = uniTAQload( ticker = ticker , from = from, to = to , trades=trades,quotes=quotes,datasource = datasource,variables=variables);
      if( ticker == tickers[1] ){ totalout = out 
      }else{ 
        totalout = merge(totalout,out) }
    }
  }
  
  if((trades&quotes)){#in case both trades and quotes
    totalout=list();
    totalout[[1]] = TAQLoad( tickers = tickers , from = from, to = to , trades=TRUE,quotes=FALSE,datasource = datasource,variables=variables);
    totalout[[2]] = TAQLoad( tickers = tickers , from = from, to = to , trades=FALSE,quotes=TRUE,datasource = datasource,variables=variables);
  }
  return(totalout);
}

uniTAQload = function(ticker,from,to,trades=TRUE,quotes=FALSE,datasource=NULL,variables=NULL){
  ##Function to load the taq data from a certain stock 
  #From&to (both included) should be in the format "%Y-%m-%d" e.g."2008-11-30"
  dates = timeSequence(as.character(from),as.character(to), format = "%Y-%m-%d", FinCenter = "GMT")
  dates = dates[isBizday(dates, holidays = holidayNYSE(1960:2040))];
  
  if(trades){ tdata=NULL;
              for(i in 1:length(dates)){
                datasourcex = paste(datasource,"/",dates[i],sep="");
                filename = paste(datasourcex,"/",ticker,"_trades.RData",sep="");
                
                ifmissingname = paste(datasourcex,"/missing_",ticker,".RData",sep="");  
                
                if(file.exists(ifmissingname)){stop(paste("No trades available on ",dates[i],sep=""))}
                if(!file.exists(filename)){stop(paste("The file ",filename," does not exist. Please read the documentation.",sep=""))}
                if(file.exists(ifmissingname)==FALSE){
                  load(filename);
                  if(i==1)	{
                    if( is.null(variables)){totaldata=tdata;
                    }else{
                      allnames=as.vector(colnames(tdata));
                      selection = allnames%in%variables;
                      qq=(1:length(selection))[selection];
                      totaldata=tdata[,qq];
                    }	  
                  };
                  if(i>1){
                    if( is.null(variables)){totaldata=rbind(totaldata,tdata);
                    }else{
                      totaldata=rbind(totaldata,tdata[,qq])};
                  }
                  rm(tdata);
                }
              }
  }
  
  if(quotes){ qdata=NULL;
              for(i in 1:length(dates)){
                datasourcex = paste(datasource,"/",dates[i],sep="");
                filename = paste(datasourcex,"/",ticker,"_quotes.RData",sep="");
                ifmissingname = paste(datasourcex,"/missingquotes_",ticker,".RData",sep="");
                
                if(file.exists(ifmissingname)){stop(paste("no quotes available on ",dates[i],sep=""))}
                if(!file.exists(filename)){stop(paste("The file ",filename," does not exist. Please read the documentation.",sep=""))}
                if(file.exists(ifmissingname)==FALSE){
                  load(filename);
                  
                  if(i==1)	{
                    if( is.null(variables)){totaldataq=qdata;
                    }else{
                      allnames=as.vector(colnames(qdata));
                      selection = allnames%in%variables;
                      qq=(1:length(selection))[selection];
                      totaldataq=qdata[,qq];
                    }	  
                  }
                  if(i>1){
                    if( is.null(variables)){totaldataq=rbind(totaldataq,qdata);
                    }else{
                      totaldataq=rbind(totaldataq,qdata[,qq])};
                  }
                  rm(qdata);
                }
              }
  }
  
  if(trades&quotes){return(list(trades = totaldata,quotes=totaldataq))}
  if(trades==TRUE & quotes==FALSE){return(totaldata)}
  if(trades==FALSE & quotes==TRUE){return(totaldataq)}
}

###### start SPOTVOL FUNCTIONS formerly in periodicityTAQ #########
# Documented function:

spotVol =  function(pdata, dailyvol = "bipower", periodicvol = "TML", on = "minutes", 
                    k = 5, dummies = FALSE, P1 = 4, P2 = 2,  marketopen = "09:30:00", 
                    marketclose = "16:00:00") 
{
  require(chron);
  dates = unique(format(time(pdata), "%Y-%m-%d"))
  cDays = length(dates)
  rdata = mR = c()
  if(on=="minutes"){
    intraday = seq(from=times(marketopen), to=times(marketclose), by=times(paste("00:0",k,":00",sep=""))) 
  }
  if(tail(intraday,1)!=marketclose){intraday=c(intraday,marketclose)}
  intraday = intraday[2:length(intraday)];
  for (d in 1:cDays) {
    pdatad = pdata[as.character(dates[d])]
    pdatad = aggregatePrice(pdatad, on = on, k = k , marketopen = marketopen, marketclose = marketclose)
    z = xts( rep(1,length(intraday)) , order.by = timeDate( paste(dates[d],as.character(intraday),sep="") , format = "%Y-%m-%d %H:%M:%S"))
    pdatad = merge.xts( z , pdatad )$pdatad
    pdatad = na.locf(pdatad)
    rdatad = makeReturns(pdatad)
    rdatad = rdatad[time(rdatad) > min(time(rdatad))]
    rdata = rbind(rdata, rdatad)
    mR = rbind(mR, as.numeric(rdatad))
  }
  mR[is.na(mR)]=0
  M = ncol(mR)
  if (cDays == 1) {
    mR = as.numeric(rdata)
    estimdailyvol = switch(dailyvol, bipower = rBPCov(mR), 
                           medrv = medRV(mR), rv = RV(mR))
  }else {
    estimdailyvol = switch(dailyvol, bipower = apply(mR, 
                                                     1, "rBPCov"), medrv = apply(mR, 1, "medRV"), rv = apply(mR, 
                                                                                                             1, "RV"))
  }
  if (cDays <= 50) {
    print("Periodicity estimation requires at least 50 observations. Periodic component set to unity")
    estimperiodicvol = rep(1, M)
  }
  else {
    mstdR = mR/sqrt(estimdailyvol * (1/M))
    selection = c(1:M)[ (nrow(mR)-apply(mR,2,'countzeroes')) >=20] 
    # preferably no na is between
    selection = c( min(selection) : max(selection) )
    mstdR = mstdR[,selection]
    estimperiodicvol_temp = diurnal(stddata = mstdR, method = periodicvol, 
                                    dummies = dummies, P1 = P1, P2 = P2)[[1]]
    estimperiodicvol = rep(1,M)
    estimperiodicvol[selection] = estimperiodicvol_temp
    mfilteredR = mR/matrix(rep(estimperiodicvol, cDays), 
                           byrow = T, nrow = cDays)
    estimdailyvol = switch(dailyvol, bipower = apply(mfilteredR, 
                                                     1, "rBPCov"), medrv = apply(mfilteredR, 1, "medRV"), 
                           rv = apply(mfilteredR, 1, "RV"))
  }
  out = cbind(rdata, rep(sqrt(estimdailyvol * (1/M)), each = M) * 
    rep(estimperiodicvol, cDays), rep(sqrt(estimdailyvol * 
    (1/M)), each = M), rep(estimperiodicvol, cDays))
  out = xts(out, order.by = time(rdata))
  names(out) = c("returns", "vol", "dailyvol", "periodicvol")
  return(out)
}


# internal non documented functions: 
HRweight = function( d,k){
  # Hard rejection weight function
  w = 1*(d<=k); return(w)
}

shorthscale = function( data )
{
  sorteddata = sort(data);
  n = length(data);
  h = floor(n/2)+1;
  M = matrix( rep(0,2*(n-h+1) ) , nrow= 2 );
  for( i in 1:(n-h+1) ){
    M[,i] = c( sorteddata[ i ], sorteddata[ i+h-1 ] )
  }
  return( 0.7413*min( M[2,]-M[1,] ) );
}

diurnal = 
  function (stddata, method = "TML", dummies = F, P1 = 6, P2 = 4) 
  {
    cDays = dim(stddata)[1]
    intraT = dim(stddata)[2]
    meannozero = function(series) {
      return(mean(series[series != 0]))
    }
    shorthscalenozero = function(series) {
      return(shorthscale(series[series != 0]))
    }
    WSDnozero = function(weights, series) {
      out = sum((weights * series^2)[series != 0])/sum(weights[series != 
        0])
      return(sqrt(1.081 * out))
    }
    if (method == "SD" | method == "OLS") {
      seas = sqrt(apply(stddata^2, 2, "meannozero"))
    }
    if (method == "WSD" | method == "TML") {
      seas = apply(stddata, 2, "shorthscalenozero")
      shorthseas = seas/sqrt(mean(seas^2))
      shorthseas[shorthseas == 0] = 1
      weights = matrix(HRweight(as.vector(t(stddata^2)/rep(shorthseas, 
                                                           cDays)^2), qchisq(0.99, df = 1)), ncol = dim(stddata)[2], 
                       byrow = T)
      for (c in 1:intraT) {
        seas[c] = WSDnozero(weights[, c], stddata[, c])
      }
    }
    seas = na.locf(seas,na.rm=F) #do not remove leading NA
    seas = na.locf(seas,fromLast=T)
    seas = seas/sqrt(mean(seas^2))
    if (method == "OLS" | method == "TML") {
      c = center()
      vstddata = as.vector(stddata)
      nobs = length(vstddata)
      vi = rep(c(1:intraT), each = cDays)
      if (method == "TML") {
        if( length(vstddata)!= length(seas)*cDays ){ print(length(vstddata)); print(length(seas)); print(cDays)}
        firststepresids = log(abs(vstddata)) - c - log(rep(seas, 
                                                           each = cDays))
      }
      X = c()
      if (!dummies) {
        if (P1 > 0) {
          for (j in 1:P1) {
            X = cbind(X, cos(2 * pi * j * vi/intraT))
          }
        }
        M1 = (intraT + 1)/2
        M2 = (2 * intraT^2 + 3 * intraT + 1)/6
        ADD = (vi/M1)
        X = cbind(X, ADD)
        ADD = (vi^2/M2)
        X = cbind(X, ADD)
        if (P2 > 0) {
          ADD = c()
          for (j in 1:P2) {
            ADD = cbind(ADD, sin(2 * pi * j * vi/intraT))
          }
        }
        X = cbind(X, ADD)
        opening = vi - 0
        stdopening = (vi - 0)/80
        almond1_opening = (1 - (stdopening)^3)
        almond2_opening = (1 - (stdopening)^2) * (opening)
        almond3_opening = (1 - (stdopening)) * (opening^2)
        X = cbind(X, almond1_opening, almond2_opening, almond3_opening)
        closing = max(vi) - vi
        stdclosing = (max(vi) - vi)/max(vi)
        almond1_closing = (1 - (stdclosing)^3)
        almond2_closing = (1 - (stdclosing)^2) * (closing)
        almond3_closing = (1 - (stdclosing)) * (closing^2)
        X = cbind(X, almond1_closing, almond2_closing, almond3_closing)
      }
      else {
        for (d in 1:intraT) {
          dummy = rep(0, intraT)
          dummy[d] = 1
          dummy = rep(dummy, each = cDays)
          X = cbind(X, dummy)
        }
      }
      selection = c(1:nobs)[vstddata != 0]
      vstddata = vstddata[selection]
      X = X[selection, ]
      if (method == "TML") {
        firststepresids = firststepresids[selection]
      }
      vy = matrix(log(abs(vstddata)), ncol = 1) - c
      if (method == "OLS") {
        Z = try(solve(t(X) %*% X), silent = T)
        if (inherits(Z, "try-error")) {
          print("X'X is not invertible. Switch to TML")
        }
        else {
          theta = solve(t(X) %*% X) %*% t(X) %*% vy
          rm(X)
          rm(vy)
        }
      }
      if (method == "TML") {
        inittheta = rep(0, dim(X)[2])
        l = -2.272
        u = 1.6675
        nonoutliers = c(1:length(vy))[(firststepresids > 
          l) & (firststepresids < u)]
        truncvy = vy[nonoutliers]
        rm(vy)
        truncX = X[nonoutliers, ]
        rm(X)
        negtruncLLH = function(theta) {
          res = truncvy - truncX %*% matrix(theta, ncol = 1)
          return(mean(-res - c + exp(2 * (res + c))/2))
        }
        grnegtruncLLH = function(theta) {
          res = truncvy - truncX %*% matrix(theta, ncol = 1)
          dres = -truncX
          return(apply(-dres + as.vector(exp(2 * (res + 
            c))) * dres, 2, "mean"))
        }
        est = optim(par = inittheta, fn = negtruncLLH, gr = grnegtruncLLH, 
                    method = "BFGS")
        theta = est$par
        rm(truncX)
        rm(truncvy)
      }
      plot(seas, main = "Non-parametric and parametric periodicity estimates", 
           xlab = "intraday period", type = "l", lty = 3)
      legend("topright", c("Parametric", "Non-parametric"), cex = 1.1,
             lty = c(1,3), lwd = 1, bty = "n")
      seas = highfrequency:::diurnalfit(theta = theta, P1 = P1, P2 = P2, intraT = intraT, 
                               dummies = dummies)
      lines(seas, lty = 1)
      return(list(seas, theta))
    }
    else {
      return(list(seas))
    }
  }

diurnalfit = function( theta , P1 , P2 , intraT , dummies=F )
{
  vi = c(1:intraT) ;  
  M1 = (intraT+1)/2 ; M2 = (2*intraT^2 + 3*intraT + 1)/6;
  
  # Regressors that do not depend on Day of Week:
  X = c()
  if(!dummies){
    if ( P1 > 0 ){ for( j in 1:P1 ){ X = cbind( X , cos(2*pi*j*vi/intraT) )   }  } 
    
    ADD = (vi/M1 ) ; X = cbind(X,ADD);
    ADD = (vi^2/M2); X = cbind(X,ADD);
    if ( P2 > 0 ){ ADD= c(); for( j in 1:P2 ){  ADD = cbind( ADD , sin(2*pi*j*vi/intraT)  ) }}; X = cbind( X , ADD ) ; 
    
    #openingeffect
    opening = vi-0 ; stdopening = (vi-0)/80 ;
    almond1_opening   = ( 1 - (stdopening)^3 );
    almond2_opening   = ( 1 - (stdopening)^2 )*( opening);
    almond3_opening   = ( 1 - (stdopening)   )*( opening^2);   
    X = cbind(  X, almond1_opening , almond2_opening , almond3_opening   )  ;
    
    #closing effect
    closing = max(vi)-vi ; stdclosing = (max(vi)-vi)/max(vi) ;
    almond1_closing   = ( 1 - (stdclosing)^3 );
    almond2_closing   = ( 1 - (stdclosing)^2 )*( closing);
    almond3_closing   = ( 1 - (stdclosing)   )*( closing^2);   
    X = cbind(  X, almond1_closing , almond2_closing , almond3_closing   )  ;
    
  }else{
    for( d in 1:intraT){
      dummy = rep(0,intraT); dummy[d]=1; 
      X = cbind(X,dummy); 
    }
  }
  # Compute fit
  seas = exp( X%*%matrix(theta,ncol=1) );
  seas = seas/sqrt(mean( seas^2) )    
  return( seas )          
}

LeeMyklandCV = function( beta = 0.999 , M = 78 )
{
  # Critical value for Lee-Mykland jump test statistic
  # Based on distribution of Maximum of M absolute normal random variables
  a = function(n){ a1=sqrt(2*log(n)) ; a2= (log(pi)+log(log(n))  )/( 2*sqrt(2*log(n))   ); return(a1-a2)             };
  b = function(n){ return( 1/sqrt(2*log(n) )  ) ; return(b)} ;
  return( -log(-log(beta))*b(M) + a(M)     )
}

center = function()
{
  g=function(y){ return( sqrt(2/pi)*exp(y-exp(2*y)/2)  )}
  f=function(y){ return( y*g(y)    )  }
  return( integrate(f,-Inf,Inf)$value )
}

 ###### end SPOTVOL FUNCTIONS formerly in periodicityTAQ #########

 ###### Liquidity functions formerly in in RTAQ  ######
.check_data = function(data){ 
  # FUNCTION sets column names according to RTAQ format using quantmod conventions, such that all the other functions find the correct information.
  require('quantmod');
  # First step: assign the xts attributes:
  data = set.AllColumns(data);
  
  # Change column names to previous RTAQ format! 
  # Adjust price col naming:  
  try( (colnames(data)[xtsAttributes(data)[['Price']]] = 'PRICE') );
  # Adjust Bid col naming:    
  try( (colnames(data)[xtsAttributes(data)[['Bid']]] = 'BID') );
  # Adjust Ask col naming:    
  try( (colnames(data)[xtsAttributes(data)[['Ask']]] = 'OFR') );
  
  # Adjust Ask size col naming:
  try( (colnames(data)[xtsAttributes(data)[['BidSize']]] = 'BIDSIZ') );
  
  # Adjust Bid size col naming:    
  try( (colnames(data)[xtsAttributes(data)[['AskSize']]] = 'OFRSIZ') );
  
  # Adjust correction column, if necessary:
  if(any(colnames(data) == "CR")){
    colnames(data)[colnames(data) == "CR"] = "CORR"
  }
  
  return(data)
} 

qdatacheck = function(qdata){
  if(!is.xts(qdata)){stop("The argument qdata should be an xts object")}
  if(!any(colnames(qdata)=="BID")){stop("The argument qdata should have a column containing the BID. Could not find that column")}
  if(!any(colnames(qdata)=="OFR")){stop("The argument qdata should have a column containing the ASK / OFR. Could not find that column")}
}

tdatacheck = function(tdata){ 
  if(!is.xts(tdata)){stop("The argument tdata should be an xts object")}
  if(!any(colnames(tdata)=="PRICE")){stop("The argument tdata should have a PRICE column")}
}

tqdatacheck = function(tqdata){ 
  if(!is.xts(tqdata)){stop("The argument tqdata should be an xts object")}
  if(!any(colnames(tqdata)=="PRICE")){ stop("The argument tqdata should have a column containing the PRICE data. Could not find that column.")}
  if(!any(colnames(tqdata)=="BID")){    stop("The argument tqdata should have a column containing the BID. Could not find that column")}
  if(!any(colnames(tqdata)=="OFR")){    stop("The argument tqdata should have a column containing the ASK / OFR. Could not find that column")}
} 

rdatacheck = function(rdata,multi=FALSE){
  #if(!is.xts(rdata)){stop("The argument rdata should be an xts object")} CAN PERFECTLY BE A MATRIX FOR ALL FUNCTIONS SO FAR...
  if((dim(rdata)[2] < 2) & (multi)){stop("Your rdata object should have at least 2 columns")}
}

matchTradesQuotes = function(tdata,qdata,adjustment=2){ ##FAST VERSION
  tdata = .check_data(tdata);
  qdata = .check_data(qdata);
  qdatacheck(qdata);
  tdatacheck(tdata);
  
  tt = dim(tdata)[2];  
  index(qdata) = index(qdata) + adjustment;
  
  #merge:
  merged = merge(tdata,qdata);
  
  ##fill NA's:
  merged[,((tt+1):dim(merged)[2])] = na.locf(as.zoo(merged[,((tt+1):dim(merged)[2])]), na.rm=FALSE);
  
  #Select trades:
  index(tdata)=  as.POSIXct(index(tdata));
  index(merged)= as.POSIXct(index(merged));  
  merged = merged[index(tdata)];
  
  #return useful parts:
  #remove duplicated SYMBOL & EX (new)
  eff =  colnames(merged);
  realnames = c("SYMBOL","EX","PRICE","SIZE","COND","CORR","G127","BID","BIDSIZ","OFR","OFRSIZ","MODE");
  condition = (1:length(eff))[eff%in%realnames];
  merged = merged[,condition];
  
  ##a bit rough but otherwise opening price disappears...
  merged = as.xts(na.locf(as.zoo(merged),fromLast=TRUE));
  
  index(merged) = as.POSIXct(index(merged));
  return(merged)
}

getTradeDirection = function(tqdata,...){
  if(hasArg(data)){ tqdata = data; rm(data) }
  tqdata = .check_data(tqdata);
  tqdatacheck(tqdata); 
  
  ##Function returns a vector with the inferred trade direction:
  ##NOTE: the value of the first (and second) observation should be ignored if price=midpoint for the first (second) observation.
  bid = as.numeric(tqdata$BID);
  offer = as.numeric(tqdata$OFR);
  midpoints = (bid + offer)/2;
  price = as.numeric(tqdata$PRICE);
  
  buy1 = price > midpoints; #definitely a buy
  equal = price == midpoints;
  dif1 = c(TRUE,0 < price[2:length(price)]-price[1:(length(price)-1)]);#for trades=midpoints: if uptick=>buy
  equal1 = c(TRUE,0 == price[2:length(price)]-price[1:(length(price)-1)]);#for trades=midpoints: zero-uptick=>buy
  dif2 = c(TRUE,TRUE,0 < price[3:length(price)]-price[1:(length(price)-2)]);
  
  buy = buy1 | (dif1 & equal) | (equal1 & dif2 & equal);
  
  buy[buy==TRUE]=1;
  buy[buy==FALSE]=-1;
  
  return(buy);
}

es = function(data){
  data = .check_data(data);
  #returns the effective spread as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  midpoints = (bid + offer)/2;
  price = as.numeric(data$PRICE);
  d = gettradedir(data);
  
  es=xts(2*d*(price-midpoints),order.by=index(data));
  return(es);
}

rs = function(data,tdata,qdata){
  data  =  .check_data(data);
  qdata =  .check_data(qdata);
  tdata =  .check_data(tdata);
  
  ###Function returns the realized spread as an xts object
  #Please note that the returned object can contain less observations that the original "data"
  #because of the need to find quotes that match the trades 5 min ahead
  
  #arguments
  #data=> xts object containing matched trades and quotes
  #tdata and qdata, the xts object containing the trades and quotes respectively
  
  ##First part solves the problem that unequal number of obs (in data and data2) is possible when computing the RS
  data2 = matchtq(tdata,qdata,adjustment =300);
  if(dim(data2)[1]>dim(data)[1]){
    condition = as.vector(as.character(index(data2)))%in%as.vector(as.character(index(data)));
    data2 = subset(data2,condition,select=1:(dim(data)[2]));
    data = subset(data,as.vector(as.character(index(data)))%in%as.vector(as.character(index(data2))),select=1:(dim(data2)[2]));
  }
  
  if(dim(data2)[1]<dim(data)[1]){
    condition = as.vector(as.character(index(data)))%in%as.vector(as.character(index(data2)));
    data = subset(data,condition,select=1:(dim(data2)[2]));
    data2 = subset(data2,as.vector(as.character(index(data2)))%in%as.vector(as.character(index(data))),select=1:(dim(data)[2]));
  }
  
  bid = as.numeric(data2$BID);
  offer = as.numeric(data2$OFR);
  midpoints = (bid + offer)/2;
  price = as.numeric(data$PRICE);
  d = gettradedir(data);
  rs = 2*d*(price-midpoints);
  
  rs_xts = xts(rs,order.by=index(data));
  return(rs_xts);
}

value_trade = function(data){
  data = .check_data(data);
  #returns the trade value as xts object
  price = as.numeric(data$PRICE);
  size = as.numeric(data$SIZE);
  
  value = xts(price*size,order.by=index(data));
  return(value);
}

signed_value_trade = function(data){
  data = .check_data(data);
  #returns the signed trade value as xts object
  price = as.numeric(data$PRICE);
  size = as.numeric(data$SIZE);
  d = gettradedir(data);
  
  value = xts(d*price*size,order.by=index(data));
  return(value);
}


signed_trade_size = function(data){
  data = .check_data(data);
  #returns the signed size of the trade as xts object
  size = as.numeric(data$SIZE);
  d = gettradedir(data);
  
  value = xts(d*size,order.by=index(data));
  return(value);
}

di_diff = function(data){
  data = .check_data(data);
  #returns the depth imbalance (as a difference) as xts object
  bidsize = as.numeric(data$BIDSIZ);
  offersize = as.numeric(data$OFRSIZ);
  
  d = gettradedir(data);
  di = (d*(offersize-bidsize))/(offersize+bidsize);
  di_xts = xts(di,order.by=index(data));
  return(di_xts);
}

di_div = function(data){
  data = .check_data(data);
  #returns the depth imbalance (as a ratio) as xts object
  bidsize = as.numeric(data$BIDSIZ);
  offersize = as.numeric(data$OFRSIZ);
  d = gettradedir(data);
  
  di = (offersize/bidsize)^d;
  di_xts = xts(di,order.by=index(data));
  return(di_xts);
}

pes = function(data){
  data = .check_data(data);
  #returns the Proportional Effective Spread as xts object
  es = es(data);
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  midpoints = (bid + offer)/2;
  
  pes = es/midpoints
  pes_xts = xts(pes,order.by=index(data));
  return(pes_xts);
}

prs = function(data,tdata,qdata){
  data  = .check_data(data);
  tdata = .check_data(tdata);
  qdata = .check_data(qdata);
  
  #returns the Proportional Realized Spread as xts object
  rs = rs(data,tdata,qdata);
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  midpoints = (bid + offer)/2;
  prs = rs/midpoints
  prs_xts = xts(prs,order.by=index(data));
  return(prs_xts);
}

price_impact = function(data,tdata,qdata){
  data = .check_data(data);
  #returns the Price impact as xts object
  rs = rs(data,tdata,qdata);
  es = es(data);
  
  pi = (es-rs)/2;
  pi_xts = xts(pi,order.by=index(data));
  return(pi_xts);
}

prop_price_impact = function(data,tdata,qdata){
  data = .check_data(data);
  #returns the Proportional Price impact as xts object
  rs = rs(data,tdata,qdata);
  es = es(data);
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  midpoints = (bid + offer)/2;
  
  prop_pi = ((es-rs)/2)/midpoints;
  prop_pi_xts = xts(prop_pi,order.by=index(data));
  return(prop_pi_xts);
}

tspread = function(data){
  data = .check_data(data);
  #returns the half traded spread as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  midpoints = (bid + offer)/2;
  price = as.numeric(data$PRICE);
  d = gettradedir(data);
  
  ts = xts(d*(price-midpoints),order.by=index(data));
  return(ts);
}

pts = function(data){
  data = .check_data(data);
  #returns the proportional half traded spread as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  midpoints = (bid + offer)/2;
  price = as.numeric(data$PRICE);
  d = gettradedir(data);
  pts = (d*(price-midpoints))/midpoints;
  
  pts_xts = xts(pts,order.by=index(data));
  return(pts_xts);
}

p_return_sqr = function(data){
  data = .check_data(data);
  #returns the squared log return on Trade prices as xts object
  price = as.numeric(data$PRICE);
  return = c(0,log(price[2:length(price)])-log(price[1:length(price)-1]));
  sqr_return = return^2;
  
  sqr_return_xts = xts(sqr_return,order.by=index(data));
  return(sqr_return_xts);
}

qs = function(data){
  data = .check_data(data);
  #returns the quoted spread as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  qs = offer-bid;
  
  qs_xts = xts(qs,order.by=index(data));
  return(qs_xts);
}

pqs = function(data){
  data = .check_data(data);
  #returns the proportional quoted spread as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  midpoints = (bid + offer)/2;
  qs = offer-bid;
  pqs = qs/midpoints;
  
  pqs_xts = xts(pqs,order.by=index(data));
  return(pqs_xts);
}

logqs = function(data){
  data = .check_data(data);
  #returns the logarithm of the quoted spread as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  logqs = log(offer/bid);
  
  logqs_xts = xts(logqs,order.by=index(data));
  return(logqs_xts);
}

logsize = function(data){
  data = .check_data(data);
  #returns the log quoted size as xts object
  bidsize = as.numeric(data$BIDSIZ);
  offersize = as.numeric(data$OFRSIZ);
  logsize = log(bidsize)+log(offersize);
  
  logsize_xts = xts(logsize,order.by=index(data));
  return(logsize_xts);
}

qslope = function(data){
  data = .check_data(data);
  #returns the quoted slope as xts object
  logsize = logsize(data);
  qs = qs(data);
  
  qslope = qs/logsize;
  
  qslope_xts = xts(qslope,order.by=index(data));
  return(qslope_xts);
}

logqslope = function(data){
  data = .check_data(data);
  #returns the log quoted slope as xts object
  logqs = logqs(data);
  logsize = logsize(data);
  
  logqslope = logqs/logsize;
  
  logqslope_xts = xts(logqslope,order.by=index(data));
  return(logqslope_xts);
}

mq_return_sqr = function(data){
  data = .check_data(data);
  #returns midquote squared returns slope as xts object
  mq_return = mq_return(data);
  
  mq_return_sqr = mq_return^2;
  
  mq_return_sqr_xts = xts(mq_return_sqr,order.by=index(data));
  return(mq_return_sqr_xts);
}

mq_return_abs = function(data){ 
  data = .check_data(data);
  #returns absolute midquote returns slope as xts object
  mq_return = mq_return(data);
  
  mq_return_abs = abs(mq_return);
  
  mq_return_abs_xts = xts(mq_return_abs,order.by=index(data));
  return(mq_return_abs_xts);
}

tqLiquidity <- function(tqdata=NULL,tdata=NULL,qdata=NULL,type,...) {
  if(hasArg(data)){ tqdata = data }
  if(!is.null(tqdata)){tqdatacheck(tqdata)}
  if(!is.null(qdata)){qdatacheck(qdata)}
  if(!is.null(tdata)){tdatacheck(tdata)}
  
  result=switch(type,
                es = es(tqdata),
                rs = rs(tqdata,tdata,qdata),
                value_trade = value_trade(tqdata),
                signed_value_trade = signed_value_trade(tqdata),
                di_diff = di_diff(tqdata),
                pes = pes(tqdata),
                prs = prs(tqdata,tdata,qdata),
                price_impact = price_impact(tqdata,tdata,qdata),
                prop_price_impact = prop_price_impact(tqdata,tdata,qdata),
                tspread = tspread(tqdata),
                pts = pts(tqdata),
                p_return_sqr = p_return_sqr(tqdata),
                p_return_abs = p_return_abs(tqdata),
                qs = qs(tqdata),
                pqs = pqs(tqdata),
                logqs = logqs(tqdata),
                logsize = logsize(tqdata),
                qslope = qslope(tqdata),
                logqslope = logqslope(tqdata),
                mq_return_sqr = mq_return_sqr(tqdata),
                mq_return_abs = mq_return_abs(tqdata),
                signed_trade_size = signed_trade_size(tqdata)
  )
  
  colnames(result)=type;
  return(result);
}

##help_function:
mq_return = function(data){
  data = .check_data(data);
  #function returns the midquote logreturns as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  midpoints = (bid + offer)/2;
  mq_return = c(0,log(midpoints[2:length(midpoints)])-log(midpoints[1:length(midpoints)-1]));
  
  mq_return_xts = xts(mq_return,order.by=index(data));
  return(mq_return_xts);
}

p_return_abs <- function (data)
{
  price = as.numeric(data$PRICE)
  return = c(0, log(price[2:length(price)]) - log(price[1:length(price) -
    1]))
  abs_return = abs(return)
  abs_return_xts = xts(abs_return, order.by = index(data))
  return(abs_return_xts)
}


### Backwards compatibility for RTAQ functions ####
 gettradedir = function(...){getTradeDirection(...)};                      
 matchtq = function(...){matchTradesQuotes(...)};                          

##################### Total cleanup functions formerly in RTAQ ################################
tradesCleanup = function(from,to,datasource,datadestination,ticker,exchanges,tdataraw=NULL,report=TRUE,selection="median",...){
  
  nresult = rep(0,5);
  if(is.null(tdataraw)){
    dates = timeSequence(from,to, format = "%Y-%m-%d", FinCenter = "GMT");
    dates = dates[isBizday(dates, holidays = holidayNYSE(1960:2040))];
    
    for(j in 1:length(dates)){
      datasourcex = paste(datasource,"/",dates[j],sep="");
      datadestinationx = paste(datadestination,"/",dates[j],sep="");
      
      for(i in 1:length(ticker)){
        dataname = paste(ticker[i],"_trades.RData",sep="");
        load(paste(datasourcex,"/",dataname,sep=""));
        
        if(class(tdata)[1]!="try-error"){
          exchange = exchanges[i];  
          
          tdata = .check_data(tdata);  nresult[1]= nresult[1]+dim(tdata)[1];
          
          ##actual clean-up: 
          ##general:
          tdata = try(nozeroprices(tdata));  nresult[2]= nresult[2]+dim(tdata)[1];
          tdata = try(selectexchange(tdata,exch=exchange));  nresult[3]= nresult[3]+dim(tdata)[1];
          
          ##trade specific:
          tdata = try(salescond(tdata));   nresult[4] = nresult[4] + dim(tdata)[1];
          tdata = try(mergeTradesSameTimestamp(tdata,selection=selection));   nresult[5] = nresult[5] + dim(tdata)[1];
          
          save(tdata, file = paste(datadestinationx,"/",dataname,sep=""));
        }
        
        if(class(tdata)=="try-error")  {
          abc=1;
          save(abc, file = paste(datadestinationx,"/missing_",ticker[i],".RData",sep=""));
        }
      }
    }
    if(report==TRUE){
      names(nresult) = c("initial number","no zero prices","select exchange",
                         "sales condition","merge same timestamp");
      return(nresult)
    }
  }
  
  if(!is.null(tdataraw)){
    if(class(tdataraw)[1]!="try-error"){
      if(length(exchanges)>1){print("The argument exchanges contains more than 1 element. Please select a single exchange, in case you provide tdataraw.")}
      tdata=tdataraw; rm(tdataraw);  
      tdata = .check_data(tdata);  nresult[1]= nresult[1]+dim(tdata)[1];
      
      ##actual clean-up: 
      ##general:
      tdata = try(nozeroprices(tdata));  nresult[2]= nresult[2]+dim(tdata)[1];
      tdata = try(selectexchange(tdata,exch=exchanges));  nresult[3]= nresult[3]+dim(tdata)[1];
      
      ##trade specific:
      tdata = try(salescond(tdata));   nresult[4] = nresult[4] + dim(tdata)[1];
      tdata = try(mergeTradesSameTimestamp(tdata,selection=selection));   nresult[5] = nresult[5] + dim(tdata)[1];
      
      if(report==TRUE){
        names(nresult) = c("initial number","no zero prices","select exchange",
                           "sales condition","merge same timestamp");
        return(list(tdata=tdata,report=nresult))
      }
      if(report!=TRUE){return(tdata)}
    }
  }
  
}

quotesCleanup = function(from,to,datasource,datadestination,ticker,exchanges, qdataraw=NULL,report=TRUE,selection="median",maxi=50,window=50,type="advanced",rmoutliersmaxi=10,...){
  nresult = rep(0,7);
  if(is.null(qdataraw)){
    dates = timeSequence(from,to, format = "%Y-%m-%d", FinCenter = "GMT");
    dates = dates[isBizday(dates, holidays = holidayNYSE(1960:2040))];
    
    for(j in 1:length(dates)){
      datasourcex = paste(datasource,"/",dates[j],sep="");
      datadestinationx = paste(datadestination,"/",dates[j],sep="");
      
      for(i in 1:length(ticker)){
        dataname = paste(ticker[i],"_quotes.RData",sep="");
        load(paste(datasourcex,"/",dataname,sep=""));
        
        if(class(qdata)[1]!="try-error"){
          exchange = exchanges[i];  
          if(exchange=="Q"){exchange="T"}
          
          qdata = .check_data(qdata); nresult[1] = nresult[1]+dim(qdata)[1];
          ##actual clean-up:
          ##general:
          qdata = try(nozeroquotes(qdata)); nresult[2] = nresult[2]+dim(qdata)[1];
          qdata = try(selectexchange(qdata,exch=exchange)); nresult[3] = nresult[3]+dim(qdata)[1];
          
          ##quote specific:
          qdata = try(rmnegspread(qdata)); nresult[4] = nresult[4]+dim(qdata)[1];
          qdata = try(rmlargespread(qdata,maxi=maxi)); nresult[5] = nresult[5]+dim(qdata)[1];
          
          qdata = try(mergequotessametimestamp(qdata,selection=selection)); nresult[6] = nresult[6]+dim(qdata)[1];
          qdata = try(rmoutliers(qdata,maxi=rmoutliersmaxi,window=window,type=type)); nresult[7] = nresult[7]+dim(qdata)[1];
          
          save(qdata, file = paste(datadestinationx,"/",dataname,sep=""));
        }
        
        if(class(qdata)=="try-error"){
          abc=1;
          save(abc, file = paste(datadestinationx,"/missingquotes_",ticker[i],".RData",sep=""));
        }
      }
    }
  }
  
  if(!is.null(qdataraw)){
    if(class(qdataraw)[1]!="try-error"){
      if(length(exchanges)>1){print("The argument exchanges contains more than 1 element. Please select a single exchange, in case you provide qdataraw.")}
      if(class(qdataraw)[1]!="try-error"){
        exchange=exchanges;
        qdata = qdataraw; rm(qdataraw) 
        
        qdata = .check_data(qdata); nresult[1] = nresult[1]+dim(qdata)[1];
        ##actual clean-up:
        ##general:
        qdata = try(nozeroquotes(qdata));                                           nresult[2] = nresult[2]+dim(qdata)[1];
        qdata = try(selectexchange(qdata,exch=exchange));                           nresult[3] = nresult[3]+dim(qdata)[1];
        
        ##quote specific:
        qdata = try(rmnegspread(qdata));                                            nresult[4] = nresult[4]+dim(qdata)[1];
        qdata = try(rmlargespread(qdata,maxi=maxi));                                nresult[5] = nresult[5]+dim(qdata)[1];
        
        qdata = try(mergequotessametimestamp(qdata,selection=selection));           nresult[6] = nresult[6]+dim(qdata)[1];
        qdata = try(rmoutliers(qdata,maxi=rmoutliersmaxi,window=window,type=type)); nresult[7] = nresult[7]+dim(qdata)[1];
        
        if(report==TRUE){
          names(nresult) = c("initial number","no zero quotes","select exchange",
                             "remove negative spread","remove large spread","merge same timestamp","remove outliers");
          return(list(qdata=qdata,report=nresult))
        }
        if(report!=TRUE){return(qdata)}
      }
    }
  }
}


tradesCleanupFinal = function(from,to,datasource,datadestination,ticker,tdata=NULL,qdata=NULL,...){
  if(is.null(tdata)&is.null(qdata)){
    dates = timeSequence(from,to, format = "%Y-%m-%d", FinCenter = "GMT");
    dates = dates[isBizday(dates, holidays = holidayNYSE(1960:2040))];
    
    for(j in 1:length(dates)){
      datasourcex = paste(datasource,"/",dates[j],sep="");
      datadestinationx = paste(datadestination,"/",dates[j],sep="");
      
      for(i in 1:length(ticker)){
        dataname = paste(ticker[i],"_trades.RData",sep="");
        dataname2 = paste(ticker[i],"_quotes.RData",sep="");
        
        # Missing file??
        m1 = paste(datasourcex,"/missing_",ticker[i],".RData",sep="");
        m2 = paste(datasourcex,"/missingquotes_",ticker[i],".RData",sep="");
        miscondition = file.exists(m1)|file.exists(m1);
        a=FALSE;#check whether tried to clean
        
        if(!miscondition){
          # load trades and quotes
          load(paste(datasourcex,"/",dataname,sep=""));
          load(paste(datasourcex,"/",dataname2,sep=""));
          
          tdata = .check_data(tdata);
          qdata = .check_data(qdata);
          
          #1 cleaning procedure that needs cleaned trades and quotes
          tdata = try(rmtradeoutliers(tdata,qdata));
          
          #save
          save(tdata, file = paste(datadestinationx,"/",dataname,sep=""));
          a=TRUE;
        }
        
        if(a==TRUE){a=(class(tdata)=="try-error")}
        
        if(miscondition|a)  {
          abc=1;
          save(abc, file = paste(datadestinationx,"/missing_",ticker[i],".RData",sep=""));
        }
      }
    }
  }
  
  if((!is.null(tdata))&(!is.null(qdata))){
    tdata = .check_data(tdata);
    qdata = .check_data(qdata);
    
    #1 cleaning procedure that needs cleaned trades and quotes
    tdata = try(rmtradeoutliers(tdata,qdata));
    return(tdata);
  }
}

##################### Specific cleanup functions formerly in RTAQ ################################
## Help functions : 
## Help function to make all time notation consistent
adjtime = function(z){ 
  zz = unlist(strsplit(z,":")); 
  if(nchar(zz[1])!=2){
    return(paste(paste(0,zz[1],sep=""),zz[2],zz[3],sep=":"))}
  return(z);
}

period.apply3 = function (x, INDEX, FUN, ...) 
{
  #small adaptation of the xts - function which experiences some troubles in multidimensional setting
  x <- try.xts(x, error = FALSE)
  FUN <- match.fun(FUN)
  xx <- sapply(1:(length(INDEX) - 1), function(y) {
    FUN(x[(INDEX[y] + 1):INDEX[y + 1]], ...)
  })
  if (is.vector(xx)) 
    xx <- t(xx)
  xx <- t(xx)
  reclass(xx, x[INDEX])
}

########## DATA CLEAN-UP: FOR ALL DATA #####################

####FUNCTION TO FILTER EXCHANGE HOURS ONLY: ExchangeHoursOnly
exchangeHoursOnly = function(data, daybegin = "09:30:00",dayend="16:00:00")
{
  data = .check_data(data);
  # a function to excerpt data within exchange trading hours
  # daybegin and dayend: two characters in the format of "HH:MM:SS",
  #                specifying the starting hour and minute and sec of an exhange
  #               trading day and the closing hour and minute and sec
  #                   of the trading day repectively
  
  if(!is(data, "xts"))
    stop("data must be an xts object")
  
  gettime = function(z){unlist(strsplit(as.character(z)," "))[2]};
  times1 = as.matrix(as.vector(as.character(index(data))));
  times = apply(times1,1,gettime); 
  tdtimes = as.POSIXct(times,format = "%H:%M:%S",tz = "GMT");
  
  #create timeDate begin and end
  tddaybegin = as.POSIXct( daybegin,format = "%H:%M:%S", tz="GMT");
  tddayend =   as.POSIXct( dayend,format = "%H:%M:%S",   tz="GMT");
  
  #select correct observations
  filteredts = data[tdtimes>=tddaybegin & tdtimes<=tddayend];
  return(filteredts);
}

noZeroPrices = function(tdata){
  tdata = .check_data(tdata);
  tdatacheck(tdata);
  ####FUNCTION TO DELETE ZERO PRICES: nozeroprices
  filteredts = tdata[as.numeric(tdata$PRICE)!= 0];
  return(filteredts);
}

selectExchange = function(data,exch="N"){ 
  data = .check_data(data);
  ###FUNCTION TO SELECT THE OBSERVATIONS OF A SINGLE EXCHANGE: selectexchange
  filteredts = data[data$EX==exch];
  return(filteredts);
}

autoSelectExchangeTrades = function(tdata){
  tdata = .check_data(tdata);
  tdatacheck(tdata);
  ## AUTOSELECT EXCHANGE WITH HIGHEST NUMBER OF SHARES TRADED (for trades) ON:
  #function returns ts with obs of only 1 exchange
  #searches exchange with a maximum on the variable "SIZE"
  nobs=c();
  
  exchanges = c("Q","A","P","B","C","N","D","X","I","M","W","Z");
  exchangenames = c("NASDAQ","AMEX","ARCA","Boston","NSX","NYSE","NASD ADF and TRF","Philadelphia","ISE","Chicago","CBOE","BATS");
  
  
  z1 = sum(as.numeric(selectexchange(tdata,"Q")$SIZE));
  z2 = sum(as.numeric(selectexchange(tdata,"T")$SIZE));
  z = max(z1,z2);
  watchout = z == z2;
  nobs = cbind(nobs,z);
  
  for(i in 2:length(exchanges)) {
    z = sum(as.numeric(selectexchange(tdata,exchanges[i])$SIZE));
    nobs = cbind(nobs,z); 
  }
  
  exch = exchanges[max(nobs)==nobs];
  
  as.character(tdata$EX[1]) == exchanges;
  namechosen = exchangenames[exch==exchanges];
  print(paste("The information of the",namechosen,"exchange was collected"));
  
  if(exch=="Q"&watchout){exch="T"}
  filteredtdata = tdata[tdata$EX==exch];
}


##### TRADE DATA SPECIFIC FUNCTIONS: ###################################

# Zivot
salesCondition <- function (tdata)
{
  tdatacheck(tdata);
  filteredts = tdata[tdata$COND == "0" | tdata$COND == "E" |
    tdata$COND == "F" | tdata$COND == "" | tdata$COND == "@F"]
  return(filteredts)
}


##Merge same timestamp:
sumN = function(a){
  a = sum(as.numeric(a));
  return(a)
}

medianN = function(a){
  a = median(as.numeric(a));
  return(a)
}

maxvol = function(a){
  p = as.numeric(a[,1]);
  s = as.numeric(a[,2]);
  
  b = median(p[s == max(s)]);
  return(b);
}

waverage = function(a){
  p = as.numeric(a[,1]);
  s = as.numeric(a[,2]);
  
  b = sum(p*s/sum(s));
  return(b);
}

mergeTradesSameTimestamp = function (tdata, selection = "median") 
{
  tdata = .check_data(tdata)
  tdatacheck(tdata)
  ep = endpoints(tdata, "secs")
  size = period.apply(tdata$SIZE, ep, sumN)
  if (selection == "median") {
    price = period.apply3(tdata$PRICE, ep, medianN)
  }
  if (selection == "maxvolume") {
    price = period.apply3(cbind(tdata$PRICE, tdata$SIZE), 
                          ep, maxvol)
  }
  if (selection == "weightedaverage") {
    price = period.apply3(cbind(tdata$PRICE, tdata$SIZE), 
                          ep, waverage)
  }
  selection = ep[2:length(ep)]
  tdata2 = tdata[selection]
  tdata2$PRICE = price
  tdata2$SIZE = size
  return(tdata2)
}

rmTradeOutliers = function(tdata,qdata){
  tdata = .check_data(tdata);
  qdata = .check_data(qdata);
  qdatacheck(qdata);
  tdatacheck(tdata);
  
  ##Function to delete entries with prices that are above the ask plus the bid-ask
  ##spread. Similar for entries with prices below the bid minus the bid-ask
  ##spread.
  data = matchtq(tdata,qdata);
  price = as.numeric(data$PRICE);
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  spread = offer - bid;
  
  upper = offer+spread;
  lower = bid-spread;
  
  tdata = tdata[(price<upper) & (price>lower)];
  return(tdata);
}

#################       QUOTE SPECIFIC FUNCTIONS:       #################

noZeroQuotes = function(qdata){
  qdata = .check_data(qdata);  
  qdatacheck(qdata);
  ####FUNCTION TO DELETE ZERO QUOTES: nozeroquotes
  filteredts = qdata[as.numeric(qdata$BID)!= 0& as.numeric(qdata$OFR)!= 0];
  return(filteredts);
}


autoSelectExchangeQuotes = function(qdata){
  qdata = .check_data(qdata);
  qdatacheck(qdata);
  ####Autoselect exchange with highest value for (bidsize+offersize)
  nobs=c();
  exchanges = c("Q","A","P","B","C","N","D","X","I","M","W","Z");
  exchangenames = c("NASDAQ","AMEX","ARCA","Boston","NSX","NYSE","NASD ADF and TRF","Philadelphia","ISE","Chicago","CBOE","BATS");
  
  selected1 = selectexchange(qdata,"Q");
  selected2 = selectexchange(qdata,"T");
  z1 = sum(as.numeric(selected1$BIDSIZ)+as.numeric(selected1$OFRSIZ));
  z2 = sum(as.numeric(selected2$BIDSIZ)+as.numeric(selected2$OFRSIZ));
  z = max(z1,z2);
  watchout = z == z2;
  nobs = cbind(nobs,z);
  
  for(i in 2:length(exchanges)) {
    selected = selectexchange(qdata,exchanges[i]);
    z = sum(as.numeric(selected$BIDSIZ)+as.numeric(selected$OFRSIZ));
    nobs = cbind(nobs,z); 
  }
  
  exch=exchanges[max(nobs)==nobs];
  
  namechosen = exchangenames[exch==exchanges];  
  print(paste("The information of the",namechosen,"exchange was collected"));
  
  if(exch=="Q"&watchout){exch="T"}
  
  filteredts = qdata[qdata$EX==exch];
  return(filteredts);
}


mergeQuotesSameTimestamp = function (qdata, selection = "median") 
{
  qdata = .check_data(qdata)
  qdatacheck(qdata)
  condition = selection == "median" | selection == "maxvolume" | 
    selection == "weightedaverage"
  if (!condition) {
    print(paste("WARNING:The result will be corrupted. Check whether", 
                selection, "is an existing option for the attribute selection."))
  }
  ep = endpoints(qdata, "secs")
  bidsize = period.apply(qdata$BIDSIZ, ep, sumN)
  offersize = period.apply(qdata$OFRSIZ, ep, sumN)
  if (selection == "median") {
    bid = period.apply(qdata$BID, ep, medianN)
    offer = period.apply(qdata$OFR, ep, medianN)
  }
  if (selection == "maxvolume") {
    bid = period.apply3(cbind(qdata$BID, qdata$BIDSIZ), ep, 
                        maxvol)
    offer = period.apply3(cbind(qdata$OFR, qdata$OFRSIZ), 
                          ep, maxvol)
  }
  if (selection == "weightedaverage") {
    bid = period.apply3(cbind(qdata$BID, qdata$BIDSIZ), ep, 
                        waverage)
    offer = period.apply3(cbind(qdata$OFR, qdata$OFRSIZ), 
                          ep, waverage)
  }
  selection = ep[2:length(ep)]
  ts2 = qdata[selection]
  ts2$BID = bid
  ts2$OFR = offer
  ts2$BIDSIZ = bidsize
  ts2$OFRSIZ = offersize
  return(ts2)
}


rmNegativeSpread = function(qdata){
  qdata = .check_data(qdata);
  qdatacheck(qdata);
  ##function to remove observations with negative spread
  condition = as.numeric(qdata$OFR)>as.numeric(qdata$BID);
  qdata[condition];
}


rmLargeSpread = function(qdata,maxi=50){
  qdata = .check_data(qdata);  
  qdatacheck(qdata);
  ##function to remove observations with a spread larger than 50 times the median spread that day
  ###WATCH OUT: works only correct if supplied input data consists of 1 day...
  spread = as.numeric(qdata$OFR)-as.numeric(qdata$BID);
  condition = ((maxi*median(spread))>spread);
  return(qdata[condition])
}

rmOutliers =  function (qdata, maxi = 10, window = 50, type = "advanced")
{
  qdata = .check_data(qdata)
  qdatacheck(qdata)
  ##function to remove entries for which the mid-quote deviated by more than 10 median absolute deviations 
  ##from a rolling centered median (excluding the observation under consideration) of 50 observations if type = "standard".
  
  ##if type="advanced":
  ##function removes entries for which the mid-quote deviates by more than 10 median absolute deviations
  ##from the variable "mediani".
  ##mediani is defined as the value closest to the midquote of these three options:
  ##1. Rolling centered median (excluding the observation under consideration)
  ##2. Rolling median of the following "window" observations
  ##3. Rolling median of the previous "window" observations
  
  ##NOTE: Median Absolute deviation chosen contrary to Barndorff-Nielsen et al.
  
  window = floor(window/2) * 2
  condition = c()
  halfwindow = window/2
  midquote = as.vector(as.numeric(qdata$BID) + as.numeric(qdata$OFR))/2
  mad_all = mad(midquote)
  midquote = xts(midquote, order.by = index(qdata))
  if (mad_all == 0) {
    m = as.vector(as.numeric(midquote))
    s = c(TRUE, (m[2:length(m)] - m[1:(length(m) - 1)] !=
                   0))
    mad_all = mad(as.numeric(midquote[s]))
  }
  medianw = function(midquote, n = window) {
    m = floor(n/2) + 1
    q = median(c(midquote[1:(m - 1)], midquote[(m + 1):(n +
                                                          1)]))
    return(q)
  }
  if (type == "standard") {
    meds = as.numeric(rollapply(midquote, width = (window +
                                                     1), FUN = medianw, align = "center"))
    #
    meds = meds[(halfwindow +
                   1):(n - halfwindow)]
  }
  if (type == "advanced") {
    advancedperrow = function(qq) {
      diff = abs(qq[1:3] - qq[4])
      select = min(diff) == diff
      value = qq[select]
      if (length(value) > 1) {
        value = median(value)
      }
      return(value)
    }
    n = length(midquote)
    allmatrix = matrix(rep(0, 4 * n), ncol = 4)
    median2 = function(a) {
      median(a)
    }
    standardmed = as.numeric(rollapply(midquote, width = (window),
                                       FUN = median2, align = "center"))
    # allmatrix[(halfwindow + 1):(n - halfwindow), 1] = as.numeric(rollapply(midquote,
    #     width = (window + 1), FUN = medianw, align = "center"))
    # Rolling centered median (excluding the observation under consideration)
    allmatrix[, 1] = as.numeric(rollapply(midquote,
                                          width = (window + 1), FUN = medianw, align = "center"))
    #Rolling median of the following "window" observations
    #allmatrix[(1:(n - window)), 2] = standardmed[2:length(standardmed)]
    allmatrix[(1:(n - 1)), 2] = standardmed[2:length(standardmed)]
    #Rolling median of the previous "window" observations
    # allmatrix[(window + 1):(n), 3] = standardmed[1:(length(standardmed) -1)]
    allmatrix[(1 + 1):(n), 3] = standardmed[1:(length(standardmed) -1)]
    allmatrix[, 4] = midquote
    meds = apply(allmatrix, 1, advancedperrow)[(halfwindow +
                                                  1):(n - halfwindow)]
  }
  midquote = as.numeric(midquote)
  maxcriterion = meds + maxi * mad_all
  mincriterion = meds - maxi * mad_all
  condition = mincriterion < midquote[(halfwindow + 1):(length(midquote) -
                                                          halfwindow)] & midquote[(halfwindow + 1):(length(midquote) -
                                                                                                      halfwindow)] < maxcriterion
  condition = c(rep(TRUE, halfwindow), condition, rep(TRUE, halfwindow))
  
  qdata[condition]
}


# Zivot : 
correctedTrades <- function (tdata){ 
  tdatacheck(tdata);
  filteredts = tdata[tdata$CR == " 0"];
  return(filteredts)
}

autoselectexchange = function(...){autoSelectExchangeTrades(...)};        
autoselectexchangeq = function(...){autoSelectExchangeQuotes(...)};       
ExchangeHoursOnly = function(...){exchangeHoursOnly(...)};                
mergequotessametimestamp = function(...){mergeQuotesSameTimestamp(...)};  
mergesametimestamp = function(...){mergeTradesSameTimestamp(...)};        
nozeroprices = function(...){noZeroPrices(...)};                          
nozeroquotes = function(...){noZeroQuotes(...)};                          
rmlargespread = function(...){rmLargeSpread(...)};                        
rmnegspread = function(...){rmNegativeSpread(...)};                       
rmoutliers = function(...){rmOutliers(...)};                              
rmtradeoutliers = function(...){rmTradeOutliers(...)};                    
salescond = function(...){salesCondition(...)};                           
selectexchange = function(...){selectExchange(...)};                      

####### Aggregation functions that were formerly in RTAQ ######################

previoustick = function(a){
  a = as.vector(a);
  b = a[length(a)];
  return(b)
}

weightedaverage = function(a){
  aa = as.vector(as.numeric(a[,1]));
  bb = as.vector(as.numeric(a[,2]));
  c = weighted.mean(aa,bb);
  return(c)
}

period.apply2 = function (x, INDEX, FUN2, ...) 
{
  x <- try.xts(x, error = FALSE)
  FUN <- match.fun(FUN2)
  xx <- sapply(1:(length(INDEX) - 1), function(y) {
    FUN(x[(INDEX[y] + 1):INDEX[y + 1]], ...)
  })
  reclass(xx, x[INDEX])
}

## AGGREGATION;
aggregatets = function (ts, FUN = "previoustick", on = "minutes", k = 1, weights = NULL,dropna=FALSE) 
{
  makethispartbetter = ((!is.null(weights))| on=="days"|on=="weeks"| (FUN!="previoustick")|dropna);
  if(makethispartbetter)  {
    
    FUN = match.fun(FUN);
    
    if (is.null(weights)) {
      ep = endpoints(ts, on, k)
      if(dim(ts)[2]==1){ ts2 = period.apply(ts, ep, FUN) }
      if(dim(ts)[2]>1){  ts2 = xts(apply(ts,2,FUN=period.apply2,FUN2=FUN,INDEX=ep),order.by=index(ts)[ep],)}
    }
    if (!is.null(weights)) {
      tsb = cbind(ts, weights)
      ep = endpoints(tsb, on, k)
      ts2 = period.apply(tsb, ep, FUN = match.fun(weightedaverage) )
    }
    if (on == "minutes" | on == "mins" | on == "secs" | on == 
      "seconds") {
      if (on == "minutes" | on == "mins") {
        secs = k * 60
      }
      if (on == "secs" | on == "seconds") {
        secs = k
      }
      a = .index(ts2) + (secs - .index(ts2)%%secs)
      ts3 = .xts(ts2, a,tzone="GMT")
    }
    if (on == "hours") {
      secs = 3600
      a = .index(ts2) + (secs - .index(ts2)%%secs)
      ts3 = .xts(ts2, a,tzone="GMT")
    }
    if (on == "days") {
      secs = 24 * 3600
      a = .index(ts2) + (secs - .index(ts2)%%secs) - (24 * 
        3600)
      ts3 = .xts(ts2, a,tzone="GMT")
    }
    if (on == "weeks") {
      secs = 24 * 3600 * 7
      a = (.index(ts2) + (secs - (.index(ts2) + (3L * 86400L))%%secs)) - 
        (24 * 3600)
      ts3 = .xts(ts2, a,tzone="GMT")
    }
    
    if (!dropna) {
      if (on != "weeks" | on != "days") {
        if (on == "secs" | on == "seconds") {
          tby = "s"
        }
        if (on == "mins" | on == "minutes") {
          tby = "min"
        }
        if (on == "hours") {
          tby = "h"
        }
        by = paste(k, tby, sep = " ")
        allindex = as.POSIXct(base:::seq(start(ts3), end(ts3), 
                                         by = by))
        xx = xts(rep("1", length(allindex)), order.by = allindex)
        ts3 = merge(ts3, xx)[, (1:dim(ts)[2])]
      }
    }
    
    index(ts3) = as.POSIXct(index(ts3));
    return(ts3);
  }
  
  if(!makethispartbetter){
    if (on == "secs" | on == "seconds") { secs = k; tby = paste(k,"sec",sep=" ")}
    if (on == "mins" | on == "minutes") { secs = 60*k; tby = paste(60*k,"sec",sep=" ")}
    if (on == "hours") {secs = 3600*k; tby = paste(3600*k,"sec",sep=" ")}
    
    FUN = match.fun(FUN);
    
    g = base:::seq(start(ts), end(ts), by = tby);
    rawg = as.numeric(as.POSIXct(g,tz="GMT"));
    newg = rawg + (secs - rawg%%secs);
    g    = as.POSIXct(newg,origin="1970-01-01",tz="GMT");
    ts3  = na.locf(merge(ts, zoo(, g)))[as.POSIXct(g,tz="GMT")]; 
    return(ts3) 
  }
}

#PRICE (specificity: opening price and previoustick)

aggregatePrice = function (ts, FUN = "previoustick", on = "minutes", k = 1,marketopen="09:30:00",marketclose = "16:00:00") 
{
  ts2 = aggregatets(ts, FUN = FUN, on, k)
  date = strsplit(as.character(index(ts)), " ")[[1]][1]
  
  #open
  a = as.POSIXct(paste(date, marketopen),tz="GMT")
  b = as.xts(matrix(as.numeric(ts[1]),nrow=1), a)
  ts3 = c(b, ts2)
  
  #close
  aa = as.POSIXct(paste(date, marketclose),tz="GMT")
  condition = index(ts3) < aa
  ts3 = ts3[condition]
  bb = as.xts(matrix(as.numeric(last(ts)),nrow=1), aa)
  ts3 = c(ts3, bb)
  
  return(ts3)
}

#VOLUME: (specificity: always sum)
agg_volume= function(ts, FUN = "sumN", on = "minutes", k = 5, includeopen = FALSE,marketopen="09:30:00",marketclose="16:00:00") 
{
  if (!includeopen) {
    ts3 = aggregatets(ts, FUN = FUN, on, k)
  }
  if (includeopen) {
    ts2 = aggregatets(ts, FUN = FUN, on, k)
    date = strsplit(as.character(index(ts)), " ")[[1]][1]
    a = as.POSIXct(paste(date, marketopen),tz="GMT")
    b = as.xts(matrix(as.numeric(ts[1]),nrow=1), a)
    ts3 = c(b, ts2)
  }
  
  aa = as.POSIXct(paste(date, marketclose),tz="GMT")
  condition = index(ts3) < aa
  ts4 = ts3[condition]
  
  lastinterval = matrix(colSums(matrix(ts3[!condition],ncol=dim(ts3)[2])),ncol=dim(ts3)[2])
  bb = xts(lastinterval, aa)
  ts4 = c(ts4, bb)
  
  return(ts4)
}

aggregateTrades =  function (tdata, on = "minutes", k = 5,marketopen="09:30:00",marketclose="16:00:00") 
{
  tdata = .check_data(tdata)
  tdatacheck(tdata)
  ## Aggregates an entire trades xts object (tdata) over a "k"-minute interval.
  ## Returned xts-object contains: SYMBOL,EX,PRICE,SIZE.
  ## Variables COND, CORR, G127 are dropped because aggregating them makes no sense.
  ## NOTE: first observation (opening price) always included.
  
  selection = colnames(tdata)%in%c("PRICE","EX","SYMBOL");
  tdata1 = tdata[,selection];
  PRICE = aggregatePrice(tdata$PRICE,on=on,k=k,marketopen=marketopen,marketclose=marketclose);
  SIZE = agg_volume(tdata$SIZE, on = on, k = k, includeopen = TRUE,marketopen=marketopen,marketclose=marketclose)
  
  EX = rep(tdata$EX[1], length(PRICE));
  SYMBOL = rep(tdata$SYMBOL[1], length(PRICE));
  
  all = data.frame(SYMBOL, EX, PRICE, SIZE);
  colnames(all) = c("SYMBOL", "EX", "PRICE", "SIZE");
  ts = xts(all, index(SIZE));
  return(ts);
}

###QUOTES AGGREGATION:
aggregateQuotes = function(qdata,on="minutes",k=5,marketopen="09:30:00",marketclose="16:00:00"){
  qdata = .check_data(qdata);
  qdatacheck(qdata);
  
  ## Aggregates an entire quotes xts object (qdata) object over a "k"-minute interval.
  ## Returned xts-object contains: SYMBOL,EX,BID,BIDSIZ,OFR,OFRSIZ.
  ## Variable MODE is dropped because aggregation makes no sense.
  ## "includeopen" determines whether to include the exact opening quotes.
  
  BIDOFR = aggregatePrice(cbind(qdata$BID,qdata$OFR),on=on,k=k,marketopen=marketopen,marketclose=marketclose);
  BIDOFRSIZ = agg_volume(cbind(qdata$BIDSIZ,qdata$OFRSIZ),on=on,k=k,includeopen=TRUE,marketopen=marketopen,marketclose=marketclose);
  
  EX = rep(qdata$EX[1],dim(BIDOFR)[1]);
  SYMBOL = rep(qdata$SYMBOL[1],dim(BIDOFR)[1]);
  
  all = data.frame(SYMBOL,EX,BIDOFR[,1],BIDOFRSIZ[,1],BIDOFR[,2],BIDOFRSIZ[,2]);
  colnames(all) =c("SYMBOL","EX","BID","BIDSIZ","OFR","OFRSIZ");
  
  ts = xts(all,index(BIDOFR));
  return(ts);
}

###########
# Likelihood for HEAVY volatility model of Shephard and Sheppard 
# Code is R-translation by Jonathan Cornelissen of matlab code of http://www.kevinsheppard.com/wiki/MFE_Toolbox
# by: kevin.sheppard@economics.ox.ac.uk

# USAGE:
#  [LL,LLS,H] = heavy_likelihood(PARAMETERS,DATA,P,Q,BACKCAST,LB,UB)

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

# In contrast to Sheppards code, I make a list for parameters (easier interpretation)
#NOTE # the parameter list has three items: O, A and B
# O is a matrix (K by 1), A and B items are (K by K) matrices

heavyModel = function(data, p=matrix( c(0,0,1,1),ncol=2 ), q=matrix( c(1,0,0,1),ncol=2 ), 
                      startingvalues = NULL, LB = NULL, UB = NULL, 
                      backcast = NULL, compconst = FALSE){
  K = dim(p)[2];
  
  # Set lower and upper-bound if not specified:
  if( is.null(LB) ){ LB = rep(0,K)   }
  if( is.null(UB) ){ UB = rep(Inf,K) }
  
  # Assign starting values if necessary:
  if( is.null(startingvalues) ){  #Very very naive, to adjust later
    startingvalues = rep(NA,K+sum(p)+sum(q));
    startingvalues[1:K] = 0.1;
    start = K+1; end = K+sum(p);
    startingvalues[start:end] = 0.3;
    start = end+1; end = start+sum(q)-1;
    startingvalues[start:end] = 0.6;}
  
  # Rescale? (useful to avoid numerical problems: TODO LATER)
  
  # Set backcast if necessary: how to initialize the model?
  if(is.null( backcast)){ 
    # For now, just unconditionally
    backcast = t( t( colMeans(data) ) );
  } 
  
  # Estimate the parameters: 
  # Set constraints: 
  KKK  = length(startingvalues);    
  ui   = diag(rep(1,KKK)); #All parameters should be larger than zero, add extra constraints with rbind...  
  ci   = rep(0,dim(ui)[2]);  
  
  x = try(optim( par = startingvalues, fn = heavy_likelihood,
                 data=data, p=p, q=q,backcast=backcast,UB=UB,LB=LB, lower=LB,upper=UB, compconst = compconst ) ); # ADJUST maxit ?!!
  
  #x = try(constrOptim( theta = startingvalues, f = heavy_likelihood, 
  #                     grad = NULL,
  #                     ui = ui, 
  #                     ci = ci, 
  #                     data=data, p=p, q=q,backcast=backcast,UB=UB,LB=LB, compconst = compconst));
  
  if( class(x) == "try-error" ){
    print("Error in likelihood optimization")
    print(x)
  }else{
    if(x$convergence != 0){
      print("Possible problem in likelihood optimization. Check convergence")   }
  }
  
  # Get the output: 
  estparams = x$par; 
  loglikelihood = x$value; 
  
  # Get the list with: total-log-lik, daily-log-lik, condvars
  xx = heavy_likelihood(par = estparams, data=data, p=p, q=q, backcast=backcast, LB=LB, UB=UB, foroptim=FALSE, compconst = compconst);
  
  # Add the timestamps and make xts: condvar and likelihoods:
  if( ! is.null(rownames(data)) ){
    xx$condvar    = xts( t(xx$condvar),  order.by   = as.POSIXct( rownames(data),tz="GMT") );     
    xx$likelihoods = xts( xx$likelihoods, order.by = as.POSIXct( rownames(data),tz="GMT"));
  }
  
  # 
  xx$estparams = matrix(estparams,ncol=1); 
  rownames(xx$estparams) = .get_param_names(estparams,p,q);
  xx$convergence = x$convergence
  
  return(xx)
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

transformparams = function( p, q, paramsvector ){
  K = dim(p)[1]; 
  pmax = max(p); qmax = max(q); # Max number of lags for innovations and cond vars
  
  O = matrix( paramsvector[1:K], ncol=1);
  A = B = list();
  start = (K+1); 
  
  for(i in 1:pmax){    # A will contain a list-item per innovation lag
    end =          start + sum(p>=i) - 1; # How many non-zero params in this loop?
    A[[i]] =       matrix(rep(0,K^2),ncol=2); 
    A[[i]][p>=i] = paramsvector[start:end];
    start  = end + 1;   
  }#end loop over number of lags for innovations
  
  for(i in 1:qmax){   # B will contain a list-item per cond var lag
    end   = start + sum(q>=i) -1; # How many non-zero params in this loop?
    B[[i]] = matrix(rep(0,K^2),ncol=2); 
    B[[i]][q >= i] = paramsvector[start:end];
    start  = end + 1;   
  }#End loop over number of lags for cond variances
  
  return( list(O,A,B) ) 
}  

heavy_likelihood = function( par, data, p, q, backcast, LB, UB, foroptim=TRUE, compconst=FALSE ){ 
  # Get the required variables
  # p is Max number of lags for innovations 
  # q is Max number of lags for conditional variances
  K    = dim(data)[2];  #Number of series to model
  T    = dim(data)[1];  #Number of time periods
  lls  = rep(NA,T);     #Vector containing the likelihoods
  h    = matrix(nrow=K,ncol=T); #Matrix to containing conditional variances
  maxp = max(p); maxq=max(q);
  
  # Get the parameters:
  x = transformparams( par, p=p, q=q );
  if( compconst ){ O = x[[1]]; } 
  A = x[[2]]; B = x[[3]]; 
  # Compute constant in case it needn't to be optimized:
  if( !compconst ){ # Don't compute the omega's but do (1-alpha-beta)*unconditional
    totalA = totalB = matrix( rep(0,K) ,ncol=1,nrow=K);
    for(j in 1:length(A) ){ totalA = totalA + t(t(rowSums(A[[j]]))); } # Sum over alphas for all models
    for(j in 1:length(B) ){ totalB = totalB + t(t(rowSums(B[[j]]))); } # Sum over betas for all models
    O = 1 - totalA - totalB; # The remaing weight after substracting A & B
    # Calculate the unconditionals ### KRIS FEEDBACK PLEASE ###
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
    } #end loop over innovation lags
    
    for(j in 1:maxp){# Loop over cond variances lags
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

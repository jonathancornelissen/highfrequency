
########################################################
## 2 Multivariate measures:    
########################################################

RTSRV <- function(pdata, startIV = NULL, noisevar = NULL, K = 300, J = 1, eta = 9) {
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
  iter <- 1
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


RTSCov_bi <- function (pdata1, pdata2, startIV1 = NULL, startIV2 = NULL, noisevar1 = NULL, 
                       noisevar2 = NULL, K = 300, J = 1,
                       K_cov = NULL, J_cov = NULL,
                       K_var1 = NULL, K_var2 = NULL,
                       J_var1 = NULL, J_var2 = NULL, 
                       eta = 9) {
  
  if (is.null(K_cov)){ K_cov <- K }
  if (is.null(J_cov)){ J_cov <- J }
  if (is.null(K_var1)){ K_var1 <- K }
  if (is.null(K_var2)){ K_var2 <- K }   
  if (is.null(J_var1)){ J_var1 <- J } 
  if (is.null(J_var2)){ J_var2 <- J }
  
  # Calculation of the noise variance and TSRV for the truncation
  
  if (is.null(noisevar1) == TRUE) {
    logprices1 = log(as.numeric(pdata1))
    n_var1 = length(logprices1)
    nbarK_var1 = (n_var1 - K_var1 + 1)/(K_var1)
    nbarJ_var1 = (n_var1 - J_var1 + 1)/(J_var1)
    adj_var1 = n_var1/((K_var1 - J_var1) * nbarK_var1) 
    
    logreturns_K1 = logreturns_J1 = c()
    for (k in 1:K_var1) {
      sel.avg = seq(k, n_var1, K_var1)
      logreturns_K1 = c(logreturns_K1, diff(logprices1[sel.avg]))
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



cfactor_RTSCV <- function(eta = 9) {
  
  # rho = 1
  c1 = pchisq(eta,df=1)/pchisq(eta,df=3) 
  # 
  rho = 0.001
  R = matrix( c(1,rho,rho,1) , ncol = 2 ) 
  int1 <- function(x) {    mvtnorm::dmvnorm(x,sigma=R) }
  num = cubature::adaptIntegrate(int1, c(-3,-3), c(3,3), tol=1e-4)$integral
  int2 <- function(x) {  x[1]*x[2]*mvtnorm::dmvnorm(x,sigma=R) }
  denom = cubature::adaptIntegrate(int2, c(-3,-3), c(3,3), tol=1e-4)$integral
  c2 = rho*num/denom   
  return( (c1+c2)/2 )
}

# Hayashi-Yoshida helper function:
rc.hy <- function(x, y, period = 1,align.by = "seconds", align.period = 1, cts = TRUE, makeReturns = FALSE, ...) {
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
                      type = NULL,                   # Deprectated
                      adj = NULL,                    # Deprectated
                      q = NULL, ...){                     # Deprectated
  # Multiday adjustment: 
  multixts <- multixts(x)
  if(multixts == TRUE) {
    result <- apply.daily(x,rv.kernel,kernel.type,kernel.param,kernel.dofadj,
                         align.by,align.period,cts,makeReturns,type,adj,q)
    return(result)
  } else { #Daily estimation:
    
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
    
    align.period <- .getAlignPeriod(align.period, align.by)         
    cdata <- .convertData(x, cts = cts, makeReturns = makeReturns)
    x <- cdata$data
    x <- .alignReturns(x, align.period)
    type <- kernelCharToInt(kernel.type)
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
  type <- kernelCharToInt(kernel.type)
  .C("kernelEstimator", as.double(x), as.double(y), as.integer(length(x)),
     as.integer(kernel.param), as.integer(ifelse(kernel.dofadj, 1, 0)),
     as.integer(type), ab=double(kernel.param + 1),
     ab2=double(kernel.param + 1),
     ans=double(1),PACKAGE="highfrequency")$ans
}

rKernel <- function(x, type = 0) {
  type <- kernelCharToInt(type)
  .C("justKernel", x = as.double(x), type = as.integer(type), ans = as.double(0), PACKAGE = "highfrequency")$ans
}

kernelCharToInt <- function(type) {
  if (is.character(type) == TRUE) {
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
    
    if(ans==-99) { 
      warning("Invalid Kernel, using Bartlet")
      1
    } else {
      ans     
    }
  } else {
    type
  }
}

rKernel.available <- function() {
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
rv.avg <- function(x, period) { 
  mean(.rv.subsample(x, period))
}

rc.avg <- function( x, y,  period ) {
  mean(.rc.subsample(x, y, period));
}

.rv.subsample <- function(x, period, cts = TRUE, makeReturns = FALSE, ...) {
  cdata <- .convertData(x, cts = cts, makeReturns = makeReturns)
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


.rc.subsample <- function(x, y, period, cts=TRUE, makeReturns=FALSE, ... ) {
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

rv.zero = function(x, period) {  
  ac <- .accum.naive(x=x,y=x,period=period)
  sum((ac*ac)==0)/length(ac)
}  

rc.zero = function(x, y, period) {  
  acy <- .accum.naive(x=y,y=y,period=period)
  acx <- .accum.naive(x=x,y=x,period=period)
  sum((acx*acy)==0)/length(acy)
}  




### ROBUST Two time scale covariance : 
rRTSCov <- function (pdata, cor = FALSE, startIV = NULL, noisevar = NULL, 
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
    if ( nrow(pdata) < (10*K) ) {
      stop("Two time scale estimator uses returns based on prices that are K ticks aways. 
           Please provide a timeseries of at least 10*K" ) 
    } 
    multixts = multixts(pdata); 
    if(multixts){ stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input"); }    
    return(RTSRV(pdata, startIV = startIV, noisevar = noisevar, 
                 K = K, J = J, eta = eta))
  }
  if (n > 1) {
    if ( nrow(pdata[[1]]) < (10*K) ) {
      stop("Two time scale estimator uses returns based on prices that are K ticks aways. 
           Please provide a timeseries of at least 10*K" ) 
    } 
    multixts <- multixts(pdata[[1]]); 
    if (multixts) { 
      stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")
    }
    
    cov <- matrix(rep(0, n * n), ncol = n)
    diagonal <- c()
    if (is.null(K_cov)) { 
      K_cov <- K 
    }
    if (is.null(J_cov)) { 
      J_cov <- J 
    }  
    if (is.null(K_var)) { 
      K_var <- rep(K,n) 
    }
    if (is.null(J_var)) { 
      J_var <- rep(J,n) 
    }        
    for (i in 1:n){ 
      diagonal[i] <- RTSRV(pdata[[i]], startIV = startIV[i], 
                          noisevar = noisevar[i], K = K_var[i], J = J_var[i], 
                          eta = eta)
    }
    diag(cov) <- diagonal
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



## Hayashi Yoshida covariance estimator
rHYCov = function(rdata, cor = FALSE, period = 1, align.by = "seconds", align.period = 1, cts = TRUE, makeReturns = FALSE, makePsd=TRUE, ...) {
  if (!is.list(rdata)){
    stop('The rdata input is not a list. Please provide a list as input for this function. Each list-item should contain the series for one asset.')
  }else{
    n = length(rdata)
    if(n == 1){
      stop('Please provide a list with multiple list-items as input. You cannot compute covariance from a single price series.')      
    }
  }  
  multixts = multixts(rdata[[1]]); 
  if(multixts){ stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")}
  
  cov = matrix(rep(0, n * n), ncol = n)
  diagonal <- c()  
  for (i in 1:n){ 
    diagonal[i] = rCov(rdata[[i]], align.by = align.by, align.period = align.period, makeReturns = makeReturns)
  } 
  diag(cov) = diagonal
  for (i in 2:n){
    for (j in 1:(i - 1)){
      cov[i, j] <- rc.hy(x = rdata[[i]], y = rdata[[j]], period = period, align.by = align.by, 
                         align.period = align.period, cts = cts, makeReturns = makeReturns)
      cov[j, i] <- cov[i, j]    
    }
  }
  
  if (cor == FALSE) {
    if (makePsd == TRUE) {
      cov <- makePsd(cov)
    }
    return(cov)
  }
  if (cor == TRUE){
    invsdmatrix <- try(solve(sqrt(diag(diag(cov)))), silent = F)
    if (!inherits(invsdmatrix, "try-error")) {
      rcor <- invsdmatrix %*% cov %*% invsdmatrix
      if (makePsd == TRUE) {
        rcor <- makePsd(rcor)
      }
      return(rcor)
    }
  }
}  

## Kernel Covariance Estimator: 
rKernelCov = function( rdata, cor=FALSE, kernel.type = "rectangular", kernel.param = 1, 
                       kernel.dofadj = TRUE, align.by = "seconds", align.period = 1, 
                       cts = TRUE, makeReturns = FALSE, type = NULL, adj = NULL, 
                       q = NULL, ...) {
  if (is.list(rdata) == FALSE){ # In case of only one stock this makes sense
    if(is.null(dim(rdata))){  
      n = 1
    } else { 
      n <- dim(rdata)[2] 
    }
    if (n == 1){
      result = rv.kernel(rdata, cor=cor, kernel.type = kernel.type, kernel.param = kernel.param, kernel.dofadj = kernel.dofadj, 
                                     align.by = align.by, align.period = align.period, cts = cts, makeReturns = makeReturns, 
                                     type = type, adj = adj, q = q)
    }
    if (n >  1){ 
      stop("Please provide a list with one list-item per stock as input.")  
    }    
    return(result)    
    #stop('The rdata input is not a list. Please provide a list as input for this function. Each list-item should contain the series for one asset.')
  } else {
    n <- length(rdata);
    if (n == 1) {
      result <- rv.kernel(rdata[[1]], cor=cor,kernel.type = kernel.type, kernel.param = kernel.param, kernel.dofadj = kernel.dofadj, 
                         align.by = align.by, align.period = align.period, cts = cts, makeReturns = makeReturns, 
                         type = type, adj = adj, q = q); return(result);
    }
    
    if (n > 1) {
      multixts <- multixts(rdata[[1]])
      if (multixts == TRUE) { 
        stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")
      }
      
      cov <- matrix(rep(0, n * n), ncol = n);
      diagonal <- c()  
      for (i in 1:n) { 
        diagonal[i] <- rv.kernel(rdata[[i]], cor=cor,kernel.type = kernel.type, kernel.param = kernel.param, kernel.dofadj = kernel.dofadj, 
                                align.by = align.by, align.period = align.period, cts = cts, makeReturns = makeReturns, 
                                type = type, adj = adj, q = q)   
      } 
      diag(cov) <- diagonal
      
      for (i in 2:n){
        for (j in 1:(i - 1)){
          cov[i, j] <- rc.kernel(x = rdata[[i]], y = rdata[[j]], kernel.type = kernel.type, kernel.param = kernel.param, 
                                 kernel.dofadj = kernel.dofadj, align.by = align.by, align.period = align.period,
                                 cts = cts, makeReturns = makeReturns, type = type, adj = adj,q = q)
          cov[j, i] <- cov[i, j]
        }
      }
      
      if(cor == FALSE){
        cov <- makePsd(cov);
        return(cov)
      }
      if(cor == TRUE){
        invsdmatrix <- try(solve(sqrt(diag(diag(cov)))), silent = F)
        if (inherits(invsdmatrix, "try-error") == FALSE) {
          rcor <- invsdmatrix %*% cov %*% invsdmatrix
          return(rcor)
        }
      }
    }
  }
}



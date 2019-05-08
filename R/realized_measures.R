#' An estimator of integrated quarticity from applying the median operator on blocks of three returns.
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#' @description Function returns the medRQ, defined in Andersen et al. (2012).
#'   
#'   Assume there is \eqn{N} equispaced returns in period \eqn{t}. Let \eqn{r_{t,i}} be a return (with \eqn{i=1, \ldots,N}) in period \eqn{t}.
#'   
#'   Then, the medRQ is given by
#'  \deqn{
#'    \mbox{medRQ}_{t}=\frac{3\pi N}{9\pi +72 - 52\sqrt{3}} \left(\frac{N}{N-2}\right) \sum_{i=2}^{N-1} \mbox{med}(|r_{t,i-1}|, |r_{t,i}|, |r_{t,i+1}|)^4
#'   }
#' @param rdata a zoo/xts object containing all returns in period t for one asset.
#' @param align.by a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param align.period an integer, align the tick data to this many [seconds|minutes|hours]. 
#' @param makeReturns boolean, should be TRUE when rdata contains prices instead of returns. FALSE by  default.
#' @param ... additional arguments.
#' 
#' @return numeric
#' 
#' @examples
#' \dontrun{
#' medRQ(rdata = sample_tdata$PRICE, align.by = "minutes", align.period = 5, makeReturns = TRUE)
#' medRQ
#' }
#' @keywords highfrequency medRQ
#' @references Andersen, T. G., D. Dobrev, and E. Schaumburg (2012). Jump-robust volatility estimation using nearest neighbor truncation. Journal of Econometrics, 169(1), 75- 93.
#' @importFrom zoo rollmedian
#' @export
medRQ <- function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE) {
  if (hasArg(data)) {
    rdata <- data
  }
  multixts <- .multixts(rdata)
  if (multixts) {
    result <- apply.daily(rdata, medRQ, align.by, align.period, makeReturns) 
    return(result)
  }
  if (!multixts) {
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata <- .aggregatets(rdata, on = align.by, k = align.period)
    }
    if(makeReturns) {
      rdata <- makeReturns(rdata)
    }
    q <- abs(as.numeric(rdata))
    q <- as.numeric(rollmedian(q, k = 3,align="center"))
    N <- length(q) + 2
    medRQ <- 3 * pi * N / (9 * pi + 72 - 52 * sqrt(3)) * (N / (N-2)) * sum(q^4)
    return(medRQ)
  } 
}

#' An estimator of integrated quarticity from applying the minimum operator on blocks of two returns.
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#' @description Function returns the minRQ, defined in Andersen et al. (2012).
#' 
#' Assume there is \eqn{N} equispaced returns in period \eqn{t}. Let \eqn{r_{t,i}} be a return (with \eqn{i=1, \ldots,N}) in period \eqn{t}.
#' 
#' Then, the minRQ is given by
#' \deqn{
#'   \mbox{minRQ}_{t}=\frac{\pi N}{3 \pi - 8} \left(\frac{N}{N-1}\right) \sum_{i=1}^{N-1} \mbox{min}(|r_{t,i}| ,|r_{t,i+1}|)^4
#' }
#' @param rdata a zoo/xts object containing all returns in period t for one asset.
#' @param align.by a string, align the tick data to "seconds"|"minutes"|"hours"
#' @param align.period an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns boolean, should be TRUE when rdata contains prices instead of returns. FALSE by  default.
#' 
#' @return numeric
#' 
#' @examples
#' \dontrun{
#' minRQ(rdata = sample_tdata$PRICE, align.by = "minutes", align.period = 5, makeReturns = TRUE)
#' minRQ
#' }
#'@references Andersen, T. G., D. Dobrev, and E. Schaumburg (2012). Jump-robust volatility estimation using nearest neighbor truncation. Journal of Econometrics, 169(1), 75- 93.
#' @importFrom zoo as.zoo
#' @importFrom zoo rollapply
#' @export
minRQ <- function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE) {
  if (hasArg(data)) {
    rdata = data
  }
  multixts = .multixts(rdata)
  if (multixts) {
    result <- apply.daily(rdata, minRQ, align.by, align.period, makeReturns)
    return(result)
  }
  if (!multixts) {
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata <- .aggregatets(rdata, on = align.by, k = align.period)
    }
    if(makeReturns) {
      rdata = makeReturns(rdata)
    }
    q     <- as.zoo(abs(as.numeric(rdata)))
    q     <- as.numeric(rollapply(q, width = 2, FUN = min, by = 1, align = "left"))
    N     <- length(q) + 1
    minRQ <- pi * N/(3 * pi - 8)*(N / (N - 1)) * sum(q^4)
    return(minRQ)
  }
}



#' Realized BiPower Covariance
#' 
#' Function returns the Realized BiPower Covariance (rBPCov), 
#'  defined in Barndorff-Nielsen and Shephard (2004).
#'  
#'  Let \eqn{r_{t,i}} be an intraday \eqn{N x 1} return vector and \eqn{i=1,...,M} 
#'  the number of intraday returns.
#'  
#'  The rBPCov is defined as the process whose value at time \eqn{t}
#'  is the \eqn{N}-dimensional square matrix with \eqn{k,q}-th element equal to
#'  \deqn{
#'    \mbox{rBPCov}[k,q]_t = \frac{\pi}{8} \bigg( \sum_{i=2}^{M} 
#'                                               \left|
#'                                                 r_{(k)t,i} + r_{(q)t,i} \right| \ \left| r_{(k)t,i-1} + r_{(q)t,i-1} \right|   \\
#'                                               - \left| r_{(k)t,i}  - r_{(q)t,i} \right| \ \left|
#'                                                 r_{(k)t,i-1} - r_{(q)t,i-1} \right|  \bigg),
#'  }
#'  where \eqn{r_{(k)t,i}} is the
#'  \eqn{k}-th component of the return vector \eqn{r_{i,t}}.
#'  
#' @param rdata a \eqn{(M x N)} matrix/zoo/xts object containing the \eqn{N}
#'    return series over period \eqn{t}, with \eqn{M} observations during \eqn{t}.
#' @param cor boolean, in case it is TRUE, the correlation is returned. FALSE by default.
#' @param align.by a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param align.period an integer, align the tick data to this many [seconds|minutes|hours]. 
#' @param makeReturns boolean, should be TRUE when rdata contains prices instead of returns. FALSE by default.
#' @param makePsd boolean, in case it is TRUE, the positive definite version of rBPCov is returned. FALSE by default.
#' @param ... additional arguments.
#'
#' @return an \eqn{N x N} matrix
#' 
#' @references 
#' Barndorff-Nielsen, O. and N. Shephard (2004). Measuring the impact of
#' jumps in multivariate price processes using bipower covariation. Discussion
#' paper, Nuffield College, Oxford University.
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' 
#' @examples 
#' # Realized Bipower Variance/Covariance for CTS aligned   
#' # at 5 minutes.
#' data(sample_tdata); 
#' data(sample_5minprices_jumps);
#'  
#' # Univariate: 
#' rbpv = rBPCov( rdata = sample_tdata$PRICE, align.by ="minutes", 
#'                 align.period =5, makeReturns=TRUE); 
#' rbpv 
#'  
#' # Multivariate: 
#' rbpc = rBPCov( rdata = sample_5minprices_jumps['2010-01-04'], makeReturns=TRUE,makePsd=TRUE); 
#' rbpc
#'  
#' @keywords volatility
#' @export
rBPCov <- function(rdata, cor = FALSE, align.by = NULL, align.period = NULL, makeReturns = FALSE, makePsd = FALSE,...) {
  if (hasArg(data) == TRUE) { 
    rdata <- data 
  }
  
  # Multiday adjustment: 
  multixts <- .multixts(rdata); 
  if (multixts == TRUE) { 
    if (is.null(dim(rdata))) {  
      n <- 1
    } else { 
      n <- dim(rdata)[2] 
    }
    if (n == 1){ 
      result <- apply.daily(rdata, rBPCov, align.by=align.by,align.period=align.period,makeReturns=makeReturns,makePsd) 
    }
    if (n > 1) { 
      result <- .applygetlist(rdata, rBPCov, cor=cor,align.by=align.by,align.period=align.period,makeReturns=makeReturns,makePsd) 
    }    
    return(result)
  } 
  
  if (!multixts) { #single day code
    if ((!is.null(align.by))&&(!is.null(align.period))) {
      rdata <- .aggregatets(rdata, on = align.by, k = align.period);
    } 
    if (makeReturns) {  
      rdata <- makeReturns(rdata) 
    }  
    if (is.null(dim(rdata))) {
      n <- 1
    } else { 
      n <- dim(rdata)[2]
    }
    
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
        diagonal[i] <- RBPVar(rdata[, i])
      }
      diag(cov) <- diagonal
      for (i in 2:n) {
        for (j in 1:(i - 1)) {
          cov[i, j] = cov[j, i] = RBPCov_bi(rdata[, i], rdata[, j])
        }
      }
      
      if (cor == FALSE){
        if (makePsd == TRUE) {
          cov <- makePsd(cov)
        }
        return(cov)
      }
      if(cor==TRUE){
        sdmatrix <- sqrt(diag(diag(cov)))
        rcor <- solve(sdmatrix) %*% cov %*% solve(sdmatrix)
        if (makePsd == TRUE) {
          rcor <- makePsd(rcor)
        }
        return(rcor)
      }
    } 
  } 
}

#' @keywords internal
RBPCov_bi <- function(ts1,ts2) {
  n <- length(ts1)
  a <- abs(ts1+ts2)
  b <- abs(ts1-ts2)
  first <- as.numeric(a[1:(n-1)])*as.numeric(a[2:n])
  last <- as.numeric(b[1:(n-1)])*as.numeric(b[2:n])
  result <-  (pi/8)*sum(first-last)
  return(result)
}

#' @keywords internal
RBPVar <- function (rdata) {
  if (hasArg(data)) { 
    rdata <- data 
  }
  returns <- as.vector(as.numeric(rdata))
  n <- length(returns)
  rbpvar <- (pi/2) * sum(abs(returns[1:(n-1)]) * abs(returns[2:n]))
  return(rbpvar)
}

#' Realized Covariance
#' 
#' @description Function returns the Realized Covariation (rCov).
#' Let \eqn{r_{t,i}} be an intraday \eqn{N x 1} return vector and \eqn{i=1,...,M}
#' the number of intraday returns.
#' 
#' Then, the rCov is given by
#' \deqn{
#'  \mbox{rCov}_{t}=\sum_{i=1}^{M}r_{t,i}r'_{t,i}.
#'  }
#'  
#' @param rdata a \eqn{(M x N)} matrix/zoo/xts object containing the \eqn{N}
#' return series over period \eqn{t}, with \eqn{M} observations during \eqn{t}.
#' @param cor boolean, in case it is TRUE, the correlation is returned. FALSE by default.
#' @param align.by a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param align.period an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns boolean, should be TRUE when rdata contains prices instead of returns. FALSE by default.
#' @param ... additional arguments.
#' 
#' @return an \eqn{N x N} matrix
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' 
#' @examples 
#' # Realized Variance/Covariance for CTS aligned   
#' # at 5 minutes.
#' data(sample_tdata)
#' data(sample_5minprices_jumps)
#' 
#' # Univariate: 
#' rv = rCov( rdata = sample_tdata$PRICE, align.by ="minutes", 
#'                    align.period =5, makeReturns=TRUE)
#' rv 
#' 
#' # Multivariate: 
#' rc = rCov( rdata = sample_5minprices_jumps['2010-01-04'], makeReturns=TRUE)
#' rc
#' @keywords volatility
#' @export
rCov <- function(rdata, cor = FALSE, align.by=NULL, align.period = NULL, makeReturns = FALSE, ...) {
  if (hasArg(data)) { 
    rdata <- data
  } 
  # Multiday adjustment: 
  multixts <- .multixts(rdata)
  if(multixts){ 
    if(is.null(dim(rdata))) {  
      n <- 1
    } else { 
      n <- dim(rdata)[2]
    }
    if (n == 1) { 
      result <- apply.daily(rdata,rCov,align.by=align.by,align.period=align.period,makeReturns=makeReturns) 
    }
    if (n > 1) { 
      result <- .applygetlist(rdata, rCov, cor=cor, align.by=align.by,align.period=align.period,makeReturns=makeReturns) 
    }    
    return(result)
  } 
  if(!multixts){ #single day code
    if((!is.null(align.by)) && (!is.null(align.period))) {
      rdata = .aggregatets(rdata, on=align.by, k=align.period);
    } 
    if (makeReturns) {  
      rdata <- makeReturns(rdata) 
    }  
    if (is.null(dim(rdata))) {  n = 1
    } else { 
      n <- dim(rdata)[2]
    }
    
    if (n == 1) {
      return(RV(rdata))
    }
    if (n > 1) {
      
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

#' Realized kurtosis of highfrequency return series. 
#' 
#' Function returns Realized kurtosis, defined in Amaya et al. (2011).
#' 
#' Assume there is \eqn{N} equispaced returns in period \eqn{t}. Let \eqn{r_{t,i}} be a return (with \eqn{i=1, \ldots,N}) in period \eqn{t}.
#' 
#' Then, the rKurt is given by
#' \deqn{
#'   \mbox{rKurt}_{t}= \frac{N \sum_{i=1}^{N}(r_{t,i})^4}{RV_{t}^2} 
#'   }
#'  in which \eqn{RV_t:} realized variance
#'   
#' @param rdata a zoo/xts object containing all returns in period t for one asset.
#' @param align.by a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param align.period an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns boolean, should be TRUE when rdata contains prices instead of returns. FALSE by   default.
#'
#' @return numeric
#'
#' @references Amaya, D., Christoffersen, P., Jacobs, K. and Vasquez, A. (2011). Do realized skewness and kurtosis predict the cross-section of equity returns?. CREATES research paper. p. 3-7.
#'
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#' 
#' @examples 
#' data(sample_tdata)
#' rKurt(sample_tdata$PRICE,align.by ="minutes", align.period =5, makeReturns = TRUE)
#' 
#' @keywords highfrequency rKurt
#' @export
rKurt <- function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE) {
  if (hasArg(data) == TRUE) {
    rdata = data
  }
  
  multixts = .multixts(rdata)
  
  if (multixts == TRUE) {
    result = apply.daily(rdata, rKurt, align.by, align.period,
                         makeReturns)
    return(result)
  }
  
  if (!multixts) {
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata <- .aggregatets(rdata, on = align.by, k = align.period)
    }
    if (makeReturns == TRUE) {
      rdata <- makeReturns(rdata)
    }
    
    q <- as.numeric(rdata)
    N <- length(q)
    
    rv <- RV(rdata)
    
    rkurt <- N * sum(q^4) / rv^(2)
    
    return(rkurt)
    
  }
}

#' Realized skewness of highfrequency return series.
#'
#' @description Function returns Realized skewness, defined in Amaya et al. (2011).
#' 
#' Assume there is \eqn{N} equispaced returns in period \eqn{t}. Let \eqn{r_{t,i}} be a return (with \eqn{i=1, \ldots,N}) in period \eqn{t}.
#' 
#' Then, the rSkew is given by
#'   \deqn{
#'     \mbox{rSkew}_{t}= \frac{\sqrt{N} \sum_{i=1}^{N}(r_{t,i})^3}{RV_{t}^{3/2}} 
#'   }
#'   
#' in which
#' \eqn{RV_{t}:} realized variance
#' 
#' @param rdata a zoo/xts object containing all returns in period t for one asset.
#' @param align.by a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param align.period an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns boolean, should be TRUE when rdata contains prices instead of returns. FALSE by   default.
#' 
#' @return numeric
#' 
#' @references  Amaya, D., Christoffersen, P., Jacobs, K. and Vasquez, A. (2011). Do realized skewness and kurtosis predict the cross-section of equity returns?. CREATES research paper. p. 3-7.
#'
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#' 
#' @examples 
#' data(sample_tdata)
#' rSkew(sample_tdata$PRICE,align.by ="minutes", align.period =5, makeReturns = TRUE)
#' 
#' @keywords highfrequency rSkew
#' @export
rSkew <- function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE) {
  if (hasArg(data)) {
    rdata <- data
  }
  
  multixts = .multixts(rdata)
  
  if (multixts) {
    result <- apply.daily(rdata, rSkew, align.by, align.period, makeReturns)
    return(result)
  }
  
  if (!multixts) {
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata <- .aggregatets(rdata, on = align.by, k = align.period)
    }
    if (makeReturns) {
      rdata <- makeReturns(rdata)
    }
    
    q <-as.numeric(rdata)
    N <- length(q)
    
    rv <- RV(rdata)
    rSkew <- sqrt(N)*sum(q^3)/rv^(3/2)
    
    return(rSkew)
  }
}

#' Realized semivariance of highfrequency return series. 
#' @description Function returns realized semivariances, defined in Barndorff-Nielsen et al. (2008).
#' 
#' Function returns two outcomes: 1.Downside realized semivariance and 2.Upside realized semivariance.
#' 
#' Assume there is \eqn{N} equispaced returns in period \eqn{t}. Let \eqn{r_{t,i}} be a return (with \eqn{i=1, \ldots,N}) in period \eqn{t}.
#' 
#' Then, the rSV is given by
#' \deqn{
#'   \mbox{rSVdownside}_{t}= \sum_{i=1}^{N} (r_{t,i})^2  \ \times \ I [ r_{t,i} <0 ]
#' }
#'   \deqn{
#'   \mbox{rSVupside}_{t}= \sum_{i=1}^{N} (r_{t,i})^2 \ \times \ I [ r_{t,i} >0 ]
#' }
#' @param rdata a zoo/xts object containing all returns in period t for one asset.
#' @param align.by a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param align.period an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns boolean, should be TRUE when rdata contains prices instead of returns. FALSE by   default.
#' @return list with to arguments. The realized positive and negative semivariance.
#' @examples 
#' \dontrun{
#' data(sample_tdata)
#' rSV(sample_tdata$PRICE,align.by ="minutes", align.period =5, makeReturns = TRUE)
#' }
#' @references Barndorff-Nielsen, O.E., Kinnebrock, S. and Shephard N. (2008). Measuring downside risk - realized semivariance. CREATES research paper. p. 3-5.
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#' @keywords  highfrequency rSV
#' @export
rSV <- function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE) {
  if (hasArg(data)) {
    rdata = data
  }
  
  multixts <-  .multixts(rdata)
  
  if (multixts) {
    result <- apply.daily(rdata, rSV, align.by, align.period, makeReturns)
    return(result)
  }
  
  if (!multixts) {
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata <- .aggregatets(rdata, on = align.by, k = align.period)
    }
    if (makeReturns)  {
      rdata <- makeReturns(rdata)
    }
    
    q <- as.numeric(rdata)
    select.down <- rdata < 0
    select.up <- rdata > 0
    
    rSVd <- sum(q[select.down]^2)
    rSVu <- sum(q[select.up]^2)
    
    out <- list(rSVdownside = rSVd, rSVupside = rSVu)
    return(out)
  }
}


#' An estimator of realized variance.
#' @param rdata a zoo/xts object containing all returns in period t for one asset.
#' 
#' @return numeric
#' 
#' @keywords highfrequency RV
#' @export
RV <- function(rdata) {
  if (hasArg(data)) { 
    rdata = data 
  }
  returns <- as.numeric(rdata)
  RV <- sum(returns^2)
  return(RV)
}

#' Realized quarticity of highfrequency return series. 
#' @description  Function returns the rQuar, defined in Andersen et al. (2012).
#'
#' Assume there is \eqn{N} equispaced returns in period \eqn{t}. Let \eqn{r_{t,i}} be a return (with \eqn{i=1, \ldots,N}) in period \eqn{t}.
#'  
#'  Then, the rQuar is given by
#'  \deqn{
#'    \mbox{rQuar}_{t}=\frac{N}{3} \sum_{i=1}^{N} \mbox(r_{t,i}^4)
#'  }
#'  
#' @param rdata a zoo/xts object containing all returns in period t for one asset.
#' @param align.by a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param align.period an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns boolean, should be TRUE when rdata contains prices instead of returns. FALSE by default.
#' 
#' @return numeric
#'
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#' @references  Andersen, T. G., D. Dobrev, and E. Schaumburg (2012). Jump-robust volatility estimation using nearest neighbor truncation. Journal of Econometrics, 169(1), 75- 93.
#' @examples 
#' \dontrun{
#' data(sample_tdata)
#' rQuar(rdata= sample_tdata$PRICE, align.by= "minutes", align.period =5, makeReturns= TRUE)
#' }
#' @keywords  highfrequency rQuar
#' @export
rQuar <- function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE) {
  if (hasArg(data)) {
    rdata <- data
  }
  multixts = .multixts(rdata)
  if (multixts) {
    result <- apply.daily(rdata, rQuar, align.by, align.period, makeReturns)
    return(result)
  }
  if (!multixts) {
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata <- .aggregatets(rdata, on = align.by, k = align.period)
    }
    if (makeReturns == TRUE) {
      rdata <- makeReturns(rdata)
    }
    q     <- as.numeric(rdata)
    N     <- length(q) + 1
    rQuar <- N/3 * sum(q^4)
    return(rQuar)
  }
}



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
  
  # self-reference for multi-day input
  if (checkMultiDays(rdata) == TRUE) {
    result <- apply.daily(rdata, medRQ, align.by, align.period, makeReturns) 
    return(result)
  } else {
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata <- aggregatets(rdata, on = align.by, k = align.period)
    }
    if (makeReturns == TRUE) {
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
  
  # self-reference for multi-day input
  if (checkMultiDays(rdata) == TRUE) {
    result <- apply.daily(rdata, minRQ, align.by, align.period, makeReturns)
    return(result)
  } else {
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata <- aggregatets(rdata, on = align.by, k = align.period)
    }
    if (makeReturns == TRUE) {
      rdata = makeReturns(rdata)
    }
    q     <- as.zoo(abs(as.numeric(rdata)))
    q     <- as.numeric(rollapply(q, width = 2, FUN = min, by = 1, align = "left"))
    N     <- length(q) + 1
    minRQ <- pi * N/(3 * pi - 8)*(N / (N - 1)) * sum(q^4)
    return(minRQ)
  }
}

#' minRV
#' 
#' @description Function returns the minRV, defined in Andersen et al. (2009).
#' 
#' Let \eqn{r_{t,i}} be a return (with \eqn{i=1,\ldots,M}) in period \eqn{t}.
#' 
#' Then, the minRV is given by
#' \deqn{
#' \mbox{minRV}_{t}=\frac{\pi}{\pi - 2}\left(\frac{M}{M-1}\right) \sum_{i=1}^{M-1} \mbox{min}(|r_{t,i}| ,|r_{t,i+1}|)^2
#' }
#' 
#' @param rdata a zoo/xts object containing all returns in period t for one asset.
#' @param align.by a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param align.period an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns boolean, should be TRUE when rdata contains prices instead of returns. FALSE by default.
#' 
#' @return numeric
#' 
#' @references 
#' Andersen, T. G., D. Dobrev, and E. Schaumburg (2012). Jump-robust volatility estimation using nearest neighbor truncation. Journal of Econometrics, 169 (1), 75-93.
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' 
#' @examples
#' data(sample_tdata)
#' minrv <- minRV(rdata = sample_tdata$PRICE, align.by = "minutes",
#'                align.period = 5, makeReturns = TRUE)
#' minrv 
#' 
#' @keywords volatility
#' @export
minRV <- function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE){
  
  # self-reference for multi-day input
  if (checkMultiDays(rdata) == TRUE) {
    result <- apply.daily(rdata, minRV, align.by, align.period, makeReturns)
    return(result)
  } else {
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata <- aggregatets(rdata, on = align.by, k = align.period)
    } 
    if (makeReturns) {
      rdata <- makeReturns(rdata)
    }  
    q <- abs(rdata)#as.zoo(abs(as.numeric(rdata))) #absolute value
    q <- rollapply(q, width = 2, FUN = min, by = 1, align = "left", by.column = TRUE, fill = NULL)
    N <- dim(q)[1] + 1 #number of obs because of fill = NULL
    minrv <- (pi/(pi - 2)) * (N/(N - 1)) * colSums(q^2, na.rm = TRUE)
    return(minrv) 
  }  
}  

#' medRV
#' 
#' @description 
#' Function returns the medRV, defined in Andersen et al. (2009).
#' 
#' Let \eqn{r_{t,i}} be a return (with \eqn{i=1,\ldots,M}) in period \eqn{t}.
#' 
#' Then, the medRV is given by
#' \deqn{
#'  \mbox{medRV}_{t}=\frac{\pi}{6-4\sqrt{3}+\pi}\left(\frac{M}{M-2}\right) \sum_{i=2}^{M-1} \mbox{med}(|r_{t,i-1}|,|r_{t,i}|, |r_{t,i+1}|)^2
#' }
#'  
#' @param rdata a zoo/xts object containing all returns in period t for one asset.
#' @param align.by a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param align.period an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns boolean, should be TRUE when rdata contains prices instead of returns. FALSE by   default.
#'  
#' @details
#' The medRV belongs to the class of realized volatility measures in this package
#' that use the series of high-frequency returns \eqn{r_{t,i}} of a day \eqn{t} 
#' to produce an ex post estimate of the realized volatility of that day \eqn{t}. 
#' medRV is designed to be robust to price jumps. 
#' The difference between RV and medRV is an estimate of the realized jump 
#' variability. Disentangling the continuous and jump components in RV 
#' can lead to more precise volatility forecasts, 
#' as shown in Andersen et al. (2007) and Corsi et al. (2010).
#' 
#' @return numeric
#' 
#' @references Andersen, T. G., D. Dobrev, and E. Schaumburg (2012). Jump-robust volatility estimation using nearest neighbor truncation. Journal of Econometrics, 169 (1), 75-93.
#' 
#' Andersen, T.G., T. Bollerslev, and F. Diebold (2007). Roughing it up: including jump components in the measurement, modelling and forecasting of return volatility. The Review of Economics and Statistics 89 (4), 701-720.
#' 
#' Corsi, F., D. Pirino, and R. Reno (2010). Threshold Bipower Variation and the Impact of Jumps on Volatility Forecasting. Journal of Econometrics 159 (2), 276-288.
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' 
#' @examples 
#' data(sample_tdata);
#' medrv <- medRV(rdata = sample_tdata$PRICE, align.by = "minutes", 
#'                align.period = 5, makeReturns = TRUE)
#' medrv 
#'  
#' @keywords volatility
#' @export
medRV <- function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE){
  
  # self-reference for multi-day input
  if (checkMultiDays(rdata) == TRUE) {
    result <- apply.daily(rdata, medRV, align.by, align.period, makeReturns)
    return(result)
  } else {
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata <- aggregatets(rdata, on = align.by, k = align.period)
    }
    if (makeReturns == TRUE) {
      rdata <- makeReturns(rdata)
    }
    
    q <- abs(as.numeric(rdata)) #absolute value
    q <- as.numeric(rollmedian(q, k=3, align="center"))
    N <- length(q) + 2
    medrv <- (pi / (6 - 4 * sqrt(3) + pi)) * (N/(N - 2)) * sum(q^2)
    return(medrv)
  }
}

#' Modulated Realized Covariance (MRC): Return univariate or multivariate preaveraged estimator.  
#' 
#' @description Function returns univariate or multivariate preaveraged estimator, as defined in Hautsch and Podolskij (2013). 
#'
#' @param pdata a list. Each list-item contains an xts object with the intraday price data of a stock.
#' @param pairwise boolean, should be TRUE when refresh times are based on pairs of assets. FALSE by default.
#' @param makePsd boolean, in case it is TRUE, the positive definite version of MRC is returned. FALSE by default.
#' 
#' @return an \eqn{d x d} matrix
#' 
#' @details 
#'   In practice, market microstructure noise leads to a departure from the pure semimartingale model. We consider the process \eqn{Y} in period \eqn{\tau}: 
#'     \deqn{
#'       \mbox{Y}_{\tau} = X_{\tau} + \epsilon_{\tau}
#'     }
#'   where, the observed \eqn{d} dimensional log-prices are the sum of underlying Brownian semimartingale process \eqn{X} and a noise term \eqn{\epsilon_{\tau}}. 
#'   
#'   \eqn{\epsilon_{\tau}} is an i.i.d process with \eqn{X}. 
#'   
#'   It is intuitive that under mean zero i.i.d. microstructure noise some form of smoothing of the observed log-price should tend to diminish the impact of the noise. Effectively, we are going to approximate a continuous function by an average of observations of Y in a neighborhood, the noise being averaged away. 
#'   
#'   Assume there is \eqn{N} equispaced returns in period \eqn{\tau} of a list (after refeshing data). Let \eqn{r_{\tau_i}} be a return (with \eqn{i=1, \ldots,N}) of an asset in period \eqn{\tau}. Assume there is \eqn{d} assets. 
#'   
#'   In order to define the univariate pre-averaging estimator, we first define the pre-averaged returns as
#'   \deqn{
#'     \bar{r}_{\tau_j}^{(k)}= \sum_{h=1}^{k_N-1}g\left(\frac{h}{k_N}\right)r_{\tau_{j+h}}^{(k)}
#'   }
#'   where g is a non-zero real-valued function \eqn{g:[0,1]} \eqn{\rightarrow} \eqn{R} given by \eqn{g(x)} = \eqn{\min(x,1-x)}. \eqn{k_N} is a sequence of integers satisfying  \eqn{\mbox{k}_{N} = \lfloor\theta N^{1/2}\rfloor}. We use \eqn{\theta = 0.8} as recommended in Hautsch & Podolskij (2013). The pre-averaged returns are simply a weighted average over the returns in a local window. This averaging diminishes the influence of the noise. The order of the window size \eqn{k_n} is chosen to lead to optimal convergence rates. The pre-averaging estimator is then simply the analogue of the Realized Variance but based on pre-averaged returns and an additional term to remove bias due to noise
#'   \deqn{
#'     \hat{C}= \frac{N^{-1/2}}{\theta \psi_2}\sum_{i=0}^{N-k_N+1}  (\bar{r}_{\tau_i})^2-\frac{\psi_1^{k_N}N^{-1}}{2\theta^2\psi_2^{k_N}}\sum_{i=0}^{N}r_{\tau_i}^2
#'   }
#'   with
#'   \deqn{
#'     \psi_1^{k_N}= k_N \sum_{j=1}^{k_N}\left(g\left(\frac{j+1}{k_N}\right)-g\left(\frac{j}{k_N}\right)\right)^2,\quad 
#'   }
#'   \deqn{
#'     \psi_2^{k_N}= \frac{1}{k_N}\sum_{j=1}^{k_N-1}g^2\left(\frac{j}{k_N}\right).
#'   }
#'   \deqn{
#'     \psi_2= \frac{1}{12}
#'   }
#'   The multivariate counterpart is very similar. The estimator is called the Modulated Realized Covariance (MRC) and is defined as
#'   \deqn{
#'     \mbox{MRC}= \frac{N}{N-k_N+2}\frac{1}{\psi_2k_N}\sum_{i=0}^{N-k_N+1}\bar{\boldsymbol{r}}_{\tau_i}\cdot \bar{\boldsymbol{r}}'_{\tau_i} -\frac{\psi_1^{k_N}}{\theta^2\psi_2^{k_N}}\hat{\Psi}
#' }
#' where \eqn{\hat{\Psi}_N = \frac{1}{2N}\sum_{i=1}^N \boldsymbol{r}_{\tau_i}(\boldsymbol{r}_{\tau_i})'}. It is a bias correction to make it consistent. However, due to this correction, the estimator is not ensured PSD. An alternative is to slightly enlarge the bandwidth such that \eqn{\mbox{k}_{N} = \lfloor\theta N^{1/2+\delta}\rfloor}. \eqn{\delta = 0.1} results in a consistent estimate without the bias correction and a PSD estimate, in which case:
#'   \deqn{
#'     \mbox{MRC}^{\delta}= \frac{N}{N-k_N+2}\frac{1}{\psi_2k_N}\sum_{i=0}^{N-k_N+1}\bar{\boldsymbol{r}}_i\cdot \bar{\boldsymbol{r}}'_i
#' }
#' 
#' @references Hautsch, N., & Podolskij, M. (2013). Preaveraging-Based Estimation of Quadratic Variation in the Presence of Noise and Jumps: Theory, Implementation, and Empirical Evidence. Journal of Business & Economic Statistics, 31(2), 165-183.
#' 
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#' 
#' @examples 
#' a <- list(sample_5minprices_jumps["2010-01-04",1], sample_5minprices_jumps["2010-01-04",2])
#' MRC(a, pairwise = TRUE, makePsd = TRUE)
#' 
#' @keywords highfrequency preaveraging
#' @export
MRC <- function(pdata, pairwise = FALSE, makePsd = FALSE) {
  
  if (is.list(pdata) == FALSE) {
    n <- 1
  } else {
    n <- length(pdata)
  }
  if (n == 1) {
    multixts = multixts(pdata);
    if (multixts == TRUE) {
      stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")
    }
    mrc <- .crv(pdata)
  }
  
  if (n > 1) {
    multixts <- multixts(pdata[[1]]);
    if (multixts == TRUE) {
      stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")
    }
    
    if (pairwise == TRUE) {
      cov <- matrix(rep(0, n * n), ncol = n)
      diagonal = c()
      for (i in 1:n) {
        diagonal[i] <- .crv(pdata[[i]])
      }
      diag(cov) <- diagonal
      
      for (i in 2:n) {
        for (j in 1:(i - 1)) {
          cov[i, j] = cov[j, i] = .preav_bi(pdata[[i]], pdata[[j]])
        }
      }
      
      mrc <- cov
      
      if (makePsd == TRUE) {
        mrc <- makePsd(mrc)
      }
      
    } else {
      x     <- refreshTime(pdata)
      N     <- nrow(x)
      theta <- 0.8 #recommendation by Hautsch and Podolskij
      kn    <- floor(theta * sqrt(N))
      
      ##psi:
      psi1 <- 1
      psi2 <- 1 / 12
      
      psi1kn <- kn * sum((.gfunction((1:kn)/kn) - .gfunction(((1:kn) - 1) / kn))^2 )
      psi2kn <- 1 / kn * sum(.gfunction((1:kn) / kn)^2)
      
      preavreturn <- c()
      for (i in 1:ncol(x)) {
        preavreturn <- cbind(preavreturn , .hatreturn(x[,i],kn) )
      }
      
      S <- rCov(preavreturn)
      
      mrc <- N / (N - kn + 2) * 1/(psi2 * kn) * S
      
      if (makePsd == TRUE) {
        mrc <- makePsd(mrc)
      }
    }
  }
  return(mrc)
} 

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
#' @param RCOVestimator can be chosen among realized covariance estimators: rCov, rAVGCov, rBPCov, rHYCov, rKernelCov, rOWCov, rRTSCov, rThresholdCov and rTSCov. rCov by default.
#' @param RVestimator can be chosen among realized variance estimators: RV, minRV and medRV. RV by default. In case of missing RVestimator, RCOVestimator function applying for rindex will be used.
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
#' a <- sample_5minprices_jumps['2010-01-04', 1]
#' b <- sample_5minprices_jumps['2010-01-04', 2]
#' rBeta(a, b, RCOVestimator = "rBPCov", RVestimator = "minRV", makeReturns = TRUE)
#' 
#' @keywords highfrequency rBeta
#' @importFrom methods hasArg
#' @importFrom utils data
#' @export
rBeta <- function(rdata, rindex, RCOVestimator = "rCov", RVestimator = "RV", makeReturns = FALSE) {
  if (hasArg(data)) {
    rdata <- data
  }
  
  if (RCOVestimator != "rRTSCov" & RCOVestimator != "rTSCov" &  makeReturns) {
    rdata <- makeReturns(rdata)
    rindex <- makeReturns(rindex)
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
  
  multixts <- multixts(rdata)
  
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
#'
#' @return an \eqn{N x N} matrix or a list of matrices if the time period spans multiple days
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
#' rbpv = rBPCov(rdata = sample_tdata$PRICE, align.by ="minutes", 
#'                 align.period =5, makeReturns=TRUE); 
#' rbpv 
#'  
#' # Multivariate: 
#' rbpc = rBPCov(rdata = sample_5minprices_jumps['2010-01-04'], makeReturns = TRUE, makePsd = TRUE)
#' rbpc
#'  
#' @keywords volatility
#' @export
rBPCov <- function(rdata, cor = FALSE, align.by = NULL, align.period = NULL, makeReturns = FALSE, makePsd = FALSE) {
  
  # self-reference for multi-day input
  if (checkMultiDays(rdata) == TRUE) { 
    if (is.null(dim(rdata))) {  
      n <- 1
    } else { 
      n <- dim(rdata)[2] 
    }
    if (n == 1){ 
      result <- apply.daily(rdata, rBPCov, align.by=align.by,align.period=align.period,makeReturns=makeReturns, makePsd) 
    }
    if (n > 1) { 
      result <- applyGetList(rdata, rBPCov, cor=cor,align.by=align.by,align.period=align.period, makeReturns = makeReturns, makePsd) 
    }    
    return(result)
  } else { #single day code
    if ((!is.null(align.by))&&(!is.null(align.period))) {
      rdata <- aggregatets(rdata, on = align.by, k = align.period);
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
      
      rdata  <- as.matrix(rdata);
      n <- dim(rdata)[2]
      cov <- matrix(rep(0, n * n), ncol = n)
      diagonal <- c()
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
#' In case of a matrix, no multi-day adjustment is possible.
#' @param cor boolean, in case it is TRUE, the correlation is returned. FALSE by default.
#' @param align.by a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param align.period an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns boolean, should be TRUE when rdata contains prices instead of returns. FALSE by default.
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
#' rv = rCov(rdata = sample_tdata$PRICE, align.by = "minutes", 
#'                    align.period = 5, makeReturns = TRUE)
#' rv 
#' 
#' # Multivariate: 
#' rc = rCov(rdata = sample_5minprices_jumps['2010-01-04'], makeReturns=TRUE)
#' rc
#' @keywords volatility
#' @export
rCov <- function(rdata, cor = FALSE, align.by = NULL, align.period = NULL, makeReturns = FALSE) {
  
  # Multiday adjustment: 
  if (checkMultiDays(rdata) == TRUE) { 
    if (is.null(dim(rdata))) {  
      n <- 1
    } else { 
      n <- dim(rdata)[2]
    }
    if (n == 1) { 
      result <- apply.daily(rdata, rCov, align.by = align.by, align.period = align.period, makeReturns = makeReturns) 
    }
    if (n > 1) { 
      result <- applyGetList(rdata, rCov, cor=cor, align.by = align.by, align.period = align.period, makeReturns = makeReturns) 
    }    
    return(result)
  } else {
    #single day code
    if((!is.null(align.by)) && (!is.null(align.period))) {
      rdata <- aggregatets(rdata, on = align.by, k = align.period)
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
      
      rdata <- as.matrix(rdata)
      covariance <- t(rdata) %*% rdata
      if (cor == FALSE) {
        return(covariance)
      }
      if (cor == TRUE){
        sdmatrix <- sqrt(diag(diag(covariance)));
        rcor <- solve(sdmatrix) %*% covariance %*% solve(sdmatrix)
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
#' rKurt(sample_tdata$PRICE, align.by = "minutes", align.period = 5, makeReturns = TRUE)
#' 
#' @keywords highfrequency rKurt
#' @importFrom xts apply.daily
#' @export
rKurt <- function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE) {
  
  # self-reference for multi-day input
  if (checkMultiDays(rdata) == TRUE) { 
    result <- apply.daily(rdata, rKurt, align.by, align.period, makeReturns)
    return(result)
  } else {
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata <- aggregatets(rdata, on = align.by, k = align.period)
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


#' Realized multipower variation (MPV), an estimator of integrated power variation. 
#' 
#' @description Function returns the rMPV, defined in Andersen et al. (2012).
#'   
#'   Assume there is \eqn{N} equispaced returns in period \eqn{t}. Let \eqn{r_{t,i}} be a return (with \eqn{i=1, \ldots,N}) in period \eqn{t}.
#'   
#'   Then, the rMPV is given by
#'   \deqn{
#'     \mbox{rMPV}_{N}(m,p)= d_{m,p} \frac{N^{p/2}}{N-m+1} \sum_{i=1}^{N-m+1}|r_{t,i}|^{p/m} \ldots |r_{t,i+m-1}|^{p/m}
#'   }
#'   
#'   in which
#'   
#'   \eqn{d_{m,p} = \mu_{p/m}^{-m}}:
#'     
#'     \eqn{m}: the window size of return blocks;
#'   
#'   \eqn{p}: the power of the variation;
#'   
#'   and \eqn{m} > \eqn{p/2}.
#' 
#' @param rdata a zoo/xts object containing all returns in period t for one asset.
#' @param m the window size of return blocks. 2 by default.
#' @param p the power of the variation. 2 by default.
#' @param align.by a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param align.period an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns boolean, should be TRUE when rdata contains prices instead of returns. FALSE by  default.
#'
#' @return numeric
#' 
#' @references Andersen, T. G., D. Dobrev, and E. Schaumburg (2012). Jump-robust volatility estimation using nearest neighbor truncation. Journal of Econometrics, 169(1), 75- 93.
#'
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#' 
#' @examples
#' data(sample_tdata)
#' rMPV(sample_tdata$PRICE, m = 2, p = 3, align.by = "minutes", align.period = 5, makeReturns = TRUE)
#' 
#' @keywords highfrequency rMPV
#' @export
rMPV <- function(rdata, m = 2, p = 2, align.by = NULL, align.period = NULL, makeReturns = FALSE) {
  
  # self-reference for multi-day input
  if (checkMultiDays(rdata) == TRUE) { 
    result <- apply.daily(rdata, rMPV, align.by, align.period, makeReturns)
    return(result)
  } else {
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata <-aggregatets(rdata, on = align.by, k = align.period)
    }
    if (makeReturns) {
      rdata <- makeReturns(rdata)
    }
    
    if (m > p/2) { 
      m <- as.numeric(m) ##m> p/2
      p <- as.numeric(p)
      q <- as.numeric(rdata)
      q <- abs(rollapply(q,width=m,FUN=prod,align="left"))
      N <- length(rdata)
      
      dmp <- (2^((p/m)/2) * gamma((p/m + 1)/2) / gamma(1/2))^(-m)
      
      rmpv <- dmp * N^(p/2) / (N - m + 1) * sum(q^(p/m))
      return(rmpv)
    } else{ 
      warning("Please supply m>p/2 for the arguments m and p")
    }
    
  }
}  

#' Realized Outlyingness Weighted Covariance
#' 
#' @description Function returns the Realized Outlyingness Weighted Covariance, defined in Boudt et al. (2008).
#' 
#' Let \eqn{r_{t,i}}, for \eqn{i=1,...,M} be a sample
#' of \eqn{M} high-frequency \eqn{(N x 1)} return vectors and \eqn{d_{t,i}}
#' their outlyingness given by the squared Mahalanobis distance between
#' the return vector and zero in terms of the reweighted MCD covariance
#' estimate based on these returns.
#' 
#' Then, the rOWCov is given by
#' \deqn{
#' \mbox{rOWCov}_{t}=c_{w}\frac{\sum_{i=1}^{M}w(d_{t,i})r_{t,i}r'_{t,i}}{\frac{1}{M}\sum_{i=1}^{M}w(d_{t,i})},
#' }
#' The weight  \eqn{w_{i,\Delta}} is one if the multivariate jump test statistic for \eqn{r_{i,\Delta}} in Boudt et al. (2008) is less
#' than the 99.9\% percentile of the chi-square distribution with \eqn{N} degrees of freedom and zero otherwise. 
#' The scalar \eqn{c_{w}} is a correction factor ensuring consistency of the rOWCov for the Integrated Covariance, 
#' under the Brownian Semimartingale with Finite Activity Jumps model. 
#' 
#' @param rdata a \eqn{(M x N)} matrix/zoo/xts object containing the \eqn{N}
#' return series over period \eqn{t}, with \eqn{M} observations during \eqn{t}.
#' @param cor boolean, in case it is TRUE, the correlation is returned. FALSE by default.
#' @param align.by a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param align.period an integer, align the tick data to this many [seconds|minutes|hours]. 
#' @param makeReturns boolean, should be TRUE when rdata contains prices instead of returns. FALSE by default.
#' @param seasadjR a \eqn{(M x N)} matrix/zoo/xts object containing 
#' the seasonaly adjusted returns. This is an optional argument.
#' @param wfunction determines whether 
#' a zero-one weight function (one if no jump is detected based on \eqn{d_{t,i}} and 0 otherwise)
#' or 
#' Soft Rejection ("SR") weight function is to be used.
#' By default a zero-one weight function (wfunction = "HR") is used.
#' @param alphaMCD a numeric parameter, controlling the size of 
#' the subsets over which the determinant is minimized. 
#' Allowed values are between 0.5 and 1 and 
#'the default is 0.75. See Boudt et al. (2008) or the \code{covMcd} function in the
#'robustbase package.
#' @param alpha is a parameter between 0 en 0.5, 
#'that determines the rejection threshold value 
#'(see Boudt et al. (2008) for details).
#' 
#' @return an \eqn{N x N} matrix
#' 
#' @details 
#' Advantages of the rOWCov compared to the \code{\link{rBPCov}} include a higher statistical efficiency, positive semidefiniteness and affine equivariance.
#' However, the rOWCov suffers from a curse of dimensionality.
#' Indeed, the rOWCov gives a zero weight to a return vector
#' if at least one of the components is affected by a jump.
#' In the case of independent jump occurrences, the average proportion of observations
#' with at least one component being affected by jumps increases fast with the dimension
#' of the series. This means that a potentially large proportion of the returns receives
#' a zero weight, due to which the rOWCov can have a low finite sample efficiency in higher dimensions
#' 
#' @references Boudt, K., C. Croux, and S. Laurent (2008). Outlyingness weighted covariation. Mimeo.
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' 
#' @examples 
#' # Realized Outlyingness Weighted Variance/Covariance for CTS aligned   
#' # at 5 minutes.
#' data(sample_tdata)
#' data(sample_5minprices_jumps)
#' 
#' # Univariate: 
#' rvoutw <- rOWCov(rdata = sample_tdata$PRICE, align.by = "minutes",
#'                    align.period = 5, makeReturns = TRUE)
#' rvoutw 
#' 
#' # Multivariate: 
#' rcoutw <- rOWCov(rdata = sample_5minprices_jumps['2010-01-04'], makeReturns = TRUE)
#' rcoutw
#' 
#' @keywords volatility
#' @export
rOWCov <- function (rdata, cor = FALSE, align.by = NULL, align.period = NULL, makeReturns = FALSE, seasadjR = NULL, wfunction = "HR" , alphaMCD = 0.75, alpha = 0.001){
  
  if (is.null(seasadjR) == TRUE) { 
    seasadjR <- rdata 
  }
  multixts <- multixts(rdata)
  if (multixts == TRUE) { 
    stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")
  }
  
  # Aggregate:
  if ((!is.null(align.by))&&(!is.null(align.period))) {
    rdata <- aggregatets(rdata, on = align.by, k = align.period)
    seasadjR <- aggregatets(seasadjR, on = align.by, k = align.period)
  }     
  if (makeReturns == TRUE) { 
    rdata <- makeReturns(rdata)
    if (is.null(seasadjR) == FALSE) { 
      seasadjR <- makeReturns(seasadjR)
    } 
  }
  
  if (is.null(dim(rdata)) == TRUE) { 
    n <- 1 
  } else { 
    n <- dim(rdata)[2]
  }        
  
  if (n == 1) { 
    return(ROWVar(rdata, seasadjR = seasadjR, wfunction = wfunction, alphaMCD = alphaMCD, alpha = alpha ))
  }
  
  if (n > 1) { 
    if ((dim(rdata)[2] < 2)) {
      stop("Your rdata object should have at least 2 columns")
    }
    
    rdata <- as.matrix(rdata)
    seasadjR <- as.matrix(seasadjR)
    intraT <- nrow(rdata)
    N <- ncol(rdata)
    perczeroes <- apply(seasadjR, 2, function(x) sum(1 * (x == 0))) / intraT
    select <- c(1:N)[perczeroes < 0.5]
    seasadjRselect <- seasadjR[, select]
    N <- ncol(seasadjRselect)
    MCDobject <- try(robustbase::covMcd(x = seasadjRselect, alpha = alphaMCD))
    
    if (length(MCDobject$raw.mah) > 1) {
      betaMCD  <- 1-alphaMCD
      asycor   <- betaMCD / pchisq(qchisq(betaMCD, df = N), df = N+2)
      MCDcov   <- (asycor * t(seasadjRselect[MCDobject$best,]) %*% seasadjRselect[MCDobject$best,]) / length(MCDobject$best)
      invMCDcov <- solve(MCDcov) 
      outlyingness <- rep(0,intraT)
      for (i in 1:intraT) { 
        outlyingness[i] = matrix(seasadjRselect[i,], ncol = N) %*% invMCDcov %*% matrix(seasadjRselect[i,],nrow=N)    
      }
    } else {
      print(c("MCD cannot be calculated")); stop();
    }
    k <- qchisq(p = 1 - alpha, df = N)
    outlierindic <- outlyingness > k
    weights <- rep(1, intraT)
    
    if (wfunction == "HR") {
      weights[outlierindic] = 0
      wR <- sqrt(weights) * rdata
      covariance <- (conHR(di = N, alpha = alpha) * t(wR) %*% wR) / mean(weights)
      if (cor == FALSE) {
        return(covariance)
      } else {
        sdmatrix = sqrt(diag(diag(covariance)))
        inv_matrix <- solve(sdmatrix)
        rcor <- inv_matrix %*% covariance %*% inv_matrix
        return(rcor)
      }
    }
    if (wfunction == "SR") {
      weights[outlierindic] <- k/outlyingness[outlierindic]
      wR <- sqrt(weights) * rdata
      covariance <- (conhuber(di = N, alpha = alpha) * t(wR) %*% wR) / mean(weights)
      if (cor == FALSE) {
        return(covariance)
      } else {
        sdmatrix <- sqrt(diag(diag(covariance)))
        rcor <- solve(sdmatrix) %*% covariance %*% solve(sdmatrix)
        return(rcor)
      } 
    } 
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
  
  # self-reference for multi-day input
  if (checkMultiDays(rdata) == TRUE) { 
    result <- apply.daily(rdata, rSkew, align.by, align.period, makeReturns)
    return(result)
  } else {
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata <- aggregatets(rdata, on = align.by, k = align.period)
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
#' rSV(sample_tdata$PRICE, align.by = "minutes", align.period = 5, makeReturns = TRUE)
#' }
#' @references Barndorff-Nielsen, O.E., Kinnebrock, S. and Shephard N. (2008). Measuring downside risk - realized semivariance. CREATES research paper. p. 3-5.
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#' @keywords  highfrequency rSV
#' @export
rSV <- function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE) {
  
  # self-reference for multi-day input
  if (checkMultiDays(rdata) == TRUE) {
    result <- apply.daily(rdata, rSV, align.by, align.period, makeReturns)
    return(result)
  } else {
    
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata <- aggregatets(rdata, on = align.by, k = align.period)
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

#' Threshold Covariance
#' @description 
#' Function returns the treshold covariance matrix proposed in Gobbi and Mancini (2009).
#' Unlike the \code{\link{rOWCov}}, the rThresholdCov uses univariate jump detection rules to truncate the effect of jumps on the covariance
#' estimate. As such, it remains feasible in high dimensions, but it is less robust to small cojumps. 
#' 
#' Let \eqn{r_{t,i}} be an intraday \eqn{N x 1} return vector and \eqn{i=1,...,M}
#' the number of intraday returns.
#' 
#' Then, the \eqn{k,q}-th element of the threshold covariance matrix is defined as
#' 
#' \deqn{
#' \mbox{tresholdcov}[k,q]_{t} = \sum_{i=1}^{M} r_{(k)t,i} 1_{\{r_{(k)t,i}^2 \leq TR_{M}\}}  \ \ r_{(q)t,i} 1_{\{r_{(q)t,i}^2 \leq TR_{M}\}},
#' }
#' with the treshold value \eqn{TR_{M}} set to \eqn{9 \Delta^{-1}} times the daily realized bi-power variation of asset \eqn{k}, 
#' as suggested in Jacod and Todorov (2009).
#' 
#' @param rdata a \eqn{(M x N)} matrix/zoo/xts object containing the \eqn{N}
#' return series over period \eqn{t}, with \eqn{M} observations during \eqn{t}.
#' @param cor boolean, in case it is TRUE, the correlation is returned. FALSE by default.
#' @param align.by a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param align.period an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns boolean, should be TRUE when rdata contains prices instead of returns. FALSE by default.
#' 
#' @return an \eqn{N x N} matrix
#' 
#' @references
#' Barndorff-Nielsen, O. and N. Shephard (2004). Measuring the impact of jumps in multivariate price processes using bipower covariation. Discussion paper, Nuffield College, Oxford University.
#' 
#' Jacod, J. and V. Todorov (2009). Testing for common arrival of jumps in discretely-observed multidimensional processes. Annals of Statistics 37, 1792-1838.
#' 
#' Mancini, C. and F. Gobbi (2009). Identifying the covariation between the diffusion parts and the co-jumps given discrete observations. Mimeo.
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' 
#' @examples # Realized threshold  Variance/Covariance: 
#' # Multivariate:
#' rcThreshold <- rThresholdCov(cbind(lltc, sbux), align.by = "minutes", align.period = 1) 
#' rcThreshold  
#' 
#' @keywords volatility
#' @export
rThresholdCov <- function(rdata, cor = FALSE, align.by = NULL, align.period = NULL, makeReturns = FALSE) {
  rdatacheck(rdata, multi = TRUE)
  # Multiday adjustment: 
  multixts <- multixts(rdata)
  if (multixts == TRUE) { 
    if (is.null(dim(rdata))) {  
      n < 1
    } else { 
      n <- dim(rdata)[2] 
    }
    if (n == 1) { 
      result <- apply.daily(rdata, rThresholdCov, cor = cor, align.by = align.by, 
                            align.period = align.period, makeReturns = makeReturns) 
    }
    if (n > 1) { 
      result <- applygetlist(rdata, rThresholdCov, cor = cor, align.by = align.by,
                              align.period = align.period, makeReturns = makeReturns)
    }    
    return(result)
  } else { #single day code
    if ((is.null(align.by) == FALSE) && (!is.null(align.period))) {
      rdata <- aggregatets(rdata, on = align.by, k = align.period)
    } 
    if (makeReturns == TRUE) { 
      rdata <- makeReturns(rdata) 
    }  
    
    rdata <- as.matrix(rdata)
    n <- dim(rdata)[1]				                  # number of observations
    delta <- 1 / n
    rbpvars <- apply(rdata, 2,FUN = RBPVar)		      # bipower variation per stock
    tresholds <- 3 * sqrt(rbpvars) * (delta^(0.49))	  # treshold per stock
    tresmatrix <- matrix(rep(tresholds, n), ncol = length(tresholds), nrow = n, byrow = TRUE)
    condition <- abs(rdata) > tresmatrix
    rdata[condition] <- 0
    covariance <- rCov(rdata)
    
    if (cor == FALSE) { 
      return(covariance) 
    }
    if (cor == TRUE) {
      sdmatrix <- sqrt(diag(diag(covariance)))
      rcor     <- solve(sdmatrix) %*% covariance %*% solve(sdmatrix)
      return(rcor)
    }
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
  returns <- as.numeric(rdata)
  RV <- sum(returns^2)
  return(RV)
}


#' Realized tri-power variation of highfrequency return series.
#' 
#' @description Function returns the rTPVar, defined in Andersen et al. (2012).
#'  
#'  Assume there is \eqn{N} equispaced returns in period \eqn{t}. Let \eqn{r_{t,i}} be a return (with \eqn{i=1, \ldots,N}) in period \eqn{t}.
#'  
#'  Then, the rTPVar is given by
#'  \deqn{
#'    \mbox{rTPVar}_{t}=\frac{N}{N-2} \left( 2^{1/3} \frac{\Gamma \left(5/6\right)}{ \Gamma \left(1/2\right)} \right)^{-3} \sum_{i=3}^{N} \mbox({|r_{t,i}|}^{2/3} {|r_{t,i-1}|}^{2/3} {|r_{t,i-2}|}^{2/3})
#'  }
#'  
#' @param rdata a zoo/xts object containing all returns in period t for one asset.
#' @param align.by a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param align.period an integer, align the tick data to this many [seconds|minutes|hours].  
#' @param makeReturns boolean, should be TRUE when rdata contains prices instead of returns. FALSE by   default.
#'  
#' @return numeric
#'  
#' @references Andersen, T. G., D. Dobrev, and E. Schaumburg (2012). Jump-robust volatility estimation using nearest neighbor truncation. Journal of Econometrics, 169(1), 75- 93.
#'  
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#'  
#' @examples 
#' data(sample_tdata)
#' rTPVar(rdata = sample_tdata$PRICE, align.by = "minutes", align.period = 5, makeReturns = TRUE)
#' rTPVar
#' 
#' @importFrom zoo rollapply
#' @keywords highfrequency rTPVar
#' @export
rTPVar <- function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE) {
  
  # self-reference for multi-day input
  if (checkMultiDays(rdata) == TRUE) { 
    result <- apply.daily(rdata, rTPVar, align.by, align.period, makeReturns)
    return(result)
  } else {
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata <- aggregatets(rdata, on = align.by, k = align.period)
    }
    if (makeReturns) {
      rdata <- makeReturns(rdata)
    }
    
    q      <- as.numeric(rdata)
    q      <- abs(rollapply(q, width = 3, FUN = prod, align = "left"))
    N      <- length(q)+2
    rTPVar <- N / (N-2) * gamma(1/2)^2/(4*gamma(7/6)^2) * sum(q^(4/3))
    return(rTPVar)
  }
}

#' Calculate the realized tripower quarticity
#' 
#' @param rdata a zoo/xts object containing all returns in period t for one asset.
#' 
#' @return numeric
#' 
#' @export
RTQ <- function(rdata) { 
  returns <- as.vector(as.numeric(rdata))
  n <- length(returns)
  tq <- n * ((2^(2/3) * gamma(7/6) * gamma(1/2)^(-1))^(-3)) *  sum(abs(returns[1:(n - 2)])^(4/3) * abs(returns[2:(n-1)])^(4/3) * abs(returns[3:n])^(4/3))
  return(tq)
} 


#' Realized quad-power variation of highfrequency return series. 
#' @description Function returns the realized quad-power variation, defined in Andersen et al. (2012).
#'  
#'  Assume there is \eqn{N} equispaced returns in period \eqn{t}. Let \eqn{r_{t,i}} be a return (with \eqn{i=1, \ldots,N}) in period \eqn{t}.
#'  
#'  Then, the rQPVar is given by
#'  \deqn{
#'    \mbox{rQPVar}_{t}=\frac{N}{N-3} \left( 2^{1/4} \frac{\Gamma \left(3/4\right)}{ \Gamma \left(1/2\right)} \right)^{-4} \sum_{i=4}^{N} \mbox({|r_{t,i}|}^{1/2} {|r_{t,i-1}|}^{1/2} {|r_{t,i-2}|}^{1/2} {|r_{t,i-3}|}^{1/2})
#'  }
#'
#' @param rdata a zoo/xts object containing all returns in period t for one asset.
#' @param align.by a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param align.period an integer, align the tick data to this many [seconds|minutes|hours].  
#' @param makeReturns boolean, should be TRUE when rdata contains prices instead of returns. FALSE by default.
#'
#' @return numeric
#' 
#' @references Andersen, T. G., D. Dobrev, and E. Schaumburg (2012). Jump-robust volatility estimation using nearest neighbor truncation. Journal of Econometrics, 169(1), 75- 93.
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#' 
#' @examples 
#' data(sample_tdata)
#' rQPVar(rdata= sample_tdata$PRICE, align.by= "minutes", align.period =5, makeReturns= TRUE)
#' rQPVar
#' 
#' @keywords highfrequency rQPVar
#' @export
rQPVar <- function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE) {
  
  # self-reference for multi-day input
  if (checkMultiDays(rdata) == TRUE) { 
    result <- apply.daily(rdata, rQPVar, align.by, align.period, makeReturns)
    return(result)
  } else {
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata <-aggregatets(rdata, on = align.by, k = align.period)
    }
    if (makeReturns) {
      rdata <- makeReturns(rdata)
    }
    q      <- as.numeric(rdata)
    q      <- abs(rollapply(q, width = 4, FUN = prod, align = "left"))
    N      <- length(q) + 3
    rQPVar <- N / (N-3) * pi^2 / 4 * sum(q)
    return(rQPVar)
  }
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
#' rQuar(rdata = sample_tdata$PRICE, align.by = "minutes", align.period = 5, makeReturns = TRUE)
#' }
#' @keywords  highfrequency rQuar
#' @export
rQuar <- function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE) {
  
  # self-reference for multi-day input
  if (checkMultiDays(rdata) == TRUE) { 
    result <- apply.daily(rdata, rQuar, align.by, align.period, makeReturns)
    return(result)
  } else {
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata <- aggregatets(rdata, on = align.by, k = align.period)
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



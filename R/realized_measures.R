
#' Available Kernels
#' 
#' @description Returns a vector of the available kernels.
#' 
#' @return character vector 
#' 
#' @references Ole E. Barndorff-Nielsen, Peter Reinhard Hansen, Asger Lunde, and Neil Shephard (2008). Designing Realized Kernels to Measure the ex post Variation of Equity Prices in the Presence of Noise. \emph{Econometrica}, 76, pp. 1481-1536.
#' 
#' @author Scott Payseur
#' 
#' @examples
#' listAvailableKernels
#' @keywords volatility
#' @export
listAvailableKernels <- function() {
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
#' @param rData a zoo/xts object containing all returns in period t for one asset.
#' @param alignBy a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param alignPeriod an integer, align the tick data to this many [seconds|minutes|hours]. 
#' @param makeReturns boolean, should be TRUE when rData contains prices instead of returns. FALSE by  default.
#' 
#' @return numeric
#' 
#' @examples
#' \dontrun{
#' medRQ(rData = sampleTData$PRICE, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)
#' medRQ
#' }
#' @keywords highfrequency medRQ
#' @references Andersen, T. G., D. Dobrev, and E. Schaumburg (2012). Jump-robust volatility estimation using nearest neighbor truncation. Journal of Econometrics, 169(1), 75- 93.
#' @importFrom zoo rollmedian
#' @export
medRQ <- function(rData, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE) {
  
  # self-reference for multi-day input
  if (checkMultiDays(rData)) {
    result <- apply.daily(rData, medRQ, alignBy, alignPeriod, makeReturns) 
    return(result)
  } else {
    if ((!is.null(alignBy)) && (!is.null(alignPeriod))) {
      rData <- fastTickAgregation(rData, on = alignBy, k = alignPeriod)
    }
    if (makeReturns) {
      rData <- makeReturns(rData)
    }
    q <- abs(as.numeric(rData))
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
#' @param rData a zoo/xts object containing all returns in period t for one asset.
#' @param alignBy a string, align the tick data to "seconds"|"minutes"|"hours"
#' @param alignPeriod an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns boolean, should be TRUE when rData contains prices instead of returns. FALSE by  default.
#' 
#' @return numeric
#' 
#' @examples
#' \dontrun{
#' minRQ(rData = sampleTData$PRICE, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)
#' minRQ
#' }
#'@references Andersen, T. G., D. Dobrev, and E. Schaumburg (2012). Jump-robust volatility estimation using nearest neighbor truncation. Journal of Econometrics, 169(1), 75- 93.
#' @importFrom zoo as.zoo
#' @importFrom zoo rollapply
#' @export
minRQ <- function(rData, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE) {
  
  # self-reference for multi-day input
  if (checkMultiDays(rData)) {
    result <- apply.daily(rData, minRQ, alignBy, alignPeriod, makeReturns)
    return(result)
  } else {
    if ((!is.null(alignBy)) && (!is.null(alignPeriod))) {
      rData <- fastTickAgregation(rData, on = alignBy, k = alignPeriod)
    }
    if (makeReturns) {
      rData = makeReturns(rData)
    }
    q     <- as.zoo(abs(as.numeric(rData)))
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
#' @param rData a zoo/xts object containing all returns in period t for one asset.
#' @param alignBy a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param alignPeriod an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns boolean, should be TRUE when rData contains prices instead of returns. FALSE by default.
#' 
#' @return numeric
#' 
#' @references 
#' Andersen, T. G., D. Dobrev, and E. Schaumburg (2012). Jump-robust volatility estimation using nearest neighbor truncation. Journal of Econometrics, 169 (1), 75-93.
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' 
#' @examples
#' data(sampleTData)
#' minrv <- minRV(rData = sampleTData$PRICE, alignBy = "minutes",
#'                alignPeriod = 5, makeReturns = TRUE)
#' minrv 
#' 
#' @keywords volatility
#' @export
minRV <- function(rData, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE){
  
  # self-reference for multi-day input
  if (checkMultiDays(rData)) {
    result <- apply.daily(rData, minRV, alignBy, alignPeriod, makeReturns)
    return(result)
  } else {
    if ((!is.null(alignBy)) && (!is.null(alignPeriod))) {
      rData <- fastTickAgregation(rData, on = alignBy, k = alignPeriod)
    } 
    if (makeReturns) {
      rData <- makeReturns(rData)
    }  
    q <- abs(rData)#as.zoo(abs(as.numeric(rData))) #absolute value
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
#' @param rData a zoo/xts object containing all returns in period t for one asset.
#' @param alignBy a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param alignPeriod an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns boolean, should be TRUE when rData contains prices instead of returns. FALSE by   default.
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
#' data(sampleTData);
#' medrv <- medRV(rData = sampleTData$PRICE, alignBy = "minutes", 
#'                alignPeriod = 5, makeReturns = TRUE)
#' medrv 
#'  
#' @keywords volatility
#' @export
medRV <- function(rData, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE){
  
  # self-reference for multi-day input
  if (checkMultiDays(rData)) {
    result <- apply.daily(rData, medRV, alignBy, alignPeriod, makeReturns)
    return(result)
  } else {
    if ((!is.null(alignBy)) && (!is.null(alignPeriod))) {
      rData <- fastTickAgregation(rData, on = alignBy, k = alignPeriod)
    }
    if (makeReturns) {
      rData <- makeReturns(rData)
    }
    
    q <- abs(as.numeric(rData)) #absolute value
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
#' @param pData a list. Each list-item contains an xts object with the intraday price data of a stock.
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
#'   It is intuitive that under mean zero i.i.d. microstructure noise some form of smoothing of the observed log-price should tend to diminish the impact of the noise. 
#'   Effectively, we are going to approximate a continuous function by an average of observations of Y in a neighborhood, the noise being averaged away. 
#'   
#'   Assume there is \eqn{N} equispaced returns in period \eqn{\tau} of a list (after refeshing data). Let \eqn{r_{\tau_i}} be a return (with \eqn{i=1, \ldots,N}) of an asset in period \eqn{\tau}. Assume there is \eqn{d} assets. 
#'   
#'   In order to define the univariate pre-averaging estimator, we first define the pre-averaged returns as
#'   \deqn{
#'     \bar{r}_{\tau_j}^{(k)}= \sum_{h=1}^{k_N-1}g\left(\frac{h}{k_N}\right)r_{\tau_{j+h}}^{(k)}
#'   }
#'   where g is a non-zero real-valued function \eqn{g:[0,1]} \eqn{\rightarrow} \eqn{R} given by \eqn{g(x)} = \eqn{\min(x,1-x)}. \eqn{k_N} is a sequence of integers satisfying  \eqn{\mbox{k}_{N} = \lfloor\theta N^{1/2}\rfloor}. 
#'   We use \eqn{\theta = 0.8} as recommended in Hautsch & Podolskij (2013). The pre-averaged returns are simply a weighted average over the returns in a local window. 
#'   This averaging diminishes the influence of the noise. The order of the window size \eqn{k_n} is chosen to lead to optimal convergence rates. 
#'   The pre-averaging estimator is then simply the analogue of the Realized Variance but based on pre-averaged returns and an additional term to remove bias due to noise
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
#' where \eqn{\hat{\Psi}_N = \frac{1}{2N}\sum_{i=1}^N \boldsymbol{r}_{\tau_i}(\boldsymbol{r}_{\tau_i})'}. It is a bias correction to make it consistent. 
#' However, due to this correction, the estimator is not ensured PSD. 
#' An alternative is to slightly enlarge the bandwidth such that \eqn{\mbox{k}_{N} = \lfloor\theta N^{1/2+\delta}\rfloor}. \eqn{\delta = 0.1} results in a consistent estimate without the bias correction and a PSD estimate, in which case:
#'   \deqn{
#'     \mbox{MRC}^{\delta}= \frac{N}{N-k_N+2}\frac{1}{\psi_2k_N}\sum_{i=0}^{N-k_N+1}\bar{\boldsymbol{r}}_i\cdot \bar{\boldsymbol{r}}'_i
#' }
#' 
#' @references Hautsch, N., & Podolskij, M. (2013). Preaveraging-Based Estimation of Quadratic Variation in the Presence of Noise and Jumps: Theory, Implementation, and Empirical Evidence. Journal of Business & Economic Statistics, 31(2), 165-183.
#' 
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#' 
#' @examples 
#' a <- list(sample5MinPricesJumps["2010-01-04",1], sample5MinPricesJumps["2010-01-04",2])
#' MRC(a, pairwise = TRUE, makePsd = TRUE)
#' 
#' @keywords highfrequency preaveraging
#' @export
MRC <- function(pData, pairwise = FALSE, makePsd = FALSE) {
  
  if (!is.list(pData)) {
    n <- 1
  } else {
    n <- length(pData)
  }
  if (n == 1) { 
    if (isMultiXts(pData)) {
      stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")
    }
    mrc <- crv(pData)
  }
  
  if (n > 1) {
    if (isMultiXts(pData[[1]])) {
      stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")
    }
    
    if (pairwise) {
      cov <- matrix(rep(0, n * n), ncol = n)
      diagonal <- c()
      for (i in 1:n) {
        diagonal[i] <- crv(pData[[i]])
      }
      diag(cov) <- diagonal
      
      for (i in 2:n) {
        for (j in 1:(i - 1)) {
          cov[i, j] = cov[j, i] = preavbi(pData[[i]], pData[[j]])
        }
      }
      
      mrc <- cov
      
      if (makePsd) {
        mrc <- makePsd(mrc)
      }
      
    } else {
      x     <- refreshTime(pData)
      N     <- nrow(x)
      theta <- 0.8 #recommendation by Hautsch and Podolskij
      kn    <- floor(theta * sqrt(N))
      
      ##psi:
      psi1 <- 1
      psi2 <- 1 / 12
      
      psi1kn <- kn * sum((gfunction((1:kn)/kn) - gfunction(((1:kn) - 1) / kn))^2 )
      psi2kn <- 1 / kn * sum(gfunction((1:kn) / kn)^2)
      
      # preavreturn <- c()
      # for (i in 1:ncol(x)) {
      #   preavreturn <- cbind(preavreturn, as.numeric(hatreturn(x[,i],kn)))
      # }
      preavreturn <- as.matrix(hatreturn(x, kn), ncol = ncol(x))
      S <- rCov(preavreturn)
      
      mrc <- N / (N - kn + 2) * 1/(psi2 * kn) * S
      
      if (makePsd) {
        mrc <- makePsd(mrc)
      }
    }
  }
  return(mrc)
} 

#' Realized Covariance: Average Subsample
#' 
#' @description Realized Covariance using average subsample.
#' 
#' @param rData a \eqn{(M x N)} matrix/zoo/xts object containing the \eqn{N}
#' return series over period \eqn{t}, with \eqn{M} observations during \eqn{t}.
#' @param cor boolean, in case it is TRUE, the correlation is returned. FALSE by default.
#' @param alignBy Align the tick data to seconds|minutes|hours. Default is \code{"minutes"}
#' @param alignPeriod Align the tick data to this many [seconds|minutes|hours]. This can be a fraction. Default is \code{5}
#' @param k numeric denoting which horizon to use for the subsambles. This can be a fraction as long as k is a divisor of alignPeriod default is \code{1}
#' @param makeReturns Prices are passed make them into log returns
#' 
#' @return Realized covariance using average subsample.
#' 
#' @references 
#' L. Zhang, P.A Mykland, and Y. Ait-Sahalia. A tale of two time scales: Determining integrated volatility with noisy high-frequency data. \emph{Journal of the American Statistical Association}, 2005.
#' 
#' Michiel de Pooter, Martin Martens, and Dick van Dijk. Predicting the daily covariance matrix for S\&P100 stocks using intraday data - but which frequency to use? \emph{Econometric Reviews}, 2008.
#' 
#' @author Scott Payseur
#' 
#' @examples 
#' # Average subsampled realized variance/covariance aligned at one minute returns at 
#' # 5 subgrids (5 minutes).
#' 
#' # Univariate
#' rvSub <- rAVGCov(rData = sampleTData$PRICE, alignBy = "minutes",
#'                  alignPeriod = 5, makeReturns = TRUE) 
#' rvSub
#' 
#' # Multivariate:
#' rcovSub <- rAVGCov(rData = cbind(lltc, sbux, fill = 0), alignBy = "minutes", 
#'                    alignPeriod = 5, makeReturns = FALSE)
#' rcovSub
#' 
#' # Multivariate with a 30 second fast aggregation and a 2.5 minute slow aggregation.
#' rcovSub <- rAVGCov(rData = cbind(lltc, sbux, fill = 0), 
#'                    alignBy = "minutes", alignPeriod = 2.5, k = 0.5, makeReturns = FALSE)
#' rcovSub
#' @importFrom data.table data.table
#' @keywords volatility
#' @export
#' 
rAVGCov <- function(rData, cor = FALSE, alignBy = "minutes", alignPeriod = 5, k = 1, makeReturns = FALSE) {
  DT <- DT_ROUND <- DT_SUBSAMPLE <- FIRST_DT <- MAXDT <- RETURN <- RETURN1 <- RETURN2 <- NULL
  
  if (isMultiXts(rData)) {
    stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")
  }
  
  if (is.null(dim(rData))) {
    n <- 1
  } else {
    n <- dim(rData)[2]
  }
  
  if(alignBy == "secs" | alignBy == "seconds"){
    scaleFactor <- alignPeriod
    scaleFactorFast <- k
  }
  if(alignBy == "mins" | alignBy == "minutes"){
    scaleFactor <- alignPeriod * 60
    scaleFactorFast <- k * 60 
  }
  if(alignBy == "hours"){
    scaleFactor <- alignPeriod * 60 * 60
    scaleFactorFast <- k * 60 * 60 
  }
  
  # We calculate how many times the fast alignment period divides the slow one and makes sure it is a positive integer.
  scalingFraction <- alignPeriod/k
  if(scalingFraction < 0 | scalingFraction %% 1){
    stop("alignPeriod must be greater than k, and the fraction of these must be an integer value")
  }
  
  
  if (n == 1) {
    rdatabackup <- data.table(DT = as.numeric(index(rData), tz = tzone(rData)), RETURN = as.numeric(rData))
    rData <- rdatabackup
    rData[, FIRST_DT := min(DT)]
    if (makeReturns) {
      
      rData[, DT_ROUND := ifelse(DT == FIRST_DT,
                                  floor(DT/scaleFactorFast) * scaleFactorFast,
                                  ceiling(DT / scaleFactorFast) * scaleFactorFast)]

      
      rData[, MAXDT := max(DT), by = "DT_ROUND"]
      rData <- rData[DT == MAXDT]
      rData[, RETURN := log(RETURN) - shift(log(RETURN), n = 1, type = "lag")]
      rData <- rData[!is.na(RETURN)]
      rData <- rData[, c("DT_ROUND", "RETURN")]
      
    } else {
      rData[, DT_ROUND := ceiling(DT / scaleFactorFast) * scaleFactorFast]
      
    }

    rvavg <- sum(rData[, DT_SUBSAMPLE := ceiling(DT_ROUND/scaleFactor) * scaleFactor][, list(RETURN = sum(RETURN)), by = list(DT_SUBSAMPLE)]$RETURN^2)
    
    
    
    for (ii in c(1:(scalingFraction - 1))) {
      rdatasub <- rData[-c(1:ii, (dim(rData)[1]-scalingFraction + ii + 1):dim(rData)[1]), ]
      rdatasub[, DT_ROUND := DT_ROUND - ii * scaleFactorFast] 
      rvavg <- rvavg + sum(rdatasub[, DT_SUBSAMPLE := ceiling(DT_ROUND/scaleFactor) * scaleFactor][, list(RETURN = sum(RETURN)), by = list(DT_SUBSAMPLE)]$RETURN^2) * (dim(rdatasub)[1]/scalingFraction + 1) / (dim(rdatasub)[1]/scalingFraction)
    }
      return(rvavg / scalingFraction)
  }
  
  if (n > 1) {
    rdatamatrix <- matrix(0, nrow = n, ncol = n)
    for (ii in 1:n) {
      # calculate variances
      rdatamatrix[ii, ii] <- rAVGCov(rData[, ii], cor = cor, alignBy = alignBy, alignPeriod = alignPeriod, k = k, makeReturns = makeReturns)
      if (ii < n) {
        for (jj in (ii+1):n) {
          rdatabackup <- data.table(DT = as.numeric(index(rData), tz = tzone(rData)), RETURN1 = as.numeric(rData[, ii]), RETURN2 = as.numeric(rData[,jj]))
          rdatabackup[, FIRST_DT := min(DT)]
          if (makeReturns) {
            
            rdatabackup[, DT_ROUND := ifelse(DT == FIRST_DT,
                                                    floor(DT/scaleFactorFast) * scaleFactorFast,
                                                    ceiling(DT/scaleFactorFast) * scaleFactorFast)]
            rdatabackup[, MAXDT := max(DT), by = "DT_ROUND"]
            rdatabackup <- rdatabackup[DT == MAXDT]
            rdatabackup[, RETURN1 := log(RETURN1) - shift(log(RETURN1), n = 1, type = "lag")]
            rdatabackup[, RETURN2 := log(RETURN2) - shift(log(RETURN2), n = 1, type = "lag")]
            rdatabackup <- rdatabackup[!is.na(RETURN1)][!is.na(RETURN2)][, c("DT_ROUND", "RETURN1", "RETURN2")]
          } else {

            rdatabackup[, DT_ROUND := ceiling(DT/scaleFactorFast) * scaleFactorFast]
            
            
          }
          returns <- rdatabackup[, DT_SUBSAMPLE := ceiling(DT_ROUND / scaleFactor) * scaleFactor][, list(RETURN1 = sum(RETURN1), RETURN2 = sum(RETURN2)), by = list(DT_SUBSAMPLE)]
          # Calculate off-diagonals
          covavg <- t(returns$RETURN1) %*% returns$RETURN2
          
          for (kk in c(1:(scalingFraction - 1))) {
            returns <- rdatabackup[, DT_SUBSAMPLE := ceiling(DT_ROUND/scaleFactorFast) * scaleFactorFast][, list(RETURN1 = sum(RETURN1), RETURN2 = sum(RETURN2)), by = list(DT_SUBSAMPLE)]
            rdatasub <- returns[-c(1:kk, (dim(returns)[1]-scalingFraction + kk + 1):dim(returns)[1]), ]
            rdatasub[, DT_SUBSAMPLE := DT_SUBSAMPLE - kk * scaleFactorFast] 
            returns <- rdatasub[, DT_SUBSAMPLE := ceiling(DT_SUBSAMPLE/scaleFactor) * scaleFactor][, list(RETURN1 = sum(RETURN1), RETURN2 = sum(RETURN2)), by = list(DT_SUBSAMPLE)]
            covavg <- covavg + t(returns$RETURN1) %*% returns$RETURN2
          }
          
          
          rdatamatrix[ii, jj] <- rdatamatrix[jj, ii] <- covavg / scalingFraction
          
        }
      }
    }
    
  }
  return(rdatamatrix)
  # 
  # r# Aggregate:
  # if (makeReturns) {
  #   if((!is.null(alignBy)) && (!is.null(alignPeriod))) {
  #     rData <- fastTickAgregation(rData, on = alignBy, k = 1)
  #   }
  #   rData <- makeReturns(rData)
  # } else {
  #   rData <- aggregateTS(rData, FUN = "sum", on = alignBy, k = 1)
  # }
  # 
  # if (is.null(dim(rData))) {
  #   n <- 1
  # } else {
  #   n <- dim(rData)[2]
  # }
  # 
  # zoo::index(rData) <- index(rData) - eval(parse(text = paste0("lubridate::", alignBy, "(", 1, ")"))) # shift due to change on boundary in aggregateTS
  # if (n == 1) {
  #   rvavg <- rCov(aggregateTS(rData, FUN = "sum", on = alignBy, k = alignPeriod))
  #   for (ii in c(1:(alignPeriod - 1))) {
  #     rdatasub <- rData[-c(1:ii, (length(rData)-alignPeriod + ii):length(rData)), ]
  #     zoo::index(rdatasub) <-  ymd_hms(index(rdatasub), tz = tz(index(rdatasub))) - eval(parse(text = paste0("lubridate::", alignBy, "(", ii, ")")))
  #     n_obs <- length(fastTickAgregation(rdatasub, on = alignBy, k = alignPeriod))
  #     rvavg <- rvavg + rCov(aggregateTS(rdatasub, FUN = "sum", on = alignBy, k = alignPeriod)) #* (n_obs + 1) / (n_obs) # correction term for one observation less
  #   }
  #   return(rvavg / alignPeriod)
  # } else {
  #   rdatamatrix <- matrix(0, nrow = n, ncol = n)
  #   for (ii in c(1:n)) {
  #     rvavg <- rCov(aggregateTS(rData[, ii], FUN = "sum", on = alignBy, k = alignPeriod))
  #     for (jj in c(1:(alignPeriod - 1))) {
  #       rdatasub <- rData[-c(1:jj, (length(rData[,ii])-alignPeriod + jj):length(rData[,ii])), ii]
  #       zoo::index(rdatasub) <- ymd_hms(index(rdatasub), tz = tz(index(rdatasub))) - eval(parse(text = paste0("lubridate::", alignBy, "(", ii, ")")))
  #       n_obs <- length(fastTickAgregation(rdatasub, on = alignBy, k = alignPeriod))
  #       rvavg <- rvavg + rCov(aggregateTS(rdatasub, FUN = "sum", on = alignBy, k = alignPeriod)) #* (n_obs + 1) / (n_obs) # correction term for one observation less
  #     }
  #     rdatamatrix[ii, ii] <- rvavg / alignPeriod
  #     if (ii < n) {
  #       for (jj in c((ii+1):n)) {
  #         rdatamatrix[ii, jj] <- 1
  #         rdatamatrix[jj, ii] <- rdatamatrix[ii, jj]
  #       }
  #     }
  #     
  #   }
  #   return(rdatamatrix)
  # }
  
  
  # rv_avg_sub <- apply(xts_one_minute, 2, rAVGCov, period = 5)
  # 
  # if (n == 1) {
  #   
  #   rollingwindow <- aggregateTS(rData, FUN = "sum", on = alignBy, k = alignPeriod)
  #   
  #   rollingwindow <- period.apply(rData, INDEX = rep(c(1:(length(rData) / alignPeriod)), each = align.p), FUN = "sum")
  #   
  #   rData
  #   
  #   rvsubsampled <- RV(rData)
  #   
  #   if (alignPeriod == 1) { # no subsampling
  #     return(rvsubsampled)
  #   } else {
  #     for (ii in c(1:alignPeriod)) {
  #       rvsubsampled <- rvsubsampled + RV(rData[-c(1:ii, (length(rData) - alignPeriod + ii + 1):length(rData)), ])
  #     }
  #     return(rvsubsampled / alignPeriod)
  #   }
  # }
  # if (n > 1) {
  #   rdatamatrix <- as.matrix(rData)
  #   covariance <- t(rdatamatrix) %*% rdatamatrix
  #   if (alignPeriod == 1) { # no subsampling
  #     if (!cor) {
  #       return(covariance)
  #     } else (cor){
  #       sdmatrix <- sqrt(diag(diag(covariance)))
  #       rcor <- solve(sdmatrix) %*% covariance %*% solve(sdmatrix)
  #       return(rcor)
  #     }
  #   } else {
  #     for (ii in c(2:alignPeriod)) {
  #       covariance <- covariance + t(rdatamatrix[-c(1:ii), ]) %*% rdatamatrix[-c(1:ii), ]
  #     }
  #     covariance / alignPeriod
  #     if (!cor) {
  #       return(covariance)
  #     } else {
  #       sdmatrix <- sqrt(diag(diag(covariance)))
  #       rcor <- solve(sdmatrix) %*% covariance %*% solve(sdmatrix)
  #       return(rcor)
  #     }
  #   }
  # }
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
#' @param rData a zoo/xts object containing all returns in period t for one asset.
#' @param rIndex a zoo/xts object containing return in period t for an index.
#' @param RCOVestimator can be chosen among realized covariance estimators: rCov, rAVGCov, rBPCov, rHYCov, rKernelCov, rOWCov, rRTSCov, rThresholdCov and rTSCov. rCov by default.
#' @param RVestimator can be chosen among realized variance estimators: RV, minRV and medRV. RV by default. In case of missing RVestimator, RCOVestimator function applying for rIndex will be used.
#' @param makeReturns boolean, should be TRUE when rData contains prices instead of returns. FALSE by  default.
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
#' a <- sample5MinPricesJumps['2010-01-04', 1]
#' b <- sample5MinPricesJumps['2010-01-04', 2]
#' rBeta(a, b, RCOVestimator = "rBPCov", RVestimator = "minRV", makeReturns = TRUE)
#' 
#' @keywords highfrequency rBeta
#' @importFrom methods hasArg
#' @importFrom utils data
#' @export
rBeta <- function(rData, rIndex, RCOVestimator = "rCov", RVestimator = "RV", makeReturns = FALSE) {
  if (hasArg(data)) {
    rData <- data
  }
  
  if (RCOVestimator != "rRTSCov" & RCOVestimator != "rTSCov" &  makeReturns) {
    rData <- makeReturns(rData)
    rIndex <- makeReturns(rIndex)
  }
  
  if(!makeReturns) {
    if (RCOVestimator == "rRTSCov" || RCOVestimator == "rTSCov"){
      if (min(rData) < 0) {
        print("when using rRTSCov, rTSCov, introduce price data - transformation to price data done")
        rData <- exp(cumsum(rData))
      }
      if( min(rIndex) <0 ){
        print("when using rRTSCov, rTSCov, introduce price data - transformation to price data done")
        rIndex <- exp(cumsum(rIndex))
      }       
    }
  }
  
  if (isMultiXts(rData)) {
    print("No support for multiple days")
  } else {
    rcovfun <- function(rData, rIndex, RCOVestimator) {
      
      switch(RCOVestimator,
             rCov = rCov(cbind(rData,rIndex) ),
             rAVGCov = rAVGCov(list(rData, rIndex) ),
             rBPCov = rBPCov(cbind(rData, rIndex) ),
             rHYCov = rHYCov(list(rData, rIndex) ),
             rKernelCov = rKernelCov(list(rData, rIndex) ),
             rOWCov = rOWCov(cbind(rData, rIndex) ),
             rRTSCov = rRTSCov(list(rData, rIndex)),
             rThresholdCov = rThresholdCov(cbind(rData, rIndex) ),
             rTSCov = rTSCov(list(rData, rIndex)))
    }
    
    rcov <- rcovfun(rData,rIndex,RCOVestimator)
    
    if (RVestimator == RCOVestimator || is.null(RVestimator)) {
      rbeta <- rcov[1,2] / rcov[2,2]
    } else {
      rvfun <- function(rIndex, RVestimator) {
        
        switch(RVestimator,
               rCov = rCov(rIndex ) ,
               RV = RV(rIndex),
               BV = RBPVar(rIndex),
               minRV = minRV(rIndex ),
               medRV = medRV(rIndex ),
               rAVGCov = rAVGCov(rIndex ) ,
               rBPCov = rBPCov(rIndex ) ,
               rHYCov = rHYCov(rIndex ) ,
               rKernelCov = rKernelCov(rIndex ) ,
               rOWCov = rOWCov(rIndex ) ,
               rRTSCov = rRTSCov(rIndex) ,
               rThresholdCov = rThresholdCov(rIndex ) ,
               rTSCov = rTSCov(rIndex))
      }
      rv <- rvfun(rIndex,RVestimator)
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
#' @param rData a \eqn{(M x N)} matrix/zoo/xts object containing the \eqn{N}
#'    return series over period \eqn{t}, with \eqn{M} observations during \eqn{t}.
#' @param cor boolean, in case it is TRUE, the correlation is returned. FALSE by default.
#' @param alignBy a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param alignPeriod an integer, align the tick data to this many [seconds|minutes|hours]. 
#' @param makeReturns boolean, should be TRUE when rData contains prices instead of returns. FALSE by default.
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
#' # Realized Bipower Variance/Covariance for a price series aligned   
#' # at 5 minutes.
#'  
#' # Univariate: 
#' rbpv <- rBPCov(rData = sampleTData$PRICE, alignBy ="minutes", 
#'                alignPeriod = 5, makeReturns = TRUE) 
#' rbpv 
#'  
#' # Multivariate: 
#' rbpc <- rBPCov(rData = sample5MinPricesJumps['2010-01-04'], makeReturns = TRUE, makePsd = TRUE)
#' rbpc
#'  
#' @keywords volatility
#' @export
rBPCov <- function(rData, cor = FALSE, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE, makePsd = FALSE) {
  
  # self-reference for multi-day input
  if (checkMultiDays(rData)) { 
    if (is.null(dim(rData))) {  
      n <- 1
    } else { 
      n <- dim(rData)[2] 
    }
    if (n == 1){ 
      result <- apply.daily(rData, rBPCov, alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = makeReturns, makePsd) 
    }
    if (n > 1) { 
      result <- applyGetList(rData, rBPCov, cor = cor, alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = makeReturns, makePsd) 
      names(result) <- unique(as.Date(index(rData)))
    }    
    return(result)
  } else { #single day code
    if ((!is.null(alignBy)) && (!is.null(alignPeriod))) {
      rData <- fastTickAgregation(rData, on = alignBy, k = alignPeriod);
    } 
    if (makeReturns) {  
      rData <- makeReturns(rData) 
    }  
    if (is.null(dim(rData))) {
      n <- 1
    } else { 
      n <- dim(rData)[2]
    }
    
    if (n == 1) {
      return(RBPVar(rData))
    }
    
    ## ACTUAL RBPCOV calculation:   
    if (n > 1) {    
      
      rData  <- as.matrix(rData);
      n <- dim(rData)[2]
      cov <- matrix(rep(0, n * n), ncol = n)
      diagonal <- c()
      for (i in 1:n) {
        diagonal[i] <- RBPVar(rData[, i])
      }
      diag(cov) <- diagonal
      for (i in 2:n) {
        for (j in 1:(i - 1)) {
          cov[i, j] = cov[j, i] = RBPCov_bi(rData[, i], rData[, j])
        }
      }
      
      if (!cor){
        if (makePsd) {
          cov <- makePsd(cov)
        }
        return(cov)
      } else {
        sdmatrix <- sqrt(diag(diag(cov)))
        rcor <- solve(sdmatrix) %*% cov %*% solve(sdmatrix)
        if (makePsd) {
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
#' @param rData a \eqn{(M x N)} matrix/zoo/xts object containing the \eqn{N}
#' return series over period \eqn{t}, with \eqn{M} observations during \eqn{t}.
#' In case of a matrix, no multi-day adjustment is possible.
#' @param cor boolean, in case it is TRUE, the correlation is returned. FALSE by default.
#' @param alignBy a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param alignPeriod an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns boolean, should be TRUE when rData contains prices instead of returns. FALSE by default.
#' 
#' @return an \eqn{N x N} matrix
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' 
#' @examples 
#' # Realized Variance/Covariance for prices aligned   
#' # at 5 minutes.
#' data(sampleTData)
#' data(sample5MinPricesJumps)
#' 
#' # Univariate: 
#' rv = rCov(rData = sampleTData$PRICE, alignBy = "minutes", 
#'                    alignPeriod = 5, makeReturns = TRUE)
#' rv 
#' 
#' # Multivariate: 
#' rc = rCov(rData = sample5MinPricesJumps['2010-01-04'], makeReturns=TRUE)
#' rc
#' @keywords volatility
#' @export
rCov <- function(rData, cor = FALSE, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE) {
  
  # Multiday adjustment: 
  if (checkMultiDays(rData)) { 
    if (is.null(dim(rData))) {  
      n <- 1
    } else { 
      n <- dim(rData)[2]
    }
    if (n == 1) { 
      result <- apply.daily(rData, rCov, alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = makeReturns) 
    }
    if (n > 1) { 
      result <- applyGetList(rData, rCov, cor=cor, alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = makeReturns)
      names(result) <- unique(as.Date(index(rData)))
    }    
    return(result)
  } else {
    #single day code
    if((!is.null(alignBy)) && (!is.null(alignPeriod))) {
      rData <- fastTickAgregation(rData, on = alignBy, k = alignPeriod)
    } 
    if (makeReturns) {  
      rData <- makeReturns(rData) 
    }  
    if (is.null(dim(rData))) {  
      n <- 1
    } else { 
      n <- dim(rData)[2]
    }
    
    if (n == 1) {
      return(RV(rData))
    }
    if (n > 1) {
      
      rData <- as.matrix(rData)
      covariance <- t(rData) %*% rData
      if (!cor) {
        return(covariance)
      }
      if (cor){
        sdmatrix <- sqrt(diag(diag(covariance)));
        rcor <- solve(sdmatrix) %*% covariance %*% solve(sdmatrix)
        return(rcor)
      }
    }
  }
}

#' Hayashi-Yoshida Covariance
#' @description Calculates the Hayashi-Yoshida Covariance estimator
#' 
#' @param rData a possibly multivariate xts object.
#' @param cor boolean, in case it is TRUE, the correlation is returned. FALSE by default.
#' @param period Sampling period 
#' @param alignBy Align the tick data to seconds|minutes|hours
#' @param alignPeriod Align the tick data to this many [seconds|minutes|hours]
#' @param makeReturns Prices are passed make them into log returns
#' @param makePsd boolean, in case it is TRUE, the positive definite version of rHYCov is returned. FALSE by default.
#' 
#' @references T. Hayashi and N. Yoshida. On covariance estimation of non-synchronously observed diffusion processes. \emph{Bernoulli}, 11, 359-379, 2005.
#' 
#' @author Scott Payseur
#' 
#' # Average Hayashi-Yoshida Covariance estimator is calculated on five-minute returns
#' 
#' # Multivariate:
#' # realized_cov <- rHYCov(rData = cbind(lltc, sbux, fill = 0), period = 5, alignBy = "minutes", 
#' #                        alignPeriod = 5, makeReturns = FALSE)
#' # realized_cov 
#' Note: for the diagonal elements the rCov is used.
#' 
#' @keywords volatility
#' @export
rHYCov <- function(rData, cor = FALSE, period = 1, alignBy = "seconds", alignPeriod = 1, makeReturns = FALSE, makePsd = TRUE) {
  
  if (isMultiXts(rData)) {
    stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")
  }
  
  rData <- fastTickAgregation(rData, on = "seconds", k = 1)
  rData <- rData[-dim(rData)[1], ]
  aggrdata <- fastTickAgregation(rData, on = alignBy, k = alignPeriod)
  
  if (makeReturns) {  
    rData <- makeReturns(rData) 
    aggrdata <- makeReturns(aggrdata)
  }  
  if (is.null(dim(rData))) {
    n <- 1
  } else { 
    n <- dim(rData)[2]
  }
  
  if (n == 1) {
    return(rCov(aggrdata))
  }
  
  if (n > 1) {    
    
    # rData  <- as.matrix(rData)
    n <- dim(rData)[2]
    cov <- matrix(rep(0, n * n), ncol = n)
    diagonal <- c()
    for (i in 1:n) {
      diagonal[i] <- rCov(aggrdata[, i])
    }
    diag(cov) <- diagonal
    alignPeriod <- getAlignPeriod(alignPeriod, alignBy)
    for (i in 2:n) {
      for (j in 1:(i - 1)) {
        cov[i, j] <- 
          sum(pcovcc(
            as.numeric(rData[,i]),
            as.double(rep(0,length(rData[, j])/(period * alignPeriod) + 1)),
            as.numeric(rData[,j]), #b
            as.double(c(1:length(rData[, j]))), #a
            as.double(rep(0, length(rData[, j])/(period * alignPeriod) + 1)), #a
            as.double(c(1:length(rData[, j]))), #b
            as.integer(length(rData[, j])), #na
            as.integer(length(rData[, j])/(period*alignPeriod)),
            as.integer(length(rData[, j])), #na
            as.integer(period * alignPeriod)))
        cov[j, i] <- cov[i, j]  
        
        # kernelEstimator(as.double(rData[, i]), as.double(rData[, j]), as.integer(length(rData[, i])),
        #                 as.integer(kernelParam), as.integer(ifelse(kernelDOFadj, 1, 0)),
        #                 as.integer(type), ab = double(kernelParam + 1),
        #                 ab2 = double(kernelParam + 1))
        
        # rc.hy( x=rData[[i]], y=rData[[j]], period = period,alignBy=alignBy, 
        #        alignPeriod = alignPeriod, cts = cts, makeReturns = makeReturns)
      }
    }
    
    if (!cor){
      if (makePsd) {
        cov <- makePsd(cov)
      }
      return(cov)
    } else {
      sdmatrix <- sqrt(diag(diag(cov)))
      rcor <- solve(sdmatrix) %*% cov %*% solve(sdmatrix)
      if (makePsd) {
        rcor <- makePsd(rcor)
      }
      return(rcor)
    }
  } 
} 

#' Realized Covariance: Kernel
#'
#' @description Realized covariance calculation using a kernel estimator.
#'
#' @param rData a \eqn{(M x N)} matrix/zoo/xts object containing the \eqn{N}
#' return series over period \eqn{t}, with \eqn{M} observations during \eqn{t}.
#' @param cor boolean, in case it is TRUE, the correlation is returned. FALSE by default.
#' @param alignBy Align the tick data to seconds|minutes|hours
#' @param alignPeriod Align the tick data to this many [seconds|minutes|hours]
#' @param makeReturns Convert to Returns
#' @param kernelType Kernel name (or number)
#' @param kernelParam Kernel parameter (usually lags)
#' @param kernelDOFadj Kernel Degree of freedom adjustment
#'
#' @details The different types of kernels can be found using \code{\link{listAvailableKernels}}.
#' 
#' @return Kernel estimate of realized covariance.
#' 
#' @references
#' Ole E. Barndorff-Nielsen, Peter Reinhard Hansen, Asger Lunde, and Neil Shephard (2008). Designing Realized Kernels to Measure the ex post Variation of Equity Prices in the Presence of Noise. \emph{Econometrica}, 76, pp. 1481-1536.
#'
#' B. Zhou. High-frequency data and volatility in foreign-exchange rates. \emph{Journal of Buiness & Economic Statistics}, 14:45-52, 1996.
#'
#' P. Hansen and A. Lunde. Realized variance and market microstructure noise. \emph{Journal of Business and Economic Statistics}, 24:127-218, 2006.
#'
#' @author Scott Payseur and Onno Kleen
#'
#' @examples
#' # Univariate:
#' rvKernel <- rKernelCov(rData = sampleTData$PRICE, alignBy = "minutes",
#'                        alignPeriod = 5, makeReturns = TRUE)
#' rvKernel
#'
#' # Multivariate:
#' rcKernel <- rKernelCov(rData = cbind(lltc, sbux, fill = 0), alignBy = "minutes",
#'                        alignPeriod = 5, makeReturns = FALSE)
#' rcKernel
#' @keywords volatility
#' @export
rKernelCov <- function(rData, cor = FALSE,  alignBy = "seconds", alignPeriod = 1,
                       makeReturns = FALSE, kernelType = "rectangular", kernelParam = 1,
                       kernelDOFadj = TRUE) {
  
  if (isMultiXts(rData)) {
    stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")
  }
  
  # # Aggregate:
  if ((!is.null(alignBy)) && (!is.null(alignPeriod))) {
    rData <- fastTickAgregation(rData, on = alignBy, k = alignPeriod)
  }
  if (makeReturns) {
    rData <- makeReturns(rData)
  }
  
  if (is.null(dim(rData))) {
    n <- 1
  } else {
    n <- dim(rData)[2]
  }
  type <- kernelCharToInt(kernelType)
  if (n == 1) {
    return(kernelEstimator(as.double(rData),
                           as.double(rData),
                           as.integer(length(rData)),
                           as.integer(kernelParam),
                           as.integer(ifelse(kernelDOFadj, 1, 0)),
                           as.integer(type),
                           ab = double(kernelParam + 1),
                           ab2 = double(kernelParam + 1)))
  }
  
  if (n > 1) {
    cov <- matrix(rep(0, n * n), ncol = n)
    diagonal <- c()
    for (i in 1:n) {
      diagonal[i] <- 
        kernelEstimator(as.double(rData[, i]), as.double(rData[, i]), as.integer(length(rData[, i])),
                        as.integer(kernelParam), as.integer(ifelse(kernelDOFadj, 1, 0)),
                        as.integer(type), ab = double(kernelParam + 1),
                        ab2 = double(kernelParam + 1))
    }
    diag(cov) <- diagonal
    
    for (i in 2:n) {
      for (j in 1:(i - 1)) {
        cov[i, j] = cov[j, i] = kernelEstimator(as.double(rData[, i]), as.double(rData[, j]), as.integer(length(rData[, i])),
                                   as.integer(kernelParam), as.integer(ifelse(kernelDOFadj, 1, 0)),
                                   as.integer(type), ab = double(kernelParam + 1),
                                   ab2 = double(kernelParam + 1))
      }
    }
    if (!cor) {
      return(makePsd(cov))
    }
    if (cor) {
      invsdmatrix <- try(solve(sqrt(diag(diag(cov)))), silent = F)
      if (!inherits(invsdmatrix, "try-error")) {
        rcor <- invsdmatrix %*% cov %*% invsdmatrix
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
#' @param rData a zoo/xts object containing all returns in period t for one asset.
#' @param alignBy a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param alignPeriod an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns boolean, should be TRUE when rData contains prices instead of returns. FALSE by   default.
#'
#' @return numeric
#'
#' @references Amaya, D., Christoffersen, P., Jacobs, K. and Vasquez, A. (2011). Do realized skewness and kurtosis predict the cross-section of equity returns?. CREATES research paper. p. 3-7.
#'
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#' 
#' @examples 
#' data(sampleTData)
#' rKurt(sampleTData$PRICE, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)
#' 
#' @keywords highfrequency rKurt
#' @importFrom xts apply.daily
#' @export
rKurt <- function(rData, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE) {
  
  # self-reference for multi-day input
  if (checkMultiDays(rData)) { 
    result <- apply.daily(rData, rKurt, alignBy, alignPeriod, makeReturns)
    return(result)
  } else {
    if ((!is.null(alignBy)) && (!is.null(alignPeriod))) {
      rData <- fastTickAgregation(rData, on = alignBy, k = alignPeriod)
    }
    if (makeReturns) {
      rData <- makeReturns(rData)
    }
    
    q <- as.numeric(rData)
    N <- length(q)
    
    rv <- RV(rData)
    
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
#' @param rData a zoo/xts object containing all returns in period t for one asset.
#' @param m the window size of return blocks. 2 by default.
#' @param p the power of the variation. 2 by default.
#' @param alignBy a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param alignPeriod an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns boolean, should be TRUE when rData contains prices instead of returns. FALSE by  default.
#'
#' @return numeric
#' 
#' @references Andersen, T. G., D. Dobrev, and E. Schaumburg (2012). Jump-robust volatility estimation using nearest neighbor truncation. Journal of Econometrics, 169(1), 75- 93.
#'
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#' 
#' @examples
#' data(sampleTData)
#' rMPV(sampleTData$PRICE, m = 2, p = 3, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)
#' 
#' @keywords highfrequency rMPV
#' @export
rMPV <- function(rData, m = 2, p = 2, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE) {
  
  # self-reference for multi-day input
  if (checkMultiDays(rData)) { 
    result <- apply.daily(rData, rMPV, alignBy, alignPeriod, makeReturns)
    return(result)
  } else {
    if ((!is.null(alignBy)) && (!is.null(alignPeriod))) {
      rData <- fastTickAgregation(rData, on = alignBy, k = alignPeriod)
    }
    if (makeReturns) {
      rData <- makeReturns(rData)
    }
    
    if (m > p/2) { 
      m <- as.numeric(m) ##m> p/2
      p <- as.numeric(p)
      q <- as.numeric(rData)
      q <- abs(rollapply(q,width=m,FUN=prod,align="left"))
      N <- length(rData)
      
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
#' @param rData a \eqn{(M x N)} matrix/zoo/xts object containing the \eqn{N}
#' return series over period \eqn{t}, with \eqn{M} observations during \eqn{t}.
#' @param cor boolean, in case it is TRUE, the correlation is returned. FALSE by default.
#' @param alignBy a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param alignPeriod an integer, align the tick data to this many [seconds|minutes|hours]. 
#' @param makeReturns boolean, should be TRUE when rData contains prices instead of returns. FALSE by default.
#' @param seasadjR a \eqn{(M x N)} matrix/zoo/xts object containing 
#' the seasonaly adjusted returns. This is an optional argument.
#' @param wFunction determines whether 
#' a zero-one weight function (one if no jump is detected based on \eqn{d_{t,i}} and 0 otherwise)
#' or 
#' Soft Rejection ("SR") weight function is to be used.
#' By default a zero-one weight function (wFunction = "HR") is used.
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
#' # Realized Outlyingness Weighted Variance/Covariance for prices aligned   
#' # at 5 minutes.
#' 
#' # Univariate: 
#' rvoutw <- rOWCov(rData = sampleTData$PRICE, alignBy = "minutes",
#'                    alignPeriod = 5, makeReturns = TRUE)
#' rvoutw 
#' 
#' # Multivariate: 
#' rcoutw <- rOWCov(rData = sample5MinPricesJumps['2010-01-04'], makeReturns = TRUE)
#' rcoutw
#' 
#' @keywords volatility
#' @export
rOWCov <- function (rData, cor = FALSE, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE, seasadjR = NULL,
                    wFunction = "HR" , alphaMCD = 0.75, alpha = 0.001){
  
  if (is.null(seasadjR)) { 
    seasadjR <- rData 
  }
  if (isMultiXts(rData)) { 
    stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")
  }
  
  # Aggregate:
  if ((!is.null(alignBy))&&(!is.null(alignPeriod))) {
    rData <- fastTickAgregation(rData, on = alignBy, k = alignPeriod)
    seasadjR <- fastTickAgregation(seasadjR, on = alignBy, k = alignPeriod)
  }     
  if (makeReturns) { 
    rData <- makeReturns(rData)
    if (!is.null(seasadjR)) { 
      seasadjR <- makeReturns(seasadjR)
    } 
  }
  
  if (is.null(dim(rData))) { 
    n <- 1 
  } else { 
    n <- dim(rData)[2]
  }        
  
  if (n == 1) { 
    return(ROWVar(rData, seasadjR = seasadjR, wFunction = wFunction, alphaMCD = alphaMCD, alpha = alpha ))
  }
  
  if (n > 1) { 
    if ((dim(rData)[2] < 2)) {
      stop("Your rData object should have at least 2 columns")
    }
    
    rData <- as.matrix(rData)
    seasadjR <- as.matrix(seasadjR)
    intraT <- nrow(rData)
    N <- ncol(rData)
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
    
    if (wFunction == "HR") {
      weights[outlierindic] = 0
      wR <- sqrt(weights) * rData
      covariance <- (conHR(di = N, alpha = alpha) * t(wR) %*% wR) / mean(weights)
      if (!cor) {
        return(covariance)
      } else {
        sdmatrix = sqrt(diag(diag(covariance)))
        inv_matrix <- solve(sdmatrix)
        rcor <- inv_matrix %*% covariance %*% inv_matrix
        return(rcor)
      }
    }
    if (wFunction == "SR") {
      weights[outlierindic] <- k/outlyingness[outlierindic]
      wR <- sqrt(weights) * rData
      covariance <- (conhuber(di = N, alpha = alpha) * t(wR) %*% wR) / mean(weights)
      if (!cor) {
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
#' @param rData a zoo/xts object containing all returns in period t for one asset.
#' @param alignBy a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param alignPeriod an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns boolean, should be TRUE when rData contains prices instead of returns. FALSE by   default.
#' 
#' @return numeric
#' 
#' @references  Amaya, D., Christoffersen, P., Jacobs, K. and Vasquez, A. (2011). Do realized skewness and kurtosis predict the cross-section of equity returns?. CREATES research paper. p. 3-7.
#'
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#' 
#' @examples 
#' data(sampleTData)
#' rSkew(sampleTData$PRICE,alignBy ="minutes", alignPeriod =5, makeReturns = TRUE)
#' 
#' @keywords highfrequency rSkew
#' @export
rSkew <- function(rData, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE) {
  
  # self-reference for multi-day input
  if (checkMultiDays(rData)) { 
    result <- apply.daily(rData, rSkew, alignBy, alignPeriod, makeReturns)
    return(result)
  } else {
    if ((!is.null(alignBy)) && (!is.null(alignPeriod))) {
      rData <- fastTickAgregation(rData, on = alignBy, k = alignPeriod)
    }
    if (makeReturns) {
      rData <- makeReturns(rData)
    }
    
    q <-as.numeric(rData)
    N <- length(q)
    
    rv <- RV(rData)
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
#' @param rData a zoo/xts object containing all returns in period t for one asset.
#' @param alignBy a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param alignPeriod an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns boolean, should be TRUE when rData contains prices instead of returns. FALSE by   default.
#' @return list with to arguments. The realized positive and negative semivariance.
#' @examples 
#' \dontrun{
#' data(sampleTData)
#' rSV(sampleTData$PRICE, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)
#' }
#' @references Barndorff-Nielsen, O.E., Kinnebrock, S. and Shephard N. (2008). Measuring downside risk - realized semivariance. CREATES research paper. p. 3-5.
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#' @keywords  highfrequency rSV
#' @export
rSV <- function(rData, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE) {
  
  # self-reference for multi-day input
  if (checkMultiDays(rData)) {
    result <- apply.daily(rData, rSV, alignBy, alignPeriod, makeReturns)
    colnames(result) = c("downside", "upside")
    return(result)
  } else {
    
    if ((!is.null(alignBy)) && (!is.null(alignPeriod))) {
      rData <- fastTickAgregation(rData, on = alignBy, k = alignPeriod)
    }
    if (makeReturns)  {
      rData <- makeReturns(rData)
    }
    
    q <- as.numeric(rData)
    select.down <- rData < 0
    select.up <- rData > 0
    
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
#' @param rData a \eqn{(M x N)} matrix/zoo/xts object containing the \eqn{N}
#' return series over period \eqn{t}, with \eqn{M} observations during \eqn{t}.
#' @param cor boolean, in case it is TRUE, the correlation is returned. FALSE by default.
#' @param alignBy a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param alignPeriod an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns boolean, should be TRUE when rData contains prices instead of returns. FALSE by default.
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
#' rcThreshold <- rThresholdCov(cbind(lltc, sbux), alignBy = "minutes", alignPeriod = 1) 
#' rcThreshold  
#' 
#' @keywords volatility
#' @export
rThresholdCov <- function(rData, cor = FALSE, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE) {
  rdatacheck(rData, multi = TRUE)
  # Multiday adjustment: 
  if (isMultiXts(rData)) { 
    if (is.null(dim(rData))) {  
      n <- 1
    } else { 
      n <- dim(rData)[2] 
    }
    if (n == 1) { 
      result <- apply.daily(rData, rThresholdCov, cor = cor, alignBy = alignBy, 
                            alignPeriod = alignPeriod, makeReturns = makeReturns) 
    }
    if (n > 1) { 
      result <- applyGetList(rData, rThresholdCov, cor = cor, alignBy = alignBy,
                              alignPeriod = alignPeriod, makeReturns = makeReturns)
      names(result) <- unique(as.Date(index(rData)))
    }    
    return(result)
  } else { #single day code
    if (!is.null(alignBy) && (!is.null(alignPeriod))) {
      rData <- fastTickAgregation(rData, on = alignBy, k = alignPeriod)
    } 
    if (makeReturns) { 
      rData <- makeReturns(rData) 
    }  
    
    rData <- as.matrix(rData)
    n <- dim(rData)[1]				                  # number of observations
    delta <- 1 / n
    rbpvars <- apply(rData, 2,FUN = RBPVar)		      # bipower variation per stock
    tresholds <- 3 * sqrt(rbpvars) * (delta^(0.49))	  # treshold per stock
    tresmatrix <- matrix(rep(tresholds, n), ncol = length(tresholds), nrow = n, byrow = TRUE)
    condition <- abs(rData) > tresmatrix
    rData[condition] <- 0
    covariance <- rCov(rData)
    
    if (!cor) { 
      return(covariance) 
    }
    if (cor) {
      sdmatrix <- sqrt(diag(diag(covariance)))
      rcor     <- solve(sdmatrix) %*% covariance %*% solve(sdmatrix)
      return(rcor)
    }
  } 
} 


#' Robust two time scale covariance estimation
#' 
#' @description Function returns the robust two time scale covariance matrix proposed in Boudt and Zhang (2010). 
#' Unlike the \code{\link{rOWCov}}, but similarly to the \code{\link{rThresholdCov}}, the rRTSCov uses  univariate jump detection rules 
#' to truncate the effect of jumps on the covariance 
#' estimate. By the use of two time scales, this covariance estimate 
#' is not only robust to price jumps, but also to microstructure noise and non-synchronic trading. 
#' 
#' @param pData a list. Each list-item i contains an xts object with the intraday price data 
#' of stock i for day t.
#' @param cor boolean, in case it is TRUE, the correlation is returned. FALSE by default.
#' @param startIV vector containing the first step estimates of the integrated variance of the assets, needed in the truncation. Is NULL by default. 
#' @param noisevar vector containing the estimates of the noise variance of the assets, needed in the truncation. Is NULL by default.
#' @param K positive integer, slow time scale returns are computed on prices that are K steps apart.
#' @param J positive integer, fast time scale returns are computed on prices that are J steps apart.
#' @param K_cov positive integer, for the extradiagonal covariance elements the slow time scale returns are computed on prices that are K steps apart.
#' @param J_cov positive integer, for the extradiagonal covariance elements the fast time scale returns are computed on prices that are J steps apart.
#' @param K_var vector of positive integers, for the diagonal variance elements the slow time scale returns are computed on prices that are K steps apart.
#' @param J_var vector of positive integers, for the diagonal variance elements the fast time scale returns are computed on prices that are J steps apart.
#' @param makePsd boolean, in case it is TRUE, the positive definite version of rRTSCov is returned. FALSE by default.
#' @param eta positive real number, squared standardized high-frequency returns that exceed eta are detected as jumps.
#'
#' @return an \eqn{N x N} matrix
#' 
#' @details 
#' The rRTSCov requires the tick-by-tick transaction prices. (Co)variances are then computed using log-returns calculated on a rolling basis 
#' on stock prices that are \eqn{K} (slow time scale) and \eqn{J} (fast time scale) steps apart.     
#' 
#' The diagonal elements of the rRTSCov matrix are the variances, computed for log-price series \eqn{X} with \eqn{n} price observations 
#' at times \eqn{  \tau_1,\tau_2,\ldots,\tau_n} as follows: 
#' \deqn{
#' (1-\frac{\overline{n}_K}{\overline{n}_J})^{-1}(\{X,X\}_T^{(K)^{*}}-\frac{\overline{n}_K}{\overline{n}_J}\{X,X\}_T^{(J)^{*}}),
#' }
#' where \eqn{\overline{n}_K=(n-K+1)/K},  \eqn{\overline{n}_J=(n-J+1)/J} and 
#' \deqn{\{X,X\}_T^{(K)^{*}} =\frac{c_\eta^{*}}{K}\frac{\sum_{i=1}^{n-K+1}(X_{t_{i+K}}-X_{t_i})^2I_X^K(i;\eta)}{\frac{1}{n-K+1}\sum_{i=1}^{n-K+1}I_X^K(i;\eta)}.} 
#' The constant  \eqn{c_\eta} adjusts for the bias due to the thresholding  and \eqn{I_{X}^K(i;\eta)} is a jump indicator function
#' that is one if 
#' \deqn{ \frac{(X_{t_{i+K}}-X_{t_{i}})^2}{(\int_{t_{i}}^{t_{i+K}} \sigma^2_sds +2\sigma_{\varepsilon_{\mbox{\tiny X}}}^2)}  \ \ \leq  \ \    \eta } 
#' and zero otherwise.  The elements in the denominator are the integrated variance (estimated recursively) and noise variance (estimated by the method in Zhang et al, 2005). 
#' 
#' The extradiagonal elements of the rRTSCov are the covariances. 
#' For their calculation, the data is first synchronized by the refresh time method proposed by Harris et al (1995).
#' It uses the function \code{\link{refreshTime}} to collect first the so-called refresh times at which all assets have traded at least once 
#' since the last refresh time point. Suppose we have two log-price series:  \eqn{X} and \eqn{Y}. Let \eqn{ \Gamma =\{ \tau_1,\tau_2,\ldots,\tau_{N^{\mbox{\tiny X}}_{\mbox{\tiny T}}}\}} and 
#' \eqn{\Theta=\{\theta_1,\theta_2,\ldots,\theta_{N^{\mbox{\tiny Y}}_{\mbox{\tiny T}}}\}} 
#' be the set of transaction times of these assets. 
#' The first refresh time corresponds to the first time at which both stocks have traded, i.e. 
#' \eqn{\phi_1=\max(\tau_1,\theta_1)}. The subsequent refresh time is defined as the first time when both stocks have again traded, i.e.
#' \eqn{\phi_{j+1}=\max(\tau_{N^{\mbox{\tiny{X}}}_{\phi_j}+1},\theta_{N^{\mbox{\tiny{Y}}}_{\phi_j}+1})}. The
#' complete refresh time sample grid is 
#' \eqn{\Phi=\{\phi_1,\phi_2,...,\phi_{M_N+1}\}}, where \eqn{M_N} is the total number of paired returns.  The
#' sampling points of asset \eqn{X} and \eqn{Y} are defined to be
#' \eqn{t_i=\max\{\tau\in\Gamma:\tau\leq \phi_i\}} and
#' \eqn{s_i=\max\{\theta\in\Theta:\theta\leq \phi_i\}}.
#' 
#' Given these refresh times, the covariance is computed as follows: 
#' \deqn{
#' c_{N}( \{X,Y\}^{(K)}_T-\frac{\overline{n}_K}{\overline{n}_J}\{X,Y\}^{(J)}_T ),
#' }
#' 
#' where
#' \deqn{\{X,Y\}^{(K)}_T =\frac{1}{K} \frac{\sum_{i=1}^{M_N-K+1}c_i (X_{t_{i+K}}-X_{t_{i}})(Y_{s_{i+K}}-Y_{s_{i}})I_{X}^K(i;\eta)
#' I_{Y}^K(i;\eta)}{\frac{1}{M_N-K+1}\sum_{i=1}^{M_N-K+1}{I_X^K(i;\eta)I_Y^K(i;\eta)}},}
#' with  \eqn{I_{X}^K(i;\eta)} the same jump indicator function as for the variance and \eqn{c_N} a constant to adjust for the bias due to the thresholding.  
#' 
#' Unfortunately, the rRTSCov is not always positive semidefinite.  
#' By setting the argument makePsd = TRUE, the function  \code{\link{makePsd}} is used to return a positive semidefinite
#' matrix. This function replaces the negative eigenvalues with zeroes. 
#' 
#' @references 
#' Boudt K. and Zhang, J. 2010. Jump robust two time scale covariance estimation and realized volatility budgets. Mimeo.
#' 
#' Harris, F., T. McInish, G. Shoesmith, and R. Wood (1995). Cointegration, error correction, and price discovery on infomationally linked security markets. Journal of Financial and Quantitative Analysis 30, 563-581.
#' 
#' Zhang, L., P. A. Mykland, and Y. Ait-Sahalia (2005). A tale of two time scales: Determining integrated volatility with noisy high-frequency data. Journal of the American Statistical Association 100, 1394-1411.
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' @examples
#' # Robust Realized two timescales Variance/Covariance
#' data(sampleTData)
#' # Univariate: 
#' rvRTS <- rRTSCov(pData = sampleTData$PRICE)
#' # Note: Prices as input
#' rvRTS 
#' 
#' # Multivariate:
#' rcRTS <- rRTSCov(pData = list(cumsum(lltc) + 100, cumsum(sbux) + 100))
#' # Note: List of prices as input
#' rcRTS 
#' 
#' @keywords volatility
#' @export
rRTSCov <- function (pData, cor = FALSE, startIV = NULL, noisevar = NULL, 
                     K = 300, J = 1, 
                     K_cov = NULL , J_cov = NULL,
                     K_var = NULL , J_var = NULL , 
                     eta = 9, makePsd = FALSE){
  if (!is.list(pData)) {
    n <- 1
  }
  else {
    n = length(pData)
    if (n == 1) {
      pData = pData[[1]]
    }
  }
  
  if (n == 1) {
    if (nrow(pData) < (10 * K) ) {
      stop("Two time scale estimator uses returns based on prices that are K ticks aways. 
           Please provide a timeseries of at least 10*K" ) 
    } 
    if (isMultiXts(pData)) { 
      stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input.")
    }    
    return(RTSRV(pData, startIV = startIV, noisevar = noisevar, 
                 K = K, J = J, eta = eta))
  }
  if (n > 1) {
    if (nrow(pData[[1]]) < (10*K)) {
      stop("Two time scale estimator uses returns based on prices that are K ticks aways. 
           Please provide a timeseries of at least 10*K" ) 
    } 
    
    if (isMultiXts(pData[[1]])) { 
      stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input.")
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
      diagonal[i] <- RTSRV(pData[[i]], startIV = startIV[i], 
                           noisevar = noisevar[i], K = K_var[i], J = J_var[i], 
                           eta = eta)
    }
    diag(cov) <- diagonal
    if( is.null(K_cov)){ K_cov = K }
    if( is.null(J_cov)){ J_cov = J }                        
    for (i in 2:n) {
      for (j in 1:(i - 1)) {
        cov[i, j] = cov[j, i] = RTSCov_bi(pData[[i]], 
                                          pData[[j]], startIV1 = diagonal[i], startIV2 = diagonal[j], 
                                          noisevar1 = noisevar[i], noisevar2 = noisevar[j], 
                                          K = K_cov, J = J_cov, eta = eta)
      }
    }
    if (!cor) {
      if (makePsd) {
        cov = makePsd(cov)
      }
      return(cov)
    } else {
      invsdmatrix = try(solve(sqrt(diag(diag(cov)))), silent = F)
      if (!inherits(invsdmatrix, "try-error")) {
        rcor = invsdmatrix %*% cov %*% invsdmatrix
        if (makePsd) {
          rcor = makePsd(rcor)
        }
        return(rcor)
      }
    }
  }
}

#' An estimator of realized variance.
#' @param rData a zoo/xts object containing all returns in period t for one asset.
#' 
#' @return numeric
#' 
#' @keywords highfrequency RV
#' @export
RV <- function(rData) {
  returns <- as.numeric(rData)
  RV <- sum(returns^2)
  return(RV)
}


#' Realized tri-power variation estimator of quarticity for a highfrequency return series.
#' 
#' @description Function returns the rTPVar, defined in Andersen et al. (2012).
#'  
#'  Assume there is \eqn{N} equispaced returns in period \eqn{t}. Let \eqn{r_{t,i}} be a return (with \eqn{i=1, \ldots,N}) in period \eqn{t}.
#'  
#'  Then, the rTPVar is given by
#'  \deqn{
#'    \mbox{rTPVar}_{t}=N\frac{N}{N-2} \left(\frac{\Gamma \left(0.5\right)}{ 2^{2/3}\Gamma \left(7/6\right)} \right)^{3} \sum_{i=3}^{N} \mbox({|r_{t,i}|}^{4/3} {|r_{t,i-1}|}^{4/3} {|r_{t,i-2}|}^{4/3})
#'  }
#'  
#' @param rData a zoo/xts object containing all returns in period t for one asset.
#' @param alignBy a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param alignPeriod an integer, align the tick data to this many [seconds|minutes|hours].  
#' @param makeReturns boolean, should be TRUE when rData contains prices instead of returns. FALSE by   default.
#'  
#' @return numeric
#'  
#' @references Andersen, T. G., D. Dobrev, and E. Schaumburg (2012). Jump-robust volatility estimation using nearest neighbor truncation. Journal of Econometrics, 169(1), 75- 93.
#'  
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#'  
#' @examples 
#' data(sampleTData)
#' rTPVar(rData = sampleTData$PRICE, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)
#' rTPVar
#' 
#' @importFrom zoo rollapply
#' @keywords highfrequency rTPVar
#' @export
rTPVar <- function(rData, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE) {
  
  # self-reference for multi-day input
  if (checkMultiDays(rData)) { 
    result <- apply.daily(rData, rTPVar, alignBy, alignPeriod, makeReturns)
    return(result)
  } else {
    if ((!is.null(alignBy)) && (!is.null(alignPeriod))) {
      rData <- fastTickAgregation(rData, on = alignBy, k = alignPeriod)
    }
    if (makeReturns) {
      rData <- makeReturns(rData)
    }
    
    q      <- as.numeric(rData)
    q      <- abs(rollapply(q, width = 3, FUN = prod, align = "left"))
    N      <- length(q)+2
    rTPVar <- N * (N/(N - 2)) * ((gamma(0.5)/(2^(2/3)*gamma( 7/6 ) ))^3) * sum(q^(4/3))
    return(rTPVar)
  }
}

#' Calculate the realized tripower quarticity
#' 
#' @param rData a zoo/xts object containing all returns in period t for one asset.
#' 
#' @return numeric
#' 
#' @export
RTQ <- function(rData) { 
  returns <- as.vector(as.numeric(rData))
  n <- length(returns)
  tq <- n * (n/(n-2)) *((2^(2/3) * gamma(7/6) * gamma(1/2)^(-1))^(-3)) *  sum(abs(returns[1:(n - 2)])^(4/3) * abs(returns[2:(n-1)])^(4/3) * abs(returns[3:n])^(4/3))
  return(tq)
} 

#' Realized quad-power variation of highfrequency return series. 
#' @description Function returns the realized quad-power variation, defined in Andersen et al. (2012).
#'  
#'  Assume there is \eqn{N} equispaced returns in period \eqn{t}. Let \eqn{r_{t,i}} be a return (with \eqn{i=1, \ldots,N}) in period \eqn{t}.
#'  
#'  Then, the rQPVar is given by
#'  \deqn{
#'    \mbox{rQPVar}_{t}=N*\frac{N}{N-3} \left(\frac{\pi^2}{4} \right)^{-4} \mbox({|r_{t,i}|} {|r_{t,i-1}|} {|r_{t,i-2}|} {|r_{t,i-3}|})
#'  }
#'
#' @param rData a zoo/xts object containing all returns in period t for one asset.
#' @param alignBy a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param alignPeriod an integer, align the tick data to this many [seconds|minutes|hours].  
#' @param makeReturns boolean, should be TRUE when rData contains prices instead of returns. FALSE by default.
#'
#' @return numeric
#' 
#' @references Andersen, T. G., D. Dobrev, and E. Schaumburg (2012). Jump-robust volatility estimation using nearest neighbor truncation. Journal of Econometrics, 169(1), 75- 93.
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#' 
#' @examples 
#' data(sampleTData)
#' rQPVar(rData= sampleTData$PRICE, alignBy= "minutes", alignPeriod =5, makeReturns= TRUE)
#' rQPVar
#' 
#' @keywords highfrequency rQPVar
#' @export
rQPVar <- function(rData, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE) {
  
  # self-reference for multi-day input
  if (checkMultiDays(rData)) { 
    result <- apply.daily(rData, rQPVar, alignBy, alignPeriod, makeReturns)
    return(result)
  } else {
    if (!is.null(alignBy) && !is.null(alignPeriod)) {
      rData <-fastTickAgregation(rData, on = alignBy, k = alignPeriod)
    }
    if (makeReturns) {
      rData <- makeReturns(rData)
    }
    q <- abs(as.numeric(rData))
    q <- rollapply(q, width = 4, FUN = prod, align = "left")
    N <- length(q) + 3
    rQPVar <- N* (N / (N-3)) * (pi^2 / 4) * sum(q)
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
#' @param rData a zoo/xts object containing all returns in period t for one asset.
#' @param alignBy a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param alignPeriod an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns boolean, should be TRUE when rData contains prices instead of returns. FALSE by default.
#' 
#' @return numeric
#'
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#' @references  Andersen, T. G., D. Dobrev, and E. Schaumburg (2012). Jump-robust volatility estimation using nearest neighbor truncation. Journal of Econometrics, 169(1), 75- 93.
#' @examples 
#' \dontrun{
#' data(sampleTData)
#' rQuar(rData = sampleTData$PRICE, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)
#' }
#' @keywords  highfrequency rQuar
#' @export
rQuar <- function(rData, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE) {
  
  # self-reference for multi-day input
  if (checkMultiDays(rData)) { 
    result <- apply.daily(rData, rQuar, alignBy, alignPeriod, makeReturns)
    return(result)
  } else {
    if ((!is.null(alignBy)) && (!is.null(alignPeriod))) {
      rData <- fastTickAgregation(rData, on = alignBy, k = alignPeriod)
    }
    if (makeReturns) {
      rData <- makeReturns(rData)
    }
    q <- as.numeric(rData)
    N <- length(q) + 1
    rQuar <- N/3 * sum(q^4)
    return(rQuar)
  }
}

#' Two time scale covariance estimation 
#' 
#' @description Function returns the two time scale covariance matrix proposed in Zhang et al (2005) and Zhang (2010).
#' By the use of two time scales, this covariance estimate 
#' is robust to microstructure noise and non-synchronic trading.
#' 
#' @param pData a list. Each list-item i contains an xts object with the intraday price data 
#' of stock i for day t.
#' @param cor boolean, in case it is TRUE, the correlation is returned. FALSE by default.
#' @param K positive integer, slow time scale returns are computed on prices that are K steps apart.
#' @param J positive integer, fast time scale returns are computed on prices that are J steps apart.
#' @param K_cov positive integer, for the extradiagonal covariance elements the slow time scale returns are computed on prices that are K steps apart.
#' @param J_cov positive integer, for the extradiagonal covariance elements the fast time scale returns are computed on prices that are J steps apart.
#' @param K_var vector of positive integers, for the diagonal variance elements the slow time scale returns are computed on prices that are K steps apart.
#' @param J_var vector of positive integers, for the diagonal variance elements the fast time scale returns are computed on prices that are J steps apart.
#' @param makePsd boolean, in case it is TRUE, the positive definite version of rTSCov is returned. FALSE by default.
#' 
#' @return an \eqn{N x N} matrix
#' 
#' @details The rTSCov requires the tick-by-tick transaction prices. (Co)variances are then computed using log-returns calculated on a rolling basis 
#' on stock prices that are \eqn{K} (slow time scale) and \eqn{J} (fast time scale) steps apart.
#' 
#' The diagonal elements of the rTSCov matrix are the variances, computed for log-price series \eqn{X} with \eqn{n} price observations 
#' at times \eqn{  \tau_1,\tau_2,\ldots,\tau_n} as follows: 
#' 
#' \deqn{(1-\frac{\overline{n}_K}{\overline{n}_J})^{-1}([X,X]_T^{(K)}-
#'        \frac{\overline{n}_K}{\overline{n}_J}[X,X]_T^{(J))}}
#'        
#' where \eqn{\overline{n}_K=(n-K+1)/K},  \eqn{\overline{n}_J=(n-J+1)/J} and
#' \deqn{[X,X]_T^{(K)} =\frac{1}{K}\sum_{i=1}^{n-K+1}(X_{t_{i+K}}-X_{t_i})^2.} 
#' 
#' The extradiagonal elements of the rTSCov are the covariances. 
#' For their calculation, the data is first synchronized by the refresh time method proposed by Harris et al (1995).
#' It uses the function \code{\link{refreshTime}} to collect first the so-called refresh times at which all assets have traded at least once 
#' since the last refresh time point. Suppose we have two log-price series:  \eqn{X} and \eqn{Y}. Let \eqn{ \Gamma =\{ \tau_1,\tau_2,\ldots,\tau_{N^{\mbox{\tiny X}}_{\mbox{\tiny T}}}\}} and 
#' \eqn{\Theta=\{\theta_1,\theta_2,\ldots,\theta_{N^{\mbox{\tiny Y}}_{\mbox{\tiny T}}}\}} 
#' be the set of transaction times of these assets. 
#' The first refresh time corresponds to the first time at which both stocks have traded, i.e. 
#' \eqn{\phi_1=\max(\tau_1,\theta_1)}. The subsequent refresh time is defined as the first time when both stocks have again traded, i.e.
#' \eqn{\phi_{j+1}=\max(\tau_{N^{\mbox{\tiny{X}}}_{\phi_j}+1},\theta_{N^{\mbox{\tiny{Y}}}_{\phi_j}+1})}. The
#' complete refresh time sample grid is 
#' \eqn{\Phi=\{\phi_1,\phi_2,...,\phi_{M_N+1}\}}, where \eqn{M_N} is the total number of paired returns.  The
#' sampling points of asset \eqn{X} and \eqn{Y} are defined to be
#' \eqn{t_i=\max\{\tau\in\Gamma:\tau\leq \phi_i\}} and
#' \eqn{s_i=\max\{\theta\in\Theta:\theta\leq \phi_i\}}.
#' 
#' Given these refresh times, the covariance is computed as follows: 
#' \deqn{
#' c_{N}( [X,Y]^{(K)}_T-\frac{\overline{n}_K}{\overline{n}_J}[X,Y]^{(J)}_T ),
#' }
#' 
#' where
#' \deqn{[X,Y]^{(K)}_T =\frac{1}{K} \sum_{i=1}^{M_N-K+1} (X_{t_{i+K}}-X_{t_{i}})(Y_{s_{i+K}}-Y_{s_{i}}).}
#' 
#' Unfortunately, the rTSCov is not always positive semidefinite.  
#' By setting the argument makePsd = TRUE, the function \code{\link{makePsd}} is used to return a positive semidefinite
#' matrix. This function replaces the negative eigenvalues with zeroes.
#' 
#' @references 
#' Harris, F., T. McInish, G. Shoesmith, and R. Wood (1995). Cointegration, error correction, and price discovery on infomationally linked security markets. Journal of Financial and Quantitative Analysis 30, 563-581.
#' Zhang, L., P. A. Mykland, and Y. Ait-Sahalia (2005). A tale of two time scales: Determining integrated volatility with noisy high-frequency data. Journal of the American Statistical Association 100, 1394-1411.
#' Zhang, L. (2011). Estimating covariation: Epps effect, microstructure noise. Journal of Econometrics 160, 33-47.
#' 
#' @author Jonathan Cornelissen and Kris Boudt

#' @examples 
#' # Robust Realized two timescales Variance/Covariance
#' 
#' # Univariate: 
#' rvts <- rTSCov(pData = sampleTData$PRICE)
#' # Note: Prices as input
#' rvts 
#' 
#' # Multivariate:
#' rcovts <- rTSCov(pData = list(cumsum(lltc) + 100, cumsum(sbux) + 100))
#' # Note: List of prices as input
#' rcovts 
#' 
#' @keywords volatility
#' @export
rTSCov <- function (pData, cor = FALSE, K = 300, J = 1, K_cov = NULL, J_cov = NULL, 
                    K_var = NULL, J_var = NULL, makePsd = FALSE) {
  if (!is.list(pData)) {
    n <- 1
  }
  else {
    n <- length(pData)
    if (n == 1) {
      pData <- pData[[1]]
    }
  }
  
  if (n == 1) {
    if (nrow(pData) < (10 * K)) {
      stop("Two time scale estimator uses returns based on prices that are K ticks aways. 
           Please provide a timeseries of at least 10 * K." ) 
    } 
    if (isMultiXts(pData)) { 
      stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input");
    }
    return(TSRV(pData, K = K, J = J))
  }
  if (n > 1) {
    if (nrow(pData[[1]]) < (10 * K)) {
      stop("Two time scale estimator uses returns based on prices that are K ticks aways. 
           Please provide a timeseries of at least 10*K" ) 
    } 
    if (isMultiXts(pData[[1]])){ 
      stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input") 
    }
    
    cov <- matrix(rep(0, n * n), ncol = n)
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
    
    diagonal <- c()
    for (i in 1:n) {
      diagonal[i] = TSRV(pData[[i]], K = K_var[i], J = J_var[i])
    }
    diag(cov) <- diagonal
    
    for (i in 2:n) {
      for (j in 1:(i - 1)) {
        cov[i, j] = cov[j, i] = TSCov_bi(pData[[i]], 
                                         pData[[j]], K = K_cov, J = J_cov)
      }
    }
    if (!cor) {
      if (makePsd) {
        cov <- makePsd(cov)
      }
      return(cov)
    } else {
      invsdmatrix <- try(solve(sqrt(diag(diag(cov)))), silent = F)
      if (!inherits(invsdmatrix, "try-error")) {
        rcor <- invsdmatrix %*% cov %*% invsdmatrix
        if (makePsd) {
          rcor <- makePsd(rcor)
        }
        return(rcor)
      }
    }
  }
}



#' rCholCov positive semi-definite covariance estimation using the CholCov algorithm
#' @description Function that estimates the integrated covariance matrix using the CholCov algorithm.
#' @param pData a list. Each list-item i contains an xts object with the intraday price data 
#' of stock i for day t. The order of the data does not matter as it will be sorted according to the criterion specified in the \code{criterion} argument
#' @param IVest integrated variance estimator, default is \code{"MRC"}. For a list of implemented estimators, use listCholCovEstimators().
#' @param COVest covariance estimator, default is \code{"MRC"}. For a list of implemented estimators, use listCholCovEstimators().
#' @param criterion criterion to use for sorting the data according to liquidity. Possible values are ["squared duration"|"duration"|"count"], defaults to \code{"squared duration"}.
#' @param ... additional arguments to pass to IVest and COVest. See details.
#' 
#' @return a list containing the covariance matrix "CholCov", and the Cholesky decomposition "L" and "G" such that L * G * L' = CholCov
#' 
#' @details
#' additional arguments for IVest and COVest should be passed in the ... argument. For the MRC estimator, the theta and delta parameters can be set. These default to 1 and 0.1 respectively.
#' 
#' @references 
#' Boudt, Laurent Lunde, Quaedvlieg, Sauri(2017) Positive semidefinite integrated covariance estimation, factorizations and asynchronicity. Journal of Econometrics 196, 347-367
#' @author Emil Sjoerup
#' 
#' @importFrom xts xts
#' @importFrom zoo coredata
#' @export
rCholCov <- function(pData, IVest = "MRC", COVest = "MRC", criterion = "squared duration", ...){
  
  if(!is.list(pData)){
    stop("pData must be a list of atleast length one")
  }
  if(!all(as.logical(lapply(pData, is.xts)))){
    stop("All the series in pData must be xts objects")
  }
  if(criterion == "squared duration"){
    criterion <- function(x) sum(as.numeric(diff(index(x)))^2)
  } else if( criterion == "duration"){
    criterion <- function(x) sum(as.numeric(diff(index(x))))
  } else if( criterion == "count"){
    criterion <- function(x) length(x)
  } else {
    stop("Criterion must be either 'squared duration' or 'duration' or 'count'")
  }
  
  if(!(IVest %in% listCholCovEstimators() & COVest %in% listCholCovEstimators())){
    stop("rCholCov IVest or COVest not in the available CholCov estimators. See listCholCovEstimators() for list of implemented estimators.")
  }
  
  
  options <- list(...)
  op <- list("delta" = 0.1, "theta" = 1, "alignBy" = "minutes", "alignPeriod" = 5, "kernelType" = "rectangular", "kernelParam" = 1, "kernelDOFadj" = TRUE,
             "startIV" = NULL, "noisevar" = NULL, "K" = 300, "J" = 1, "K_cov" = NULL, "J_cov" = NULL, "K_var" = NULL, "J_var" = NULL, "eta" = 9, "makePsd" = FALSE, "k" = 1)
  op[names(options)] <- options
  delta <- op[["delta"]]
  theta <- op[["theta"]]
  alignBy <- op[["alignBy"]]
  alignPeriod <- op[["alignPeriod"]]
  kernelType <- op[["kernelType"]]
  kernelParam <- op[["kernelParam"]]
  kernelDOFadj <- op[["kernelDOFadj"]]
  startIV <- op[["startIV"]]
  noisevar <- op[["noisevar"]]
  K_cov <- op[["K_cov"]]
  J_cov <- op[["J_cov"]]
  K_var <- op[["K_var"]]
  J_var <- op[["J_var"]]
  eta <- op[["eta"]]
  makePsd <- op[["makePsd"]]
  K <- op[["K"]]
  J <- op[["J"]]
  k <- op[["k"]]
  
  if(length(delta) != 1 | !is.numeric(delta)){
    stop("delta must be a numeric of length one")
  }
  if(length(theta) != 1 | !is.numeric(theta)){
    stop("theta must be a numeric of length one")
  }
  
  vec <- sort(sapply(pData, criterion), index.return = TRUE)$ix
  nameVec <- names(pData)[vec]
  
  D <- length(pData)
  
  G <- matrix(0, D, D)
  Ltemp <- L <- diag(1,D,D)
  
  #G[1,1] <- rCov(exp(pData[[vec[1]]]), makeReturns = TRUE)
  
    for (d in 1:D) {
      
      dat <- refreshTime(lapply(vec[1:d], function(x) pData[[x]]))
      returns <- diff(dat)[-1,]
      f <- matrix(0, nrow(returns), d)
      f[,1] <- returns[,1]
      if(d>1){ # We shouldn't do this on the first pass.
        for (l in 2:d) {
          
          for (m in 1:(l-1)) {
            
            COV <- switch(COVest,
                   MRC = cholCovMRC(as.matrix(coredata(cbind(returns[,l], f[,m]))), delta = delta, theta = theta),
                   rCov = rCov(exp(cumsum(cbind(returns[,l], f[,m]))), alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = TRUE),
                   rAVGCov = rAVGCov(exp(cumsum(cbind(returns[,l], f[,m]))), alignBy = alignBy, alignPeriod = alignPeriod, k = k, makeReturns = TRUE),
                   rBPCov = rBPCov(exp(cumsum(cbind(returns[,l], f[,m]))), alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = TRUE),
                   rHYCov = rHYCov(exp(cumsum(cbind(returns[,l], f[,m]))), alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = TRUE),
                   rKernelCov = rKernelCov(exp(cumsum(cbind(returns[,l], f[,m]))), alignBy = alignBy, alignPeriod = alignPeriod,
                                           makeReturns = TRUE, kernelType = kernelType, kernelParam = kernelParam, kernelDOFadj = kernelDOFadj),
                   rOWCov = rOWCov(exp(cumsum(cbind(returns[,l], f[,m]))), alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = TRUE),
                   rRTSCov = rRTSCov(exp(cumsum(cbind(returns[,l], f[,m]))), cor = FALSE, startIV = startIV, noisevar = noisevar, K = K, J = J, 
                                     K_cov = K_cov, J_cov=J_cov, K_var=K_var, J_var = J_var, eta = eta, makePsd = makePsd ),
                   rThresholdCov = rThresholdCov(exp(cumsum(cbind(returns[,l], f[,m]))), alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = TRUE),
                   rSemiCov = rSemiCov(exp(cumsum(cbind(returns[,l], f[,m]))), alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = TRUE)
                   )
            
            
            Ltemp[l,m] <- COV[1,2]/COV[2,2]
            
          }
          
          f[,l] <- returns[,l] - f[,1:l] %*% Ltemp[l,1:l]
          
        }
      }
      L[d ,] <- Ltemp[d,]
      
      
      # In this switch, we need to use xts on the data to get the aggregation to work
      G[d,d] <- switch(IVest, 
                       MRC = cholCovMRC(as.matrix(coredata(f[,d])), delta = delta, theta = theta),
                       rCov =          rCov(xts(exp(cumsum(f[,d])), order.by = index(returns)), alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = TRUE),
                       rAVGCov =       rAVGCov(xts(exp(cumsum(f[,d])), order.by = index(returns)), alignBy = alignBy, alignPeriod = alignPeriod, k = k, makeReturns = TRUE),
                       rBPCov =        rBPCov(xts(exp(cumsum(f[,d])), order.by = index(returns)), alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = TRUE),
                       rHYCov =        rHYCov(xts(exp(cumsum(f[,d])), order.by = index(returns)), alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = TRUE),
                       rKernelCov =    rKernelCov(xts(exp(cumsum(f[,d])), order.by = index(returns)), alignBy = alignBy, alignPeriod = alignPeriod,                                        
                                                  makeReturns = TRUE, kernelType = kernelType, kernelParam = kernelParam, kernelDOFadj = kernelDOFadj),
                       rOWCov =        rOWCov(xts(exp(cumsum(f[,d])), order.by = index(returns)), alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = TRUE),
                       rRTSCov =       rRTSCov(xts(exp(cumsum(f[,d])), order.by = index(returns)), cor = FALSE, startIV = startIV, noisevar = noisevar, K = K, J = J,                              
                                               K_cov = K_cov, J_cov=J_cov, K_var=K_var, J_var = J_var, eta = eta, makePsd = makePsd ),
                       rThresholdCov = rThresholdCov(xts(exp(cumsum(f[,d])), order.by = index(returns)), alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = TRUE),
                       rSemiCov = rSemiCov(xts(exp(cumsum(f[,d])), order.by = index(returns)), alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = TRUE)
                       )
    }
    
  
  
  CholCov <- L %*% G %*% t(L)
  
  colnames(L) <- rownames(L) <- colnames(G) <- rownames(G) <- colnames(CholCov) <- rownames(CholCov) <- nameVec
  
  out <- list("CholCov" = CholCov, "L" = L, "G" = G)
  return(out)
  
}



#' Realized Semicovariance
#' 
#' @description Function returns the Realized Semicovariances (rSemiCov).
#' Let \eqn{r_{t,i}} be an intraday \eqn{M x N} return matrix and \eqn{i=1,...,M}
#' the number of intraday returns. Then, let p = max(r_{t,i},0) and n = min(r_{t,i}).
#' 
#' Then, the realized semicovariance is given by the following three matrices:
#' 
#'\deqn{
#'  \mbox{pos}_t =\sum_{i=1}^{M}p_{t,i}p'_{t,i}
#'} 
#'\deqn{
#'  \mbox{neg}_t =\sum_{i=1}^{M}n_{t,i}n'_{t,i}
#'} 
#'\deqn{
#'  \mbox{mixed}_t =\sum_{i=1}^{M}(p_{t,i}n'_{t,i} + n_{t,i}p'_{t,i})
#'}
#'
#' The mixed covariance matrix will have 0 on the diagonal.
#' From these three matrices, the realized covariance can be constructed as pos + neg + mixed.
#' The concordant semicovariance matrix is pos + neg.
#' The off-diagonals of the concordant matrix is always positive, while for the mixed matrix, it is always negative.
#'
#'  
#' @param rData a \eqn{(M x N)} matrix/zoo/xts object containing the \eqn{N}
#' return series over period \eqn{t}, with \eqn{M} observations during \eqn{t}.
#' In case of a matrix, no multi-day adjustment is possible.
#' @param cor logical, in case it is TRUE, the correlation is returned. FALSE by default.
#' @param alignBy a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param alignPeriod an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns logical, should be TRUE when rData contains prices instead of returns. FALSE by default.
#' 
#' @return In case the data consists of one day a list of four \eqn{N x N} matrices are returned. These matrices are the mixed, positive, negative and concordant
#' In case the data spans more than one day, the list for each day will be put into another list named according to the date of the estimates.
#' 
#' @details In the case that cor is TRUE, the mixed matrix will be an \eqn{N x N} matrix filled with NA as mapping the mixed covariance matrix into correlation space is impossible due to the 0-diagonal.
#' 
#' @author Emil Sjoerup
#' 
#' @examples 
#' # Realized semi-variance/semi-covariance for prices aligned   
#' # at 5 minutes.
#' data(sampleTData)
#' data(sample5MinPricesJumps)
#' 
#' # Univariate: 
#' rSV = rSemiCov(rData = sampleTData$PRICE, alignBy = "minutes", 
#'                    alignPeriod = 5, makeReturns = TRUE)
#' 
#' # Multivariate single day: 
#' rSC = rSemiCov(rData = sample5MinPricesJumps['2010-01-04'], makeReturns=TRUE)
#' 
#' \dontrun{
#' # Multivariate multi day:
#' rSC <- rSemiCov(sample5MinPrices, makeReturns = TRUE) # rSC is a list of lists
#' # We extract the covariance between stock 1 and stock 2 for all three covariances.
#' mixed <- do.call(rbind, lapply(rSC, function(x) x[["mixed"]][1,2]))
#' neg <- do.call(rbind, lapply(rSC, function(x) x[["negative"]][1,2]))
#' pos <- do.call(rbind, lapply(rSC, function(x) x[["positive"]][1,2]))
#' covariances <- xts(cbind(mixed, neg, pos), as.Date(rownames(pos)))
#' colnames(covariances) <- c("mixed", "neg", "pos")
#' # We make a quick plot of the different covariances
#' plot(covariances)
#' addLegend(lty = 1) # Add legend so we can distinguish the series.
#' }
#' @author Emil Sjoerup
#' @keywords volatility
#' @importFrom data.table between
#' @export
rSemiCov <- function(rData, cor = FALSE, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE){
  
  N <- ncol(rData)
  if (checkMultiDays(rData)) { 
    if (N == 1) { 
      result <- apply.daily(rData, rSemiCov, alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = makeReturns) 
    }
    if (N > 1) { 
      result <- applyGetList(rData, rSemiCov, cor=cor, alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = makeReturns)
      names(result) <- unique(as.Date(index(rData)))
    }    
    return(result)
  }
  
  if((!is.null(alignBy)) && (!is.null(alignPeriod))) {
    rData <- fastTickAgregation(rData, on = alignBy, k = alignPeriod)
  } 
  if (makeReturns) {  
    rData <- makeReturns(rData) 
  }  
  
  
  # create p and n
  pos <- pmax(rData, 0)
  neg <- pmin(rData, 0)
  
  # calculate the mixed covariance (variances will be 0)
  mixCov <- t(pos) %*% neg + t(neg) %*% pos
  
  # Calculate negative covariance
  negCov <- t(neg) %*% neg
  # Calculate positive covariance
  posCov <- t(pos) %*% pos
  
  # Construct the concordant covariance
  concordantCov <- negCov + posCov
  # We also return the realized covariance 
  rCovEst <- mixCov + concordantCov
  
  if(cor){
    
    ## Calculate the correlations from the covariance matrices.
    mixCor <- matrix(NA, N, N)
    
    sdmatrix <- sqrt(diag(diag(negCov)))
    negCor <- solve(sdmatrix) %*% negCov %*% solve(sdmatrix)  
    
    sdmatrix <- sqrt(diag(diag(posCov)))
    posCor <- solve(sdmatrix) %*% posCov %*% solve(sdmatrix)  
    
    sdmatrix <- sqrt(diag(diag(concordantCov)))
    concordantCor <- solve(sdmatrix) %*% concordantCov %*% solve(sdmatrix)  
    
    sdmatrix <- sqrt(diag(diag(rCovEst)))
    rCorEst <- solve(sdmatrix) %*% rCovEst %*% solve(sdmatrix)  
    
    return(list("mixed" = mixCor, "negative" = negCor,  "positive" = posCor, "concordant" = concordantCor))
  }
  
  return(list("mixed" = mixCov, "negative" = negCov,  "positive" = posCov, "concordant" = concordantCov))
  
}

#' Utility function listing the available estimators for the CholCov estimation
#' 
#' 
#' @return This function returns a character vector containing the available estimators.
#' @export
listCholCovEstimators <- function(){
  c("MRC",
    "rCov",
    "rAVGCov",
    "rBPCov",
    "rHYCov",
    "rKernelCov",
    "rOWCov",
    "rRTSCov",
    "rThresholdCov",
    "rSemiCov")
}


#' ReMeDI
#' estimates auto-covariance of market-microstructure noise 
#' 
#' @param pData xts or data.table containing the log-prices of the asset
#' @param kn numeric of length 1 determining the tuning parameter kn this controls the lengths of the non-overlapping interval in the ReMeDI estimation
#' @param lags numeric containing integer values indicating 
#' @param correctTime logical indicating whether to use the time-adjusted ReMeDI measure, default is FALSE
#' @param jumpsIndex Indices of jump(s) detected
#' @param makeCorrelation logical indicating whether to transform the autocovariances into autocorrelations
#' 
#' @references remedi paper, muzafer's paper
#' @keywords microstructure noise autocovariance autocorrelation
#' 
#' @author Emil Sjoerup
#' @export
ReMeDI <- function(pData, kn = 1, lags = 1, correctTime = FALSE, jumpsIndex = NULL, makeCorrelation = FALSE){
  time <- DT <- PRICE <- NULL
  # Check input
  if(is.data.table(pData)){ # We have a data.table
    
    if(!("PRICE" %in% colnames(pData))){
      stop("ReMeDI with data.table input requires a PRICE column")
    }
    
    if(correctTime){
      if(!("DT" %in% colnames(pData))){
        stop("ReMeDI with correctTime set to TRUE needs a DT (date-time) column when the input is a data.table")
      } else {
        time <- as.numeric(pData[, DT])    
      }
    }
    
    prices <- as.numeric(pData[, PRICE])
    
  } else if( is.xts(pData) ) { # We have an xts object
    if(correctTime){
      time <- as.numeric(index(pData))
    }    
    prices <- as.numeric(pData)
  } else {
    stop("Error in ReMeDI: pData must be an xts or a data.table")
  }
  
  correctJumps <- FALSE
  
  if(is.numeric(jumpsIndex)){
    if(!all(jumpsIndex %% 1 == 0)){
      stop("Error in ReMeDI: jumpsIndex must be a numeric of integer values")
    }
    correctJumps <- TRUE  
  }
  
  if(!all(lags %% 1 == 0 )){
    stop("lags must be contain integer values")
  }
  
  res <- numeric(length(lags))
  nObs <- length(prices)
  
  kn <- c(-kn, 2 * kn)
  resIDX <- 1
  
  if(makeCorrelation){
    lags <- c(0,lags) # We make sure we have 0 lag first in the series (we remove it later again)
  }
  
  for (lag in lags) {
    thisLag <- c(lag, 0)
    remedi <- (kn[2] + 1):(nObs - lag + kn[1])
    idx <- 1
    
    for (i in (kn[2] + 1):(nObs - lag + kn[1])) { # Calculate ReMeDI
      remedi[idx] <- prod(prices[i + thisLag] - prices[i + thisLag - kn])
      idx <- idx + 1
    }
    
    ## Use the time corrections Muzafer provided
    if(correctTime){
      
      timeIDX <- sweep(matrix(rep((kn[2] + 1):(nObs - lag + kn[1]), 4), ncol = 4),2, c(thisLag , thisLag - kn), FUN = "+")
      
      ## Follow up with muzafer whether this is correct
      timeCorrection <- (time[timeIDX[,3]] - time[timeIDX[,1]]) * (time[timeIDX[,2]] - time[timeIDX[,4]])
      
      remedi <- remedi * timeCorrection
      
    }
    
    
    ## Use the jump correction Muzafer provided
    if(correctJumps){
      jumpIDX <- matrix(0, nrow = length(jumpsIndex), ncol = 3 + (thisLag[1] != 0))
      
      for (i in 1:length(jumpsIndex)) {
        if(thisLag[1] != 0){
          jumpIDX[i,] <- c(jumpsIndex[i] - sum(abs(kn)) - thisLag[1], jumpsIndex[i] - rep(kn[2], 1 + sum(thisLag != 0)) - thisLag, jumpsIndex[i] )
        } else {
          jumpIDX[i,] <- c(jumpsIndex[i] - sum(abs(kn)) - thisLag[1], jumpsIndex[i] - kn[2] - thisLag[1], jumpsIndex[i])
        }  
        
        remedi <- remedi[-as.numeric(jumpIDX)]
      }
      
      
    }
    
    
    
    res[resIDX] <- sum(remedi) / nObs
    resIDX <- resIDX +1
  }
  
  
  if(makeCorrelation){
    res <- res[-1]/res[1] # We transform the autocovariances into autocorrelations (and remove the 0-lag we added earlier)
  }
  
  return(res)
}

#' ReMeDI tuning parameter
#' function to choose the tuning parameter, kn in ReMeDI estimation
#' 
#' @param pData xts or data.table containing the log-prices of the asset.
#' @param correctTime logical indicating whether to use the time-adjusted ReMeDI measure, default is FALSE
#' @param jumpsIndex Indices of jump(s) detected
#' @param knMax max value of kn to be considered
#' @param tol tolerance for the minimizing value. If tol is high, the algorithm will choose a lower optimal value.
#' @param size size of the local window
#' @param lower lower boundary for the method if it fails to find an optimal value. If this is the case, the best kn between lower and upper is returned
#' @param upper upper boundary for the method if it fails to find an optimal value. If this is the case, the best kn between lower and upper is returned
#' @param plot logical whether to plot the errors.
#' @details This is the algorithm B.2 in the appendix of the Li and Linton (2019) working paper
#' 
#' @examples 
#' optimalKn <- knChooseReMeDI(sampleTDataMicroseconds, correctTime = FALSE, 
#'                             jumpsIndex = NULL, knMax = 10, tol = 0.05, 
#'                             size = 3, lower = 2, upper = 5, plot = TRUE)
#' optimalKn
#' \dontrun{
#' # We can also have a much larger search-space
#' optimalKn <- knChooseReMeDI(sampleTDataMicroseconds, correctTime = FALSE, 
#'                             jumpsIndex = NULL, knMax = 50, tol = 0.05,
#'                             size = 3, lower = 2, upper = 5, plot = TRUE)
#' optimalKn
#' }
#' 
#' @author Emil Sjoerup
#' @importFrom stats plot.ts
#' @references A ReMeDI for Microstructure Noise
#' @return integer containing the optimal kn
#' @export
knChooseReMeDI <- function(pData, correctTime = FALSE, jumpsIndex = NULL, knMax = 10, tol = 0.05, size = 3, lower = 2, upper = 5, plot = FALSE){
  
  kn <- 1:(knMax + size +1)
  err <- vapply(kn, ReMeDI, FUN.VALUE = numeric(4), pData = pData, correctTime = correctTime, jumpsIndex = jumpsIndex, lags = 0:3)
  err <- (err[1,] - err[2,] - err[3,] + err[4,] - ReMeDI(pData, kn = 1, lags = 0, correctTime = correctTime, jumpsIndex = jumpsIndex))^2

  if(plot){
    plot.ts(err, ylab = "error", xlab = "kn")
  }
  
  errMax <- max(err[1:(round(knMax/2))])
  
  kns <- vapply(1:(knMax+1), flat, FUN.VALUE = numeric(1), err = err, errMax = errMax, size = size, tol = tol)
  kns <- kns[!is.na(kns)]
  kn <- kns[1]
  
  if(is.na(kn)){
    kn <- which(err == min(err[lower:upper]))
  }
  
  return(as.integer(kn))
  
}

#### #' 
#### #' #' Autocorrelation of noise estimation
#### #' #' @importFrom zoo coredata
#### #' #' @export
#### #' autoCorrelationOfNoise <- function(pData, kn, lags){
#### #'   ## Make general:
#### #'   reticulate::source_python("../pickle_reader.py")
#### #'   dat <- read_pickle_file("/data/data/pickles/AIG_Tr_20140102_cleaned.pickle")
#### #'   pData <- xts(dat$price, anytime::anytime(dat$timestamp))
#### #'   colnames(pData) <- "PRICE"
#### #'   prices <- pData$PRICE
#### #'   kn <- 2
#### #'   lags <- 30
#### #'   averagedPrices <- filter(coredata(prices), rep(1, kn)/kn, sides = 1)[-seq(1, (kn-1))]
#### #'   nObs <- length(averagedPrices)
#### #'   J <- lags
#### #'   R_est <- rep(0, J + 1)
#### #'   r_est <- rep(0, J + 1)
#### #'   
#### #'   U_0 <- rep(0, J + 1)
#### #'   
#### #'   for (j in 0:J) {
#### #'     
#### #'     ind <- seq(1, nObs + 1 - j - 2 * 2 * kn)
#### #'     U_0[(j+1)] <- sum((prices[ind] - averagedPrices[(j + kn + ind)]) * (prices[ j+ ind] - averagedPrices[(j + 3 * kn + ind)]))
#### #'     R_est[(j+1)] <- U_0[j + 1]/length(ind)
#### #'     
#### #'   }
#### #'   
#### #'   
#### #'   UU_2 <- UU_3 <- UU_4 <- UU_1 <- matrix(0 ,nrow= kn + 1, ncol= J + 1) 
#### #'   
#### #'   for (m in 0:kn) {
#### #'     for (j in 0:J) {
#### #'       mu <- j + m
#### #'       ind <- seq(1, nObs + 1 - mu - 8 * kn)
#### #'       
#### #'       UU_1[(m+1), (j+1)] <- 
#### #'         sum((prices[ind] - averagedPrices[(mu + kn + ind)]) * 
#### #'             (prices[j + ind] - averagedPrices[(mu + 3 * kn + ind)]) *
#### #'             (prices[m + ind] - averagedPrices[(mu + 5 * kn + ind)]) *
#### #'             (prices[j + m + ind] - averagedPrices[(mu + 7 * kn + ind)])) 
#### #'       
#### #'       UU_2[(m+1),(j+1)] <- 
#### #'         sum( (prices[m + ind] - averagedPrices[(mu + kn + ind)]) * 
#### #'              (prices[j + m + ind] - averagedPrices[(mu + 3 * kn + ind)] ) *
#### #'              (prices[ind] - averagedPrices[(mu + 5 * kn + ind)] ) *
#### #'              (prices[j + ind] - averagedPrices[(mu + 7 * kn + ind)] )  )  
#### #'       
#### #'       UU_3[(m+1),(j+1)] <- 
#### #'         sum( (prices[ind] - averagedPrices[(mu + kn + ind)]) * 
#### #'              (prices[j + ind] - averagedPrices[(mu + 3 * kn + ind)] ) *
#### #'              (prices[m + ind] - averagedPrices[(mu + 5 * kn + ind)] ) *
#### #'              (prices[m + ind] - averagedPrices[(mu + 7 * kn + ind)] )  )  
#### #'       
#### #'       
#### #'       UU_4[(m+1),(j+1)] <- 
#### #'         sum( (prices[m + ind] - averagedPrices[(mu + kn + ind)]) * 
#### #'              (prices[j + m + ind] - averagedPrices[(mu + 3 * kn + ind)] ) *
#### #'              (prices[ind] - averagedPrices[(mu + 5 * kn + ind)] ) *
#### #'              (prices[ind] - averagedPrices[(mu + 7 * kn + ind)] ))  
#### #'     }
#### #'   }
#### #'   
#### #'   U_bar_1 <- rep(0, J + 1) # U_bar(0,j,0,j)
#### #'   U_bar_2 <- rep(0, J + 1) # U_bar(0,j,0,0)
#### #'   
#### #'   
#### #'   for (j in 0:J) {
#### #'     mu <- j
#### #'     mu_pp <- j + j
#### #'     
#### #'     ind <- seq(1, nObs + 1 - mu_pp - 9 * kn)
#### #'     
#### #'     U_bar_1[(j+1)] <- sum( (prices[ind] - averagedPrices[(mu + kn + ind)]) * 
#### #'                            (prices[j+ ind] - averagedPrices[(mu + 3 * kn + ind)] ) *
#### #'                            (prices[mu + 5 * kn + ind] - averagedPrices[(mu_pp + 5 * kn + kn + ind)] ) *
#### #'                            (prices[mu + 5 * kn + j + ind] - averagedPrices[(mu_pp + 5 * kn + 3 * kn + ind)] ) )
#### #'     
#### #'     U_bar_2[(j+1)] <- sum( (prices[ind] - averagedPrices[(mu + kn + ind)]) * 
#### #'                            (prices[ j+ ind] - averagedPrices[(mu + 3*kn + ind)] ) *
#### #'                            (prices[ mu + 5*kn + ind] - averagedPrices[(mu + 5 * kn + kn + ind)] ) *
#### #'                            (prices[ mu + 5*kn + ind] - averagedPrices[(mu + 5 * kn + 3 * kn + ind)] ) )
#### #'   }
#### #'   
#### #'   S <- rep(0, J + 1) # S[0,j,0,j]
#### #'   Bza <- rep(0, J + 1) # Bza[0,j,0,j]
#### #'   
#### #'   S_1 <- rep(0, J + 1) # S[0,j,0,0]
#### #'   Bza_1 <- rep(0, J + 1) # Bza[0,j,0,0]
#### #'   sigma_r <- rep(1, J) # 
#### #'   
#### #'   j<-0
#### #'   S[(j+1)] <- (sum(UU_1[,(j+1)]) + sum(UU_2[,(j+1)]) - UU_1[1,(j+1)]) - (2*kn + 1)*U_bar_1[(j+1)]
#### #'   Bza[(j+1)] <- S[(j+1)]/nObs + U_bar_1[(j+1)]/nObs - R_est[(j+1)]^2
#### #'   
#### #'   S_1[(j+1)] <- (sum(UU_3[,(j+1)]) + sum(UU_4[,(j+1)]) - UU_3[1,(j+1)] ) - (2*kn + 1)*U_bar_2[(j+1)]
#### #'   Bza_1[(j+1)] <- S_1[(j+1)]/nObs + U_bar_2[(j+1)]/nObs - R_est[1]*R_est[(j+1)]
#### #'   
#### #'   
#### #'   for (j in 1:J) {
#### #'     S[(j+1)] <- (sum(UU_1[,(j+1)]) + sum(UU_2[,(j+1)]) - UU_1[1,(j+1)]) - (2*kn + 1)*U_bar_1[(j+1)]
#### #'     Bza[(j+1)] <- S[(j+1)]/nObs + U_bar_1[(j+1)]/nObs - R_est[(j+1)]^2
#### #'     
#### #'     S_1[(j+1)] <- (sum(UU_3[,(j+1)]) + sum(UU_4[,(j+1)]) - UU_3[1,(j+1)] ) - (2*kn + 1)*U_bar_2[(j+1)]
#### #'     Bza_1[(j+1)] <- S_1[(j+1)]/nObs + U_bar_2[(j+1)]/nObs - R_est[1]*R_est[(j+1)]
#### #'     sigma_r[j] <- (R_est[1]^2*Bza[(j+1)] + R_est[(j+1)]^2 * Bza[1] - (2 * R_est[1] * R_est[(j+1)] * Bza_1[(j+1)]))/(R_est[1]^4)
#### #'   }
#### #'   
#### #'   
#### #'   
#### #'   
#### #'   
#### #'   plot.ts(R_est)
#### #'   
#### #'   #plot.ts(sigma_r)
#### #'   
#### #' }
#### #' 
#### #' 
#### #' 
#### #' 
#### #' 
#### #' 
#### #' 
#### #' 
#### #' 
#### #' 
#### #' 
#### 
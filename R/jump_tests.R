
#' @keywords internal
ABDJumptest <- function(RV, BPV, TQ) { # Compute jump detection stat mentioned in roughing paper
  mu1  <- sqrt(2 / pi)
  n <- length(RV)
  zstat <- ((1/n)^(-1/2)) * ((RV-BPV)/RV)*(  (mu1^(-4) + 2 * (mu1^(-2))-5) * pmax(1, TQ * (BPV^(-2))))^(-1/2)
  return(zstat)
}

#' Ait-Sahalia and Jacod (2009) tests for the presence of jumps in the price series. 
#' 
#' @description 
#' This test examines the presence of jumps in highfrequency price series. It is based on the theory of Ait-Sahalia and Jacod (2009) (AJ). It consists in comparing the multipower variation of equispaced returns computed at a fast time scale (\eqn{h}), \eqn{r_{t,i}} (\eqn{i=1, \ldots,N}) and those computed at the slower time scale (\eqn{kh}), \eqn{y_{t,i}}(\eqn{i=1, \ldots ,\mbox{N/k}}).
#' 
#' They found that the limit (for \eqn{N} \eqn{\to} \eqn{\infty} ) of the realized power variation is invariant for different sampling scales and that their ratio is \eqn{1} in case of jumps and \eqn{\mbox{k}^{p/2}-1} if no jumps.
#' Therefore the AJ test detects the presence of jump using the ratio of realized power variation sampled from two scales. The null hypothesis is no jumps.
#' 
#' The function returns three outcomes: 1.z-test value 2.critical value under confidence level of \eqn{95\%} and 3.p-value.
#' 
#' Assume there is \eqn{N} equispaced returns in period \eqn{t}. Let \eqn{r_{t,i}} be a return (with \eqn{i=1, \ldots,N}) in period \eqn{t}.
#' 
#' And there is \eqn{N/k} equispaced returns in period \eqn{t}. Let \eqn{y_{t,i}} be a return (with \eqn{i=1, \ldots ,\mbox{N/k}}) in period \eqn{t}.
#' 
#'  Then the AJjumpTest is given by: 
#'    \deqn{
#'      \mbox{AJjumpTest}_{t,N}= \frac{S_t(p,k,h)-k^{p/2-1}}{\sqrt{V_{t,N}}}
#'    }
#'  
#'  in which, 
#'  
#'  \deqn{
#'    \mbox{S}_t(p,k,h)= \frac{PV_{t,M}(p,kh)}{PV_{t,M}(p,h)}
#'  }
#'  
#'  \deqn{
#'    \mbox{PV}_{t,N}(p,kh)= \sum_{i=1}^{N/k}{|y_{t,i}|^p}
#'  }
#'  
#'  \deqn{
#'    \mbox{PV}_{t,N}(p,h)= \sum_{i=1}^{N}{|r_{t,i}|^p}
#'  }
#'  
#'  \deqn{
#'    \mbox{V}_{t,N}= \frac{N(p,k) A_{t,N(2p)}}{N A_{t,N(p)}}
#'  }
#'  
#'  \deqn{
#'    \mbox{N}(p,k)= \left(\frac{1}{\mu_p^2}\right)(k^{p-2}(1+k))\mu_{2p} + k^{p-2}(k-1) \mu_p^2 - 2k^{p/2-1}\mu_{k,p}
#'  }
#'  
#'  \deqn{
#'    \mbox{A}_{t,n(2p)}= \frac{(1/N)^{(1-p/2)}}{\mu_p} \sum_{i=1}^{N}{|r_{t,i}|^p} \ \ \mbox{for} \ \ |r_j|< \alpha(1/N)^w
#'  }
#'  
#'  \deqn{
#'    \mu_{k,p}=  E(|U|^p  |U+\sqrt{k-1}V|^p)
#'  } 
#'  
#'  \eqn{U, V}: independent standard normal random variables; \eqn{h=1/N}; \eqn{p, k, \alpha, w}: parameters. 
#'  
#' @param pData a zoo/xts object containing all prices in period t for one asset.
#' @param p can be chosen among 2 or 3 or 4. The author suggests 4. 4 by default.
#' @param k can be chosen among 2 or 3 or 4. The author suggests 2. 2 by default.
#' @param alignBy a string, align the tick data to "seconds"|"minutes"|"hours"
#' @param alignPeriod an integer, align the tick data to this many [seconds|minutes|hours].
#' @param alphaMultiplier alpha multiplier
#' @param makeReturns boolean, should be TRUE when rData contains prices instead of returns. FALSE by default.
#' @param ... additional arguments.
#' 
#' @return list or xts in case the input prices span more than one day.
#' 
#' @details 
#'  The theoretical framework underlying jump test is that the logarithmic price process \eqn{X_t} belongs to the class of Brownian semimartingales, which can be written as:
#'    \deqn{
#'      \mbox{X}_{t}=  \int_{0}^{t} a_udu + \int_{0}^{t}\sigma_{u}dW_{u} + Z_t
#'    }
#'  where \eqn{a} is the drift term, \eqn{\sigma} denotes the spot volatility process, \eqn{W} is a standard Brownian motion and \eqn{Z} is a jump process defined by:
#'    \deqn{
#'      \mbox{Z}_{t}=  \sum_{j=1}^{N_t}k_j
#'    }
#'  where \eqn{k_j} are nonzero random variables. The counting process can be either finite or infinite for finite or infinite activity jumps.
#'  
#'  The Ait-Sahalia and Jacod test is that: Using the convergence properties of power variation and its dependence on the time scale on which it is measured, Ait-Sahalia and Jacod (2009) define a new variable which converges to 1 in the presence of jumps in the underlying return series, or to another deterministic and known number in the absence of jumps. (Theodosiou& Zikes(2009))
#'  
#' @references 
#' Ait-Sahalia, Y. and Jacod, J. (2009). Testing for jumps in a discretely observed process. The Annals of Statistics, 37(1), 184- 222.
#' 
#' Theodosiou, M., & Zikes, F. (2009). A comprehensive comparison of alternative tests for jumps in asset prices. Unpublished manuscript, Graduate School of Business, Imperial College London.
#' 
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#'
#' @examples
#' AJjumpTest(sampleTData$PRICE, p = 2, k = 3, alignBy = "seconds", 
#'   alignPeriod = 5, makeReturns = TRUE)
#' 
#' @keywords highfrequency AJjumpTest
#' @importFrom stats qnorm
#' @importFrom stats pnorm
#' @export
AJjumpTest <- function(pData, p = 4 , k = 2, alignBy = NULL, alignPeriod = NULL, alphaMultiplier = 4, makeReturns = FALSE, ...) {

  if (checkMultiDays(pData)) {
    
    result <- 
      apply.daily(pData, 
                  function(x){
                    tmp <- AJjumpTest(x, p = p, k = k, alignBy = alignBy, alignPeriod = alignPeriod, alphaMultiplier = alphaMultiplier, makeReturns = makeReturns, ...)
                    return(cbind(tmp[[1]], tmp[[2]][1], tmp[[2]][2], tmp[[3]]))
                    })
    colnames(result) <- c("ztest", "lower", "upper", "p-value")
    universalThreshold <- 2 * pnorm(-sqrt(log(ndays(result$ztest) * 2)))
    
    result$universalThresholdLower <- qnorm(universalThreshold)
    result$universalThresholdUpper <- -qnorm(universalThreshold)
    
    return(result)
  } else {
    pData <- fastTickAgregation(pData, on = "seconds", k = 1)
  }

  N <- length(pData)-1
  p <- as.numeric(p)
  k <- as.numeric(k)

  alpha <- alphaMultiplier * sqrt(rCov(pData, alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = makeReturns))
  w <- 0.47
  cvalue <- alpha * (1/N)^w

  h <- alignPeriod * scale(alignBy)
  hk <- h * k

  seq1 <- seq(1, N, h)
  seq2 <- seq(1, N, hk)

  # return data
  pData1 <- pData[seq1]
  pData2 <- pData[seq2]

  r  <- abs(makeReturns(pData))
  r1 <- abs(makeReturns(pData1))
  r2 <- abs(makeReturns(pData2))

  pv1 <- sum(r1^p)
  pv2 <- sum(r2^p)

  S <- pv2 /pv1

  ## selection return:
  selection <- abs(r) < cvalue
  rse <- abs(makeReturns(pData[selection]))

  ## AJ test: 
  AJtest <- (S-k^(p/2-1))/sqrt(calculateV(rse, p, k, N))

  out <- {}
  out$ztest <- AJtest
  out$critical.value <- qnorm(c(0.025,0.975))
  out$pvalue <- 2 * pnorm(-abs(AJtest))
  return(out)
}

#' Barndorff-Nielsen and Shephard (2006) tests for the presence of jumps in the price series.
#' 
#' @description This test examines the presence of jumps in highfrequency price series. It is based on theory of Barndorff- Nielsen and Shephard (BNS). The null hypothesis is no jumps. 
#' Depending on the choice of estimator (integrated variance (IVestimator), integrated quarticity (IQestimator)), mechanism (linear, ratio) and adjustment (logarith), the function returns the result.
#' Function returns three outcomes: 1.z-test value 2.critical value(with confidence level of 95\%) and 3.pvalue of the test. 
#' Assume there is \eqn{N} equispaced returns in period \eqn{t}. 
#' 
#' Assume the Realized variance (RV), IVestimator and IQestimator are based on \eqn{N} equispaced returns. 
#' 
#' Let \eqn{r_{t,i}} be a return (with \eqn{i=1, \ldots,N}) in period \eqn{t}. 
#' 
#' Then the BNSjumpTest is given by: 
#' \deqn{
#' \mbox{BNSjumpTest}= \frac{RV - IVestimator}{\sqrt{(\theta-2)\frac{1}{N} {IQestimator}}}
#' }
#' in which, \eqn{IVestimator} can be: bipower variance (BV), minRV, medRV. 
#' \eqn{IQestimator} can be: tripower quarticity (TP), quadpower quarticity (QP), minRQ, medRQ.
#' 
#' \eqn{\theta}: depends on IVestimator (Huang and Tauchen (2005)).
#' 
#' @param rData a zoo/xts object containing all returns in period t for one asset.
#' @param IVestimator can be chosen among jump robust integrated variance estimators: BV, minRV, medRV and corrected threshold bipower variation (CTBV). If CTBV is chosen, an argument of \eqn{startV}, start point of auxiliary estimators in threshold estimation (Corsi et al. (2010) can be included. BV by default.
#' @param IQestimator can be chosen among jump robust integrated quarticity estimators: TP, QP, minRQ and medRQ. TP by default.
#' @param type a method of BNS testing: can be linear or ratio. Linear by default.
#' @param logTransform boolean, should be TRUE when QVestimator and IVestimator are in logarith form. FALSE by default.
#' @param max boolean, should be TRUE when max adjustment in SE. FALSE by default.
#' @param alignBy a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param alignPeriod an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns boolean, should be TRUE when rData contains prices instead of returns. FALSE by default.
#' @param alpha numeric of length one with the significance level to use for the jump test(s). Defaults to 0.975.
#' 
#' @return list or xts in case the input prices span more than one day.
#' 
#' @details The theoretical framework underlying jump test is that the logarithmic price process \eqn{X_t} belongs to the class of Brownian semimartingales, which can be written as:
#' \deqn{
#' \mbox{X}_{t}=  \int_{0}^{t} a_udu + \int_{0}^{t}\sigma_{u}dW_{u} + Z_t
#' }
#' where \eqn{a} is the drift term, \eqn{\sigma} denotes the spot volatility process, \eqn{W} is a standard Brownian motion and \eqn{Z} is a jump process defined by:
#' \deqn{
#' \mbox{Z}_{t}=  \sum_{j=1}^{N_t}k_j
#' }
#' where \eqn{k_j} are nonzero random variables. The counting process can be either finite or infinite for finite or infinite activity jumps.
#' 
#' Since the realized volatility converges to the sum of integrated variance and jump variation, while the robust IVestimator converges to the integrated variance,  it follows that the difference between #' \eqn{RV_{t,N}} and the IVestimator captures the jump part only, and this observation underlines the BNS test for jumps. (Theodosiou& Zikes(2009))
#' 
#' @references Barndorff-Nielsen, O. E., & Shephard, N. (2006). Econometrics of testing for jumps in financial economics using bipower variation. Journal of financial Econometrics, 4(1), 1-30. 
#' 
#' Corsi, F., Pirino, D., & Reno, R. (2010). Threshold bipower variation and the impact of jumps on volatility forecasting. Journal of Econometrics, 159(2), 276-288.
#' 
#' Huang, X., & Tauchen, G. (2005). The relative contribution of jumps to total price variance. Journal of financial econometrics, 3(4), 456-499.
#' 
#' Theodosiou, M., & Zikes, F. (2009). A comprehensive comparison of alternative tests for jumps in asset prices. Unpublished manuscript, Graduate School of Business, Imperial College London.
#' 
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#' 
#' @examples 
#' BNSjumpTest(sampleTData$PRICE, IVestimator= "minRV", 
#'             IQestimator = "medRQ", type= "linear", makeReturns = TRUE)
#' 
#' @keywords highfrequency BNSjumpTest
#' @export
BNSjumpTest <- function (rData, IVestimator = "BV", IQestimator = "TP", type = "linear",
                         logTransform = FALSE, max = FALSE, alignBy = NULL, alignPeriod = NULL,
                         makeReturns = FALSE, alpha = 0.975) {
  
  if (checkMultiDays(rData)) {
    
    result <- 
      apply.daily(rData, 
                  function(x) {
                    tmp <- BNSjumpTest(x, IVestimator, IQestimator, type, logTransform, max, alignBy, alignPeriod, makeReturns, alpha)
                    return(cbind(tmp[[1]], tmp[[2]][1], tmp[[2]][2], tmp[[3]]))
                  })
    
    # browser()
    colnames(result) <- c("ztest", "lower", "upper", "p-value")
    
    universalThreshold <- 2 * pnorm(-sqrt(log(ndays(result$ztest) * 2)))
    result$universalThresholdLower <- qnorm(universalThreshold)
    result$universalThresholdUpper <- -qnorm(universalThreshold)
    
    # p.adjust(as.numeric(result$`p-value`), "fdr")
    # qnorm(p.adjust(as.numeric(result$`p-value`), "fdr"))
    # result$ztest
    
    return(result)
  } else {
    if ((!is.null(alignBy)) && (!is.null(alignPeriod))) {
      rData <- fastTickAgregation(rData, on = alignBy, k = alignPeriod)
    }
    if (makeReturns) {
      rData <- makeReturns(rData)
    }
    N <- length(rData)
    hatQV <- RV(rData)
    hatIV <- hatIV(rData, IVestimator)
    theta <- tt(IVestimator)
    hatIQ <- hatIQ(rData, IQestimator)
    if (type == "linear") {
      if (logTransform) {
        hatQV <- log(RV(rData))
        hatIV <- log(hatIV(rData, IVestimator))
      }
      if (!logTransform) {
        hatQV <- RV(rData)
        hatIV <- hatIV(rData, IVestimator)
      }
      if (max) {
        product <- max(1, hatIQ(rData, IQestimator)/hatIV(rData, IVestimator)^2)
      }
      if (!max) {
        product <- hatIQ(rData, IQestimator)
      }
      a <- sqrt(N) * (hatQV - hatIV)/sqrt((theta - 2) * product)
      out <- list()
      out$ztest <- a
      out$critical.value <- qnorm(c(1-alpha, alpha))
      out$pvalue <- 2 * pnorm(-abs(a))
      return(out)
    }
    if (type == "ratio") {
      if (max) {
        product <- max(1, hatIQ(rData, IQestimator)/hatIV(rData, IVestimator)^2)
      }
      if (!max) {
        product <- hatIQ(rData, IQestimator)/hatIV(rData, IVestimator)^2
      }
      a <- sqrt(N) * (1 - hatIV(rData, IVestimator, N)/RV(rData))/sqrt((theta - 2) * product)
      out <- list()
      out$ztest <- a
      out$critical.value <- qnorm(c(1- alpha, alpha))
      out$pvalue <- 2 * pnorm(-abs(a))
      
      return(out)
    }
  }
}

#' Jiang and Oomen (2008) tests for the presence of jumps in the price series.
#' @description 
#'   
#'   This test examines the jump in highfrequency data. It is based on theory of Jiang and Oomen (JO). They found that the difference of simple return and logarithmic return can capture one half of integrated variance if there is no jump in the underlying sample path. The null hypothesis is no jumps.
#'   
#'   Function returns three outcomes: 1.z-test value 2.critical value under confidence level of \eqn{95\%} and 3.p-value.  
#'   
#' Assume there is \eqn{N} equispaced returns in period \eqn{t}.
#'  
#'  Let \eqn{r_{t,i}} be a logarithmic return (with \eqn{i=1, \ldots,N}) in period \eqn{t}.
#'  
#'  Let \eqn{R_{t,i}} be a simple return (with \eqn{i=1, \ldots,N}) in period \eqn{t}.
#'  
#'  Then the JOjumpTest is given by: 
#'    \deqn{
#'      \mbox{JOjumpTest}_{t,N}= \frac{N BV_{t}}{\sqrt{\Omega_{SwV}} \left(1-\frac{RV_{t}}{SwV_{t}} \right)}
#'    }
#'  in which, 
#'  \eqn{BV}: bipower variance;
#'  \eqn{RV}: realized variance (defined by Andersen et al. (2012));
#'  \deqn{
#'    \mbox{SwV}_{t}=2 \sum_{i=1}^{N}(R_{t,i}-r_{t,i})
#'  }
#'  \deqn{
#'    \Omega_{SwV}= \frac{\mu_6}{9} \frac{{N^3}{\mu_{6/p}^{-p}}}{N-p-1} \sum_{i=0}^{N-p}\prod_{k=1}^{p}|r_{t,i+k}|^{6/p}
#'  } 
#'  \deqn{
#'    \mu_{p}= \mbox{E}[|\mbox{U}|^{p}] = 2^{p/2} \frac{\Gamma(1/2(p+1))}{\Gamma(1/2)}
#'    % \mbox{E}[|\mbox{U}|^p]=
#'  }
#'  %\eqn{ \mbox{E}[|\mbox{U}|]^{\mbox{p}}}
#'  \eqn{U}: independent standard normal random variables
#'  
#'  p: parameter (power).
#'  
#' @param pData a zoo/xts object containing all prices in period t for one asset.
#' @param power can be chosen among 4 or 6. 4 by default.
#' @param alignBy a string, align the tick data to "seconds"|"minutes"|"hours". Defaults to NULL, denoting testing by using tick by tick returns
#' @param alignPeriod an integer, align the tick data to this many [seconds|minutes|hours]. Defaults to NULL, denoting testing by using tick by tick returns or if the returns are already aligned as 
#' @param alpha numeric of length one with the significance level to use for the jump test(s). Defaults to 0.975.
#' @param ... additional arguments. (Currently unused)
#'
#' @return list
#' 
#' @details  
#' The theoretical framework underlying jump test is that the logarithmic price process \eqn{X_t} belongs to the class of Brownian semimartingales, which can be written as:
#'    \deqn{
#'      \mbox{X}_{t}=  \int_{0}^{t} a_udu + \int_{0}^{t}\sigma_{u}dW_{u} + Z_t
#'    }
#'  where \eqn{a} is the drift term, \eqn{\sigma} denotes the spot volatility process, \eqn{W} is a standard Brownian motion and \eqn{Z} is a jump process defined by:
#'    \deqn{
#'      \mbox{Z}_{t}=  \sum_{j=1}^{N_t}k_j
#'    }
#'  where \eqn{k_j} are nonzero random variables. The counting process can be either finite or infinite for finite or infinite activity jumps.
#'  
#'  The Jiang and Oomen test is that: in the absence of jumps, the accumulated difference between the simple return and the log return captures one half of the integrated variance. (Theodosiou& Zikes(2009))
#'  
#' @references 
#'  Andersen, T. G., D. Dobrev, and E. Schaumburg (2012). Jump-robust volatility estimation using nearest neighbor truncation. Journal of Econometrics, 169(1), 75- 93.
#' 
#' Jiang, J.G. and Oomen R.C.A (2008). Testing for jumps when asset prices are observed with noise- a "swap variance" approach. Journal of Econometrics,144(2), 352-370.
#' 
#' Theodosiou, M., & Zikes, F. (2009). A comprehensive comparison of alternative tests for jumps in asset prices. Unpublished manuscript, Graduate School of Business, Imperial College London.
#' 
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#' 
#' @examples
#' JOjumpTest(sample5MinPricesJumps$stock1, power = 6)
#' 
#' @keywords highfrequency JOjumpTest
#' @importFrom stats qnorm
#' @importFrom stats pnorm
#' @importFrom zoo as.zoo
#' @export
JOjumpTest <- function(pData, power = 4, alignBy = NULL, alignPeriod = NULL, alpha = 0.975, ...) {
  
  if (checkMultiDays(pData)) {
    
    result <- 
      apply.daily(pData,
                  function(x){
                    tmp <- JOjumpTest(x, power, alignBy, alignPeriod, alpha, ...)
                    return(cbind(tmp[[1]], tmp[[2]][1], tmp[[2]][2], tmp[[3]]))})
    
    colnames(result) <- c("ztest", "lower", "upper", "p-value")
    
    universalThreshold <- 2 * pnorm(-sqrt(log(ndays(result$ztest) * 2)))
    result$universalThresholdLower <- qnorm(universalThreshold)
    result$universalThresholdUpper <- -qnorm(universalThreshold)
    
    return(result)
  }
  
  if ((!is.null(alignBy)) && (!is.null(alignPeriod))) {
    pData <- fastTickAgregation(pData, on = alignBy, k = alignPeriod)
  }
  
  R  <- as.zoo(simre(pData))
  r  <- as.zoo(makeReturns(pData))
  N  <- length(pData) - 1
  bv <- RBPVar(r)
  rv <- RV(r)

  SwV <- 2 * sum(R-r, na.rm = TRUE)
  mu1 <- 2^(6/2) * gamma(1/2*(6+1)) / gamma(1/2)

  ##mupower:
  if (power == 4) {
    q      <- abs(rollapply(r, width = 4, FUN = prod, align = "left",na.rm = TRUE))
    mu2    <- 2^((6/4)/2) * gamma(1/2 * (6/4+1)) / gamma(1/2)
    av     <- mu1/9 * N^3 * (mu2)^(-4) / (N-4-1) * sum(q^(6/4),na.rm = TRUE)   ##check formula
    JOtest <- N * bv / sqrt(av) * (1- rv/SwV)

    out                <- {}
    out$ztest          <- JOtest
    out$critical.value <- qnorm(c(1-alpha, alpha))
    out$pvalue         <- 2 * pnorm(-abs(JOtest))
    return(out)
  }

  if (power == 6) {
    q <- abs(rollapply(r, width = 6, FUN = prod, align = "left",na.rm = TRUE))
    mu2 <- 2^((6/6)/2)*gamma(1/2*(6/6+1))/gamma(1/2)
    av <- mu1/9 * N^3*(mu2)^(-6)/(N-6-1)*sum(q^(6/6),na.rm= TRUE)   ##check formula
    JOtest <- N*bv/sqrt(av)*(1- rv/SwV)

    out                <- {}
    out$ztest          <- JOtest
    out$critical.value <- qnorm(c(1-alpha,alpha))
    out$pvalue         <- 2*pnorm(-abs(JOtest))
    return(out)
  }
}


#' Intraday jump tests
#' 
#' This function can be used to  test for jumps in intraday price paths.
#' The tests are of the form \eqn{L(t) = (R(t) - mu(t))/sigma(t)}.
#' 
#' The null hypothesis of the tests in this function is that
#' @param pData xts or data.table of the price data in levels. This data can (and should in some cases) be tick-level data. The data can span more than one day.
#' @param volEstimator character denoting which volatility estimator to use for the tests. See \link{spotVol}. Default = \code{"RM"} denoting realized measures.
#' @param driftEstimator character denoting which drift estimator to use for the tests. See \link{spotDrift}. Default = \code{"none"} denoting no drift esitmation.
#' @param on string indicating the time scale in which \code{k} is expressed.
#' Possible values are: \code{"secs", "seconds", "mins", "minutes", "hours"}.
#' @param k positive integer, indicating the number of periods to aggregate
#' over. E.g. to aggregate an \code{xts} object to the 5 minute frequency, set
#' \code{k = 5} and \code{on = "minutes"}.
#' @param marketOpen the market opening time. This should be in the time zone
#' specified by \code{tz}. By default, \code{marketOpen = "09:30:00"}.
#' @param marketClose the market closing time. This should be in the time zone
#' specified by \code{tz}. By default, \code{marketClose = "16:00:00"}.
#' @param tz string specifying the time zone to which the times in \code{data}
#' and/or \code{marketOpen}/ \code{marketClose} belong. Default = \code{"GMT"}.
#' 
#' @examples 
#' \dontrun{
#' # We can easily make a Lee-Mykland jump test.
#' LMtest <- intradayJumpTest(pData = sampleTDataMicroseconds[, list(DT, PRICE)], 
#'                            volEstimator = "RM", driftEstimator = "none",
#'                            RM = "bipower", lookBackPeriod = 10,
#'                            on = "minutes", k = 5, marketOpen = "09:30:00", 
#'                            marketClose = "16:00:00")
#' plot(LMtest)}
#' 
#' @importFrom zoo index
#' @export

intradayJumpTest <- function(pData, volEstimator = "RM", driftEstimator = "none", alpha = 0.95, ..., on = "minutes", k = 5,
                             marketOpen = "09:30:00", marketClose = "16:00:00", tz = "GMT"){

  PRICE = DATE = RETURN = DT = NULL
  
  if (!("PRICE" %in% colnames(pData))) {
    if (dim(pData)[2] == 1) {
      names(pData) <- "PRICE"
    } else {
      stop("data.table or xts needs column named PRICE.")
    }
  }
    
  dataWasXts <- FALSE
  if (!is.data.table(pData)) {
    if (is.xts(pData)) {
      pData <- setnames(as.data.table(pData), old = "index", new = "DT")
      pData[, PRICE := as.numeric(PRICE)]
      dataWasXts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (!("DT" %in% colnames(pData))) {
      stop("Data.table needs DT column containing the time-stamps of the trades.") # added the timestamp comment for verbosity.
    }
  }
  
  D <- ndays(pData)
  isMultiDay <- FALSE
  if (D > 1) {
    isMultiDay <- TRUE
  } 
  
  vol <- spotVol(pData, method = volEstimator, on = on, k = k, marketOpen = marketOpen, marketClose = marketClose, tz = tz, ...)

  if (volEstimator == "RM") {
    
    op <- list(RM = "bipower", lookBackPeriod = 10)
    options <- list(...)
    op[names(options)] <- options
    
    #vol$spot <- lag(vol$spot)
    # if(op$RM == "bipower"){
    #   vol$spot <- sqrt((vol$spot^2  / (pi/2))) 
    # }
    
  }

  
  if (volEstimator != "PARM") {
    prices <- aggregatePrice(pData, on = on, k = k , marketOpen = marketOpen,
                   marketClose = marketClose, tz = tz, fill = TRUE)
    setkeyv(prices, "DT")
    prices[, DATE := as.Date(DT, tz = tzone(prices$DT))]
    returns <- prices[, RETURN := log(PRICE) - shift(log(PRICE), type = "lag"), by = "DATE"][!is.na(RETURN)]
    
  } else { # volEstimator == "PARM" i.e. we have pre-averaged realized measures
    nObs <- length(pData$PRICE)
    
    op <- list(RM = "bipower", lookBackPeriod = 50)
    options <- list(...)
    op[names(options)] <- options
    
    if (isMultiDay) {
      dates <- NULL 
      if(is.data.table(pData)){
        dates <- unique(as.Date(pData$DT))
      } else {
        dates <- unique(as.Date(index(pData)))
      }
      
      preAveragedReturns <- testingIndices <- c()
      
      for (d in 1:D) {
        tmp <- seq(op$lookBackPeriod - 2 + vol$kn[d] +1, vol$nObs[d]-vol$kn[d], by = vol$kn[d]) + sum(vol$nObs[0:(d-1)])
        
        if(op$RM == "medrv"){ # We can expand this if need be. 
          tmp <- tmp[-length(tmp)]
        } 
        testingIndices <- c(testingIndices, tmp)
        preAveragedReturns <- c(preAveragedReturns, c(as.numeric(hatreturn(as.xts(pData[as.Date(pData$DT) == dates[d]])$PRICE, vol$kn[d])), rep(NA, vol$kn[d] - 2)))
      }
      
      returns <- pData[, RETURN := preAveragedReturns][testingIndices, "RETURN"]
      
    } else {
      testingIndices <- seq(op$lookBackPeriod - 2 + vol$kn +1, nObs-vol$kn, by = vol$kn)
      if (op$RM == "medrv") { # We can expand this if need be. 
        testingIndices <- testingIndices[-length(testingIndices)]
      }
      
      returns <- pData[, RETURN := c(as.numeric(hatreturn(as.xts(pData)$PRICE, vol$kn)), rep(NA, vol$kn - 2))][testingIndices,"RETURN"]
    }
    
    prices <- NULL
  }
  
  
  if(driftEstimator != "none"){
    if(volEstimator == "PARM"){
      drift <- 0
      warning("Drift estimation not currently supported together with pre-averaged realized measures. Setting drift to 0")
    } else {
      drift <- as.numeric(unlist(spotDrift(pData, method = driftEstimator, on = on, k = k, marketOpen = marketOpen, marketClose = marketClose, tz = tz, ...)))
    }
  } else {
    drift <- 0
  }
  
  vol$spot <- sqrt(vol$spot^2  * 1/(op$lookBackPeriod-2))
  
  tests <- (returns$RETURN - drift)/(vol$spot)
  
  tests[is.infinite(tests)] <- NA
  # Calculating the critical value of the test.
  const <- 0.7978846 # = sqrt(2/pi)
  n <- nrow(returns) / D
  Cn <- sqrt(2 * log(n))/const - (log(pi) + log( log(n) ))/(2 * const*sqrt((2 * log(n))))
  Sn <- 1/sqrt(const * 2 * log(n))
  betastar <- -log(-log(1-alpha))
  criticalValue <- Cn + Sn * betastar
  
  if (dataWasXts) {
    pData <- as.xts(pData[ , list(DT, PRICE)])
  } else {
    pData <- pData[ , list(DT, PRICE)]
  }
  
  
  out <- list("ztest" = tests, "vol" = vol,  "drift" = drift, "criticalValue" = criticalValue, 
              "pData" = pData, "prices" = prices, "isMultiDay" = isMultiDay)
  class(out) <- c( "intradayJumpTest", "list")
  
  # testingTimes <- trimws(gsub("1970-01-01", "", index(vol$spot)))
  # testType <- "LM"
  #   foo <- switch (testType,
  #     LM = LeeMyklandtest(testData, testingTimes, windowSize, K, alpha),
  #     FoF = FoFJumpTest(bar, 0.5, 50, alpha),
  #     rank = rankJumpTest(testData[,1], testData[,-1], alpha = alpha, K = windowSize, kn = K,
  #                         r = r, BoxCox = BoxCox, nBoot = nBoot,
  #                         dontTestAtBoundaries = dontTestAtBoundaries))
  # 
  # browser()    
  # foo$L - tests
  #     
  #   )
  #   if(setClass){
  #     
  #     if(testType == "rank"){
  #       #browser()
  #       tmp <- list() # So we don't override the output list.
  #       if(is.null(pData)){
  #         tmp[["tests"]] <- cbind(testData, c(out$jumpIndices, rep(NA, nrow(testData) - length(out$jumpIndices))))
  #       } else {
  #         
  #         print("ask Onno, Kris, and Nabil about best action")
  #         temp <- NULL
  #         for (date in dates){
  #           temp <- rbind(temp, aggregateTS(pData[date], on = "minutes", k = windowSize))
  #         }
  #         tmp[["tests"]] <- cbind(temp, c(out$jumpIndices, rep(NA, nrow(temp) - length(out$jumpIndices)))) 
  #       }
  #       
  #       
  #       #"tests" = cbind(ifelse(is.null(pData), testData, pData), c(out$jumpIndices, rep(NA, ifelse(is.null(pData), nrow(testData), nrow(pData)) - length(out$jumpIndices))))
  #       tmp[["information"]] <- list("testType" = testType, "isMultiDay" = isMultiDay, 
  #                                                            "criticalValues" = out$criticalValues ,"K" = K, 
  #                                                            "kn" = K, "testStatistic" = out$testStatistic)
  #       colnames(tmp[["tests"]]) <- c(colnames(tmp[["tests"]])[1:(ncol(tmp[["tests"]]) -1 )], "jumps")
  #       out <- tmp
  #     }
  #     else {
  #       out <- list("tests" = merge.xts(pData, out), "information" = list("testType" = testType, "isMultiDay" = isMultiDay))
  #     }
  #     class(out) <- c( "intradayJumpTest", "list")
  # 
  #   }
  # 
  # }

  return(out)

}


#' @importFrom xts addPolygon xts
#' @importFrom graphics lines
#' @importFrom zoo na.locf0
#' @export
plot.intradayJumpTest <- function(x, ...){
  #unpack values
  isMultiday <- x[["isMultiDay"]]

  if (isMultiday) {
    
    D <- ndays(x$pData)
    prices <- as.xts(x$pData)
    dates <- as.character(unique(as.Date(index(prices)))) # Get the dates in the sample.
    for (d in 1:D){ # We loop through all the days and ask the user whether we should continue or stop
      
      thisDat <- prices[dates[d]]
      p1 <- plot(thisDat, main = "intraday jump test", lty = ifelse(is.null(x$prices), 1, 2))
      
      if(!is.null(x$prices)){
        if(!is.xts(x$prices)){
          p1 <- lines(na.locf0(cbind(thisDat, as.xts(x$prices[,c("DT","PRICE")])))[ ,2], col = "blue", lwd = 2)
        } else {
          p1 <- lines(na.locf0(cbind(x$pData, x$prices))[ ,2], col = "blue", lwd = 2)
        }
        p1 <- addLegend(legend.loc = 'topleft', legend.names = c("Actual prices", "Sub-sampled prices", "Jump detection zone"), lwd = c(2,2,0), pch=c(NA,NA,15), col =c(1, "blue", 2))
      }
      
      
      
      
      ## Add shaded region to the plot where we detect jumps
      # if(testType == "Lee-Mykland"){
      #   p1 <- plot(na.locf0(thisDat[, 1]), main = main.text, lty = 2)
      #   
      #   p1 <- lines(na.locf0(thisDat[,2]), col = "blue", lwd = 2)
      #   
      #   p1 <- addLegend(legend.loc = 'topleft', legend.names = c("Actual prices", "Sub-sampled prices", "Jump detection zone"), lwd = c(2,2,0), pch=c(NA,NA,15), col =c(1, "blue", 2))
      #   
      # } else if(testType == "FoF") {
      #   p1 <- plot(na.locf0(thisDat[, 1]), main = main.text, lty = 1)
      #   
      #   p1 <- addLegend(legend.loc = 'topleft', legend.names = c("Actual prices", "Jump detection zone"), lwd = c(2,0), pch=c(NA,15), col =c(1, 2))
      #   
      # }else if(testType == "rank"){
      #   
      #   shade <- na.omit(jumps[day])
      #   
      #   p1 <- plot(na.locf0(thisDat[, 1:(ncol(thisDat)-1)]), main = main.text, lty = 1)        
      # }
      # 
      
      
      shade <- abs( x$ztest[dates[d]] ) > x$criticalValue
      shade <- cbind(upper = shade * as.numeric(max(thisDat, na.rm = TRUE) +1e5), 
                     lower = shade * as.numeric(min(thisDat, na.rm = TRUE)) -1e5)
      colnames(shade) <- c("upper", "lower")
      shade <- na.omit(shade)
      p1 <- addPolygon(shade, on = -1, col = 2)
      plot(p1)

      if(readline("Press Enter for next figure or 0 to exit:\n") == "0"){
        break # We break out of the for loop of plotting
      }

    }

  } else {
    shade <- abs( x$ztest ) > x$criticalValue
    
    if (is.xts(x$pData)) {
      shade <- cbind(upper = shade * as.numeric(max(x$pData, na.rm = TRUE) +1e5), lower = shade * as.numeric(min(x$pData, na.rm = TRUE)) -1e5)
    } else {
      shade <- cbind(upper = shade * as.numeric(max(x$pData$PRICE, na.rm = TRUE) +1e5), lower = shade * as.numeric(min(x$pData$PRICE, na.rm = TRUE)) -1e5)
    }
    
    colnames(shade) <- c("upper", "lower")
    p1 <- plot(na.locf0(as.xts(x$pData)), main = "intraday jump test", lty = ifelse(is.null(x$prices), 1, 2))

    if(!is.null(x$prices)){
      if(!is.xts(x$prices)){
        p1 <- lines(na.locf0(cbind(as.xts(x$pData), as.xts(x$prices[,c("DT","PRICE")])))[ ,2], col = "blue", lwd = 2)
      } else {
        p1 <- lines(na.locf0(cbind(x$pData, x$prices))[ ,2], col = "blue", lwd = 2)
      }
      p1 <- addLegend(legend.loc = 'topleft', legend.names = c("Actual prices", "Sub-sampled prices", "Jump detection zone"), 
                      lwd = c(2,2,0), pch = c(NA,NA,15), col =c(1, "blue", 2))
    } else {
      p1 <- addLegend(legend.loc = 'topleft', legend.names = c("Prices", "Jump detection zone"), 
                      lwd = c(2,0), pch=c(NA,15), col =c(1, 2))
    } 
    ## Add shaded region to the plot where we detect jumps
    shade <- na.omit(shade)
    p1 <- addPolygon(shade, on = -1, col = 2)
    plot(p1)
  }

  invisible(p1)

}



#' General framework for testing for jumps on an intraday basis
#' 
#' @param marketPrice data.table or xts containing the market prices in levels
#' @param stockPrices list containing the individual stock prices in either data.table or xts format. The format should be the the same as \code{marketPrice}
#' @param alpha signicance level (in standard deviations) to use for the jump detections. Default is \code{c(5,3)} for 5 and 3 in the market and stocks respectively.
#' @param localWindow numeric denoting the local window for the bootstrap algorithm. Default is \code{30}
#' @param coarseFreq numeric denoting the coarse sampling frequency. Default is \code{10}
#' @param rank rank of the jump matrix under the null hypothesis. Default is \code{1}
#' @param BoxCox numeric of exponents for the Box-Cox transformation, default is \code{1}
#' @param nBoot numeric denoting how many replications to be used for the bootstrap algorithm. Default is \code{1000}
#' @param on string indicating the time scale in which \code{k} is expressed.
#' Possible values are: \code{"secs", "seconds", "mins", "minutes", "hours"}.
#' @param k positive integer, indicating the number of periods to aggregate
#' over. E.g. to aggregate an \code{xts} object to the 5 minute frequency, set
#' \code{k = 5} and \code{on = "minutes"}.
#' @param marketOpen the market opening time. This should be in the time zone
#' specified by \code{tz}. By default, \code{marketOpen = "09:30:00"}.
#' @param marketClose the market closing time. This should be in the time zone
#' specified by \code{tz}. By default, \code{marketClose = "16:00:00"}.
#' @param tz string specifying the time zone to which the times in \code{data}
#' and/or \code{marketOpen}/ \code{marketClose} belong. Default = \code{"GMT"}.
#' This parameter will also help determine the testing times as the test is done on non-overlapping pre-averaged returns.
#' 
#' @importFrom stats na.omit quantile runif
#' @importFrom zoo coredata
#' @export
rankJumpTest <- function(marketPrice, stockPrices, alpha = c(5,3), coarseFreq = 10, localWindow = 30, rank = 1, BoxCox = 1, nBoot = 1000, dontTestAtBoundaries = TRUE, on = "minutes", k = 5,
                         marketOpen = "09:30:00", marketClose = "16:00:00", tz = "GMT"){
  
  ## Preparation of data
  PRICE = DATE = RETURN = DT = NULL
  
  if (!all.equal(class(marketPrice), class(stockPrices[[1]]))) {
    stop("Please provide marketPrice and stockPrice as the same class (either xts or data.table)")
  }
  
  if (!"PRICE" %in% colnames(marketPrice)) {
    if (dim(marketPrice)[2] == 1) {
      names(marketPrice) <- "PRICE"
    } else {
      stop("data.table or xts needs column named PRICE.")
    }
  }
  
  dummyWasXts <- FALSE
  if (!is.data.table(marketPrice)) {
    if (is.xts(marketPrice)) {
      marketPrice <- setnames(as.data.table(marketPrice), old = "index", new = "DT")
      marketPrice[, PRICE := as.numeric(PRICE)]
      
      dataWasXts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (!("DT" %in% colnames(marketPrice))) {
      stop("Data.table needs DT column containing the time-stamps of the trades.") # added the timestamp comment for verbosity.
    }
    
  }

  marketPrice <- aggregatePrice(marketPrice, on = on, k = k , marketOpen = marketOpen,
                          marketClose = marketClose, tz = tz, fill = TRUE)
  marketPrice[, DATE := as.Date(DT, tz = tzone(marketPrice$DT))]
  setkeyv(marketPrice, "DT")
  marketPrice <- marketPrice[, RETURN := log(PRICE) - shift(log(PRICE), type = "lag"), by = "DATE"][!is.na(RETURN)]
  marketReturns <- xts(marketPrice$RETURN, order.by = marketPrice$DT)
  
  stockReturns <- NULL
  for (stock in 1:length(stockPrices)) {
    tmp <- setnames(as.data.table(stockPrices[[stock]]), old = "index", new = "DT")
    colnames(tmp) <- c("DT", "PRICE")
    tmp <- aggregatePrice(tmp, on = on, k = k , marketOpen = marketOpen,
                                 marketClose = marketClose, tz = tz, fill = TRUE)
    tmp[, DATE := as.Date(DT, tz = tzone(tmp$DT))]
    setkeyv(tmp, "DT")
    
    tmp <- tmp[, RETURN := log(PRICE) - shift(log(PRICE), type = "lag"), by = "DATE"][!is.na(RETURN)]
    stockReturns <- cbind(stockReturns, as.xts(tmp[, list(DT, PRICE)])$PRICE)
  }

  
  if (!is.numeric(alpha)) {
    stop("alpha must be a numeric and should be of length one or two")
  }
  if (length(alpha) == 1) {
    alpha = rep(alpha,2)
  } else if (length(alpha) > 2) {
    warning("alpha should be a numeric of length one or two")
    alpha = alpha[1:2]
  }
  
  if (any(alpha < 1)) {
    warning("alpha should be specified in terms of standard deviations in the rank jump test.")
  }
  ## Data prep ends
  
  nDays <- ndays(marketReturns)
  nRets <- nrow(marketReturns)/nDays
  marketJumpDetection <- jumpDetection(marketReturns, alpha[1], nRets = nRets, nDays = nDays)
  jumpIndices <- marketJumpDetection$jumpIndices
  if(length(jumpIndices) == 0){ # We detected no jumps, we can just skip. and return all NULL's
    return(list("criticalValues" = NULL, "testStatistic" = NULL, "dimensions" = NULL,
                "marketJumpDetections" = NULL, "stockJumpDetections" = NULL, "jumpIndices" = NULL))
  }
  nAssets <- ncol(stockReturns)
  stockJumpDetections <- stockReturns
  for(j in 1:nAssets){
    stockJumpDetections[, j] <- jumpDetection(stockJumpDetections[,j], alpha[2], nRets = nRets, nDays = nDays)[["Un"]]
  }
  
  jumps <- matrix(coredata(stockReturns)[jumpIndices,], ncol = ncol(stockReturns), byrow = FALSE)
  
  for (i in 1:(coarseFreq-1)) {
    jumps <- jumps + matrix(stockReturns[jumpIndices + i, ], ncol = ncol(stockReturns), byrow = FALSE)
  }
  
  jumps <- t(jumps) # Transpose here so we don't have to transpose every time in the loop.
  
  # Set nu and nv because we need full SVD (see https://stackoverflow.com/questions/41972419/different-svd-result-in-rank-and-matlab)
  decomp <- svd(jumps, nu = nrow(jumps), nv = ncol(jumps))
  U2 <- decomp$u[, (rank+1):ncol(decomp$u)]
  V2 <- decomp$v[, (rank+1):ncol(decomp$v)]
  
  singularValues <- decomp$d[(rank+1):length(decomp$d)]^2
  
  testStatistic <- numeric(length(BoxCox))
  for (i in 1:length(BoxCox)) {
    a <- BoxCox[i]
    testStatistic[i] <- sum(BoxCox__(singularValues, a))
  }
  
  ## Start bootstrapping of the critical values
  p <- ncol(jumps)
  dxc <- pmax(pmin(coredata(stockReturns), coredata(stockJumpDetections)), -coredata(stockJumpDetections))
  siumulatedTestStatistics <- numeric(nBoot)
  
  for (b in 1:nBoot) {
    zetaStar <- matrix(0, nrow = nAssets, ncol = p)
    for (i in 1:p) {
      jmp <- jumpIndices[i] 
      
      if (dontTestAtBoundaries) {
        # We need to make sure that we don't take data from the previous day
        pos <- ((jmp - 1) %% nRets) + 1
        leftKN <- min(localWindow, pos-1)
        rightKN <- min(localWindow, nRets - pos)
      } else {
        leftKN <- localWindow
        rightKN <- localWindow
      }
      
      jumpLeft <- ceiling(runif(1) * leftKN)
      jumpRight <- ceiling(runif(1) * rightKN)
      
      dxcLeft <- t(dxc[jmp - jumpLeft,])
      dxcRight <- t(dxc[jmp + jumpRight,])
      
      kappaStar <- runif(1)
      
      zetaStar[,i]  <- sqrt(kappaStar) * dxcLeft + sqrt(coarseFreq - kappaStar) * dxcRight
      
    }
    
    tmp <- t(U2) %*% zetaStar %*% V2
    siumulatedTestStatistics[b] <- t(as.numeric(tmp)) %*% as.numeric(tmp)
    
  }
  
  criticalValues <- quantile(siumulatedTestStatistics, c(0.9, 0.95, 0.99))
  
  out <- list("criticalValues" = criticalValues, "testStatistic" = testStatistic, "dimensions" = dim(jumps),
              "marketJumpDetections" = marketJumpDetection, "stockJumpDetections" = stockJumpDetections, "jumpIndices" = jumpIndices)
  
  return(out)
}


#' @keywords internal
timeOfDayAdjustments <- function(returns, n, m, polyOrder){
  
  
  timePolyMatrix <- matrix(rep(1:nrow(returns), each = polyOrder + 1)^(0:polyOrder), nrow = nrow(returns), ncol = polyOrder + 1, byrow = TRUE)
  
  timeOfDayScatter <- 1.249531 * rowMeans((abs(returns[,1:(m-2)])* abs(returns[,2:(m-1)]) * abs(returns[,3:m]))^(2/3))
  
  timeOfDayBeta <- as.numeric(solve(t(timePolyMatrix) %*% timePolyMatrix) %*% t(timePolyMatrix) %*% timeOfDayScatter)
  
  timeOfDayFit <- timePolyMatrix %*% timeOfDayBeta
  
  # Normalize the fit
  timeOfDayFit <- timeOfDayFit / mean(timeOfDayFit)
  
  timeOfDayScatter <- timeOfDayScatter/mean(timeOfDayScatter)
  out <- list("timeOfDayScatter" = timeOfDayScatter, "timeOfDayFit" = timeOfDayFit, "timeOfDayBeta" = timeOfDayBeta, "timePolyMatrix" = timePolyMatrix) 
  
  return(out)
  
}

#' @keywords internal
#' @importFrom zoo coredata
jumpDetection <- function(returns, alpha, nRets, nDays){
  
  returns <- matrix(coredata(returns), nrow = nRets, ncol = nDays, byrow = FALSE) #remap returns
  bpv <- pi/2 * colSums(abs(returns[1:(nRets-1),]) * abs(returns[2:nRets,]))
  rv <- colSums(returns^2)
  TODadjustments <- timeOfDayAdjustments(returns, n=nRets,  m = nDays, polyOrder = 2)
  Un <- alpha * sqrt(kronecker(pmin(bpv,rv), TODadjustments$timeOfDayFit)) * (1/nRets) ^0.49
  
  jumpIndices <- which(abs(as.numeric(returns)) > Un) # Where does a jump in the market occur?
  
  out <- list("jumpIndices" = jumpIndices, "Un" = Un, "timeOfDayADJ" = TODadjustments$timeOfDayFit)
  return(out)
  
}

#' @keywords internal
#find better name?
BoxCox__ <- function(x, lambda){
  if (!lambda) {
    return(log(1+x))
  } else {
    return(((1+x)^lambda - 1)/lambda)
  }
}



#' #' @importFrom xts xts
#' #' @keywords internal
#' LeeMyklandtest <- function(testData, testingTimes, windowSize, K, alpha){
#' 
#'   oldK <- K
#'   const <- 0.7978846 # c in formula 10 in the paper
#'   return <- spotBPV <- Cn <- Sn <- L <- numeric(length(testingTimes))
#'   betastar <- -log(-log(1-alpha))
#'   dateOfData <- as.character(as.Date(index(testData[1])))
#' 
#'   # The testingTimes can be provided in terms of seconds after midnight. 
#'   if(is.numeric(testingTimes)){
#'     testingTimes <- as.POSIXct(testingTimes, origin = dateOfData)
#'     testingTimes <- trimws(gsub(dateOfData, '', testingTimes)) # We only want hours:mins:sec
#'   }
#' 
#'   for (i in (K+1):length(testingTimes)) {
#'     # Here we keep only the data needed for this test.
#'     thisData <- testData[paste0("/", dateOfData, ' ', testingTimes[i])]
#' 
#' 
#'     # here we check if enough data is available to conduct this test. If not we have to truncate K. 
#'     # In the else clause we simply set K to the user selected value such that we only use a 'wrong' K when there is not enough data.
#'     # This means that the generalization to more dates should be easier.
#' 
#'     if (K > length(thisData)){
#'       # Here it is not possible to construct a test.
#'       if(length(thisData) < 5){
#'         L[i] <- 0
#'         Cn[i] <- 0
#'         Sn[i] <- 0
#'         warning(paste0("Not enough data to test at time ", testingTimes[i], " Skipping!\n This happened on test ", i))
#'         # We need to reset K so we can have more than one test fail.
#'         K <- oldK
#'         next
#'       }
#'       warning(paste0("The window K mandates using more data than is available in the data provided\n using less data. This happened on test ", i))
#'       K <- length(thisData) -1
#'     }
#' 
#' 
#'     # We can drop the xts attribute so we don't have to use as.numeric multiple times below
#'     thisData <- as.numeric(thisData[(length(thisData)-K):length(thisData)])
#' 
#'     return[i] <- log(thisData[length(thisData)] / thisData[length(thisData) - 1])
#' 
#'     returns <- diff(log(thisData[-length(thisData)]))
#'     n <- length(returns)
#'     spotBPV[i] <- 1/(K-2) * sum(abs(returns[1:(n-1)]) * abs(returns[2:n]))
#' 
#'     L[i] <- return[i]/sqrt(spotBPV[i])
#'     # We have Cn and Sn as vectors as the size of these may change.
#'     Cn[i] <- sqrt(2 * log(n))/const - (log(pi) + log( log(n) ))/(2 * const*sqrt((2 * log(n))))
#'     Sn[i] <- 1/sqrt(const * 2 * log(n))
#'     
#'     K <- oldK
#'   }
#'   jumps <- (abs(L) - Cn)/Sn > betastar
#' 
#'   jumps <- xts(jumps, as.POSIXct(paste(dateOfData, testingTimes)))
#'   out <- merge.xts(testData, merge.xts(jumps, L, spotBPV, return))
#'   return(out)
#' 
#' }
#' 
#' 
#' 
#' 
#' #' @keywords internal
#' FoFJumpTest <- function(pData, theta, M, alpha){
#' 
#'   # Users will not directly control the testing times, instead they can choose the M parameter.
#'   dateOfData <- as.character(as.Date(index(pData[1])))
#'   betastar <- -log(-log(1-alpha))
#'   nObs <- length(pData)
#' 
#'   ## We need to ensure kn is even, thus we round half and multiply by 2
#'   kn <- round(theta * sqrt(nObs))
#'   kn <- kn + kn%%2
#' 
#'   const <- 0.7978846 # = sqrt(2/pi)
#'   Cn <- sqrt(2 * log(kn))/const - (log(pi) + log( log(kn) ))/(2 * const*sqrt((2 * log(kn))))
#'   Sn <- 1/sqrt(const * 2 * log(kn))
#'   criticalValue <- Cn + Sn * betastar
#' 
#'   # Measuring jump variation during the entire day.
#'   preAveragedReturns <- hatreturn(pData, kn)
#'   preAveragedReturns <- c(as.numeric(preAveragedReturns), rep(NA, length(pData) - length(preAveragedReturns)))#, as.POSIXct(index(pData), origin = dateOfData)) # maybe we want to add back in xts, but it's removed for now...
#' 
#'   psi1kn <- kn * sum((gfunction((1:kn)/kn) - gfunction(( (1:kn) - 1 )/kn ) )^2)
#' 
#'   psi2kn <- 1 / kn * sum(gfunction((1:kn)/kn)^2)
#' 
#'   psi2kn <- (1 + (2*kn)^-2)/12
#'   returns <- diff(log(pData))
#'   omegaHat <- -1/(nObs-1) * sum(returns[2:nObs] * returns[1:(nObs-1)])
#' 
#'   biasCorrection <- omegaHat^2/theta^2 * psi1kn/psi2kn
#' 
#'   preAveragedRealizedVariance <- nObs/(nObs - kn + 2) * 1/(kn * psi2kn) * sum(preAveragedReturns^2, na.rm= TRUE) - biasCorrection
#'   preAveragedBipowerVariation <- nObs/(nObs - 2*kn + 2) * 1/(kn * psi2kn) * pi/2 * sum(abs(preAveragedReturns[1:(nObs-2*kn)]) * abs(preAveragedReturns[1:(nObs-2*kn) + kn])) - biasCorrection
#' 
#'   ## We need to compute the variance covariance matrix to get 
#'   
#'   jumpVariation <- (preAveragedRealizedVariance - preAveragedBipowerVariation)/preAveragedRealizedVariance
#' 
#' 
#'   testingIndices <- seq(M - 2 + kn +1, nObs-kn, by = kn)# + 1  to account for edge-effect!
#'   PABPV <- PAreturns <- numeric(length(testingIndices))
#' 
#' 
#'   ind <- 1
#'   for (i in testingIndices) {
#'     PABPV[ind] <- pi/2 * 1/(M-2) * sum(abs(preAveragedReturns[(i - M + 2):(i-1)]) * abs(preAveragedReturns[(i-M+2-kn):(i-1-kn)]))
#'     ind <- ind + 1 # increment index
#'   }
#' 
#' 
#'   L <- preAveragedReturns[testingIndices]/sqrt(PABPV)
#' 
#' 
#'   jumps <- xts((abs(L) > criticalValue), index(pData)[testingIndices])
#'   
#'   out <-  merge.xts(jumps, L, jumpVariation, PABPV)
#'   return(out)
#' 
#' }
#' 

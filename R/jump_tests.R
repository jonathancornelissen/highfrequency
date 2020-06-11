
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
#'  Then the AJjumptest is given by: 
#'    \deqn{
#'      \mbox{AJjumptest}_{t,N}= \frac{S_t(p,k,h)-k^{p/2-1}}{\sqrt{V_{t,N}}}
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
#' @param pdata a zoo/xts object containing all prices in period t for one asset.
#' @param p can be chosen among 2 or 3 or 4. The author suggests 4. 4 by default.
#' @param k can be chosen among 2 or 3 or 4. The author suggests 2. 2 by default.
#' @param align.by a string, align the tick data to "seconds"|"minutes"|"hours"
#' @param align.period an integer, align the tick data to this many [seconds|minutes|hours].
#' @param alpha.multiplier alpha multiplier
#' @param makeReturns boolean, should be TRUE when rdata contains prices instead of returns. FALSE by default.
#' @param ... additional arguments.
#' 
#' @return list
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
#' AJjumptest(sampleTData$PRICE, p = 2, k = 3, align.by = "seconds", 
#'   align.period = 5, makeReturns = TRUE)
#' 
#' @keywords highfrequency AJjumptest
#' @importFrom stats qnorm
#' @importFrom stats pnorm
#' @export
AJjumptest <- function(pdata, p = 4 , k = 2, align.by = NULL, align.period = NULL, alpha.multiplier = 4, makeReturns = FALSE, ...) {

  if (checkMultiDays(pdata) == TRUE) {
    result <- apply.daily(pdata, AJjumptest, align.by, align.period, makeReturns)
    return(result)
  } else {
    pdata <- fastTickAgregation(pdata, on = "seconds", k = 1)
  }

  N <- length(pdata)-1
  p <- as.numeric(p)
  k <- as.numeric(k)

  alpha <- alpha.multiplier * sqrt(rCov(pdata, align.by = align.by, align.period = align.period, makeReturns = makeReturns))
  w <- 0.47
  cvalue <- alpha * (1/N)^w

  h <- align.period * scale(align.by)
  hk <- h * k

  seq1 <- seq(1, N, h)
  seq2 <- seq(1, N, hk)

  # return data
  pdata1 <- pdata[seq1]
  pdata2 <- pdata[seq2]

  r  <- abs(makeReturns(pdata))
  r1 <- abs(makeReturns(pdata1))
  r2 <- abs(makeReturns(pdata2))

  pv1 <- sum(r1^p)
  pv2 <- sum(r2^p)

  S <- pv2 /pv1

  ## selection return:
  selection <- abs(r) < cvalue
  rse <- abs(makeReturns(pdata[selection]))

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
#' Then the BNSjumptest is given by: 
#' \deqn{
#' \mbox{BNSjumptest}= \frac{RV - IVestimator}{\sqrt{(\theta-2)\frac{1}{N} {IQestimator}}}
#' }
#' in which, \eqn{IVestimator} can be: bipower variance (BV), minRV, medRV. 
#' \eqn{IQestimator} can be: tripower quarticity (TP), quadpower quarticity (QP), minRQ, medRQ.
#' 
#' \eqn{\theta}: depends on IVestimator (Huang and Tauchen (2005)).
#' 
#' @param rdata a zoo/xts object containing all returns in period t for one asset.
#' @param IVestimator can be chosen among jump robust integrated variance estimators: BV, minRV, medRV and corrected threshold bipower variation (CTBV). If CTBV is chosen, an argument of \eqn{startV}, start point of auxiliary estimators in threshold estimation (Corsi et al. (2010) can be included. BV by default.
#' @param IQestimator can be chosen among jump robust integrated quarticity estimators: TP, QP, minRQ and medRQ. TP by default.
#' @param type a method of BNS testing: can be linear or ratio. Linear by default.
#' @param logtransform boolean, should be TRUE when QVestimator and IVestimator are in logarith form. FALSE by default.
#' @param max boolean, should be TRUE when max adjustment in SE. FALSE by default.
#' @param align.by a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param align.period an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns boolean, should be TRUE when rdata contains prices instead of returns. FALSE by default.
#' 
#' @return list
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
#' BNSjumptest(sampleTData$PRICE, IVestimator= "minRV", 
#'             IQestimator = "medRQ", type= "linear", makeReturns = TRUE)
#' 
#' @keywords highfrequency BNSjumptest
#' @export
BNSjumptest <- function (rdata, IVestimator = "BV", IQestimator = "TP", type = "linear",
                         logtransform = FALSE, max = FALSE, align.by = NULL, align.period = NULL,
                         makeReturns = FALSE) {
  if (checkMultiDays(rdata) == TRUE) {
    result <- apply.daily(rdata, BNSjumptest, align.by, align.period, makeReturns)
    return(result)
  } else {
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata <- fastTickAgregation(rdata, on = align.by, k = align.period)
    }
    if (makeReturns == TRUE) {
      rdata <- makeReturns(rdata)
    }
    N <- length(rdata)
    hatQV <- RV(rdata)
    hatIV <- hatIV(rdata, IVestimator)
    theta <- tt(IVestimator)
    hatIQ <- hatIQ(rdata, IQestimator)
    if (type == "linear") {
      if (logtransform) {
        hatQV <- log(RV(rdata))
        hatIV <- log(hatIV(rdata, IVestimator))
      }
      if (!logtransform) {
        hatQV <- RV(rdata)
        hatIV <- hatIV(rdata, IVestimator)
      }
      if (max) {
        product <- max(1, hatIQ(rdata, IQestimator)/hatIV(rdata, IVestimator)^2)
      }
      if (!max) {
        product <- hatIQ(rdata, IQestimator)
      }
      a <- sqrt(N) * (hatQV - hatIV)/sqrt((theta - 2) * product)
      out <- list()
      out$ztest <- a
      out$critical.value <- qnorm(c(0.025, 0.975))
      out$pvalue <- 2 * pnorm(-abs(a))
      return(out)
    }
    if (type == "ratio") {
      if (max) {
        product <- max(1, hatIQ(rdata, IQestimator)/hatIV(rdata, IVestimator)^2)
      }
      if (!max) {
        product <- hatIQ(rdata, IQestimator)/hatIV(rdata, IVestimator)^2
      }
      a <- sqrt(N) * (1 - hatIV(rdata, IVestimator, N)/RV(rdata))/sqrt((theta - 2) * product)
      out <- list()
      out$ztest <- a
      out$critical.value <- qnorm(c(0.025, 0.975))
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
#'  Then the JOjumptest is given by: 
#'    \deqn{
#'      \mbox{JOjumptest}_{t,N}= \frac{N BV_{t}}{\sqrt{\Omega_{SwV}} \left(1-\frac{RV_{t}}{SwV_{t}} \right)}
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
#' @param pdata a zoo/xts object containing all prices in period t for one asset.
#' @param power can be chosen among 4 or 6. 4 by default.
#' @param ... additional arguments.
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
#'  The Jiang and Oomen test is that: in the absence of jumps, the accumulated difference between the simple return and the log return captures one half of the integrated variance.(Theodosiou& Zikes(2009))
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
#' JOjumptest(sample5MinPricesJumps$stock1, power = 6)
#' 
#' @keywords highfrequency JOjumptest
#' @importFrom stats qnorm
#' @importFrom stats pnorm
#' @importFrom zoo as.zoo
#' @export
JOjumptest <- function(pdata, power = 4, ...) {

  R  <- as.zoo(simre(pdata))
  r  <- as.zoo(makeReturns(pdata))
  N  <- length(pdata) - 1
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
    out$critical.value <- qnorm(c(0.025, 0.975))
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
    out$critical.value <- qnorm(c(0.025,0.975))
    out$pvalue         <- 2*pnorm(-abs(JOtest))
    return(out)
  }
}


####### extraArgs is a list of args I haven't found a name for yet #######

#' General framework for testing for jumps on an intraday basis
#' 
#' @param pData (currently only xts) of the price data in levels. This data can (and should in some cases) be tick-level data. This data can span more than one day
#' @param testType type of test to use. Currently supports "LM" test of Lee and Mykland (2018), and "FoF" as "Fact or Friction" test from Christensen, Oomen, and Podolskij (2014)
#' @param testingTimes a character vector of times to test for jumps, for example  \code{c("11:30", "11:35", "14:30")}. 
#' This argument can also be a numeric vector containing times to test in seconds after midnight. For example, using \code{seq(39200, 57600, 300)}, 
#' where testing will take place every five minutes, starting fifty minutes after opening. (On when open is 09:30). This argument is not used in the FoF type test.
#' @param windowSize the size of the window to test for jumps in, in minutes. Default = 5. Not used in the FoF type test.
#' @param K In the LM test, K, is the amount of windowSizes to use for estimation of the local variance. 
#' In case of the FoF test K will be used as M in eqn: 21 in the paper, which changes the meaning into how many non-overlapping pre-averaged returns should be used in each test.
#' @param alpha numeric significance level to use for the jump tests.
#' @param theta numeric parameter in determining the pre-averaging horizon. Default = 0.5. The pre-averaging horizon is \code{round(theta * sqrt(n))}, where n is the number of observations. 
#' This parameter will also help determine the testing times as the test is done on non-overlapping pre-averaged returns.
#' @param ... used internally. Don't set this parameter.
#' @references COP2014, LM 2008
#' @importFrom zoo index
#' @export
intradayJumpTest <- function(pData = NULL, testType = "LM", testingTimes = NULL, windowSize = 5, K = 10, alpha = 0.05, theta = 0.5, ...){


  print("make pData able to use data.table")

  ## Make space for data preparation
  if(ncol(pData) >1 ){
    print("multiple assets are not supported yet. -> working on it!")
  }

  internals <- list(...)
  if(!("setClass" %in% names(internals))){
    setClass <- TRUE
  } else {
    setClass <- internals[["setClass"]]
  }

  internals <- list(...)
  if(!("returnData" %in% names(internals))){
    returnData <- TRUE
  } else {
    returnData <- internals[["returnData"]]
  }



  if(highfrequency:::checkMultiDays(pData)){

    dates <- as.character(unique(as.Date(index(pData)))) # We will need to loop over all days in the sample
    out <- vector(mode = "list", length = 2)
    names(out) <- c("tests", "information")

    out[["tests"]] <- vector(mode = "list", length = length(dates))

    names(out[["tests"]]) <- dates
    for (date in dates) {
      ## Conduct the testing day-by-day
      out[["tests"]][[date]] <- merge.xts(pData[date], intradayJumpTest(pData[date], testType, testingTimes, windowSize, K, alpha, theta, setClass = FALSE))
    }
    out[["information"]] <- list("testType" = testType, "isMultiDay" = TRUE)
    class(out) <- c("intradayJumpTest", "list")

  } else {
    # Lee-Mykland needs this - maybe others too
    testData <- aggregatets(pData, on = "minutes", k = windowSize)

    out <- switch (testType,
      LM = LeeMyklandtest(testData, testingTimes, windowSize, K, alpha),
      FoF = FoFJumpTest(pData, theta, K, alpha)
    )

    if(setClass){
      out <- list("tests" = merge.xts(pData, out), "information" = list("testType" = testType, "isMultiDay" = FALSE))
      class(out) <- c( "intradayJumpTest", "list")

    }

  }



  return(out)


}


#' @importFrom xts addPolygon
#' @export
plot.intradayJumpTest <- function(x, ...){
  #unpack values
  isMultiday <- x[["information"]][["isMultiDay"]]
  # Here, we have more than one day
  if(isMultiday){
    dates <- names(x[["tests"]])
    dat <- NULL

    for (i in 1:length(dates)) {
      if(is.null(dat)){
        dat <- x[["tests"]][[i]]
      } else {
        dat <- rbind(dat, x[["tests"]][[i]])
      }
    }
  } else {
    dat <- x[["tests"]]
  }

  # Make the shade to show where we detect jumps


  testType <- x[["information"]][["testType"]]

  if(testType == "LM"){
    testType <- "Lee-Mykland"
    main.text <- paste0("Intraday Jump Test: ", testType)
  }

  if(testType == "FoF"){
    main.text <- paste0("Intraday Jump Test: Fact or Friction \nJump Variation:", round(max(0, mean(dat$jumpVariation, na.rm=TRUE)), 4))
  }


  if(isMultiday){
    print("Support for plotting intradayJumpTests when the data spans more than one day is not ready yet.")

    for(day in dates){ # We loop through all the days and ask the user whether we should continue or stop
      thisDat <- dat[day]
      shade <- thisDat$jumps


      if(testType == "FoF"){
        # Here we update the jump variation in the main text in the case of the FoF test.
        main.text <- paste0("Intraday Jump Test: Fact or Friction \nJump Variation:", round(max(0, mean(thisDat$jumpVariation, na.rm=TRUE)), 4))
      }


      shade <- cbind(upper = shade * as.numeric(max(thisDat$pData, na.rm = TRUE) +1e5), lower = shade * as.numeric(min(thisDat$pData, na.rm = TRUE)) -1e5)

      colnames(shade) <- c("upper", "lower")

      p1 <- plot(na.locf0(thisDat[, 1]), main = main.text, lty = 2)
      ## Add shaded region to the plot where we detect jumps
      if(testType == "Lee-Mykland"){
        p1 <- lines(na.locf0(thisDat$ts), col = "blue", lwd = 2)
        p1 <- addLegend(legend.loc = 'topleft', legend.names = c("Actual prices", "Sub-sampled prices", "Jump detection zone"), lwd = c(2,2,0), pch=c(NA,NA,15), col =c(1, "blue", 2))
      } else {
        p1 <- addLegend(legend.loc = 'topleft', legend.names = c("Actual prices", "Jump detection zone"), lwd = c(2,0), pch=c(NA,15), col =c(1, 2))
      }


      shade <- na.omit(shade)
      p1 <- addPolygon(shade, on = -1, col = 2)
      plot(p1)

      if(readline("Press Enter for next figure or 0 to exit:\n") == "0"){
        break # We break out of the for loop of plotting
      }

    }

  } else {

    shade <- dat$jumps
    shade <- cbind(upper = shade * as.numeric(max(dat$pData, na.rm = TRUE) +1e5), lower = shade * as.numeric(min(dat$pData, na.rm = TRUE)) -1e5)
    colnames(shade) <- c("upper", "lower")


    paste0("Intraday Jump Test: ", testType)


    p1 <- plot(na.locf0(dat[, "pData"]), main = main.text, lty = ifelse(testType == "LM", 2, 1))


    if(testType == "Lee-Mykland"){
      p1 <- lines(na.locf0(dat$ts), col = "blue", lwd = 2)
      p1 <- addLegend(legend.loc = 'topleft', legend.names = c("Actual prices", "Sub-sampled prices", "Jump detection zone"), lwd = c(2,2,0), pch=c(NA,NA,15), col =c(1, "blue", 2))
    } else {
      p1 <- addLegend(legend.loc = 'topleft', legend.names = c("Actual prices", "Jump detection zone"), lwd = c(2,0), pch=c(NA,15), col =c(1, 2))
    }
    ## Add shaded region to the plot where we detect jumps
    shade <- na.omit(shade)
    p1 <- addPolygon(shade, on = -1, col = 2)
    plot(p1)
  }



  invisible(p1)

}





###### All the functions under this comment will be moved to internal_jump_tests but for working purposes, I keep them here to have them near where they're used.

#' @importFrom xts xts
#' @keywords internal
LeeMyklandtest <- function(testData, testingTimes, windowSize, K, alpha){

  oldK <- K
  const <- 0.7978846 # c in formula 10 in the paper
  Cn <- Sn <- L <- numeric(length(testingTimes))
  betastar <- -log(-log(1-alpha))
  dateOfData <- as.character(as.Date(index(testData[1])))

  # The testingTimes can be provided in terms of seconds after midnight. 
  if(is.numeric(testingTimes)){
    testingTimes <- as.POSIXct(testingTimes, origin = dateOfData)
    testingTimes <- trimws(gsub(dateOfData, '', testingTimes)) # We only want hours:mins:sec
  }


  for (i in 1:length(testingTimes)) {
    # Here we keep only the data needed for this test.
    thisData <- testData[paste0("/", dateOfData, ' ', testingTimes[i])]


    # here we check if enough data is available to conduct this test. If not we have to truncate K. 
    # In the else clause we simply set K to the user selected value such that we only use a 'wrong' K when there is not enough data.
    # This means that the generalization to more dates should be easier.

    if (K > length(thisData)){
      # Here it is not possible to construct a test.
      if(length(thisData) < 5){
        L[i] <- 0
        Cn[i] <- 0
        Sn[i] <- 0
        warning(paste0("Not enough data to test at time ", testingTimes[i], " Skipping!\n This happened on test ", i))
        # We need to reset K so we can have more than one test fail.
        K <- oldK
        next
      }
      warning(paste0("The window K mandates using more data than is available in the data provided\n using less data. This happened on test ", i))
      K <- length(thisData) -1
    }


    # We can drop the xts attribute so we don't have to use as.numeric multiple times below
    thisData <- as.numeric(thisData[(length(thisData)-K):length(thisData)])

    return <- log(thisData[length(thisData)] / thisData[length(thisData) - 1])

    returns <- diff(log(thisData[-length(thisData)]))
    n <- length(returns)
    spotBPV <- 1/(K-2) * sum(abs(returns[1:(n-1)] * returns[2:n]))


    L[i] <- return/sqrt(spotBPV)
    # We have Cn and Sn as vectors as the size of these may change.
    Cn[i] <- sqrt(2 * log(n))/const - (log(pi) + log( log(n) ))/(2 * const*sqrt((2 * log(n))))
    Sn[i] <- 1/sqrt(const * 2 * log(n))

    K <- oldK
  }

  jumps <- (abs(L) - Cn)/Sn > betastar

  jumps <- xts(jumps, as.POSIXct(paste(dateOfData, testingTimes)))
  out <- merge.xts(testData, merge.xts(jumps, L))
  return(out)

}




#' @keywords internal
FoFJumpTest <- function(pData, theta, M, alpha){

  # Users will not directly control the testing times, instead they can choose the M parameter.
  dateOfData <- as.character(as.Date(index(pData[1])))
  betastar <- -log(-log(1-alpha))
  nObs <- length(pData)

  ## We need to ensure kn is even, thus we round half and multiply by 2
  kn <- round(theta * sqrt(nObs))
  kn <- kn + kn%%2

  const <- 0.7978846
  Cn <- sqrt(2 * log(kn))/const - (log(pi) + log( log(kn) ))/(2 * const*sqrt((2 * log(kn))))
  Sn <- 1/sqrt(const * 2 * log(kn))



  # Measuring jump variation during the entire day.
  preAveragedReturns <- hatreturn(pData, kn)
  preAveragedReturns <- c(as.numeric(preAveragedReturns), rep(NA, length(pData) - length(preAveragedReturns)))#, as.POSIXct(index(pData), origin = dateOfData)) # maybe we want to add back in xts, but it's removed for now...

  psi1kn <- kn * sum((gfunction((1:kn)/kn) - gfunction(( (1:kn) - 1 )/kn ) )^2)

  psi2kn <- 1 / kn * sum(gfunction((1:kn)/kn)^2)

  psi2kn <- (1 + (2*kn)^-2)/12
  returns <- diff(log(pData))
  omegaHat <- -1/(nObs-1) * sum(returns[2:nObs] * returns[1:(nObs-1)])

  biasCorrection <- omegaHat^2/theta^2 * psi1kn/psi2kn

  preAveragedRealizedVariance <- nObs/(nObs - kn + 2) * 1/(kn * psi2kn) * sum(preAveragedReturns^2, na.rm= TRUE) - biasCorrection
  preAveragedBipowerVariation <- nObs/(nObs - 2*kn + 2) * 1/(kn * psi2kn) * pi/2 * sum(abs(preAveragedReturns[1:(nObs-2*kn)]) * abs(preAveragedReturns[1:(nObs-2*kn) + kn])) - biasCorrection



  jumpVariation <- (preAveragedRealizedVariance - preAveragedBipowerVariation)/preAveragedRealizedVariance


  testingIndices <- seq(M - 2 + kn +1, nObs-kn, by = kn)# + 1  to account for edge-effect!
  PABPV <- PAreturns <- numeric(length(testingIndices))


  ind <- 1
  for (i in testingIndices) {

    PABPV[ind] <- pi/2 * 1/(M-2) * sum(abs(preAveragedReturns[(i - M + 2):(i-1)]) * abs(preAveragedReturns[(i-M+2-kn):(i-1-kn)]))

    ind <- ind + 1 # increment index
  }



  L <- preAveragedReturns[testingIndices]/sqrt(PABPV)


  jumps <- (abs(L) - Cn)/Sn > betastar

  jumps <- xts(jumps, index(pData)[testingIndices])
  out <-  merge.xts(jumps, L, jumpVariation)
  return(out)

}




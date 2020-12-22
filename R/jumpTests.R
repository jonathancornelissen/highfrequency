
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
#' This test examines the presence of jumps in highfrequency price series. It is based on the theory of Ait-Sahalia and Jacod (2009). 
#' It consists in comparing the multi-power variation of equi-spaced returns computed at a fast time scale (\eqn{h}), \eqn{r_{t,i}} (\eqn{i=1, \ldots,N}) and those computed at the slower time scale (\eqn{kh}), \eqn{y_{t,i}}(\eqn{i=1, \ldots ,\mbox{N/k}}).
#' 
#' They found that the limit (for \eqn{N} \eqn{\to} \eqn{\infty} ) of the realized power variation is invariant for different sampling scales and that their ratio is \eqn{1} in case of jumps and \eqn{\mbox{k}^{p/2}-1} if no jumps.
#' Therefore the AJ test detects the presence of jump using the ratio of realized power variation sampled from two scales. The null hypothesis is no jumps.
#' 
#' The function returns three outcomes: 1.z-test value 2.critical value under confidence level of \eqn{95\%} and 3. \eqn{p}-value.
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
#' @param pData either an \code{xts} or a \code{data.table} containing the prices of a single asset, possibly over multiple days.
#' @param p can be chosen among 2 or 3 or 4. The author suggests 4. 4 by default.
#' @param k can be chosen among 2 or 3 or 4. The author suggests 2. 2 by default.
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. 
#' Possible values are: \code{"secs"}, \code{"seconds"}, \code{"mins"}, \code{"minutes"}, \code{"hours"}.
#' To aggregate based on a 5 minute frequency, set \code{alignPeriod} to 5 and \code{alignBy} to "minutes".
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. For example, to aggregate
#' based on a 5 minute frequency, set \code{alignPeriod = 5} and \code{alignBy = "minutes"}.
#' @param alphaMultiplier alpha multiplier
#' @param alpha numeric of length one with the significance level to use for the jump test(s). Defaults to 0.975.
#' @param ... used internally
#' 
#' @return a list or \code{xts} in depending on whether input prices span more than one day.
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
#' Using the convergence properties of power variation and its dependence on the time scale on which it is measured, 
#' Ait-Sahalia and Jacod (2009) define a new variable which converges to 1 in the presence of jumps in the underlying return series, 
#' or to another deterministic and known number in the absence of jumps (Theodosiou and Zikes, 2009).
#'  
#' @references 
#' Ait-Sahalia, Y. and Jacod, J. (2009). Testing for jumps in a discretely observed process. The Annals of Statistics, 37(1), 184-222.
#' 
#' Theodosiou, M., & Zikes, F. (2009). A comprehensive comparison of alternative tests for jumps in asset prices. Unpublished manuscript, Graduate School of Business, Imperial College London.
#' 
#' @author Giang Nguyen, Jonathan Cornelissen, Kris Boudt, and Emil Sjoerup.
#'
#' @examples
#' jt <- AJjumpTest(sampleTData[, list(DT, PRICE)], p = 2, k = 3, 
#'                  alignBy = "seconds", alignPeriod = 5, makeReturns = TRUE)
#' 
#' @keywords highfrequency AJjumpTest
#' @importFrom stats qnorm pnorm
#' @export
AJjumpTest <- function(pData, p = 4 , k = 2, alignBy = NULL, alignPeriod = NULL, alphaMultiplier = 4, alpha = 0.975,...) {
  op <- list(...)
  flag <- TRUE
  if("flag" %in% names(op)){
    flag <- FALSE
  }
  
  if(is.xts(pData) && checkMultiDays(pData)) {
    
    result <- 
      apply.daily(pData, 
                  function(x){
                    tmp <- AJjumpTest(x, p = p, k = k, alignBy = alignBy, alignPeriod = alignPeriod, alphaMultiplier = alphaMultiplier, ...)
                    return(cbind(tmp[[1]], tmp[[2]][1], tmp[[2]][2], tmp[[3]]))
                    })
    colnames(result) <- c("ztest", "lower", "upper", "p-value")
    universalThreshold <- 2 * pnorm(-sqrt(log(ndays(result$ztest) * 2)))
    
    result$universalThresholdLower <- qnorm(universalThreshold)
    result$universalThresholdUpper <- -qnorm(universalThreshold)
    
    return(result)
  } else if (is.data.table(pData) && flag){
    DATE <- .N <- DT <- NULL
    
    setkey(pData, "DT")
    dates <- pData[, list(end = .N), by = list(DATE = as.Date(DT))][, `:=`(end = cumsum(end), DATE = as.character(DATE))][, start := shift(end, fill = 0) + 1]
    res <- vector(mode = "list", length = nrow(dates))
    names(res) <- as.character(dates$DATE)
    starts <- dates$start
    ends <- dates$end
    dates <- dates$DATE
    for (i in 1:length(dates)) {
      res[[dates[i]]] <- AJjumpTest(pData[starts[i]:ends[i], ], p = p, k = k, alignBy = alignBy, alignPeriod = alignPeriod,
                                    alphaMultiplier = alphaMultiplier, flag = FALSE)
    }
    
    return(res)
    
  } else {
    if(is.data.table(pData)){
      pData <- fastTickAgregation_DATA.TABLE(pData, alignBy = "seconds", alignPeriod = 1)
    } else {
      pData <- fastTickAgregation(pData, alignBy = "seconds", alignPeriod = 1)
    }
  }

  p <- as.numeric(p)
  k <- as.numeric(k)
  if(is.data.table(pData)){
    alpha <- alphaMultiplier * sqrt(rCov(pData, alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = TRUE)$RV)
    pData <- as.matrix(pData[, !"DT"])
    
  } else {
    alpha <- alphaMultiplier * sqrt(rCov(pData, alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = TRUE))
  }
  N <- length(pData)-1
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
  out$critical.value <- qnorm(c(1- alpha, alpha))
  out$pvalue <- 2 * pnorm(-abs(AJtest))
  return(out)
}

#' Barndorff-Nielsen and Shephard (2006) tests for the presence of jumps in the price series.
#' 
#' @description This test examines the presence of jumps in highfrequency price series. It is based on theory of Barndorff-Nielsen and Shephard (2006). 
#' The null hypothesis is that there are no jumps. 
#' 
#' @param rData either an \code{xts} or a \code{data.table} containing the log-returns or prices of a single asset, possibly over multiple days-
#' @param IVestimator can be chosen among jump robust integrated variance estimators: 
#' \code{\link{rBPCov}}, \code{\link{rMinRV}}, \code{\link{rMedRV}} and corrected threshold bipower variation (\code{\link{rThresholdCov}}). 
#' If \code{\link{rThresholdCov}} is chosen, an argument of \code{startV}, start point of auxiliary estimators in threshold estimation can be included. \code{\link{rBPCov}} by default.
#' @param IQestimator can be chosen among jump robust integrated quarticity estimators: \code{\link{rTPQuar}}, \code{\link{rQPVar}}, \code{\link{rMinRQ}} and \code{\link{rMedRQ}}. 
#' \code{\link{rTPQuar}} by default.
#' @param type a method of BNS testing: can be linear or ratio. Linear by default.
#' @param logTransform boolean, should be \code{TRUE} when \code{QVestimator} and \code{IVestimator} are in logarithm form. \code{FALSE} by default.
#' @param max boolean, should be \code{TRUE} when max adjustment in SE. \code{FALSE} by default.
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. 
#' Possible values are: \code{"secs"}, \code{"seconds"}, \code{"mins"}, \code{"minutes"}, \code{"hours"}.
#' To aggregate based on a 5 minute frequency, set \code{alignPeriod = 5} and \code{alignBy = "minutes"}.
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. For example, to aggregate
#' based on a 5 minute frequency, set \code{alignPeriod = 5} and \code{alignBy = "minutes"}.
#' @param makeReturns boolean, should be \code{TRUE} when \code{pData} contains prices. \code{FALSE} by default.
#' @param alpha numeric of length one with the significance level to use for the jump test(s). Defaults to 0.975.
#' 
#' @return a list or \code{xts} (depending on whether input prices span more than one day)
#' with the following values:
#' \itemize{
#' \item \eqn{z}-test value.
#' \item critical value (with confidence level of 95\%).
#' \item \eqn{p}-value of the test. 
#' }
#' 
#' @details 
#' 
#' Assume there is \eqn{N} equispaced returns in period \eqn{t}. 
#' Assume the Realized variance (RV), IVestimator and IQestimator are based on \eqn{N} equi-spaced returns. 
#' 
#' Let \eqn{r_{t,i}} be a return (with \eqn{i = 1, \ldots, N}) in period \eqn{t}. 
#' 
#' Then the BNSjumpTest is given by
#' \deqn{
#' \mbox{BNSjumpTest}= \frac{\code{RV} - \code{IVestimator}}{\sqrt{(\theta-2)\frac{1}{N} {\code{IQestimator}}}}.
#' }
#' The options for \code{IVestimator} and \code{IQestimator} are listed above. \eqn{\theta} depends on the chosen \code{IVestimator} (Huang and Tauchen, 2005).
#' 
#' The theoretical framework underlying the jump test is that the logarithmic price process \eqn{X_t} belongs to the class of Brownian semimartingales, which can be written as:
#' \deqn{
#' \mbox{X}_{t}=  \int_{0}^{t} a_u \ du + \int_{0}^{t}\sigma_{u} \ dW_{u} + Z_t
#' }
#' where \eqn{a} is the drift term, \eqn{\sigma} denotes the spot volatility process, \eqn{W} is a standard Brownian motion and \eqn{Z} is a jump process defined by:
#' \deqn{
#' \mbox{Z}_{t}=  \sum_{j=1}^{N_t}k_j
#' }
#' where \eqn{k_j} are nonzero random variables. The counting process can be either finite or infinite for finite or infinite activity jumps.
#' 
#' Since the realized volatility converges to the sum of integrated variance and jump variation, while the robust \code{IVestimator} converges to the integrated variance, 
#' it follows that the difference between \code{RV} and the \code{IVestimator} captures the jump part only, and this observation underlines the BNS test for jumps (Theodosiou and Zikes, 2009).
#' 
#' @references Barndorff-Nielsen, O. E., and Shephard, N. (2006). Econometrics of testing for jumps in financial economics using bipower variation. \emph{Journal of Financial Econometrics}, 4, 1-30. 
#' 
#' Corsi, F., Pirino, D., and Reno, R. (2010). Threshold bipower variation and the impact of jumps on volatility forecasting. \emph{Journal of Econometrics}, 159, 276-288.
#' 
#' Huang, X., and Tauchen, G. (2005). The relative contribution of jumps to total price variance. \emph{Journal of Financial Econometrics}, 3, 456-499.
#' 
#' Theodosiou, M., and Zikes, F. (2009). A comprehensive comparison of alternative tests for jumps in asset prices. Unpublished manuscript, Graduate School of Business, Imperial College London.
#' 
#' @author Giang Nguyen, Jonathan Cornelissen, Kris Boudt, and Emil Sjoerup.
#' 
#' @examples 
#' bns <- BNSjumpTest(sampleTData[, list(DT, PRICE)], IVestimator= "rMinRV",
#'                    IQestimator = "rMedRQ", type= "linear", makeReturns = TRUE)
#' bns
#' 
#' @keywords highfrequency BNSjumpTest
#' @export
BNSjumpTest <- function (rData, IVestimator = "BV", IQestimator = "TP", type = "linear",
                         logTransform = FALSE, max = FALSE, alignBy = NULL, alignPeriod = NULL,
                         makeReturns = FALSE, alpha = 0.975) {
  
  if (is.xts(rData) && checkMultiDays(rData)) {
    
    result <- 
      apply.daily(rData, 
                  function(x) {
                    tmp <- BNSjumpTest(rData = x, IVestimator = IVestimator, IQestimator = IQestimator, type = type, logTransform = logTransform, 
                                       max = max, alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = makeReturns, alpha = alpha)
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
  } else if (is.data.table(rData)){
    DATE <- .N <- DT <- NULL
    if(!is.null(alignBy) && !is.null(alignPeriod) && makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    
    if(!is.null(alignBy) && !is.null(alignPeriod) && !makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    
    setkey(rData, "DT")
    dates <- rData[, list(end = .N), by = list(DATE = as.Date(DT))][, `:=`(end = cumsum(end), DATE = as.character(DATE))][, start := shift(end, fill = 0) + 1]
    res <- vector(mode = "list", length = nrow(dates))
    names(res) <- as.character(dates$DATE)
    starts <- dates$start
    ends <- dates$end
    dates <- dates$DATE
    dat <- as.matrix(rData[, !"DT"])
    for (i in 1:length(dates)) {
      res[[dates[i]]] <- BNSjumpTest(dat[starts[i]:ends[i], ], IVestimator = IVestimator, IQestimator = IQestimator, type = type, logTransform = logTransform, 
                                    max = max, alignBy = NULL, alignPeriod = NULL, makeReturns = makeReturns, alpha = alpha)
    }
    
    return(res)
    
  } else {
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && makeReturns) {
      rData <- fastTickAgregation(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && !makeReturns) {
      rData <- fastTickAgregation_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
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
#' This test examines the jump in highfrequency data. It is based on theory of Jiang and Oomen (JO). They found that the difference of simple return and logarithmic return can capture one half of integrated variance if there is no jump in the underlying sample path. The null hypothesis is no jumps.
#'   
#' Function returns three outcomes: 1.z-test value 2.critical value under confidence level of \eqn{95\%} and 3.p-value.  
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
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. Possible values are: "secs", "seconds", "mins", "minutes","hours".
#' To aggregate based on a 5 minute frequency, set \code{alignPeriod} to 5 and \code{alignBy} to "minutes".
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. E.g. to aggregate
#' based on a 5 minute frequency, set \code{alignPeriod} to 5 and \code{alignBy} to "minutes".
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
#'  The Jiang and Oomen test is that: in the absence of jumps, the accumulated difference between the simple return and the log return captures one half of the integrated variance (Theodosiou and Zikes, 2009).
#'  
#' @references 
#' Andersen, T. G., Dobrev, D., and Schaumburg, E. (2012). Jump-robust volatility estimation using nearest neighbor truncation. \emph{Journal of Econometrics}, 169, 75- 93.
#' 
#' Jiang, J. G., and Oomen, R. C. A (2008). Testing for jumps when asset prices are observed with noise- a "swap variance" approach. \emph{Journal of Econometrics}, 144, 352-370.
#' 
#' Theodosiou, M., Zikes, F. (2009). A comprehensive comparison of alternative tests for jumps in asset prices. Unpublished manuscript, Graduate School of Business, Imperial College London.
#' 
#' @author Giang Nguyen, Jonathan Cornelissen, Kris Boudt, and Emil Sjoerup
#' 
#' @examples
#' joDT <- JOjumpTest(sampleTData[, list(DT, PRICE)])
#' @keywords highfrequency JOjumpTest
#' @importFrom stats qnorm
#' @importFrom stats pnorm
#' @importFrom zoo as.zoo
#' @export
JOjumpTest <- function(pData, power = 4, alignBy = NULL, alignPeriod = NULL, alpha = 0.975, ...) {
  
  if (is.xts(pData) && checkMultiDays(pData)) {
    
    result <- 
      apply.daily(pData,
                  function(x){
                    tmp <- JOjumpTest(x, power = power, alignBy = alignBy, alignPeriod = alignPeriod, alpha = alpha, ...)
                    return(cbind(tmp[[1]], tmp[[2]][1], tmp[[2]][2], tmp[[3]]))})
    
    colnames(result) <- c("ztest", "lower", "upper", "p-value")
    
    universalThreshold <- 2 * pnorm(-sqrt(log(ndays(result$ztest) * 2)))
    result$universalThresholdLower <- qnorm(universalThreshold)
    result$universalThresholdUpper <- -qnorm(universalThreshold)
    
    return(result)
  } else if (is.data.table(pData)){
    DATE <- .N <- DT <- NULL
    if((!is.null(alignBy)) && (!is.null(alignPeriod))) {
      pData <- fastTickAgregation_DATA.TABLE(pData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    setkey(pData, "DT")
    dates <- pData[, list(end = .N), by = list(DATE = as.Date(DT))][, `:=`(end = cumsum(end), DATE = as.character(DATE))][, start := shift(end, fill = 0) + 1]
    res <- vector(mode = "list", length = nrow(dates))
    names(res) <- as.character(dates$DATE)
    starts <- dates$start
    ends <- dates$end
    dates <- dates$DATE
    dat <- as.matrix(pData[, !"DT"])
    for (i in 1:length(dates)) {
      res[[dates[i]]] <- JOjumpTest(dat[starts[i]:ends[i], ], power = power, alignBy = alignBy, alignPeriod = alignPeriod, alpha = alpha, ...)
    }
    
    return(res)
    
  } else {
    if ((!is.null(alignBy)) && (!is.null(alignPeriod))) {
      pData <- fastTickAgregation(pData, alignBy = alignBy, alignPeriod = alignPeriod)
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
      q      <- abs(rollApplyProdWrapper(r, 4))
      mu2    <- 2^((6/4)/2) * gamma(1/2 * (6/4+1)) / gamma(1/2)
      av     <- mu1/9 * N^3 * (mu2)^(-4) / (N-4-1) * sum(q^(6/4), na.rm = TRUE)   ##check formula
      JOtest <- N * bv / sqrt(av) * (1- rv/SwV)
  
      out                <- {}
      out$ztest          <- JOtest
      out$critical.value <- qnorm(c(1-alpha, alpha))
      out$pvalue         <- 2 * pnorm(-abs(JOtest))
      return(out)
    }
  
    if (power == 6) {
      
      
      q <- abs(rollApplyProdWrapper(r, 6))
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
}


#' Intraday jump tests
#' @description 
#' This function can be used to  test for jumps in intraday price paths.
#' 
#' The tests are of the form \eqn{L(t) = (R(t) - mu(t))/sigma(t)}.
#' 
#' See \code{\link{spotVol}} and \code{\link{spotDrift}} for Estimators for \eqn{\sigma(t)} and \eqn{\mu(t)}, respectively.
#' 
#' @param pData \code{xts}or data.table of the price data in levels. This data can (and should in some cases) be tick-level data. The data can span more than one day.
#' @param volEstimator character denoting which volatility estimator to use for the tests. See \code{\link{spotVol}}. Default = \code{"RM"} denoting realized measures.
#' @param driftEstimator character denoting which drift estimator to use for the tests. See \code{\link{spotDrift}}. Default = \code{"none"} denoting no drift estimation.
#' @param alpha numeric of length one determining what confidence level to use when constructing the critical values.
#' @param alignBy string indicating the time scale in which \code{alignPeriod} is expressed.
#' Possible values are: \code{"secs", "seconds", "mins", "minutes", "hours"}.
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. E.g. to aggregate
#' based on a 5 minute frequency, set \code{alignPeriod} to 5 and \code{alignBy} to "minutes".
#' \code{alignPeriod = 5} and \code{alignBy = "minutes"}.
#' @param marketOpen the market opening time. This should be in the time zone
#' specified by \code{tz}. By default, \code{marketOpen = "09:30:00"}.
#' @param marketClose the market closing time. This should be in the time zone
#' specified by \code{tz}. By default, \code{marketClose = "16:00:00"}.
#' @param tz fallback time zone used in case we we are unable to identify the timezone of the data, by default: \code{tz = NULL}. We attempt to extract the timezone from the DT column (or index) of the data, which may fail. 
#' In case of failure we use \code{tz} if specified, and if it is not specified, we use \code{"UTC"}
#' @param ... extra arguments passed on to \code{\link{spotVol}} for the volatility estimation, and to \code{\link{spotDrift}}.
#' 
#' The null hypothesis of the tests in this function is that there are no jumps in the price series
#' 
#' @references 
#' 
#' Christensen, K., Oomen, R. C. A., Podolskij, M. (2014): Fact or Friction: Jumps at ultra high frequency. \emph{Journal of Financial Economics}, 144, 576-599
#' 
#' 
#' 
#' @examples 
#' \dontrun{
#' # We can easily make a Lee-Mykland jump test.
#' LMtest <- intradayJumpTest(pData = sampleTData[, list(DT, PRICE)], 
#'                            volEstimator = "RM", driftEstimator = "none",
#'                            RM = "bipower", lookBackPeriod = 20,
#'                            alignBy = "minutes", alignPeriod = 5, marketOpen = "09:30:00", 
#'                            marketClose = "16:00:00")
#' plot(LMtest)
#' 
#' # We can just as easily use the pre-averaged version from the "Fact or Friction" paper
#' FoFtest <- intradayJumpTest(pData = sampleTData[, list(DT, PRICE)], 
#'                             volEstimator = "PARM", driftEstimator = "none",
#'                             RM = "bipower", lookBackPeriod = 20, theta = 1.2,
#'                             marketOpen = "09:30:00", marketClose = "16:00:00")
#' plot(FoFtest)
#' 
#' }
#' @author Emil Sjoerup
#' @importFrom zoo index
#' @export

intradayJumpTest <- function(pData, volEstimator = "RM", driftEstimator = "none", alpha = 0.95, alignBy = "minutes", alignPeriod = 5,
                             marketOpen = "09:30:00", marketClose = "16:00:00", tz = "GMT", ...){

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
  
  vol <- spotVol(pData, method = volEstimator, alignBy = alignBy, alignPeriod = alignPeriod, marketOpen = marketOpen, marketClose = marketClose, tz = tz, ...)

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
    prices <- aggregatePrice(pData, alignBy = alignBy, alignPeriod = alignPeriod , marketOpen = marketOpen,
                   marketClose = marketClose, tz = tz, fill = TRUE)
    setkeyv(prices, "DT")
    prices[, DATE := as.Date(DT, tz = tz)]
    returns <- prices[, RETURN := log(PRICE) - shift(log(PRICE), type = "lag"), by = "DATE"][!is.na(RETURN)]
    
  } else { # volEstimator == "PARM" i.e. we have pre-averaged realized measures
    nObs <- length(pData$PRICE)
    
    op <- list(RM = "bipower", lookBackPeriod = 50)
    options <- list(...)
    op[names(options)] <- options
    
    if (isMultiDay) {
      dates <- NULL 
      if(is.data.table(pData)){
        dates <- unique(as.Date(pData$DT, tz = tz))
      } else {
        dates <- unique(as.Date(index(pData), tz = tz))
      }
      
      preAveragedReturns <- testingIndices <- c()
      
      for (d in 1:D) {
        tmp <- seq(op$lookBackPeriod - 2 + vol$kn[d] +1, vol$nObs[d]-vol$kn[d], by = vol$kn[d]) + sum(vol$nObs[0:(d-1)])
        
        if(op$RM == "medrv"){ # We can expand this if need be. 
          tmp <- tmp[-length(tmp)]
        } 
        testingIndices <- c(testingIndices, tmp)
        preAveragedReturns <- c(preAveragedReturns, c(as.numeric(hatreturn(as.xts(pData[as.Date(pData$DT, tz = tz) == dates[d]])$PRICE, vol$kn[d])), rep(NA, vol$kn[d] - 2)))
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
      drift <- as.numeric(unlist(spotDrift(pData, method = driftEstimator, alignBy = alignBy, alignPeriod = alignPeriod, marketOpen = marketOpen, marketClose = marketClose, tz = tz, ...)))
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
    pData <- as.xts(pData[ , list(DT, PRICE)], tz = tz)
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
  #           temp <- rbind(temp, aggregateTS(pData[date], alignBy = "minutes", k = windowSize))
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
      
      if(interactive() && readline("Press Enter for next figure or 0 to exit:\n") == "0"){
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



#' Rank jump test
#' 
#' @description 
#' 
#' Calculate the 
#' 
#' @param marketPrice data.table or \code{xts}containing the market prices in levels
#' @param stockPrices list containing the individual stock prices in either data.table or \code{xts}format. The format should be the the same as \code{marketPrice}
#' @param alpha significance level (in standard deviations) to use for the jump detections. Default is \code{c(5,3)} for 5 and 3 in the market and stocks respectively.
#' @param localWindow numeric denoting the local window for the bootstrap algorithm. Default is \code{30}
#' @param coarseFreq numeric denoting the coarse sampling frequency. Default is \code{10}
#' @param rank rank of the jump matrix under the null hypothesis. Default is \code{1}
#' @param BoxCox numeric of exponents for the Box-Cox transformation, default is \code{1}
#' @param quantiles numeric denoting which quantiles of the bootstrapped critical values to return and compare against. Default is \code{c(0.9, 0.95, 0.99)}
#' @param nBoot numeric denoting how many replications to be used for the bootstrap algorithm. Default is \code{1000}
#' @param dontTestAtBoundaries logical determining whether to exclude data across different days. Default is \code{TRUE}
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. Possible values are: "secs", "seconds", "mins", "minutes","hours", and "ticks".
#' To aggregate based on a 5 minute frequency, set \code{alignPeriod} to 5 and \code{alignBy} to \code{"minutes"}.
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. E.g. to aggregate
#' based on a 5 minute frequency, set \code{alignPeriod} to 5 and \code{alignBy} to \code{"minutes"}.
#' @param marketOpen the market opening time, by default: \code{marketOpen = "09:30:00"}.
#' @param marketClose the market closing time, by default: \code{marketClose = "16:00:00"}.
#' @param tz fallback time zone used in case we we are unable to identify the timezone of the data, by default: \code{tz = NULL}. 
#' We attempt to extract the timezone from the DT column (or index) of the data, which may fail. 
#' In case of failure we use \code{tz} if specified, and if it is not specified, we use \code{"UTC"}
#' 
#' @return A list containing \code{criticalValues} which are the bootstrapped critical values, \code{testStatistic} the test statistic of the jump test, \code{dimensions} which are the dimensions of the jump matrix
#'  \code{marketJumpDetections} the jumps detected in the market prices, \code{stockJumpDetections} the co-jumps detected in the individual stock prices, and \code{jumpIndices} which are the indices of the detected jumps.
#' 
#' @references Li, j., Todorov, V., Tauchen, G., and Lin, H. (2019). Rank Tests at Jump Events. \emph{Journal of Business & Economic Statistics}, 37, 312-321.
#' 
#' @author Emil Sjoerup, based on Matlab code provided by Li et al. (2019)
#' @importFrom stats na.omit quantile runif
#' @importFrom zoo coredata
#' @export
rankJumpTest <- function(marketPrice, stockPrices, alpha = c(5,3), coarseFreq = 10, localWindow = 30, rank = 1, BoxCox = 1, quantiles = c(0.9, 0.95, 0.99), 
                         nBoot = 1000, dontTestAtBoundaries = TRUE, alignBy = "minutes", alignPeriod = 5,
                         marketOpen = "09:30:00", marketClose = "16:00:00", tz = "GMT"){
  
  ## Preparation of data
  PRICE = DATE = RETURN = DT = NULL
  if(!is.list(stockPrices)){
    stop("stockPrices must be a list")
  }
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
  
  if (!is.data.table(marketPrice)) {
    if (is.xts(marketPrice)) {
      marketPrice <- setnames(as.data.table(marketPrice), old = "index", new = "DT")
      marketPrice[, PRICE := as.numeric(PRICE)]
      
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (!("DT" %in% colnames(marketPrice))) {
      stop("Data.table needs DT column containing the time-stamps of the trades.") # added the timestamp comment for verbosity.
    }
    
  }

  marketPrice <- aggregatePrice(marketPrice, alignBy = alignBy, alignPeriod = alignPeriod , marketOpen = marketOpen,
                          marketClose = marketClose, tz = tz, fill = TRUE)
  marketPrice[, DATE := as.Date(DT, tz = tzone(marketPrice$DT))]
  setkeyv(marketPrice, "DT")
  marketPrice <- marketPrice[, RETURN := log(PRICE) - shift(log(PRICE), type = "lag"), by = "DATE"][!is.na(RETURN)]
  marketReturns <- xts(marketPrice$RETURN, order.by = marketPrice$DT)
  
  stockReturns <- NULL
  for (stock in 1:length(stockPrices)) {
    tmp <- setnames(as.data.table(stockPrices[[stock]]), old = "index", new = "DT", skip_absent = TRUE)
    colnames(tmp) <- c("DT", "PRICE")
    tmp <- aggregatePrice(tmp, alignBy = alignBy, alignPeriod = alignPeriod , marketOpen = marketOpen,
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
  
  if(dim(jumps) == c(1,1)){
    stop("Singular value decomposition cannot be calculated")
  }
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
  
  criticalValues <- quantile(siumulatedTestStatistics, quantiles)
  
  out <- list("criticalValues" = criticalValues, "testStatistic" = testStatistic, "dimensions" = dim(jumps),
              "marketJumpDetections" = marketJumpDetection, "stockJumpDetections" = stockJumpDetections, "jumpIndices" = jumpIndices)
  
  return(out)
}



#' @keywords internal
ABDJumptest <- function(RV, BPV, TQ) { # Comput jump detection stat mentioned in roughing paper
  mu1  <- sqrt(2 / pi)
  n <- length(RV)
  zstat <- ((1/n)^(-1/2))*((RV-BPV)/RV)*(  (mu1^(-4) + 2*(mu1^(-2))-5) * pmax( 1,TQ*(BPV^(-2))))^(-1/2)
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
#' data(sample_tdata)
#' AJjumptest(sample_tdata$PRICE, p = 2, k = 3, align.by = "seconds", 
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
    pdata <- aggregatets(pdata, on = "seconds", k = 1)
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
  
  S <- pv1/pv2
  
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
#' @param pdata  a zoo/xts object containing all prices in period t for one asset.
#' @param power  can be chosen among 4 or 6. 4 by default.
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
#' data(sample_5minprices_jumps)
#' JOjumptest(sample_5minprices_jumps[,1], power = 6)
#' 
#' @keywords highfrequency JOjumptest
#' @importFrom stats qnorm
#' @importFrom stats pnorm
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
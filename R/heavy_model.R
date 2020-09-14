
#' HEAVY Model estimation
#' @description This function calculates the High frEquency bAsed VolatilitY (HEAVY) model proposed in Shephard and Sheppard (2010). This function is used as a predictive volatility model built to exploit highfrequency data.
#'
#' @param data a (T x K) matrix containing the data, with T the number of days. For the traditional HEAVY model: K = 2, the first column contains the squared daily demeaned returns, the second column contains the realized measures.
#' @param p a (K x K) matrix containing the lag length for the model innovations. Position (i, j) in the matrix indicates the number of lags in equation i of the model for the innovations in data column j. For the traditional heavy model p is given by matrix(c(0,0,1,1), ncol = 2) (default).
#' @param q a (K x K) matrix containing the lag length for the conditional variances. Position (i, j) in the matrix indicates the number of lags in equation i of the model for conditional variances corresponding to series j. For the traditional heavy model introduced above q is given by matrix( c(1,0,0,1),ncol=2 ) (default).
#' @param startingValues a vector containing the starting values to be used in the optimization to find the optimal parameters estimates.
#' @param LB a vector of length K indicating the lower bounds to be used in the estimation. If NULL it is set to a vector of zeros by default.
#' @param UB a vector of length K indicating the upper bounds to be used in the estimation. If NULL it is set to a vector of Inf by default.
#' @param backCast a vector of length K used to initialize the estimation. If NULL the unconditional estimates are taken.
#' @param compConst a boolean variable. In case TRUE, the omega values are estimated in the optimization. In case FALSE, volatility targeting is done and omega is just 1 minus the sum of all relevant alpha's and beta's multiplied by the unconditional variance.
#'
#' @details Assume there are \eqn{T} daily returns and realized measures in the period \eqn{t}. Let \eqn{r_i} and \eqn{RM_i} be the \eqn{i^{th}} daily return and daily realized measure respectively (with \eqn{i=1, \ldots,T}).
#'
#' The most basic heavy model is the one with lag matrices p of \eqn{\left( \begin{array}{ccc} 0 & 1 \\ 0 & 1 \end{array} \right)} and q of \eqn{\left( \begin{array}{ccc} 1 & 0 \\ 0 & 1 \end{array} \right)}. This can be represented by the following equations:
#' \deqn{
#'      \mbox{var}{\left(r_t \right)} = h_t = w + \alpha RM_{t-1} + \beta h_{t-1}; w,\alpha \geq 0, \beta \in [0,1]
#' }
#' \deqn{
#'  \mbox{E}{\left(RM_t \right)} = \mu_t = w_R + \alpha_R RM_{t-1} + \beta_R \mu_{t-1}; w_R,\alpha_R, \beta_R \geq 0, \alpha_R+\beta_R \in [0,1]
#' }
#'
#' Equivalently, they can be presented in terms of matrix notation as below:
#' \deqn{
#' \left( \begin{array}{ccc} h_t \\ \mu_t \end{array} \right) = \left( \begin{array}{ccc} w \\ w_R \end{array} \right)  + \left( \begin{array}{ccc} 0 & \alpha \\ 0 & \alpha_R \end{array} \right) \left( \begin{array}{ccc} r^2_{t-1} \\ RM_{t-1} \end{array} \right) + \left( \begin{array}{ccc} \beta & 0 \\ 0 & \beta_R \end{array} \right) \left( \begin{array}{ccc} h_{t-1} \\ \mu_{t-1} \end{array} \right)
#' }
#'
#' In this version, the parameters vector to be estimated is \eqn{\left( w, w_R,\alpha, \alpha_R, \beta, \beta_R \right) }.
#'
#' In terms of startingValues, Shephard and Sheppard recommend for this version of the Heavy model to set  \eqn{\beta} be around 0.6 and sum of \eqn{\alpha}+\eqn{\beta} to be close to but slightly less than one.
#' In general, the lag length for the model innovation and the conditional covariance can be greater than 1. Consider, for example, matrix p is  \eqn{\left( \begin{array}{ccc} 0 & 2 \\ 0 & 1 \end{array} \right)} and matrix q is the same as above. Matrix notation will be as below:
#' \deqn{
#' \left( \begin{array}{ccc} h_t \\ \mu_t \end{array} \right) = \left( \begin{array}{ccc} w \\ w_R \end{array} \right)  + \left( \begin{array}{ccc} 0 & \alpha_1 \\ 0 & \alpha_R \end{array} \right) \left( \begin{array}{ccc} r^2_{t-1} \\ RM_{t-1} \end{array} \right) +\left( \begin{array}{ccc} 0 & \alpha_2 \\ 0 & 0 \end{array} \right) \left( \begin{array}{ccc} r^2_{t-2} \\ RM_{t-2} \end{array} \right) + \left( \begin{array}{ccc} \beta & 0 \\ 0 & \beta_R \end{array} \right) \left( \begin{array}{ccc} h_{t-1} \\ \mu_{t-1} \end{array} \right)}
#'
#' In this version, the parameters vector to be estimated is \eqn{\left( w, w_R,\alpha_1, \alpha_R, \alpha_2, \beta, \beta_R \right) }.
#'
#' @return A list with the following values:
#' (i) loglikelihood: the log likelihood evaluated at the parameter estimates.
#' (ii) likelihoods: an xts object of length T containing the log likelihoods per day.
#' (iii) condvar: a (T x K) xts object containing the conditional variances
#' (iv) estparams: a vector with the parameter estimates. The order in which the
#' parameters are reported is as follows: First the estimates for omega then the
#' estimates for the non-zero alpha's with the most recent lags first in case max(p) > 1,
#' then the estimates for the non-zero beta's with the most recent lag first in case
#' max(q) > 1.
#' (v) convergence: an integer code indicating the successfulness of the optimization. See \verb{optim} for more information.
#'
#' @references Shephard, N. and K. Sheppard (2010). Realising the future: forecasting with high frequency based volatility (heavy) models. Journal of Applied Econometrics 25, 197-231.
#'
#' @examples
#' # Implementation of the heavy model on DJI:
#' returns <-  realizedLibrary$open_to_close
#' bv      <-  realizedLibrary$bv
#' returns <- returns[!is.na(bv)]
#' bv <- bv[!is.na(bv)] # Remove NA's
#' data <- cbind( returns^2, bv) # Make data matrix with returns and realized measures
#' backCast <- matrix(c(var(returns), mean(bv)), ncol = 1)
#'
#' #For traditional (default) version:
#' startvalues <- c(0.004,0.02,0.44,0.41,0.74,0.56) # Initial values
#' output <- HEAVYmodel(data = as.matrix(data,ncol=2), compConst=FALSE,
#'                      startingValues = startvalues, backCast=backCast)
#' #For general version:
#' startvalues <- c(0.004, 0.02, 0.44, 0.4, 0.41, 0.74, 0.56) # Initial values;
#' p <- matrix(c(2, 0, 0, 1), ncol = 2)
#' q <- matrix(c(1, 0, 0, 1), ncol = 2)
#'
#' heavy_model <- HEAVYmodel(data = as.matrix(data, ncol = 2), p = p, q = q, compConst = FALSE,
#'                       startingValues = startvalues, backCast = backCast)
#'
#' @author Giang Nguyen, Jonathan Cornelissen, Kris Boudt and Onno Kleen.
#' @importFrom stats optim
#' @export
HEAVYmodel <- function(data, p = matrix(c(0, 0, 1, 1), ncol = 2), q = matrix(c(1, 0, 0, 1), ncol = 2), 
                       startingValues = NULL, LB = NULL, UB = NULL, backCast = NULL, compConst = FALSE) {
    K  <- ncol(data)
    TT <- nrow(data)
    means <- c(colMeans(data))
    maxp  <- max(p)
    maxq <- max(q)

    if (is.null(LB)) {
      LB <- rep(0, K)
    }

    if (is.null(UB)) {
      UB <- rep(10^6, K)
    }

    if (is.null(startingValues)) {
      startingValues <- rep(NA, K + sum(p) + sum(q))
      startingValues[1:K] <- 0.1
      start <- K + 1
      end <- K + sum(p)
      startingValues[start:end] <- 0.3
      start <- end + 1
      end <- start + sum(q) - 1
      startingValues[start:end] <- 0.6
    }

    if (is.null(backCast)) {
      backCast <- t(t(colMeans(data)))
    }

    KKK <- length(startingValues)
    ui  <- diag(rep(1, KKK))
    ci  <- rep(0, dim(ui)[2])

    splittedparams <- transToSplit(startingValues, p, q)[[1]]
    
    x <- try(optim(par = splittedparams, fn = heavyLikelihoodllC, data = data, p = p, q = q,
                  backCast = backCast, LB = LB, UB = UB, compConst = compConst,
                  return.only.llh = TRUE,
                  method = "L-BFGS-B"))

    if (class(x) == "try-error") {
      print("Error in likelihood optimization")
      print(x)
    }
    else {
      if (x$convergence != 0) {
        print("Possible problem in likelihood optimization. Check convergence")
      }
    }
    output <- list()
    output$loglikelihood <- x$value
    
    xx <- heavyLikelihoodllC(splittedparams = x$par, 
                             data = data, p = p, q = q,
                             backCast = backCast, LB = LB, UB = UB, compConst = compConst,
                             return.only.llh = FALSE)

    if (!is.null(rownames(data))) {
      output$condvar <- xts(t(matrix(xx$h, K)), order.by = as.POSIXct(rownames(data)))
      output$likelihoods <- xts(t(matrix(xx$lls, 1)), order.by = as.POSIXct(rownames(data)))
    }
    
    output$estparams <- matrix(transToPar(x$par, p, q), ncol = 1)
    rownames(output$estparams) <- getParamNames(x$par, p, q)
    output$convergence <- x$convergence
    return(output)
}

#' @keywords internal
getParamNames <- function(estparams, p, q) {
  K <- dim(p)[2]
  nAlpha <- sum(p)
  nBeta  <- sum(q)
  omegas <- paste("omega", 1:K, sep = "")
  alphas <- paste("alpha", 1:nAlpha, sep = "")
  betas  <- paste("beta",  1:nBeta, sep = "")
  names  <- c(omegas, alphas, betas)
}

#' @keywords internal
heavyLikelihoodllC <- function(splittedparams, data, p, q, backCast, LB, UB, compConst = FALSE, return.only.llh = FALSE) {
  K  <- ncol(data)
  TT <- nrow(data)
  means <- c(colMeans(data))
  maxp  <- max(p)
  maxq  <- max(q)
  
  par <- transToPar(splittedparams, p, q)

  out <- heavy_likelihoodR(
    parameters = as.double(par),
    data = as.double(t(data)),
    T1 = as.integer(TT),
    K = as.integer(K),
    means = as.double(means),
    p = as.integer(p),
    q = as.integer(q),
    pMax = as.integer(maxp),
    qMax = as.integer(maxq),
           backCast = as.double(t(backCast)),
           LB = as.double(LB),
           UB = as.double(UB),
           compConst = as.integer(compConst),
           h = as.double(matrix(rep(0, K * TT), nrow = K, ncol = TT)),
           lls = as.double(rep(0, TT)),
           llRM = as.double(rep(0, K)))
  
  if (return.only.llh) {
    return(out$ll)
  } else {
    return(out)
  }
}

#' @keywords internal
heavy_likelihoodR <- function(parameters, data, T1, K, means, p, q, pMax, qMax,
                              backCast, LB, UB, compConst, h, lls, llRM) {
  ll = 0
  # int i,j,t,l,k;
  sum = 0.0
  sum1 = 0
  sum2 <- 0
  lll = 0.0
  # htemp
  
  # int K, T, pMax, qMax, compConst;
  TT = T1
  
  O <- rep(0, times = K)
  A <- rep(0, times = K*K*pMax)
  B <- rep(0, times = (K*K*qMax))
  temp <- rep(NA, times = K)
  
  for (i in c(1:K)) {
    O[i] <- 0.0
    for (k in c(1:K)) {
      for (j in c(1:pMax)) {
        A[K * K * j + K * i + k] <- 0.0
      }
      for (j in c(1:qMax)) {
        B[K * K * j + K * i + k] <- 0.0;
      }
    }
    llRM[i] <- 0.0;
  }
  # list(ll = sum(parameters^2))
  # browser()
  if(compConst == 1) {
    transform <- heavy_parameter_transformR_(parameters, K, p, q, O, A, B,pMax,qMax)
    A <- transform$A
    B <- transform$B
  } else {
    transform <- heavy_parameter_transform_RetrackR_(parameters, K, p, q, means, O, A, B, pMax,qMax)
    O <- transform$O
    A <- transform$A
    B <- transform$B
  }
  
  for (i in c(1:K)) {
    if (O[i] < 0) {
      O[i] = .000001;
    }
  }
  
  blub <- heavy_likelihoodR_(h = h, O = O, A = A, B = B, TT = TT, K = K, pMax = pMax, qMax = qMax, data = data, backCast = backCast, 
                             LB = LB, UB = UB, llRM = llRM, lls = lls)
  
  
  return(blub)
}

#' @keywords internal
transToSplit <- function(paramsvector, p, q) {
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
  K <- nrow(p)[1]

  # intercept paramaters
  vo     <- paramsvector[1:K]
  # data paramaters
  pmax <- max(p)
  qmax <- max(q) # Max number of lags for innovations and cond vars
  mA <- c()
  mB <- c()
  start <- (K+1)

  for (i in 1:pmax) {    # A will contain a list-item per innovation lag
    end      <- start + sum(p>=i) - 1; # How many non-zero params in this loop?
    Ai       <- matrix(rep(NA,K^2),ncol=K)
    Ai[p>=i] <- paramsvector[start:end]
    mA       <- cbind(mA,Ai)
    start    <- end + 1
  }#end loop over number of lags for innovations


  # autoregressive term parameters
  for(i in 1:qmax){   # B will contain a list-item per cond var lag
    end      <- start + sum(q>=i) -1; # How many non-zero params in this loop?
    Bi       <- matrix(rep(NA,K^2),ncol = K)
    Bi[q>=i] <- paramsvector[start:end]
    mB       <- cbind(mB,Bi)
    start    <- end + 1
  }#End loop over number of lags for cond variances

  all <- cbind(vo , mA , mB)
  tall <- t(all)
  theta <- tall[!is.na(tall)]

  vk <- rep(0, K)
  for (i in 1: K) {
    vk[i] <- 1 + sum(p[i,]) + sum(q[i,])
  }
  list(theta,vk)
}

#' @keywords internal
transToPar <- function(theta, p, q){
  K <- nrow(p)
  maxp <- max(p)
  maxq <- max(q)

  # Determine vk:
  vk <- rep(0, K);

  for (i in 1: K) {
    vk[i] <- 1 + sum(p[i, ]) + sum(q[i, ])
  }

  # matrix O
  vo <- matrix(rep(1,K), ncol = 1)
  # matrix A, B
  mA <- c()
  mB <- c()

  for (i in 1:maxp) {    # A will contain a list-item per innovation lag
    Ai       <- matrix(rep(0,K^2),ncol=K)
    Ai[p>=i] <- 1
    mA       <- cbind(mA,Ai)
  }#end loop over number of lags for innovations


  # autoregressive term parameters
  for (i in 1:maxq) {   # B will contain a list-item per cond var lag
    Bi       <- matrix(rep(0, K^2), ncol = K)
    Bi[q>=i] <- 1
    mB       <- cbind(mB,Bi)
  }#End loop over number of lags for cond variances

  # Merge vO,mA, mB
  all <- matrix(cbind(vo , mA , mB), nrow = K)

  nma <- ncol(all)
  start <- 1

  for (i in 1:K) {
    for (j in 1:nma) {
      if(all[i,j] == 1) {
        all[i,j] <- theta[start]
      } else {
        all[i,j] <- NA
        next
      }
      start <- start + 1
    }
  }
  params <- all[!is.na(all)]
  return(params)
}


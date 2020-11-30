

#' HEAVY Model estimation
#' @description This function calculates the High frEquency bAsed VolatilitY (HEAVY) model proposed in Shephard and Sheppard (2010). 
#'
#' @param ret a vector of demeaned returns 
#' @param rm a vector of realized stock market variation
#' @param startingValues a vector of alternative starting values: first three arguments for variance equation and last three arguments for measurement equation.
#' 
#' @return The function outputs an object of class \code{HEAVYmodel}, a list containing
#' \itemize{
#'   \item coefficients = estimated coefficients
#'   \item se = robust standard errors based on inverted Hessian matrix
#'   \item residuals = the residuals in the return equation
#'   \item llh = the two-component log-likelihood values
#'   \item varCondVariances = conditional variances in the variance equation
#'   \item RMCondVariances = conditional variances in the RM equation
#'   \item data = the input data
#' }
#' 
#' @references Shephard, N. and K. Sheppard (2010). Realising the future: Forecasting with high frequency based volatility (HEAVY) models. Journal of Applied Econometrics 25, 197--231.
#' @importFrom numDeriv jacobian hessian
#' @author Onno Kleen
#' 
#' @examples 
#' 
#' # Calculate annualized returns in percentages
#' logReturns <- 100 * makeReturns(log(SPYRM$CLOSE))
#' 
#' # Returns are assumed to be demeaned
#' logReturns <- logReturns - mean(logReturns)
#' 
#' # Due to return calculation, the first observation is missing
#' estimatedHEAVY <- HEAVYmodel(logReturns[-1], SPYRM$RK5[-1]^2 / 252)
#' 
#' # Calculate iterative multi-step-ahead forecasts
#' predict(estimatedHEAVY, stepsAhead = 12)
#' @export
HEAVYmodel <- function(ret, rm, startingValues = NULL) {
  
  ret <- as.numeric(ret)
  rm <- as.numeric(rm)
  
  if (abs(mean(ret)) > 0.00001) {
    warning("Returns are assumed to be demeaned but mean(ret) is unequal zero. Please check your data.")
  }
  
  heavyLLH <- function(par, RMEq = FALSE) {
    condVar <- calcRecVarEq(par, rm)
    if (RMEq) {
      if (sum(condVar < 0) > 0 | par[1] < 0 | par[2] < 0 | par[3] < 0 | par[2] + par[3] >= 1) {
        NA
      } else {
        - 1 / 2 * (log(condVar) + rm / condVar)
      }
    } else {
      if (sum(condVar < 0) > 0 | par[1] < 0 | (par[2] < 0) | par[3] < 0 | par[3] >= 1) {
        NA
      } else {
        - 1 / 2 * (log(condVar) + ret^2 / condVar)
      }
    }
  }
  
  if (is.null(startingValues)) {
    startingValuesVar <- c(var(ret) * (1 - 0.3 - 0.5), 0.3, 0.5) 
    startingValuesRM <- c(mean(rm) * (1 - 0.6 - 0.3), 0.6, 0.3)
    # startingValuesRM <- c(0.04, 0.6, 0.3) 
  } else {
    startingValuesVar <-startingValues[1:3]
    startingValuesRM <-startingValues[4:6]
  }
  
  # Try BFGS first, otherwise Nelder-Mead
  parVarEq <- try({optim(startingValuesVar, function(x) -sum(heavyLLH(x)), method = "BFGS")}, 
                  silent = TRUE)
  
  parRMEq <- try({optim(startingValuesRM, function(x) -sum(heavyLLH(x, RMEq = TRUE)), method = "BFGS", 
                        control = list(reltol = 1e-12))},
                 silent = TRUE)
  
  if (class(parVarEq) == "try-error") {
    parVarEq <- optim(startingValuesVar, function(x) -sum(heavyLLH(x)))
  }
  
  if (class(parRMEq) == "try-error") {
    parRMEq <- optim(startingValuesRM, function(x) -sum(heavyLLH(x, RMEq = TRUE)))
  }
  
  varCondVariances <- calcRecVarEq(parVarEq$par, rm)
  RMCondVariances <- calcRecVarEq(parRMEq$par, rm)
  
  modelEstimates <- c(parVarEq$par, parRMEq$par)
  names(modelEstimates) <- c("omegaVar", "alphaVar", "betaVar",
                             "omegaRM", "alphaRM", "betaRM")
  
  invHessianVarEq <- try({
    solve(-suppressWarnings(hessian(x = parVarEq$par, func = function (theta) {
      -sum(heavyLLH(theta))
    }, method.args=list(d = 0.0001, r = 6))))
  }, silent = TRUE)
  invHessianRMEq <- try({
    solve(-suppressWarnings(hessian(x = parRMEq$par, func = function (theta) {
      -sum(heavyLLH(theta, RMEq = TRUE))
    }, method.args=list(d = 0.00005, r = 6))))
  }, silent = TRUE)
  
  if (class(invHessianVarEq)[1] == "try-error") {
    warning("Inverting the Hessian matrix failed. No robust standard errors calculated for variance equation.")
   robStdErrVarEq <- rep(NA, times = 3)
  } else {
   robStdErrVarEq <- 
     sqrt(diag(invHessianVarEq %*% 
                 crossprod(jacobian(function(theta) -sum(heavyLLH(theta)), parVarEq$par)) %*% 
                 invHessianVarEq))
  }
  if (class(invHessianRMEq)[1] == "try-error") {
    warning("Inverting the Hessian matrix failed. No robust standard errors calculated for RM equation.")
    robStdErrRMEq <- rep(NA, times = 3)
  } else {
    robStdErrRMEq <-
      sqrt(diag(invHessianRMEq %*% 
                  crossprod(jacobian(function(theta) -sum(heavyLLH(theta, RMEq = TRUE)), parRMEq$par)) %*% 
                  invHessianRMEq))
  }
  
  model <- list()
  model$coefficients <- modelEstimates
  model$se <- c(robStdErrVarEq, robStdErrRMEq)
  model$residuals <- ret / sqrt(varCondVariances)
  model$llh <- c(llhVar = parVarEq$value, llhRM = parRMEq$value)
  model$varCondVariances <- varCondVariances
  model$RMCondVariances <- RMCondVariances
  model$data <- data.frame(ret = ret, rm = rm)
  
  class(model) <- "HEAVYmodel"
  
  model
}

#' Iterative multi-step-ahead forecasting for HEAVY models
#' 
#' @param object an object of class HEAVYmodel
#' @param stepsAhead the number of days iterative forecasts are calculated for (default 10)
#' @param ... further arguments passed to or from other methods
#' @export
predict.HEAVYmodel <- function(object, stepsAhead = 10, ...) {
  
  parRMEq <- object$coefficients[1:3]
  parVarEq <- object$coefficients[4:6]
  
  Bj <- function(j) {
    if (j == 0) {
      matrix(c(1, 0, 0, 1), nrow = 2)
    } else {
      nu <- parRMEq[2] + parRMEq[3]
      rbind(c(parVarEq[2]^j, 
              parVarEq[1] * sum(sapply(c(1:(j)), function(l) nu^(j - l) * parVarEq[2]^(l-1)))),
            c(0, nu^j))
    }
  }
  
  forecastsHEAVY <- function(stepsAhead) {
    
    oneStep <- c(parVarEq[1] + parVarEq[2] * last(object$data$rm) + parVarEq[3] * last(object$varCondVariances),
                  parRMEq[1] + parRMEq[2] * last(object$data$rm) + parRMEq[3] * last(object$RMCondVariances))
    if (stepsAhead == 1) {
      oneStep
    } else {
      sumB <- 
        Reduce('+', lapply(c(0:(stepsAhead - 1)), function(s) Bj(s))) %*% 
        c(parVarEq[1], parRMEq[1]) + 
        Bj(stepsAhead) %*% 
        oneStep
      sumB
    }
  } 
  
  fcts <- sapply(c(1:stepsAhead), function(x) forecastsHEAVY(x)[1])
  names(fcts) <- c(1:stepsAhead)
  fcts
}

#' @export
print.HEAVYmodel <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("")
  print(summary(x)$coefficients)
  cat(" \n")
  cat(paste0("The log-likelihoods are: \n",
             "Variance equation:", sprintf("%.3f", x$llh[1]), "\n", 
             "RM equation:", sprintf("%.3f", x$llh[2])))
}

#' @export
summary.HEAVYmodel <- function(object, ...) {
  ans <- list()
  ans$coefficients <- 
    cbind(Estimate = object$coefficients, 
          `Std. Error` = object$se, 
          `t value` = object$coefficients / object$se,
          `Pr(>|t|)` = 2 * pnorm(abs(object$coefficients / object$se), 
                              lower.tail = FALSE))
  class(ans) <- "summary.heavy"
  ans
}


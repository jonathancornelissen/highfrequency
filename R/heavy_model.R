

#' HEAVY Model estimation
#' @description This function calculates the High frEquency bAsed VolatilitY (HEAVY) model proposed in Shephard and Sheppard (2010). 
#'
#' @param data an \code{xts} object where the first column is  a vector of demeaned returns 
#' and the second column is a vector of realized stock market variation
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
#' The class HEAVYmodel has the following methods: plot.HEAVYmodel, predict.HEAVYmodel, 
#' print.HEAVYmodel, and summary.HEAVYmodel.
#' 
#' @details 
#' Let \eqn{r_{t}} and \eqn{RM_{t}} be series of demeaned returns and realized measures of
#' daily stock price variation. The HEAVY model is a two-component model.
#' We assume \eqn{r_{t} = h_{t}^{1/2} Z_{t}} where \eqn{Z_t} is an i.i.d. zero-mean 
#' and unit-variance innovation term. The dynamics of the HEAVY model are given by
#'
#' \deqn{
#'    h_{t} = \omega + \alpha RM_{t-1} + \beta h_{t-1}
#'  }
#'  and 
#'  \deqn{
#'    \mu_{t} = \omega_{R} + \alpha_{R} RM_{t-1} + \beta_{R} \mu_{t-1}.
#'  }
#'  
#' The two equations are estimated separately as mentioned in Shephard and Sheppard (2010).
#' We report robust standard errors based on the matrix-product of inverted Hessians and
#' the outer product of gradients.
#' 
#' @references Shephard, N. and K. Sheppard (2010). Realising the future: Forecasting with high frequency based volatility (HEAVY) models. Journal of Applied Econometrics 25, 197--231.
#' @importFrom numDeriv jacobian hessian
#' @importFrom stats nlminb
#' @author Onno Kleen
#' 
#' @examples 
#' 
#' # Calculate annualized returns in percentages
#' logReturns <- 100 * makeReturns(SPYRM$CLOSE)[-1]
#' 
#' # Returns are assumed to be demeaned
#' logReturns <- logReturns - mean(logReturns)
#' 
#' # Returns and 
#' SPYxts <- xts::xts(cbind(logReturns, SPYRM$RK5[-1] * 10000), order.by = SPYRM$DT[-1])
#' 
#' # Due to return calculation, the first observation is missing
#' fittedHEAVY <- HEAVYmodel(SPYxts)
#' 
#' # Examine tThe estimated coefficients and robust standard errors
#' fittedHEAVY
#' 
#' # Calculate iterative multi-step-ahead forecasts
#' predict(fittedHEAVY, stepsAhead = 12)
#' @seealso 
#' 
#' \code{\link{predict.HEAVYmodel}}
#' 
#' @export
HEAVYmodel <- function(data, startingValues = NULL) {
  
  ret <- as.numeric(data[,1])
  rm <- as.numeric(data[,2])
  
  if (abs(mean(ret)) > 0.001) {
    warning("Returns are assumed to be demeaned but mean(ret) is unequal zero. Please check your data.")
  }
  
  heavyLLH <- function(par, RMEq = FALSE) {
    # if (RMEq) {
    #   browser()
    # }
    condVar <- calcRecVarEq(par, rm)
    if (RMEq) {
      if (sum(condVar < 0) > 0 | par[1] < 0 | par[2] < 0 | par[3] < 0 | par[2] + par[3] >= 1) {
        NA
      } else {
        -1/2 * log(2 * pi) - 1 / 2 * (log(condVar) + rm / condVar)
      }
    } else {
      if (sum(condVar < 0) > 0 | par[1] < 0 | (par[2] < 0) | par[3] < 0 | par[3] >= 1) {
        NA
      } else {
        -1/2 * log(2 * pi) - 1 / 2 * (log(condVar) + ret^2 / condVar)
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
  
  control_neg <- list(fnscale = -1)
  
  # Get into the direction of global minimum via nlminb
  parVarEq <- try({suppressWarnings(nlminb(startingValuesVar, function(x) -sum(heavyLLH(x))))}, 
                  silent = TRUE)
  
  parRMEq <- try({suppressWarnings(nlminb(startingValuesRM, function(x) -sum(heavyLLH(x, RMEq = TRUE))))},
                 silent = TRUE)
  
  parVarEq$value <- parVarEq$objective
  parRMEq$value <- parRMEq$objective
  
  # Optimize w.r.t. gradient
  parVarEqBFGS<- try({optim(parVarEq$par, function(x) -sum(heavyLLH(x)), method = "BFGS")}, silent = TRUE)
  parRMEqBFGS<- try({optim(parRMEq$par, function(x) -sum(heavyLLH(x, RMEq = TRUE)), method = "BFGS")}, silent = TRUE)
  
  if (class(parVarEqBFGS) != "try-error") {
    parVarEq <- parVarEqBFGS
  }
  if (class(parRMEqBFGS) != "try-error") {
    parRMEq <- parRMEqBFGS
  }
  
  varCondVariances <- calcRecVarEq(parVarEq$par, rm)
  RMCondVariances <- calcRecVarEq(parRMEq$par, rm)
  
  modelEstimates <- c(parVarEq$par, parRMEq$par)
  names(modelEstimates) <- c("omega", "alpha", "beta",
                             "omegaR", "alphaR", "betaR")
  
  invHessianVarEq <- try({
    solve(-suppressWarnings(hessian(x = parVarEq$par, func = function (theta) {
      sum(heavyLLH(theta))
    }, method.args=list(d = 0.0001, r = 6))))
  }, silent = TRUE)
  invHessianRMEq <- try({
    solve(-suppressWarnings(hessian(x = parRMEq$par, func = function (theta) {
      sum(heavyLLH(theta, RMEq = TRUE))
    }, method.args=list(d = 0.0001, r = 6))))
  }, silent = TRUE)
  
  if (class(invHessianVarEq)[1] == "try-error") {
    warning("Inverting the Hessian matrix failed. No robust standard errors calculated for variance equation.")
   robStdErrVarEq <- rep(NA, times = 3)
  } else {
   robStdErrVarEq <- 
     sqrt(diag(invHessianVarEq %*% 
                 crossprod(jacobian(function(theta) heavyLLH(theta), parVarEq$par)) %*% 
                 invHessianVarEq))
  }
  if (class(invHessianRMEq)[1] == "try-error") {
    warning("Inverting the Hessian matrix failed. No robust standard errors calculated for RM equation.")
    robStdErrRMEq <- rep(NA, times = 3)
  } else {
    robStdErrRMEq <-
      sqrt(diag(invHessianRMEq %*% 
                  crossprod(jacobian(function(theta) heavyLLH(theta, RMEq = TRUE), parRMEq$par)) %*% 
                  invHessianRMEq))
  }
  
  model <- list()
  model$coefficients <- modelEstimates
  model$se <- c(robStdErrVarEq, robStdErrRMEq)
  model$residuals <- ret / sqrt(varCondVariances)
  model$llh <- c(llhVar = -parVarEq$value, llhRM = -parRMEq$value)
  model$varCondVariances <- varCondVariances
  model$RMCondVariances <- RMCondVariances
  model$data <- data
  
  class(model) <- "HEAVYmodel"
  
  model
}


#' Plotting method for HEAVYmodel objects
#' @param x an object of class \code{HEAVYmodel}
#' @param ... extra arguments, see details
#' @details The plotting method has the following optional parameter:
#' \itemize{
#' \item{\code{legend.loc}}{ A string denoting the location of the legend passed on to \code{addLegend} of the \pkg{xts} package}
#' }
#' 
#' @importFrom grDevices dev.interactive
#' @importFrom graphics plot panel.smooth points par
#' @importFrom stats residuals
#' @importFrom xts addLegend
#' @export
plot.HEAVYmodel <- function(x, ...){
  options <- list(...)
  #### List of standard options
  opt <- list(legend.loc = "topleft")
  #### Override standard options where user passed new options
  opt[names(options)] <- options
  
  observed   <- x$data[,1]^2
  fitted     <- x$varCondVariances
  dates <- index(x$data)
  # dates      <- x$data
  # dates      <- as.POSIXct(dates)
  observed   <- xts(observed, order.by = dates)
  fitted     <- xts(fitted, order.by = dates)
  # type       <- x$type
  legend.loc <- opt$legend.loc
  g_range <- range(fitted,observed)
  g_range[1] <- 0.95 * g_range[1]
  g_range[2] <- 1.05 * g_range[2]
  #ind = seq(1,length(fitted),length.out=5);
  title <- "Observed squeared returns and fitted variances based on HEAVY Model"
  plot(cbind(fitted,observed), col = c('blue', 'red'), main = title, lty = c(1,2), yaxis.right = FALSE)
  
  addLegend(legend.loc = legend.loc, on = 1,
            legend.names = c("Fitted condidional variances", "Observed squared returns"),
            lty = c(1, 2), lwd = c(2, 2),
            col = c("blue", "red"))
}

#' Iterative multi-step-ahead forecasting for HEAVY models
#' 
#' Calculates forecasts for \eqn{h_{T+k}}, where \eqn{T} denotes the end of the estimation
#' period for fitting the HEAVYmodel and \eqn{k = 1, \dots, \code{stepsAhead}}.
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
    
    oneStep <- c(parVarEq[1] + parVarEq[2] * last(object$data[,2]) + parVarEq[3] * last(object$varCondVariances),
                  parRMEq[1] + parRMEq[2] * last(object$data[,2]) + parRMEq[3] * last(object$RMCondVariances))
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
             "Variance equation: ", sprintf("%.3f", x$llh[1]), "\n", 
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


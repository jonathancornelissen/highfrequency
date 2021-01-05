

#' HEAVY model estimation
#' 
#' @description This function calculates the High frEquency bAsed VolatilitY (HEAVY) model proposed in Shephard and Sheppard (2010). 
#'
#' @param data an \code{xts} object where the first column is a vector of returns 
#' and the second column is a vector of realized stock market variation
#' @param startingValues a vector of alternative starting values: first three arguments for variance equation and last three arguments for measurement equation.
#' 
#' @return The function outputs an object of class \code{HEAVYmodel}, a list containing
#' \itemize{
#'   \item coefficients = estimated coefficients.
#'   \item se = robust standard errors based on inverted Hessian matrix.
#'   \item residuals = the residuals in the return equation.
#'   \item llh = the two-component log-likelihood values.
#'   \item varCondVariances = conditional variances in the variance equation.
#'   \item RMCondVariances = conditional variances in the RM equation.
#'   \item data = the input data.
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
#' Note that we always demean the returns in the data input as we don't include a constant in the mean equation.
#' 
#' @references Shephard, N. and Sheppard, K. (2010). Realising the future: Forecasting with high frequency based volatility (HEAVY) models. Journal of Applied Econometrics 25, 197--231.
#' @importFrom numDeriv jacobian hessian
#' @importFrom stats nlminb
#' @importFrom Rsolnp solnp
#' @author Onno Kleen and Emil Sjorup.
#' 
#' @examples 
#' 
#' # Calculate annualized returns in percentages
#' logReturns <- 100 * makeReturns(SPYRM$CLOSE)[-1]
#' 
#' # Combine both returns and realized measures into one xts
#' dataSPY <- xts::xts(cbind(logReturns, SPYRM$BPV5[-1] * 10000), order.by = SPYRM$DT[-1])
#' 
#' # Due to return calculation, the first observation is missing
#' fittedHEAVY <- HEAVYmodel(dataSPY)
#' 
#' # Examine the estimated coefficients and robust standard errors
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
  ret <- ret - mean(ret)
  
  if (is.null(startingValues)) {
    startingValuesVar <- c(mean(ret^2) * (1 - 0.3 * mean(rm) / mean(ret^2) - 0.5), 0.3, 0.5) 
    startingValuesRM <- c(mean(rm) * (1 - 0.6 - 0.3), 0.6, 0.3)
  } else {
    startingValuesVar <-startingValues[1:3]
    startingValuesRM <-startingValues[4:6]
  }
  
  rsolVar <- solnp(pars = startingValuesVar, fun = function(x) -sum(heavyLLH(x, ret = ret, rm = rm)),
                   ineqfun = function(x) c(x, x[2] + x[3]),  ineqLB = c(0,0,0,0), ineqUB = c(Inf, Inf, 1, Inf), control = list(trace = 0))
  rsolRM <- solnp(pars = startingValuesRM, fun = function(x) -sum(heavyLLH(x, rm = rm, RMEq = TRUE)),
                  ineqfun = function(x) c(x, x[2] + x[3]),  ineqLB = c(0,0,0,0), ineqUB = c(Inf, 1, 1, 1), control = list(trace = 0))
  
  
  varCondVariances <- calcRecVarEq(rsolVar$par, rm)
  RMCondVariances <- calcRecVarEq(rsolRM$par, rm)
  
  modelEstimates <- c(rsolVar$par, rsolRM$par)
  names(modelEstimates) <- c("omega", "alpha", "beta",
                             "omegaR", "alphaR", "betaR")
  
  
  
  invHessianVarEq <- try({
    solve(-suppressWarnings(hessian(x = rsolVar$par, func = function (theta) {
      sum(heavyLLH(theta, rm = rm, ret = ret))
    }, method.args=list(d = 0.0001, r = 6))))
  }, silent = TRUE)
  invHessianRMEq <- try({
    solve(-suppressWarnings(hessian(x = rsolRM$par, func = function (theta) {
      sum(heavyLLH(theta, rm = rm, RMEq = TRUE))
    }, method.args = list(d = 0.0001, r = 6))))
  }, silent = TRUE)
  
  if (class(invHessianVarEq)[1] == "try-error") {
    warning("Inverting the Hessian matrix failed. No robust standard errors calculated for variance equation.")
    robStdErrVarEq <- rep(NA, times = 3)
  } else {
    robStdErrVarEq <- sqrt(diag(invHessianVarEq %*% 
                                  crossprod(jacobian(function(theta) heavyLLH(theta, rm = rm, ret = ret), rsolVar$par)) %*% 
                                  invHessianVarEq))
  }
  if (class(invHessianRMEq)[1] == "try-error") {
    warning("Inverting the Hessian matrix failed. No robust standard errors calculated for RM equation.")
    robStdErrRMEq <- rep(NA, times = 3)
  } else {
    robStdErrRMEq <-
      sqrt(diag(invHessianRMEq %*% 
                  crossprod(jacobian(function(theta) heavyLLH(theta, rm = rm, RMEq = TRUE), rsolRM$par)) %*% 
                  invHessianRMEq))
  }
  
  model <- list(
    "coefficients" = modelEstimates,
    "se" = c(robStdErrVarEq, robStdErrRMEq),
    "residuals" = ret / sqrt(varCondVariances),
    "llh" = c(llhVar = -rsolVar$value, llhRM = -rsolRM$value),
    "varCondVariances" = varCondVariances,
    "RMCondVariances" = RMCondVariances,
    "data" = data,
    "rsolvar" = rsolVar,
    "rsolRM" = rsolRM
  )
  
  class(model) <- "HEAVYmodel"
  
  model
}


#' Plotting method for HEAVYmodel objects
#' @param x an object of class \code{HEAVYmodel}.
#' @param ... extra arguments, see details.
#' @details 
#' The plotting method has the following optional parameter:
#' \itemize{
#' \item{\code{legend.loc}}{ A string denoting the location of the legend passed on to \code{addLegend} of the \pkg{xts} package}
#' \item{\code{type}}{ A string denoting the type of lot to be made. If \code{type} is \code{"condVar"} the fitted values of the conditional variance of the returns
#' is shown. If \code{type} is different from \code{"condVar"}, the fitted values of the realized measure is shown. Default is \code{"condVar"}}
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
  opt <- list(legend.loc = "topleft", type = "condVar")
  #### Override standard options where user passed new options
  opt[names(options)] <- options
  
  
  if(opt$type == "condVar"){
    observed   <- x$data[,1]^2
    fitted     <- x$varCondVariances
    # dates      <- x$data
    # dates      <- as.POSIXct(dates)
    # type       <- x$type
    legend.loc <- opt$legend.loc
    
    
    title <- "Observed squared returns and fitted variances based on HEAVY Model"
    plot(cbind(fitted,observed), col = c('blue', 'red'), main = title, lty = c(1,2), yaxis.right = FALSE)
    
    addLegend(legend.loc = legend.loc, on = 1,
              legend.names = c("Fitted condidional variances", "Observed squared returns"),
              lty = c(1, 2), lwd = c(2, 2),
              col = c("blue", "red"))
  } else {
    observed   <- x$data[,2]
    fitted     <- x$RMCondVariances
    # dates      <- x$data
    # dates      <- as.POSIXct(dates)
    # type       <- x$type
    legend.loc <- opt$legend.loc
    
    
    title <- "Observed realized measure and fitted realized measure based on HEAVY Model"
    plot(cbind(fitted,observed), col = c('blue', 'red'), main = title, lty = c(1,2), yaxis.right = FALSE)
    
    addLegend(legend.loc = legend.loc, on = 1,
              legend.names = c("Fitted realized measure", "Observed realized measure"),
              lty = c(1, 2), lwd = c(2, 2),
              col = c("blue", "red"))
  }
  
}

#' Iterative multi-step-ahead forecasting for HEAVY models
#' 
#' Calculates forecasts for \eqn{h_{T+k}}, where \eqn{T} denotes the end of the estimation
#' period for fitting the HEAVYmodel and \eqn{k = 1, \dots, \code{stepsAhead}}.
#' 
#' @param object an object of class HEAVYmodel.
#' @param stepsAhead the number of days iterative forecasts are calculated for (default 10).
#' @param ... further arguments passed to or from other methods.
#' @export
predict.HEAVYmodel <- function(object, stepsAhead = 10, ...) {
  
  parVarEq <- object$coefficients[1:3]
  parRMEq <- object$coefficients[4:6]
  
  Bjs <- lapply(c(0:(stepsAhead)), function(s) Bj(s, parVarEq, parRMEq))
  
  fcts <- t(vapply(c(1:stepsAhead), function(x) forecastsHEAVY(x, object, parVarEq, parRMEq, Bjs), numeric(2)))
  
  rownames(fcts) <- paste0("T + ", c(1:stepsAhead))
  colnames(fcts) <- c('CondVar', 'CondRM')
  fcts
}


#' @export
print.HEAVYmodel <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("")
  print(summary(x)$coefficients)
  cat(" \n")
  cat(paste0("The log-likelihoods are: \n",
             "Variance equation: ", sprintf("%.3f", x$llh[1]), "\n", 
             "RM equation:", sprintf("%.3f", x$llh[2])), "\n")
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


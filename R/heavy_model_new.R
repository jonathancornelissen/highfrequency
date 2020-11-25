
#' keywords internal
#' calcRecVarEq <- function(par, rm) {
#'   h <- rep(1, times = length(rm))
#'   h[1] <- mean(rm)
#'   for (ii in c(2:length(rm))) {
#'     h[ii] <- par[1] + par[2] * rm[ii-1] + par[3] * h[ii-1]
#'   }
#'   h
#' }

# blub <-
#   SPYRM %>%
#   tibble::as_tibble() %>%
#   dplyr::mutate(ret = 100 * (log(CLOSE) - log(dplyr::lag(CLOSE, order_by = DT)))) %>%
#   dplyr::filter(!is.na(ret))
# 
# ret <- blub$ret
# rm <- blub$RV5
# 
# meanRet <- mean(ret)
# ret <- ret - meanRet
# 
# blub1 <- HEAVYmodelNew(ret, rm)
# summary(blub1)
# 
# predict(blub1)



#' HEAVY Model estimation
#' @description This function calculates the High frEquency bAsed VolatilitY (HEAVY) model proposed in Shephard and Sheppard (2010). 
#'
#' @param ret (Log)-returns 
#' @param rm realized measure of intraday stock price variation
#' @param startingValues alternative starting values 
#' 
#' @references Shephard, N. and K. Sheppard (2010). Realising the future: Forecasting with high frequency based volatility (HEAVY) models. Journal of Applied Econometrics 25, 197--231.
#' @importFrom numDeriv jacobian
#' @export
HEAVYmodelNew <- function(ret, rm, startingValues = NULL) {
  
  if (abs(mean(ret)) > 0.000000001) {
    warning("Returns are assumed to be demeaned but mean(ret) is unequal zero. Please check your data.")
  }
  
  heavyLLH <- function(par, rmEq = FALSE) {
    condVar <- calcRecVarEq(par, rm)
    if (rmEq == TRUE) {
      if (sum(condVar < 0) > 0 | par[1] < 0 | par[2] < 0 | par[3] < 0 | par[2] + par[3] >= 1) {
        NA
      } else {
        - 1 / 2 * (log(condVar) + rm / condVar)
      }
    } else {
      if (sum(condVar < 0) > 0 | (par[1] < 0) | (par[2] < 0) | par[3] < 0 | par[2] > 1) {
        NA
      } else {
        - 1 / 2 * (log(condVar) + ret^2 / condVar)
      }
    }
  }
  
  if (!is.null(startingValues)) {
    startingValues <- c(0.04, 0.2, 0.6) 
  }
  
  # Try BFGS first, otherwise Nelder-Mead
  parVarEq <- try({optim(startingValues, function(x) -sum(heavyLLH(x)), method = "BFGS")}, 
                  silent = TRUE)
  
  parRMEq <- try({optim(startingValues, function(x) -sum(heavyLLH(x, rmEq = TRUE)), method = "BFGS")},
                 silent = TRUE)
  
  if (class(parVarEq) == "try-error") {
    parVarEq <- optim(startingValues, function(x) -sum(heavyLLH(x)))
  }
  if (class(parRMEq) == "try-error") {
    parRMEq <- optim(startingValues, function(x) -sum(heavyLLH(x, rmEq = TRUE)))
  }
  
  
  varCondVariances <- calcRecVarEq(parVarEq$par, rm)
  RMCondVariances <- calcRecVarEq(parRMEq$par, rm)
  
  modelEstimates <- c(parVarEq$par, parRMEq$par)
  names(modelEstimates) <- c("varOmega", "varAlpha", "varBeta",
                             "rmOmega", "rmAlpha", "rmBeta")
  
  invHessianVarEq <- try({
    solve(-suppressWarnings(numDeriv::hessian(x = parVarEq$par, func = function (theta) {
      -sum(heavyLLH(theta))
    })))
  }, silent = TRUE)
  invHessianRMEq <- try({
    solve(-suppressWarnings(numDeriv::hessian(x = parRMEq$par, func = function (theta) {
      -sum(heavyLLH(theta, rmEq = TRUE))
    })))
  }, silent = TRUE)
  
  if (class(invHessianVarEq)[1] == "try-error") {
    warning("Inverting the Hessian matrix failed. No robust standard errors calculated for variance equation.")
   robStdErrVarEq <- NA
  } else {
   robStdErrVarEq <- sqrt(diag(invHessianVarEq %*% crossprod(numDeriv::jacobian(func = function(theta) -sum(heavyLLH(theta)), x = parVarEq$par)) %*% invHessianVarEq))
  }
  if (class(invHessianRMEq)[1] == "try-error") {
    warning("Inverting the Hessian matrix failed. No robust standard errors calculated for RM equation.")
    robStdErrRMEq <- NA
  } else {
    robStdErrRMEq <- sqrt(diag(invHessianRMEq %*% crossprod(numDeriv::jacobian(func = function(theta) -sum(heavyLLH(theta, rmEq = TRUE)), x = parRMEq$par)) %*% invHessianRMEq))
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

#' @export
predict.HEAVYmodel <- function(object, warnings = TRUE, stepsAhead = 10, ...) {
  
  parRMEq$par <- object$coefficients[1:3]
  parVarEq$par <- object$coefficients[4:6]
  
  B_j <- function(j) {
    if (j == 0) {
      matrix(c(1, 0, 0, 1), nrow = 2)
    } else {
      nu <- parRMEq$par[2] + parRMEq$par[3]
      rbind(c(parVarEq$par[2]^j, 
              parVarEq$par[1] * sum(sapply(c(1:(j)), function(l) nu^(j - l) * parVarEq$par[2]^(l-1)))),
            c(0, nu^j))
    }
  }
  
  forecastsHEAVY <- function(steps.ahead) {
    
    oneStep <- c(parVarEq$par[1] + parVarEq$par[2] * last(object$data$rm) + parVarEq$par[3] * last(object$varCondVariances),
                  parRMEq$par[1] + parRMEq$par[2] * last(object$data$rm) + parRMEq$par[3] * last(object$RMCondVariances))
    if (steps.ahead == 1) {
      oneStep
    } else {
      sum_B <- 
        Reduce('+', lapply(c(0:(steps.ahead - 2)), function(s) B_j(s))) %*% 
        c(parVarEq$par[1], parRMEq$par[2]) + 
        B_j(steps.ahead - 1) %*% 
        oneStep
      sum_B
    }
  } 
  
  fcts <- sapply(c(1:stepsAhead), function(x) forecastsHEAVY(x)[1])
  fcts
}

#' @export
print.HEAVYmodel <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  print(summary(x)$coefficients)
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




# fit_heavy_model <- function(ret, rm) {
#   
#   h_var_eq <- function(rm, omega, alpha, beta) {
#     h <- rep(1, times = length(rm))
#     h[1] <- mean(rm)
#     for (ii in c(2:length(rm))) {
#       h[ii] <- omega + alpha * rm[ii-1] + beta * h[ii-1]
#     }
#     h
#   }
#   
#   rm_eq <- function(rm, omega.rm, alpha.rm, beta.rm) { 
#     mu <- rep(1, times = length(rm))
#     mu[1] <- mean(rm)
#     for (ii in c(2:length(rm))) {
#       mu[ii] <- omega.rm + alpha.rm * rm[ii-1] + beta.rm * mu[ii-1]
#     }
#     mu
#   }
#   
#   heavy_llh_var_eq_optim <- function(par) {
#     h_llh <- h_var_eq(rm = rm,
#                       omega = par["omega_var"],
#                       alpha = par["alpha_var"],
#                       beta = par["beta_var"])
#     if (sum(h_llh < 0) > 0 | (par["alpha_var"] < 0) | (par["omega_var"] < 0) | par["beta_var"] < 0 | par["beta_var"] > 1) {
#       NA
#     } else {
#       - 1 / 2 * (log(h_llh) + ret^2 / h_llh)
#     }
#   }
#   heavy_llh_rm_eq_optim <- function(par) {
#     rm_llh <- rm_eq(rm = rm,
#                     omega.rm = par["omega_rm"],
#                     alpha.rm = par["alpha_rm"],
#                     beta.rm = par["beta_rm"])
#     
#     if (sum(rm_llh < 0) > 0 | (par["alpha_rm"] < 0) | par["beta_rm"] < 0 | par["omega_rm"] < 0 | par["alpha_rm"] + par["beta_rm"] >= 1) {
#       NA
#     } else {
#       - 1 / 2 * (log(rm_llh) + rm / rm_llh)
#     }
#   }
#   
#   parVarEq <- try({optim(par = c("omega_var" = 0.04, "alpha_var" = 0.2, "beta_var" = 0.6), fn = function(x) -sum(heavy_llh_var_eq_optim(x)), method = "BFGS")})
#   parRMEq <- try({optim(par = c("omega_rm" = 0.02, "alpha_rm" = 0.2, "beta_rm" = 0.6), fn = function(x) -sum(heavy_llh_rm_eq_optim(x)), method = "BFGS")})
#   
#   if (class(parVarEq) == "try-error") {
#     parVarEq <- optim(par = c("omega_var" = 0.04, "alpha_var" = 0.2, "beta_var" = 0.6), fn = function(x) -sum(heavy_llh_var_eq_optim(x)))
#   }
#   if (class(parRMEq) == "try-error") {
#     parRMEq <- optim(par = c("omega_rm" = 0.02, "alpha_rm" = 0.2, "beta_rm" = 0.6), fn = function(x) -sum(heavy_llh_rm_eq_optim(x)))
#   }
#   
#   
#   B_j <- function(j) {
#     if (j == 0) {
#       matrix(c(1, 0, 0, 1), nrow = 2)
#     } else {
#       nu <- parRMEq$par["alpha_rm"] + parRMEq$par["beta_rm"]
#       rbind(c(parVarEq$par["beta_var"]^j, 
#               parVarEq$par["alpha_var"] * sum(sapply(c(1:(j)), function(l) nu^(j - l) * parVarEq$par["beta_var"]^(l-1)))),
#             c(0, nu^j))
#     }
#   }
#   
#   last_h <-
#     last(h_var_eq(rm = rm,
#                   omega = parVarEq$par["omega_var"],
#                   alpha = parVarEq$par["alpha_var"],
#                   beta = parVarEq$par["beta_var"]))
#   last_mu <-
#     last(rm_eq(rm = rm,
#                omega.rm = parRMEq$par["omega_rm"],
#                alpha.rm = parRMEq$par["alpha_rm"],
#                beta.rm = parRMEq$par["beta_rm"]))
#   
#   forecasts_heavy<- function(step.ahead) {
#     
#     one_step <- c(parVarEq$par["omega_var"] + parVarEq$par["alpha_var"] * last(rm) + parVarEq$par["beta_var"] * last_h,
#                   parRMEq$par["omega_rm"] + parRMEq$par["alpha_rm"] * last(rm) + parRMEq$par["beta_rm"] * last_mu)
#     if (step.ahead == 1) {
#       one_step
#     } else {
#       sum_B <- 
#         Reduce('+', lapply(c(0:(step.ahead - 2)), function(s) B_j(s))) %*% 
#         c(parVarEq$par["omega_var"], parRMEq$par["omega_rm"]) + 
#         B_j(step.ahead - 1) %*% 
#         one_step
#       sum_B
#     }
#   } 
#   
#   fcts <- sapply(c(1:66), function(x) forecasts_heavy(x)[1])
#   
#   list(par_var = parVarEq$par,
#        par_rm = parRMEq$par,
#        forecasts = fcts)
#   
# }
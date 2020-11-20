#' HEAVY Model estimation
#' @description This function calculates the High frEquency bAsed VolatilitY (HEAVY) model proposed in Shephard and Sheppard (2010). 
#'
#' @param ret (Log)-returns 
#' @param rm Realized measure of intraday stock variation
#' 
#' @references Shephard, N. and K. Sheppard (2010). Realising the future: Forecasting with high frequency based volatility (HEAVY) models. Journal of Applied Econometrics 25, 197--231.
#' @export
HEAVYmodelNew <- function(ret, rm, varianceTargeting = FALSE, startingValues = NULL) {
  
  class(model) <- "HEAVY"
}

#' @export
predict.HARmodel <- function(object, newdata = NULL, warnings = TRUE) {
  
}

#' @export
print.HEAVYmodel <- function(x, digits = max(3, getOption("digits") - 3), ...){

}

#' @export
summary.HEAVYmodel <- function(object, ...) {
  
}




# fit_heavy_model <- function(ret, rm) {
#   
#   h_var_eq <- function(rm, omega.var, alpha.var, beta.var) {
#     h <- rep(1, times = length(rm))
#     h[1] <- mean(rm)
#     for (ii in c(2:length(rm))) {
#       h[ii] <- omega.var + alpha.var * rm[ii-1] + beta.var * h[ii-1]
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
#                       omega.var = par["omega_var"],
#                       alpha.var = par["alpha_var"],
#                       beta.var = par["beta_var"])
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
#   par_var_eq <- try({optim(par = c("omega_var" = 0.04, "alpha_var" = 0.2, "beta_var" = 0.6), fn = function(x) -sum(heavy_llh_var_eq_optim(x)), method = "BFGS")})
#   par_rm_eq <- try({optim(par = c("omega_rm" = 0.02, "alpha_rm" = 0.2, "beta_rm" = 0.6), fn = function(x) -sum(heavy_llh_rm_eq_optim(x)), method = "BFGS")})
#   
#   if (class(par_var_eq) == "try-error") {
#     par_var_eq <- optim(par = c("omega_var" = 0.04, "alpha_var" = 0.2, "beta_var" = 0.6), fn = function(x) -sum(heavy_llh_var_eq_optim(x)))
#   }
#   if (class(par_rm_eq) == "try-error") {
#     par_rm_eq <- optim(par = c("omega_rm" = 0.02, "alpha_rm" = 0.2, "beta_rm" = 0.6), fn = function(x) -sum(heavy_llh_rm_eq_optim(x)))
#   }
#   
#   
#   B_j <- function(j) {
#     if (j == 0) {
#       matrix(c(1, 0, 0, 1), nrow = 2)
#     } else {
#       nu <- par_rm_eq$par["alpha_rm"] + par_rm_eq$par["beta_rm"]
#       rbind(c(par_var_eq$par["beta_var"]^j, 
#               par_var_eq$par["alpha_var"] * sum(sapply(c(1:(j)), function(l) nu^(j - l) * par_var_eq$par["beta_var"]^(l-1)))),
#             c(0, nu^j))
#     }
#   }
#   
#   last_h <-
#     last(h_var_eq(rm = rm,
#                   omega.var = par_var_eq$par["omega_var"],
#                   alpha.var = par_var_eq$par["alpha_var"],
#                   beta.var = par_var_eq$par["beta_var"]))
#   last_mu <-
#     last(rm_eq(rm = rm,
#                omega.rm = par_rm_eq$par["omega_rm"],
#                alpha.rm = par_rm_eq$par["alpha_rm"],
#                beta.rm = par_rm_eq$par["beta_rm"]))
#   
#   forecasts_heavy<- function(step.ahead) {
#     
#     one_step <- c(par_var_eq$par["omega_var"] + par_var_eq$par["alpha_var"] * last(rm) + par_var_eq$par["beta_var"] * last_h,
#                   par_rm_eq$par["omega_rm"] + par_rm_eq$par["alpha_rm"] * last(rm) + par_rm_eq$par["beta_rm"] * last_mu)
#     if (step.ahead == 1) {
#       one_step
#     } else {
#       sum_B <- 
#         Reduce('+', lapply(c(0:(step.ahead - 2)), function(s) B_j(s))) %*% 
#         c(par_var_eq$par["omega_var"], par_rm_eq$par["omega_rm"]) + 
#         B_j(step.ahead - 1) %*% 
#         one_step
#       sum_B
#     }
#   } 
#   
#   fcts <- sapply(c(1:66), function(x) forecasts_heavy(x)[1])
#   
#   list(par_var = par_var_eq$par,
#        par_rm = par_rm_eq$par,
#        forecasts = fcts)
#   
# }
#' Internal HEAVY functions
#' @keywords internal
Bj <- function(j, parRMEq, parVarEq) {
  if (j == 0) {
    matrix(c(1, 0, 0, 1), nrow = 2)
  } else {
    nu <- parRMEq[2] + parRMEq[3]
    rbind(c(parVarEq[2]^j, 
            parVarEq[1] * sum(sapply(c(1:(j)), function(l) nu^(j - l) * parVarEq[2]^(l-1)))),
          c(0, nu^j))
  }
}


#' Internal HEAVY functions
#' @keywords internal
forecastsHEAVY <- function(stepsAhead, object, parRMEq, parVarEq) {
  oneStep <- c(parVarEq[1] + parVarEq[2] * last(object$data[,2]) + parVarEq[3] * last(object$varCondVariances),
               parRMEq[1] + parRMEq[2] * last(object$data[,2]) + parRMEq[3] * last(object$RMCondVariances))
  if (stepsAhead == 1) {
    oneStep
  } else {
    sumB <- 
      Reduce('+', lapply(c(0:(stepsAhead - 1)), function(s) Bj(s, parRMEq, parVarEq))) %*% 
      c(parVarEq[1], parRMEq[1]) + 
      Bj(stepsAhead, parRMEq, parVarEq) %*% 
      oneStep
    sumB
  }
} 



heavyLLH <- function(par, ret = NULL, rm = NULL, RMEq = FALSE) {
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
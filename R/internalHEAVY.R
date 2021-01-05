#' Internal HEAVY functions
#' @keywords internal
Bj <- function(j,  parVarEq, parRMEq) {
  if (j == 0) {
    matrix(c(1, 0, 0, 1), nrow = 2)
  } else {
    # browser()
    nu <- parRMEq[2] + parRMEq[3]
    matrix(c(parVarEq[3]^j, parVarEq[2] * sum(sapply(c(1:(j)), function(l) nu^(j - l) * parVarEq[3]^(l-1))), 
             0, nu^j), 
           ncol = 2, byrow = TRUE)
  }
}

#' @keywords internal
forecastsHEAVY <- function(stepsAhead, object, parVarEq, parRMEq, Bjs) {
  oneStep <- c(parVarEq[1] + parVarEq[2] * last(object$data[,2]) + parVarEq[3] * last(object$varCondVariances),
               parRMEq[1] + parRMEq[2] * last(object$data[,2]) + parRMEq[3] * last(object$RMCondVariances))
  if (stepsAhead == 1) {
    oneStep
  } else {
    sumB <- 
      Reduce('+', Bjs[1:(stepsAhead)]) %*% 
      matrix(c(parVarEq[1], parRMEq[1]), ncol = 1) + 
      Bjs[[stepsAhead + 1]] %*% 
      oneStep
    sumB
  }
} 

#' @keywords internal
heavyLLH <- function(par, ret = NULL, rm = NULL, RMEq = FALSE) {
  if (RMEq) {
    condVar <- calcRecVarEq(par, rm)
    -1/2 * log(2 * pi) - 1 / 2 * (log(condVar) + rm / condVar)
  } else {
    condVar <- calcRecVarEq(par, ret^2)
    -1/2 * log(2 * pi) - 1 / 2 * (log(condVar) + ret^2 / condVar)
  }
}
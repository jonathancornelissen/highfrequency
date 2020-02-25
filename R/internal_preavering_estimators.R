#' @keywords internal
crv <- function(pdata) {
  N <- nrow(pdata)
  theta <- 0.8 ##recommendation by Hautsch and Podolskij
  kn <- floor(theta*sqrt(N))

  ##psi:
  psi1 <- 1
  psi2 <- 1/12

  psi1kn <- kn * sum((gfunction((1:kn)/kn) - gfunction(( (1:kn) - 1 )/kn ) )^2)

  psi2kn <- 1 / kn * sum(gfunction((1:kn)/kn)^2)

  r1 <- hatreturn(pdata,kn=kn)
  rdata <- makeReturns(pdata)
  crv <- 1 / (sqrt(N) * theta * psi2kn) * sum(r1^2, na.rm = TRUE) - psi1kn  *(1/N) / (2 * theta^2 * psi2kn) * sum(rdata^2, na.rm=TRUE)
  return(crv)
}

#' @keywords internal
hatreturn <- function(pdata, kn) {
  rdata <- makeReturns(pdata)
  class(rdata) <- "zoo"
  kn <- as.numeric(kn)
  if (kn == 1) {
    hatre <- rdata
  } else {
    x <- (1:(kn-1)) / kn
    x[x > (1-x)] <- (1-x)[x > (1-x)]
    weightedsum <- function(series){
      return(sum(x * series))
    }
    hatre <- rollapply(rdata, width = kn - 1, FUN = weightedsum, align = "left")
    if (sum(is.na(hatre)) > 0) {
      hatre[is.na(hatre)] <- rdata[is.na(hatre)]
    }
  }
  return(hatre)
}

#' @keywords internal
gfunction <- function(x) {
  # returns the minimum of x and 1-x
  # whenevr x > 1-x , replace with 1-x
  x[x > (1-x)] <- (1-x)[x > (1-x)]
  return(x)
}

#' @keywords internal
preavbi <- function(pdata1, pdata2) {
  x <- refreshTime(list(pdata1, pdata2))
  newprice1 <- x[, 1]
  newprice2 <- x[, 2]

  N <- nrow(x)
  theta <- 0.8 ##recommendation by Hautsch and Podolskij
  kn <- floor(theta * sqrt(N))

  ##psi:
  psi1 <- 1
  psi2 <- 1/12

  psi1kn <- kn* sum((gfunction((1:kn)/kn) - gfunction(((1:kn) - 1 )/kn))^2)

  psi2kn <- 1 / kn * sum(gfunction((1:kn) / kn)^2)

  r1 <- hatreturn(newprice1, kn = kn)
  r2 <- hatreturn(newprice2, kn = kn)

  mrc <- N / (N - kn + 2) * 1 / (psi2 * kn) * (sum(r1 * r2, na.rm = TRUE))

  return(mrc)
}


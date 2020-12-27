#' @keywords internal
crv <- function(pData) {
  N <- nrow(pData)
  theta <- 0.8 ##recommendation by Hautsch and Podolskij
  kn <- floor(theta*sqrt(N))

  ##psi:
  psi1 <- 1
  psi2 <- 1/12

  psi1kn <- kn * sum((gfunction((1:kn)/kn) - gfunction(( (1:kn) - 1 )/kn ) )^2)

  psi2kn <- 1 / kn * sum(gfunction((1:kn)/kn)^2)

  r1 <- hatreturn(pData,kn=kn)
  rData <- makeReturns(pData)
  crv <- 1 / (sqrt(N) * theta * psi2kn) * sum(r1^2, na.rm = TRUE) - psi1kn  *(1/N) / (2 * theta^2 * psi2kn) * sum(rData^2, na.rm=TRUE)
  return(crv)
}

#' @keywords internal
hatreturn <- function(pData, kn) {
  rData <- makeReturns(pData)
  class(rData) <- "zoo"
  kn <- as.numeric(kn)
  if (kn == 1) {
    hatre <- rData
  } else {
    x <- (1:(kn-1)) / kn
    x[x > (1-x)] <- (1-x)[x > (1-x)]
    hatre <- preAveragingReturnsInternal(coredata(rData), kn)
    if (sum(is.na(hatre)) > 0) {
      hatre[is.na(hatre)] <- rData[is.na(hatre)]
    }
  }
  return(hatre)
}



#' @keywords internal
gfunction <- function(x) {
  # returns the minimum of x and 1-x
  # whenever x > 1-x , replace with 1-x
  x[x > (1-x)] <- (1-x)[x > (1-x)]
  return(x)
}

#' @keywords internal
preavbi <- function(pData1, pData2) {
  x <- refreshTime(list(pData1, pData2))
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


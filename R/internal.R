#' @importFrom zoo zoo na.locf
#' @importFrom stats start end
#' @keywords internal
fastTickAgregation <- function (ts, on = "minutes", k = 1, tz = "GMT") {
  
  if (on == "secs" | on == "seconds") {
    secs <- k
    tby <- paste(k, "sec", sep = " ")
  } 
  if (on == "mins" | on == "minutes") {
    secs <- 60 * k
    tby <- paste(60 * k, "sec", sep = " ")
  } 
  if (on == "hours"){
    secs <- 3600 * k
    tby <- paste(3600 * k, "sec", sep = " ")
  } 
  
  g <- base::seq(start(ts), end(ts), by = tby)
  rawg <- as.numeric(as.POSIXct(g, tz = tz))
  newg <- rawg + (secs - rawg %% secs)
  g    <- as.POSIXct(newg, origin = "1970-01-01", tz = tz)
  ts <- na.locf(merge(ts, zoo(NULL, g)))[as.POSIXct(g, tz = tz)]
  return(ts)
}

# system.time(aggregateTS(sampleTData$PRICE, k = 5))
# # 
# system.time({blub <- sampleTData$PRICE[endpoints(index(sampleTData$PRICE), on = "minutes", k = 5), ]
# blub <- xts(blub, order.by = ceiling_date(index(blub), unit = paste(5, "minutes")))})
# blub[1]
# 
# 
# if (index(blub[dim(blub)[1] - 1, ]) == index(blub[dim(blub)[1], ])) {
#   blub <- blub[-(dim(blub)[1] - 1), ]
# }
# 
# as.xts(index = )

### Do a daily apply but with list as output:
#' @importFrom xts try.xts
#' @importFrom xts endpoints
#' @keywords internal
applyGetList <- function(x, FUN, cor = FALSE, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE, makePsd = NULL,...){
  on <- "days" 
  k <- 1
  x <- try.xts(x, error = FALSE)
  INDEX <- endpoints(x, on = on, k = k)
  D <- length(INDEX)-1
  result <- list()
  FUN <- match.fun(FUN)
  for(i in 1:(length(INDEX)-1)){
    if (is.null(makePsd)) {
      result[[i]] <- FUN(x[(INDEX[i] + 1):INDEX[i + 1]], cor, alignBy, alignPeriod, makeReturns)
    } else {
      result[[i]] <- FUN(x[(INDEX[i] + 1):INDEX[i + 1]], cor, alignBy, alignPeriod, makeReturns, makePsd)
    }
    
  }
  return(result)
}

#' Returns the positive semidinite projection of a symmetric matrix using the eigenvalue method
#' 
#' @description Function returns the positive semidinite projection of a symmetric matrix using the eigenvalue method.
#' 
#' @param S matrix.
#' @param method character, indicating whether the negative eigenvalues of the correlation or covariance should be replaced by zero. Possible values are "covariance" and "correlation".
#' 
#' @details We use the eigenvalue method to transform \eqn{S} into a positive
#' semidefinite covariance matrix (see e.g. Barndorff-Nielsen and Shephard, 2004, and Rousseeuw and Molenberghs, 1993).  Let \eqn{\Gamma} be the
#' orthogonal matrix consisting of the \eqn{p} eigenvectors of \eqn{S}. Denote
#' \eqn{\lambda_1^+,\ldots,\lambda_p^+} its \eqn{p} eigenvalues, whereby the negative eigenvalues have been replaced by zeroes.
#' Under this approach, the positive semi-definite
#' projection of \eqn{S} is \eqn{ S^+ = \Gamma' \mbox{diag}(\lambda_1^+,\ldots,\lambda_p^+) \Gamma}. 
#' 
#' If method = "correlation", the eigenvalues of the correlation matrix corresponding to the matrix \eqn{S} are 
#' transformed. See Fan et al (2010).  
#' 
#' @return An xts object containing the aggregated trade data.
#'
#' @references 
#' Barndorff-Nielsen, O. and N. Shephard (2004). Measuring the impact of jumps in multivariate price processes using bipower covariation. Discussion paper, Nuffield College, Oxford University.
#' Fan, J., Y. Li, and K. Yu (2010). Vast volatility matrix estimation using high frequency data for portfolio selection. Working paper.
#' Rousseeuw, P. and G. Molenberghs (1993). Transformation of non positive semidefinite correlation matrices. Communications in Statistics - Theory and Methods 22, 965-984.
#' 
#' @author Jonathan Cornelissen and Kris Boudt
#' @keywords data manipulation
#' @export
makePsd <- function(S, method = "covariance") {
  if (method == "correlation" & !any(diag(S) <= 0) ) {
    # Fan, J., Y. Li, and K. Yu (2010). Vast volatility matrix estimation using high frequency data for portfolio selection.
    D <- matrix(diag(S)^(1/2), ncol=1)
    R <- S / (D %*% t(D))
    out <- eigen(x = R , symmetric = TRUE)
    mGamma <- t(out$vectors)
    vLambda <- out$values
    vLambda[vLambda<0] <- 0
    Apsd  <- t(mGamma)%*%diag(vLambda)%*%mGamma
    dApsd <- matrix(diag(Apsd)^(1/2),ncol=1)
    Apsd  <- Apsd/(dApsd%*%t(dApsd))
    D     <- diag( as.numeric(D)  , ncol = length(D) )
    Spos  <- D %*% Apsd %*% D
    return(Spos)
  } else {
    # Rousseeuw, P. and G. Molenberghs (1993). Transformation of non positive semidefinite correlation matrices. Communications in Statistics - Theory and Methods 22, 965-984.
    out     <- eigen(x = S , symmetric = TRUE)
    mGamma  <- t(out$vectors)
    vLambda <- out$values
    vLambda[vLambda<0] <- 0
    Apsd    <- t(mGamma) %*% diag(vLambda) %*% mGamma
  }
}

#' @importFrom xts is.xts
#' @importFrom xts ndays
#' @keywords internal
isMultiXts <- function(x, y = NULL) { 
  if (is.null(y)) {
    test <- is.xts(x) && (ndays(x)!=1)
    return(test)
  } else {
    test <- (is.xts(x) && (ndays(x)!=1)) || (ndays(y)!=1 && is.xts(y))
    if (test) {
      isEqualDimensions <- (dim(y) == dim(x))
      if (isEqualDimensions) { 
        test <- list(TRUE, cbind(x,y))
        return(test) 
      } else {
          warning("Please make sure x and y have the same dimensions") 
      }
    } 
  } 
} 

#' @importFrom stats weighted.mean
#' @keywords internal
weightedaverage <- function(a){
  aa <- as.vector(as.numeric(a[,1]))
  bb <- as.vector(as.numeric(a[,2]))
  c  <- weighted.mean(aa,bb)
  return(c)
}

#' @keywords internal
previoustick <- function(a) {
  a <- as.vector(a)
  b <- a[length(a)]
  return(b)
}

#' @importFrom xts reclass
#' @keywords internal
periodApply2 <- function (x, INDEX, FUN2, ...) {
  x <- try.xts(x, error = FALSE)
  FUN <- match.fun(FUN2)
  xx <- sapply(1:(length(INDEX) - 1), function(y) {
    FUN(x[(INDEX[y] + 1):INDEX[y + 1]], ...)
  })
  reclass(xx, x[INDEX])
}

#' @importFrom xts is.xts
#' @importFrom xts ndays
#' @keywords internal
checkMultiDays <- function(x) { 
  
  if ((is.matrix(x) | is.numeric(x)) & !is.xts(x)) {
    return(FALSE)
  }
  if (!is.xts(x)) {
    stop("Please provide xts-object or simple numeric vector.")
  }
  if (ndays(x) != 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
} 

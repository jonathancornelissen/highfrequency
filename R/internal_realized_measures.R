#' @keywords internal
refreshTime <- function (pdata) {
  dim <- length(pdata)
  lengths <- rep(0, dim + 1)
  for (i in 1:dim) {
    lengths[i + 1] <- length(pdata[[i]])
  }
  minl <- min(lengths[(2:(dim + 1))])
  lengths <- cumsum(lengths)
  alltimes <- rep(0, lengths[dim + 1])
  for (i in 1:dim) {
    alltimes[(lengths[i] + 1):lengths[i + 1]] <- as.numeric(as.POSIXct(index(pdata[[i]]), tz = "GMT"))
  }
  # x <- .C("refreshpoints", as.integer(alltimes), as.integer(lengths),
  #        as.integer(rep(0, minl)), as.integer(dim), as.integer(0),
  #        as.integer(rep(0, minl * dim)), as.integer(minl), PACKAGE="highfrequency")
  print("not correct yet - look out!!!")
  x <- c(1:100)
  newlength <- x[[5]]
  pmatrix <- matrix(ncol = dim, nrow = newlength)
  for (i in 1:dim) {
    selection <- x[[6]][((i - 1) * minl + 1):(i * minl)]
    pmatrix[, i] <- pdata[[i]][selection[1:newlength]]
  }
  time <- as.POSIXct(x[[3]][1:newlength], origin = "1970-01-01", tz = "GMT")
  resmatrix <- xts(pmatrix, order.by = time)
  return(resmatrix)
}
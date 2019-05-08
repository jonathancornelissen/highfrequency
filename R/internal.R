
# Aggregation function: FAST previous tick aggregation
#' @importFrom zoo zoo
#' @importFrom zoo na.locf
#' @keywords internal
.aggregatets = function (ts, on = "minutes", k = 1) {
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
  rawg <- as.numeric(as.POSIXct(g, tz = "GMT"))
  newg <- rawg + (secs - rawg%%secs)
  g    <- as.POSIXct(newg, origin = "1970-01-01",tz = "GMT")
  ts3 <- na.locf(merge(ts, zoo(NULL, g)))[as.POSIXct(g, tz = "GMT")]
  return(ts3)
} #Very fast and elegant way to do previous tick aggregation :D!

#' @importFrom xts xts
#' @importFrom zoo index
#' @keywords internal
makeReturns <- function (ts) {
  l <- dim(ts)[1]
  x <- matrix(as.numeric(ts), nrow = l)
  x[(2:l), ] <- log(x[(2:l), ]) - log(x[(1:(l - 1)), ])
  x[1, ] <- rep(0, dim(ts)[2])
  x <- xts(x, order.by = index(ts))
  return(x)
}

#' @importFrom xts is.xts
#' @importFrom xts ndays
#' @keywords internal
.multixts <- function(x, y = NULL) { 
  if(is.null(y)){
    test <- is.xts(x) && (ndays(x)!=1);
    return(test);
  }
  if(!is.null(y)){
    test <- (is.xts(x) && (ndays(x)!=1)) || (ndays(y)!=1 && is.xts(y))
    if (test == TRUE){
      test1 = dim(y) == dim(x)
      if (!test1) { 
        warning("Please make sure x and y have the same dimensions")
        }
      if (test1 == TRUE) {  
        test = list(TRUE, cbind(x,y))
        return(test) 
      }
    } 
  } 
} 
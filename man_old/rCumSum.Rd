\name{rCumSum}
\alias{rCumSum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plot cummulative returns }
\description{
Plots cummulative returns at a certain alignment given a return series.
}
\usage{
rCumSum(x, period = 1, align.by="seconds",align.period = 1, 
  plotit = FALSE, type = "l", cts = TRUE, makeReturns = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{x}{Tick data in xts object.}
  \item{period}{ Sampling period }
  \item{align.by}{Align the tick data to seconds|minutes|hours}
  \item{align.period}{ Align the returns to this period first}
  \item{plotit}{ T for plot }
    \item{type}{ Line or points}
  \item{cts}{ Create calendar time sampling if a non realizedObject is passed}
  \item{makeReturns}{ Prices are passed make them into log returns}

  
}
\value{
Cummulative return vector if plotit = F
}

\author{ Scott Payseur <scott.payseur@gmail.com>}

\examples{
data(sbux.xts)

cumm <- list() 
cumm[[1]] <- rCumSum(sbux.xts, period=1, align.by="seconds", align.period=60) 
cumm[[2]] <- rCumSum(sbux.xts, period=10, align.by="seconds", align.period=60) 
cumm[[3]] <- rCumSum(sbux.xts, period=20, align.by="seconds", align.period=60) 
cumm[[4]] <- rCumSum(sbux.xts, period=30, align.by="seconds", align.period=60) 
plot(cumm[[1]], xlab="", ylab="Cumulative Ruturns", main="Starbucks (SBUX)", 
   sub='20110701', type="p", col=16, lwd=2) 
lines(cumm[[2]], col=2, lwd=2) 
lines(cumm[[3]], col=3, lwd=2) 
lines(cumm[[4]], col=4, lwd=2)
}
\keyword{methods}

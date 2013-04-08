\name{rScatterReturns}
\alias{rScatterReturns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Scatterplot of aligned returns }
\description{
Creates a scatterplot of cross returns.
}
\usage{
rScatterReturns(x,y, period, align.by="seconds", align.period=1,numbers=FALSE,xlim= NULL, ylim=NULL, plotit=TRUE, pch=NULL, cts=TRUE, makeReturns=FALSE, scale.size=0, col.change=FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{x}{Tick data in xts object.}
  \item{y}{Tick data in xts object.}
  \item{period}{ Sampling period }
   \item{align.by}{Align the tick data to seconds|minutes|hours}
  \item{align.period}{ Align the returns to this period first}
  \item{cts}{ Create calendar time sampling if a non realizedObject is passed}
  \item{makeReturns}{ Prices are passed make them into log returns}
  \item{plotit}{ T for plot }
  \item{numbers}{ T for count }
  \item{pch}{type of point}
  \item{ylim}{ylimit}
  \item{xlim}{xlimit}
  \item{scale.size}{.}
  \item{col.change}{.}
    \item{...}{ ...}
}
\details{
Scatterplot of returns.
}

\references{
S. W. Payseur.  A One Day Comparison of Realized Variance and Covariance Estimators. \emph{Working Paper: University 
of Washington}, 2007
}
\author{ Scott Payseur <scott.payseur@gmail.com>}


\examples{
data(sbux.xts)
data(lltc.xts)
par(mfrow=c(2,1))
rScatterReturns(sbux.xts,y=lltc.xts, period=1, align.period=20,ylab="LLTC",xlab="SBUX",numbers=FALSE) 
rScatterReturns(sbux.xts,y=lltc.xts, period=1, align.period=20,ylab="LLTC",xlab="SBUX",numbers=TRUE) 
}
\keyword{methods}
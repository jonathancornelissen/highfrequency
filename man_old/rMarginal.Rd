\name{rMarginal}
\alias{rMarginal}

\title{ Maginal Contribution to Realized Estimate }
\description{
Plots the marginal contribution to the realized estimate. 
}
\usage{
rMarginal(x, y = NULL, period,align.by="seconds", align.period = 1, 
  plotit = FALSE, cts = TRUE,makeReturns = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{x}{  RealizedObject or TimeSeries for S+}
  \item{y}{  RealizedObject or TimeSeries for S+}
  \item{period}{ Sampling period }
  \item{align.by}{Align the tick data to seconds|minutes|hours}
  \item{align.period}{ Align the returns to this period first}
  \item{plotit}{ T for plot }
  \item{cts}{ Create calendar time sampling if a non realizedObject is passed}
  \item{makeReturns}{ Prices are passed make them into log returns}
}
\details{
Plots the marginal contribution to the realized estimate.  This is a good tool to determine what obersations are adding (possibly subtracting for covariance) to the estimate.  
}
\value{
Marginal contribution vector if plotit = F
}
\references{
S. W. Payseur.  A One Day Comparison of Realized Variance and Covariance Estimators. \emph{Working Paper: University 
of Washington}, 2007
}
\author{ Scott Payseur <scott.payseur@gmail.com}
\seealso{ \code{\link{rAccumulation}} }

\examples{
data(sbux.xts)
par(mfrow=c(2,1))
plot(rCumSum(sbux.xts, period=10, align.by="seconds", align.period=60), 
   xlab="", ylab="Cumulative Ruturns", main="Starbucks (SBUX)",
    sub='20110701', type="p")
barplot(rMarginal(sbux.xts, period=10, align.by="seconds", align.period=60)$y,
    main="Marginal Contribution Plot") 
}
\keyword{methods}

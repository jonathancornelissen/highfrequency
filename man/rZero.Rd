\name{rZero}
\alias{rZero}
\title{ Calculates the percentage of co-zero returns at a specified sampling period }
\description{
Calculates the percentage of co-zero returns at a specified sampling period.
}
\usage{
rZero(rdata, period=1, align.by="seconds", align.period = 1, cts = TRUE, 
   makeReturns = FALSE, ...)
}

\arguments{
  \item{rdata}{ an xts object containing the tick data or a list. In case of list: each list-item i contains an xts object with the intraday data of stock i for day t.}
  \item{period}{ Sampling period }
  \item{align.by}{Align the tick data to seconds|minutes|hours}
  \item{align.period}{ Align the returns to this period first}
  \item{cts}{ Create calendar time sampling if a non realizedObject is passed}
  \item{makeReturns}{ Prices are passed make them into log returns}
   \item{...}{ ...}
}
\value{
Percentage of co-zero returns.
}
\references{
S. W. Payseur.  A One Day Comparison of Realized Variance and Covariance Estimators. \emph{Working Paper: University 
of Washington}, 2007
}
\author{ Scott Payseur <scott.payseur@gmail.com>}

\examples{
data(sbux.xts)
data(lltc.xts)
rZero( rdata = list(sbux.xts, lltc.xts) , period = 60, align.by ="seconds", align.period=1)
}

\keyword{methods}

\name{rAVGCov}
\alias{rAVGCov}

\title{Realized Covariance: Average Subsample}
\description{
Realized Covariance using average subsample.
}
\usage{
rAVGCov(rdata, cor = FALSE, period = 1, align.by = "seconds", 
        align.period = 1, cts = TRUE, makeReturns = FALSE, ...) 
}
\arguments{  
  \item{rdata}{In the multivariate case: a list. Each list-item i contains an xts object with the intraday data of stock i for day t. In the univariate case: an xts object containing the (tick) data for one day.}
  \item{cor}{boolean, in case it is TRUE, the correlation is returned. FALSE by default.}  
  \item{period}{ Sampling period }
  \item{align.by}{Align the tick data to seconds|minutes|hours}
  \item{align.period}{Align the tick data to this many [seconds|minutes|hours]}
  \item{cts}{ Create calendar time sampling if a non realizedObject is passed}
  \item{makeReturns}{ Prices are passed make them into log returns}
  \item{...}{ ...}
}
\value{
Realized covariance using average subsample.
}
\references{ 
L. Zhang, P.A Mykland, and Y. Ait-Sahalia. A tale of two time scales: Determining integrated volatility
with noisy high-frequency data. \emph{Journal of the American Statistical Association}, 2005.

Michiel de Pooter, Martin Martens, and Dick van Dijk. Predicting the daily covariance matrix for sp100
stocks using intraday data - but which frequency to use? \emph{Econometric Reviews}, 2008.
 }
 
\author{ Scott Payseur <scott.payseur@gmail.com> }

\examples{ 
 # Average subsampled realized variance/covariance for CTS aligned at one minute returns at 
 # 5 subgrids (5 minutes).
 data(sample_tdata); 
 data(lltc.xts);
 data(sbux.xts);
 
 # Univariate
 rvSub = rAVGCov( rdata = sample_tdata$PRICE, period = 5, align.by ="minutes", 
                   align.period=5, makeReturns=TRUE); 
 rvSub
 
 # Multivariate:
 rcSub = rAVGCov( rdata = list(lltc.xts,sbux.xts), period = 5, align.by ="minutes", 
                   align.period=5, makeReturns=FALSE); 
 rcSub
}

\keyword{volatility}

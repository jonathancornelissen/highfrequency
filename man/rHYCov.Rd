\name{rHYCov} 
\alias{rHYCov} 
\title{Hayashi-Yoshida Covariance} 
\description{Hayashi-Yoshida Covariance} 
\usage{ 
rHYCov(rdata, cor = FALSE, period = 1, align.by = "seconds", 
       align.period = 1, cts = TRUE, makeReturns = FALSE, makePsd = TRUE, ...)
} 

\arguments{
  \item{rdata}{ a list. Each list-item i contains an xts object with the intraday data of stock i   for day t.}
  \item{cor}{ boolean, in case it is TRUE, the correlation is returned. FALSE by default.}
  \item{period}{ Sampling period }
  \item{align.by}{Align the tick data to seconds|minutes|hours}
  \item{align.period}{Align the tick data to this many [seconds|minutes|hours]}
  \item{cts}{ Create calendar time sampling if a non realizedObject is passed}
  \item{makeReturns}{ Prices are passed make them into log returns}
  \item{makePsd}{ boolean, in case it is TRUE, the positive definite version of rTSCov is returned. FALSE by default.}
  \item{...}{ ...}
}  

\references{ T. Hayashi and N. Yoshida. On covariance estimation of non-synchronously observed diffusion processes.
\emph{Bernoulli}, 11:359-379, 2005. }
\author{ Scott Payseur }
\examples{
 ## Average Realized Kernel Variance/Covariance for CTS aligned at one minute returns at 
 ## 5 subgrids (5 minutes).
# data(lltc.xts); 
# data(sbux.xts); 
 ## Multivariate:
# rHYCov = rHYCov( rdata = list(lltc.xts,sbux.xts), period = 5, align.by ="minutes", 
#                   align.period=5, makeReturns=FALSE); 
# rHYCov 
 ##Note: for the diagonal elements the rCov is used.
} 

\keyword{volatility}
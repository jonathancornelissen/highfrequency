\name{rKernelCov}  
\alias{rKernelCov} 

\title{Realized Covariance: Kernel} 

\description{ 
Realized covariance calculation using a kernel estimator. 
}  

\usage{ 
rKernelCov(rdata, cor = FALSE, kernel.type = "rectangular", kernel.param = 1, 
            kernel.dofadj = TRUE, align.by = "seconds", align.period = 1, 
            cts = TRUE, makeReturns = FALSE, type = NULL, adj = NULL, 
            q = NULL, ...) 
} 

\arguments{ 
  \item{rdata}{In the multivariate case: a list. Each list-item i contains an xts object with the intraday data of stock i for day t. In the univariate case: an xts object containing the (tick) data for one day.}
  \item{cor}{boolean, in case it is TRUE, the correlation is returned. FALSE by default.}  
  \item{kernel.type}{Kernel name (or number)}
  \item{kernel.param}{Kernel parameter (usually lags)}
  \item{kernel.dofadj}{Kernel Degree of freedom adjustment}
  \item{align.by}{Align the tick data to seconds|minutes|hours}
  \item{align.period}{Align the tick data to this many [seconds|minutes|hours]}
  \item{cts}{Calendar Time Sampling is used}
  \item{makeReturns}{Convert to Returns}
  \item{type}{Deprecated, use kernel.type}
  \item{adj}{Deprecated, use kernel.dofadj}
  \item{q}{Deprecated, use kernel.param}
  \item{...}{...}
}

\details{
The different types of kernels can be found using \code{\link{rKernel.available}}.
}
\value{
Kernel estimate of realized covariance.
}
\references{
Ole E. Barndorff-Nielsen, Peter Reinhard Hansen, Asger Lunde, and Neil Shephard. Regular and modified kernel-based estimators of integrated variance: The case with independent noise. \emph{Working Paper}, 2004.

B. Zhou. High-frequency data and volatility in foreign-exchange rates. \emph{Journal of Buiness & Economic Statistics}, 14:45-52, 1996.

P. Hansen and A. Lunde. Realized variance and market microstructure noise. \emph{Journal of Business and Economic Statistics}, 24:127-218, 2006.}

\author{Scott Payseur <scott.payseur@gmail.com>}

\seealso{\code{\link{rKernel.available}}}

\examples{ 
 # Average Realized Kernel Variance/Covariance for CTS aligned at one minute returns at 
 # 5 subgrids (5 minutes).
 data(sample_tdata); 
 data(lltc.xts); 
 data(sbux.xts); 
 
 # Univariate: 
 rvKernel = rKernelCov( rdata = sample_tdata$PRICE, period = 5, align.by ="minutes", 
                   align.period=5, makeReturns=TRUE); 
 rvKernel 
 
 # Multivariate:
 rcKernel = rKernelCov( rdata = list(lltc.xts,sbux.xts), period = 5, align.by ="minutes", 
                   align.period=5, makeReturns=FALSE); 
 rcKernel 
}

\keyword{volatility}

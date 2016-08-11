\name{rSkew}
\alias{rSkew}
\title{
Realized skewness of highfrequency return series.
}
\description{
 
Function returns Realized skewness, defined in Amaya et al. (2011).

Assume there is \eqn{N} equispaced returns in period \eqn{t}. Let \eqn{r_{t,i}} be a return (with \eqn{i=1, \ldots,N}) in period \eqn{t}.

Then, the rSkew is given by
\deqn{
\mbox{rSkew}_{t}= \frac{\sqrt{N} \sum_{i=1}^{N}(r_{t,i})^3}{RV_{t}^{3/2}} 
}

in which
\eqn{RV_{t}:} realized variance
}

\usage{
rSkew (rdata,align.by=NULL,align.period=NULL,makeReturns=FALSE,...)
}

\arguments{
  \item{rdata}{a zoo/xts object containing all returns in period t for one asset.}
  \item{align.by}{a string, align the tick data to "seconds"|"minutes"|"hours"}
  \item{align.period}{an integer, align the tick data to this many [seconds|minutes|hours].}  
  \item{makeReturns}{boolean, should be TRUE when rdata contains prices instead of returns. FALSE by   default.} 
  \item{...}{additional arguments.}
}

\value{
numeric
}
\references{
Amaya, D., Christoffersen, P., Jacobs, K. and Vasquez, A. (2011). Do realized skewness and kurtosis predict the cross-section of equity returns?. CREATES research paper. p. 3-7.
}
\author{
Giang Nguyen, Jonathan Cornelissen and Kris Boudt
}

\examples{
data(sample_tdata)
rSkew(sample_tdata$PRICE,align.by ="minutes", align.period =5, makeReturns = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ highfrequency }
\keyword{ rSkew }

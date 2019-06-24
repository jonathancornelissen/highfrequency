\name{rQuar}
\alias{rQuar}
\title{
Realized quarticity of highfrequency return series. 
}
\description{
 
Function returns the rQuar, defined in Andersen et al. (2012).

Assume there is \eqn{N} equispaced returns in period \eqn{t}. Let \eqn{r_{t,i}} be a return (with \eqn{i=1, \ldots,N}) in period \eqn{t}.

Then, the rQuar is given by
\deqn{
\mbox{rQuar}_{t}=\frac{N}{3} \sum_{i=1}^{N} \mbox(r_{t,i}^4)
}
}
\usage{
rQuar (rdata, align.by=NULL, align.period=NULL, makeReturns=FALSE,...)
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
Andersen, T. G., D. Dobrev, and E. Schaumburg (2012). Jump-robust volatility estimation using nearest neighbor truncation. Journal of Econometrics, 169(1), 75- 93.
}
\author{
Giang Nguyen, Jonathan Cornelissen and Kris Boudt
}

\examples{
data(sample_tdata)
rQuar(rdata= sample_tdata$PRICE, align.by= "minutes", align.period =5, makeReturns= TRUE)
rQuar
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ highfrequency }
\keyword{ rQuar}

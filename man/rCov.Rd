\name{rCov}
\Rdversion{1.1}
\alias{rCov}
\title{
Realized Covariance
}

\description{
Function returns the Realized Covariation (rCov).

Let \eqn{r_{t,i}} be an intraday \eqn{N x 1} return vector and \eqn{i=1,...,M} 
the number of intraday returns.

Then, the rCov is given by
\deqn{
\mbox{rCov}_{t}=\sum_{i=1}^{M}r_{t,i}r'_{t,i}.
}
}

\usage{ 
rCov(rdata,cor = FALSE, align.by = NULL, align.period = NULL, makeReturns = FALSE, ...) 
} 

\arguments{ 
  \item{rdata}{ a \eqn{(M x N)} matrix/zoo/xts object containing the \eqn{N}
return series over period \eqn{t}, with \eqn{M} observations during \eqn{t}.}
  \item{cor}{ boolean, in case it is TRUE, the correlation is returned. FALSE by default.}
  \item{align.by}{a string, align the tick data to "seconds"|"minutes"|"hours".}
  \item{align.period}{an integer, align the tick data to this many [seconds|minutes|hours].}  
  \item{makeReturns}{boolean, should be TRUE when rdata contains prices instead of returns. FALSE by default.}
  \item{...}{additional arguments.}
} 

\value{
an \eqn{N x N} matrix
}

\author{Jonathan Cornelissen and Kris Boudt}

\examples{
 # Realized Variance/Covariance for CTS aligned   
 # at 5 minutes.
 data(sample_tdata); 
 data(sample_5minprices_jumps);
 
 # Univariate: 
 rv = rCov( rdata = sample_tdata$PRICE, align.by ="minutes", 
                    align.period =5, makeReturns=TRUE); 
 rv 
 
 # Multivariate: 
 rc = rCov( rdata = sample_5minprices_jumps['2010-01-04'], makeReturns=TRUE); 
 rc
}  

\keyword{volatility}
\name{heavyModel}  
\Rdversion{1.1}
\alias{heavyModel}
\title{HEAVY Model estimation}

\description{This function calculatest the High frEquency bAsed VolatilitY
(HEAVY) model proposed in Shephard and Sheppard (2010).} 

\usage{
heavyModel(data, p=matrix( c(0,0,1,1),ncol=2 ), q=matrix( c(1,0,0,1),ncol=2 ), 
          startingvalues = NULL, LB = NULL, UB = NULL, 
            backcast = NULL, compconst = FALSE);
}

\arguments{ 
   \item{data}{ a (T x K) matrix containing the data, with T the number of days. For the traditional HEAVY model: K = 2, the first column contains the squared daily demeaned returns, the second column contains the realized measures.
   }
   \item{p}{ a (K x K) matrix containing the lag length for the model innovations. Position (i, j) in the matrix indicates the number of lags in equation i of the model for the innovations in data column j. For the traditional heavy model p is given by matrix( c(0,0,1,1),ncol=2 ) (default). 
   }
   \item{q}{ 
   a (K x K) matrix containing the lag length for the conditional variances. Position (i, j) in the matrix indicates the number of lags in equation i of the model for conditional variances corresponding to series j. For the traditionalheavy model introduced above q is given by matrix( c(1,0,0,1),ncol=2 ) (default).
   }
   \item{startingvalues}{ a vector containing the starting values to be used in the optimizat  ion to find the optimal parameters estimates.
   }
   \item{LB}{ a vector of length K indicating the lower bounds to be used in the esti-
mation. If NULL it is set to a vector of zeros by default.
   }
   \item{UB}{ a vector of length K indicating the upper bounds to be used in the
estimation. If NULL it is set to a vector of Inf by default.}
   \item{backcast}{ a vector of length K used to initialize the estimation. If NULL the
unconditional estimates are taken.
   }
   \item{compconst}{ a boolean variable. In case TRUE, the omega values are estimated in
the optimization. In case FALSE, volatility targeting is done and omega is just 
1 minus the sum of all relevant alpha's and beta's multiplied by the unconditional variance.
   }
}

\section{Details}{ 
See vignette.
NOTE: The implementation of the heavyModel is not completely finished. For the moment only bound constraints on the parameters are imposed in the optimization. Future developments also include outputting standard errors, and a c-implementation of the likelihood function to speed up the QML estimation.
}

\value{ 
A list with the following values:
(i) loglikelihood: The log likelihood evaluated at the parameter estimates.
(ii) likelihoods: an xts object of length T containing the log likelihoods per day.
(iii) condvar: a (T x K) xts object containing the conditional variances
(iv) estparams: a vector with the parameter estimates. The order in which the
parameters are reported is as follows: First the estimates for omega then the
estimates for the non-zero alpha's with the most recent lags first in case max(p) > 1,
then the estimates for the non-zero beta's with the most recent lag first in case
max(q) > 1.
(v) convergence: an integer code indicating the successfulness of the optimization. See \verb{optim} for more information.
}

\references{
Shephard, N. and K. Sheppard (2010). Realising the future: forecasting with high
frequency based volatility (heavy) models. Journal of Applied Econometrics 25,
197-231. 
}

\examples{ 
 # Implementation of the heavy model on DJI:
 data("realized_library");
 returns =  realized_library$Dow.Jones.Industrials.Returns; 
 rk      =  realized_library$Dow.Jones.Industrials.Realized.Kernel; 
 returns = returns[!is.na(rk)];  rk = rk[!is.na(rk)]; # Remove NA's 
 data = cbind( returns^2, rk ); # Make data matrix with returns and realized measures
 backcast = matrix( c(var(returns),mean(rk)) ,ncol=1);
 
 startvalues = c(0.004,0.02,0.44,0.41,0.74,0.56); # Initial values
# output = heavyModel( data = as.matrix(data,ncol=2), compconst=FALSE, 
#                    startingvalues = startvalues, backcast=backcast); 
}

\author{Jonathan Cornelissen and Kris Boudt}
\keyword{forecasting}

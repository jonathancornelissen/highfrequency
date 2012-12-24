\name{makeReturns}
\Rdversion{1.1}
\alias{makeReturns}
\title{
Compute log returns
}

\description{
Function returns an xts object with the log returns as xts object.

\deqn{
\mbox{log return}_t =  (\log(\mbox{PRICE}_{t})-\log(\mbox{PRICE}_{t-1})).
}
}

\usage{
makeReturns(ts);
}

\arguments{
  \item{ts}{ xts object
}
}

\section{Details}{
Note: the first (row of) observation(s) is set to zero.
}

\value{
an xts object containing the log returns.
}

\author{ Jonathan Cornelissen and Kris Boudt}
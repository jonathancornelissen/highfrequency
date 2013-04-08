\name{mergeTradesSameTimestamp}
\Rdversion{1.1}
\alias{mergeTradesSameTimestamp}
\title{
Merge multiple transactions with the same time stamp
}
\description{
Function replaces multiple transactions that have the same time stamp by a single one and returns
an xts object with unique time stamps only.
}

\usage{
mergeTradesSameTimestamp(tdata,selection="median")
}

\arguments{
  \item{tdata}{ an xts object containing the time series data, with 
one column named "PRICE" indicating the transaction price 
and one column "SIZE" indicating the number of shares traded. }
  \item{selection}{indicates how the price for a certain time stamp
should be calculated in case of multiple observation for a certain time
stamp. By default, selection="median", and the median price is taken. Alternatively:
\itemize{
\item selection = "maxvolume": use the price of the transaction with
largest volume.
\item selection = "weightedaverage": take the weighted average of all prices.
}
}
}

\value{
xts object
}

\author{ Jonathan Cornelissen and Kris Boudt}

\keyword{ cleaning }
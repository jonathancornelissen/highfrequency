\name{rmTradeOutliers}
\Rdversion{1.1}
\alias{rmTradeOutliers}
\title{
Delete transactions with unlikely transaction prices
}
\description{
Function deletes entries with prices that are above the ask plus the bid-ask spread.
Similar for entries with prices below the bid minus the bid-ask spread.}

\usage{
rmTradeOutliers(tdata,qdata)
}

\arguments{
  \item{tdata}{ an xts object containing the time series data, with 
at least the column "PRICE", containing the transaction price.}
  \item{qdata}{ an xts object containing the time series data,
with at least the columns "BID" and "OFR", containing the bid and 
ask prices.
}
}

\section{Details}{
Note: in order to work correctly, the input data of this function should be
cleaned trade (tdata) and quote (qdata) data respectively.
}

\value{
xts object
}

\author{ Jonathan Cornelissen and Kris Boudt}
\keyword{ cleaning }
\name{mergeQuotesSameTimestamp}
\Rdversion{1.1}
\alias{mergeQuotesSameTimestamp}
\title{
Merge multiple quote entries with the same time stamp
}
\description{
Function replaces multiple quote entries that have the same time stamp 
by a single one and returns an xts object with unique time stamps only.
}

\usage{
mergeQuotesSameTimestamp(qdata,selection="median")
}

\arguments{
  \item{qdata}{ an xts object containing the time series data, with 
at least two columns named "BID" and "OFR" indicating the bid and ask price 
and two columns "BIDSIZ", "OFRSIZ" indicating the number of round lots available at these 
prices. }
  \item{selection}{indicates how the bid and ask price for a certain time stamp
should be calculated in case of multiple observation for a certain time
stamp. By default, selection="median", and the median price is taken. Alternatively:
\itemize{
\item selection = "maxvolume": use the (bid/ask) price of the entry with
largest (bid/ask) volume.
\item selection = "weightedaverage": take the weighted average of all bid (ask) prices,
weighted by "BIDSIZ" ("OFRSIZ").
}
}
}


\value{
xts object
}

\author{ Jonathan Cornelissen and Kris Boudt}

\keyword{ cleaning }
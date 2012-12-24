\name{aggregateQuotes}
\Rdversion{1.1}
\alias{aggregateQuotes}
\title{
Aggregate an xts object containing quote data}


\description{
Function returns an xts object containing the aggregated quote data with columns "SYMBOL", "EX", "BID","BIDSIZ","OFR","OFRSIZ". 
See \code{\link{sample_qdata}} for an example of the argument qdata.
}

\usage{
aggregateQuotes(qdata,on="minutes",k=5,marketopen,marketclose)
}

\arguments{
\item{qdata}{  xts object to be aggregated, containing the intraday quote data of a stock for one day. }
\item{on}{ character, indicating the time scale in which "k" is expressed. Possible values are: "secs", "seconds", "mins", "minutes","hours".}
\item{k}{ positive integer, indicating the number of periods to aggregate over. E.g. to aggregate a 
xts object to the 5 minute frequency, set k=5 and on="minutes".}
\item{marketopen}{the market opening time, by default: marketopen= "09:30:00". }
\item{marketclose}{the market closing time, by default: marketclose = "16:00:00". }
}

\section{Details}{

The output "BID" and "OFR" columns are constructed using previous tick aggregation.

The variables "BIDSIZ" and "OFRSIZ" are aggregated by taking the sum of the respective inputs over each interval.

For the variables "SYMBOL" and "EX" aggregation doesn't really make sense, 
thus the first value of the input is taken as the value for the complete output.

Column "MODE" is dropped because aggregating makes absolutely no sense.

The timestamps of the new time series are the closing times of the intervals. 

Please Note:
Returned objects always contain the first observation (i.e. opening quotes,...).
}

\value{
An xts object containing the aggregated quote data.
}

\author{Jonathan Cornelissen and Kris Boudt}
\keyword{data manipulation}

\examples{
#load data
data("sample_qdata");
#aggregate quote data to the 30 second frequency
xx = aggregateQuotes(sample_qdata,on="seconds",k=30);
head(xx);
}

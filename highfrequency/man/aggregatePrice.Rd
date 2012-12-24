\name{aggregatePrice}
\Rdversion{1.1}
\alias{aggregatePrice}
\title{
Aggregate a time series but keep first and last observation}

\description{
Function returns new time series as xts object where first observation is always the opening price
and subsequent observations are the closing prices over the interval with as endpoint the timestamp 
of the result.
}

\usage{
aggregatePrice(ts,FUN = previoustick,on="minutes",k=1,
marketopen,marketclose)
}

\arguments{
\item{ts}{ xts object to be aggregated, containing the intraday price series of a stock for one day.}
\item{FUN}{ function to apply over each interval. By default, previous tick aggregation is done.}
\item{on}{ character, indicating the time scale in which "k" is expressed. Possible values are: "secs", "seconds", "mins", "minutes","hours".}
\item{k}{ positive integer, indicating the number of periods to aggregate over. E.g. to aggregate a 
xts object to the 5 minute frequency set k=5 and on="minutes".}
\item{marketopen}{the market opening time, by default: marketopen = "09:30:00". }
\item{marketclose}{the market closing time, by default: marketclose = "16:00:00". }
}

\section{Details}{
The timestamps of the new time series are the closing times and/or days of the intervals. 

In case of previous tick aggregation, 
for on="seconds"/"minutes"/"hours",
 the element of the returned series with e.g. timestamp 09:35:00 contains 
the last observation up to that point, excluding the value at 09:35:00 itself. An exception 
is marketclose (i.e. 16:00:00 ET by default), where the observation at 16:00:00 is included in the interval, since this is the
end of a trading day at the NYSE.

Please input an object containing ONE day of data.
}

\value{
An xts object containing the aggregated time series.
}

\author{ Jonathan Cornelissen and Kris Boudt}
\keyword{data manipulation}

\examples{
#load data
data("sample_tdata");
#aggregate price data to the 30 second frequency
head(sample_tdata$PRICE)
head(aggregatePrice(sample_tdata$PRICE,on="secs",k=30));
}

\name{aggregateTrades}
\Rdversion{1.1}
\alias{aggregateTrades}
\title{Aggregate an xts object containing trade data}


\description{
Function returns an xts object containing the aggregated trade data with columns "SYMBOL", "EX", "PRICE", "SIZE". 
See \code{\link{sample_tdata}} for an example of the argument tdata.
}

\usage{
aggregateTrades(tdata,on="minutes",k=5,marketopen,marketclose)
}

\arguments{
\item{tdata}{ xts object to be aggregated, containing the intraday trade data of a stock for one day.}
\item{on}{ character, indicating the time scale in which "k" is expressed. Possible values are: "secs", "seconds", "mins", "minutes","hours".}
\item{k}{ positive integer, indicating the number of periods to aggregate over. E.g. to aggregate a 
xts object to the 5 minute frequency set k=5 and on="minutes".}
\item{marketopen}{the market opening time, by default: marketopen= "09:30:00". }
\item{marketclose}{the market closing time, by default: marketclose = "16:00:00". }
}

\section{Details}{
The output "PRICE" column is constructed using previous tick aggregation.

The variable "SIZE" is aggregated by taking the sum over each interval.

For the variables "SYMBOL" and "EX" aggregation doesn't really make sense, 
thus the first value of the input is taken as the value for the complete output.

Columns "COND", "CORR", "G127" are dropped because aggregating them makes absolutely no sense.

The timestamps of the new time series are the closing times of the intervals. 

Please Note:

Returned objects always contain the first observation (i.e. opening price,...).

Please input an object containing ONE day of data.
}

\value{
An xts object containing the aggregated trade data.
}


\author{ Jonathan Cornelissen and Kris Boudt}
\keyword{data manipulation}

\examples{
data("sample_tdata");
#aggregate trade data to 5 minute frequency
x = aggregateTrades(sample_tdata,on="minutes",k=5)
head(x);
}

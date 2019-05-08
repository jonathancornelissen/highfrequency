\name{refreshTime}
\Rdversion{1.1}
\alias{refreshTime}
\title{ Synchronize (multiple) irregular timeseries by refresh time}

\description{
This function implements the refresh time synchronization scheme proposed by Harris et al. (1995). 
It picks the so-called refresh times at which all assets have traded at least once since the last refresh time point. 
For example, the first refresh time corresponds to the first time at which all stocks have traded. 
The subsequent refresh time is defined as the first time when all stocks have again traded.
This process is repeated untill the end of one time series is reached.
}

\usage{
refreshTime(pdata)
}

\arguments{
\item{pdata}{ a list. Each list-item contains an xts object  
containing the original time series (one day only and typically a price series).}
}


\value{
An xts object containing the synchronized time series.
}

\references{
Harris, F., T. McInish, G. Shoesmith, and R. Wood (1995). Cointegration, error
correction, and price discovery on infomationally linked security markets. Journal
of Financial and Quantitative Analysis 30, 563-581.
}

\examples{
#suppose irregular timepoints:
start = as.POSIXct("2010-01-01 09:30:00")
ta = start + c(1,2,4,5,9);    	
tb = start + c(1,3,6,7,8,9,10,11);

#yielding the following timeseries:
a = as.xts(1:length(ta),order.by=ta);
b = as.xts(1:length(tb),order.by=tb);

#Calculate the synchronized timeseries:
refreshTime(list(a,b))
}

\author{Jonathan Cornelissen and Kris Boudt}
\keyword{data manipulation}
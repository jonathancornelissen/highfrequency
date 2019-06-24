\name{convert}
\Rdversion{1.1}
\alias{convert}
\title{Convert trade or quote data into xts object saved in the RData format}

\description{
Function converts both trade and quote data stored as "txt" or "csv" and
structured as illustrated in the pdf documentation into xts-objects and 
stores them in the "RData" format. 
Subsequently, the data can be loaded into the R console by \code{\link{TAQLoad}}.}

\usage{
convert(from,to,datasource,datadestination,trades=TRUE,quotes=TRUE,
        ticker,dir=FALSE,extension="txt",header=FALSE,
        tradecolnames=NULL,quotecolnames=NULL,
        format="\%Y\%m\%d \%H:\%M:\%S",onefile=FALSE)        
}

\arguments{
\item{from}{ first date to convert e.g. "2008-01-30".}
\item{to}{ last date to convert e.g. "2008-01-31".}
\item{datasource}{ folder in which the original data is stored.}
\item{datadestination}{ folder in which the converted data should be stored.}
\item{trades}{ boolean, determines whether trades are converted.}
\item{quotes}{ boolean, determines whether quotes are converted.}
\item{ticker}{ vector with tickers to be converted.}
\item{dir}{ boolean, if TRUE the datadestination folder and subfolders will be created automatically.}
\item{extension}{character, indicating the data format of the original data. Can be either "txt" or "csv". In case your data comes from "www.tickdata.com", set extension="tickdatacom".}
\item{header}{ boolean, indicating whether each data file contains a header.}
\item{tradecolnames}{ vector containing column names of your trade data. By default, 
the standard NYSE data format is taken, see pdf documentation for more details.}
\item{quotecolnames}{ vector containing column names of your quote data. By default, 
the standard NYSE data format is taken, see pdf documentation for more details.}
\item{format}{ character, indicates in what format TIME and DATE are recorded in the original data.
By default, \%Y\%m\%d \%H:\%M:\%S" is taken, which means the date is denoted by e.g. "20080130" and the time by e.g. "09:30:00".}
\item{onefile}{  
indicates the way the source data is stored. By default FALSE, which means the function expects that the source data is saved in a folder sturcture with a folder for each date (see vignette for more info). In case the data for multiple days is stored in one file, set onefile=TRUE. The naming convention for files is, e.g. for ticker="AAPL", "AAPL_trades.extension" and "AAPL_quotes.extension" for trades and quotes respectively.}
}

\value{For each day an xts object is saved into the folder of that date, containing the converted data.
This procedure is performed for each stock in "ticker". More information can be found in the pdf documentation.}


\author{ Jonathan Cornelissen and Kris Boudt}

\examples{
#In order for these examples to work, the folder datasource 
#should contain two folders named 2008-01-02 and 2008-01-03.
#These folder contain the files with the trade data,
#which are named "AAPL_trades.txt" or "AA_trades.txt".

from="2008-01-02";
to = "2008-01-03";
\dontrun{datasource=datadestination="C:\\data"

### txt files from NYSE:

convert(from,to,datasource,datadestination,trades=TRUE,
          quotes=FALSE,ticker=c("AA","AAPL"),dir=FALSE,extension="txt",
          header=FALSE,tradecolnames=NULL,quotecolnames=NULL,
          format="\%Y\%m\%d \%H:\%M:\%S");

#Now, the folder datadestination will contain two folders
#named 2008-01-02 and 2008-01-03 containing 
#the files AAPL_trades.RData and AAPL_trades.RData containing the trades.
#The data can now be loaded with the TAQLoad function.

########## Csv file from WRDS
#Suppose the datasource folder contains one csv file from WRDS 
#with data on IBM for multiple days.
#The file should be named "IBM_trades.csv" and can be easily converted into xts 
#and then saved in RData format by:

convert(from=from, to=to, datasource=datasource, datadestination=datadestination, 
    trades = T, quotes = T, ticker="IBM", dir = FALSE, extension = "csv", 
    header = TRUE, tradecolnames = NULL, quotecolnames = NULL, format = format, 
    onefile = TRUE )  

####### ASC file from www.tickdata.com
#Suppose the datasource folder contains asc files for trades and quotes 
#from "www.tickdata.com" for GLP. 
#The files "GLP_quotes.asc" and "GLP_trades.asc" should be saved in datasource folder.

 convert(from=from, to=to, datasource=datasource, datadestination=datadestination, 
    trades = T, quotes = T, ticker="GLP", dir = TRUE, extension = "tickdatacom", 
    header = TRUE, tradecolnames = NULL, quotecolnames = NULL, format = "%d/%m/%Y %H:%M:%OS", 
    onefile = TRUE );  
}
}

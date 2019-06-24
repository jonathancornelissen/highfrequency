\name{matchTradesQuotes}
\Rdversion{1.1}
\alias{matchTradesQuotes}
\title{Match trade and quote data}

\description{
Function matches the trades and quotes and returns an xts-object containing both.}

\usage{
matchTradesQuotes(tdata,qdata,adjustment=2)
}

\arguments{
\item{ tdata }{ xts-object containing the trade data. }
\item{ qdata }{ xts-object containing the quote data. }
\item{ adjustment }{ numeric, number of seconds the quotes are registered faster than
the trades (should be round and positive). Based on the research of
Vergote (2005), we set 2 seconds as the default.}
}

\value{xts-object containing the matched trade and quote data}

\references{
Vergote, O. (2005). How to match trades and quotes for NYSE stocks?
K.U.Leuven working paper.}

\author{ Jonathan Cornelissen and Kris Boudt}

\keyword{data manipulation}

\examples{
#load data samples
data("sample_tdata");
data("sample_qdata");
#match the trade and quote data
tqdata = matchTradesQuotes(sample_tdata,sample_qdata);
#have a look
head(tqdata);
}
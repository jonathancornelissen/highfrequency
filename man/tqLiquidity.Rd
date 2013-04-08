\name{tqLiquidity}
\Rdversion{1.1}
\alias{tqLiquidity}
\title{Calculate numerous (23) liquidity measures}

\description{
Function returns an xts object containing one of the following liquidity measures:

es, rs, value_trade, signed_value_trade,  
       di_diff, di_div, pes, prs, price_impact, prop_price_impact,  
       tspread, pts, p_return_sqr, p_return_abs, qs, pqs,  
       logqs, logsize, qslope, logqslope, mq_return_sqr,  
       mq_return_abs.
}

\usage{
tqLiquidity(tqdata,tdata,qdata,type,...)
}

\arguments{
\item{tqdata}{ xts object, containing joined trades and quotes (e.g. using \code{\link{matchTradesQuotes}})}
\item{tdata}{ xts-object containing the trade data.}
\item{qdata}{ xts-object containing the quote data.}
\item{type}{ a character vector containing the name of one of the above mentioned liquidity measures, e.g. type="es" to get the effective spread.}
\item{...}{additional arguments.}
}

\section{Details}{
The respective liquidity measures are defined as follows:
\itemize{
\item{es: effective spread}{
\deqn{
\mbox{effective spread}_t =  2*D_t*(\mbox{PRICE}_{t} - \frac{(\mbox{BID}_{t}+\mbox{OFR}_{t})}{2}),
}
where \eqn{D_t} is 1 (-1) if \eqn{trade_t} was buy (sell) (see Boehmer (2005), Bessembinder (2003)). 
Note that the input of this function consists of the matched trades and quotes,
 so this is were the time indication refers to (and thus not to the registered quote timestamp).
} 
\item{rs: realized spread}{ 
\deqn{
\mbox{realized spread}_t =  2*D_t*(\mbox{PRICE}_{t} - \frac{(\mbox{BID}_{t+300}+\mbox{OFR}_{t+300})}{2}),
}

where \eqn{D_t} is 1 (-1) if \eqn{trade_t} was buy (sell) (see Boehmer (2005), Bessembinder (2003)). 
Note that the time indication of \eqn{\mbox{BID}} and \eqn{\mbox{OFR}} refers 
to the registered time of the quote in seconds.
}
 
\item{value_trade: trade value}{ 
\deqn{
\mbox{trade value}_t =  \mbox{SIZE}_{t}*\mbox{PRICE}_{t}.
}
} 

\item{signed_value_trade: signed trade value}{
\deqn{
\mbox{signed trade value}_t =  D_t * (\mbox{SIZE}_{t}*\mbox{PRICE}_{t}),}
where \eqn{D_t} is 1 (-1) if \eqn{trade_t} was buy (sell) 
(see Boehmer (2005), Bessembinder (2003)).
}  

\item{di_diff: depth imbalance (as a difference)}{
\deqn{
\mbox{depth imbalace (as difference)}_t =  \frac{D_t *(\mbox{OFRSIZ}_{t}-\mbox{BIDSIZ}_{t})}{(\mbox{OFRSIZ}_{t}+\mbox{BIDSIZ}_{t})},
}
where \eqn{D_t} is 1 (-1) if \eqn{trade_t} was buy (sell) (see Boehmer (2005), Bessembinder (2003)). 
Note that the input of this function consists of the matched trades and quotes,
 so this is were the time indication refers to (and thus not to the registered quote timestamp).
 } 

\item{di_div: depth imbalace (as ratio)}{
\deqn{
\mbox{depth imbalace (as ratio)}_t =  (\frac{D_t *\mbox{OFRSIZ}_{t}}{\mbox{BIDSIZ}_{t}})^{D_t},
}
where \eqn{D_t} is 1 (-1) if \eqn{trade_t} was buy (sell) (see Boehmer (2005), Bessembinder (2003)). 
Note that the input of this function consists of the matched trades and quotes,
 so this is were the time indication refers to (and thus not to the registered quote timestamp).
} 

\item{pes: proportional effective spread}{
\deqn{
\mbox{proportional effective spread}_t =  \frac{\mbox{effective spread}_t}{(\mbox{OFR}_{t}+\mbox{BID}_{t})/2}
}
(Venkataraman, 2001).

Note that the input of this function consists of the matched trades and quotes,
 so this is were the time indication refers to (and thus not to the registered quote timestamp).
} 

\item{prs: proportional realized spread}{
\deqn{
\mbox{proportional realized spread}_t =  \frac{\mbox{realized spread}_t}{(\mbox{OFR}_{t}+\mbox{BID}_{t})/2}
}
(Venkataraman, 2001).

Note that the input of this function consists of the matched trades and quotes,
 so this is were the time indication refers to (and thus not to the registered 
} 

\item{price_impact: price impact}{
\deqn{
\mbox{price impact}_t =  \frac{\mbox{effective spread}_t - \mbox{realized spread}_t}{2}
}
(see Boehmer (2005), Bessembinder (2003)). 
}
 
\item{prop_price_impact: proportional price impact}{
\deqn{
\mbox{proportional price impact}_t =  \frac{\frac{(\mbox{effective spread}_t - \mbox{realized spread}_t)}{2}}{\frac{\mbox{OFR}_{t}+\mbox{BID}_{t}}{2}}
}
(Venkataraman, 2001).
Note that the input of this function consists of the matched trades and
quotes, so this is where the time indication refers to (and thus not to the
registered quote timestamp).
}  

\item{tspread: half traded spread}{
\deqn{
\mbox{half traded spread}_t =  D_t*(\mbox{PRICE}_{t} - \frac{(\mbox{BID}_{t}+\mbox{OFR}_{t})}{2}),
}
where \eqn{D_t} is 1 (-1) if \eqn{trade_t} was buy (sell) (see Boehmer (2005), Bessembinder (2003)). 
Note that the input of this function consists of the matched trades and quotes,
 so this is were the time indication refers to (and thus not to the registered quote timestamp).
} 

\item{pts: proportional half traded spread}{ 
\deqn{
\mbox{proportional half traded spread}_t =  \frac{\mbox{half traded spread}_t}{\frac{\mbox{OFR}_{t}+\mbox{BID}_{t}}{2}}.
}
Note that the input of this function consists of the matched trades and quotes,
 so this is were the time indication refers to (and thus not to the registered quote timestamp).
}
 
\item{p_return_sqr: squared log return on trade prices}{
\deqn{
\mbox{squared log return on Trade prices}_t =  (\log(\mbox{PRICE}_{t})-\log(\mbox{PRICE}_{t-1}))^2.
}
} 

\item{p_return_abs: absolute log return on trade prices}{
\deqn{
\mbox{absolute log return on Trade prices}_t =  |\log(\mbox{PRICE}_{t})-\log(\mbox{PRICE}_{t-1})|.
}
} 

\item{qs: quoted spread}{
\deqn{
\mbox{quoted spread}_t =  \mbox{OFR}_{t}-\mbox{BID}_{t}
}
Note that the input of this function consists of the matched trades and
quotes, so this is where the time indication refers to (and thus not to the
registered quote timestamp).
} 

\item{pqs: proportional quoted spread}{
\deqn{
\mbox{proportional quoted spread}_t =  \frac{\mbox{quoted spread}_t}{\frac{\mbox{OFR}_{t}+\mbox{BID}_{t}}{2}}
}
(Venkataraman, 2001).
Note that the input of this function consists of the matched trades and
quotes, so this is where the time indication refers to (and thus not to the
registered quote timestamp).
}  

\item{logqs: log quoted spread}{
\deqn{
\mbox{log quoted spread}_t =  \log(\frac{\mbox{OFR}_{t}}{\mbox{BID}_{t}})
}

(Hasbrouck and Seppi, 2001).
Note that the input of this function consists of the matched trades and
quotes, so this is where the time indication refers to (and thus not to the
registered quote timestamp).

}
 
\item{logsize: log quoted size}{
\deqn{
\mbox{log quoted size}_t =  \log(\mbox{OFRSIZ}_{t})-\log(\mbox{BIDSIZ}_{t})
}

(Hasbrouck and Seppi, 2001).
Note that the input of this function consists of the matched trades and
quotes, so this is where the time indication refers to (and thus not to the
registered quote timestamp).
} 

\item{qslope: quoted slope}{
\deqn{
\mbox{quoted slope}_t =  \frac{\mbox{quoted spread}_t}{\mbox{log quoted size}_t}
}
(Hasbrouck and Seppi, 2001).
}

\item{logqslope: log quoted slope}{
\deqn{
\mbox{log quoted slope}_t =  \frac{\mbox{log quoted spread}_t}{\mbox{log quoted size}_t}.
}
} 

\item{mq_return_sqr: midquote squared return}{
\deqn{
\mbox{midquote squared return}_t =  (\log(\mbox{midquote}_{t})-\log(\mbox{midquote}_{t-1}))^2,
}

where \eqn{\mbox{midquote}_{t} = \frac{\mbox{BID}_{t} + \mbox{OFR}_{t}}{2}}.
}

\item{mq_return_abs: midquote absolute return}{
\deqn{
\mbox{midquote absolute return}_t =  |\log(\mbox{midquote}_{t})-\log(\mbox{midquote}_{t-1})|,
}

where \eqn{\mbox{midquote}_{t} = \frac{\mbox{BID}_{t} + \mbox{OFR}_{t}}{2}}.
}

\item{signed_trade_size: signed trade size}{
\deqn{
\mbox{signed trade size}_t =  D_t * \mbox{SIZE}_{t},}

where \eqn{D_t} is 1 (-1) if \eqn{trade_t} was buy (sell).
}
}
}

\value{
an xts object containing one of the above mentioned liquidity measures.
}

\references{
Bessembinder, H. (2003). Issues in assessing trade execution costs. Journal of
Financial Markets, 223-257.

Boehmer, E. (2005). Dimensions of execution quality: Recent evidence for US
equity markets. Journal of Financial Economics 78 (3), 553-582.

Hasbrouck, J. and D. J. Seppi (2001). 
Common factors in prices, order flows and liquidity. 
Journal of Financial Economics, 383-411.

Venkataraman, K. (2001). Automated versus floor trading: 
An analysis of execution costs on the paris and new york exchanges. 
The Journal of Finance,
56, 1445-1485.
}

\author{ Jonathan Cornelissen and Kris Boudt}

\keyword{liquidity}

\examples{
#load data samples
data("sample_tdata");
data("sample_qdata");
tdata = sample_tdata;
qdata = sample_qdata;
#match the trade and quote data
tqdata = matchTradesQuotes(tdata,qdata);

#calculate the proportional realized spread:
prs = tqLiquidity(tqdata,tdata,qdata,type="prs");

#calculate the effective spread:
es = tqLiquidity(tqdata,type="es");
}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%cd C:\Documents and Settings\n09022\Desktop\rtaq
%R CMD Rdconv --type=html --output=test.htm tqliquidity.Rd


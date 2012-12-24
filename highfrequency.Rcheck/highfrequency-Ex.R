pkgname <- "highfrequency"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('highfrequency')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("TAQload")
### * TAQload

flush(stderr()); flush(stdout())

### Name: TAQLoad
### Title: Load trade or quote data into R
### Aliases: TAQLoad
### Keywords: data manipulation

### ** Examples

#In order for these examples to work, the folder datasource 
#should contain two folders named 2008-01-02 and 2008-01-03.
#These folder contain the files with the trade data,
#which are named "AAPL_trades.RData" or "AA_trades.RData".

from="2008-01-02";
to = "2008-01-03";
## Not run: datasource="C:\data";

#TAQLoad: load data for stock AAPL
## Not run: 
##D xx = TAQLoad(tickers="AAPL",from,to,trades=TRUE,quotes=FALSE,
##D datasource=datasource,variables=NULL)
## End(Not run)
## Not run: head(xx);

#Load only price data for stocks AA and AAPL
## Not run: 
##D xx = TAQLoad(tickers=c("AA","AAPL"),from,to,trades=TRUE,
##D quotes=FALSE,datasource=datasource,variables="PRICE")
## End(Not run)
## Not run: head(xx);                                                                                                    
## Not run: tail(xx);                                                                                                    



cleanEx()
nameEx("aggregatePrice")
### * aggregatePrice

flush(stderr()); flush(stdout())

### Name: aggregatePrice
### Title: Aggregate a time series but keep first and last observation
### Aliases: aggregatePrice
### Keywords: data manipulation

### ** Examples

#load data
data("sample_tdata");
#aggregate price data to the 30 second frequency
head(sample_tdata$PRICE)
head(aggregatePrice(sample_tdata$PRICE,on="secs",k=30));



cleanEx()
nameEx("aggregateQuotes")
### * aggregateQuotes

flush(stderr()); flush(stdout())

### Name: aggregateQuotes
### Title: Aggregate an xts object containing quote data
### Aliases: aggregateQuotes
### Keywords: data manipulation

### ** Examples

#load data
data("sample_qdata");
#aggregate quote data to the 30 second frequency
xx = aggregateQuotes(sample_qdata,on="seconds",k=30);
head(xx);



cleanEx()
nameEx("aggregateTrades")
### * aggregateTrades

flush(stderr()); flush(stdout())

### Name: aggregateTrades
### Title: Aggregate an xts object containing trade data
### Aliases: aggregateTrades
### Keywords: data manipulation

### ** Examples

data("sample_tdata");
#aggregate trade data to 5 minute frequency
x = aggregateTrades(sample_tdata,on="minutes",k=5)
head(x);



cleanEx()
nameEx("aggregatets")
### * aggregatets

flush(stderr()); flush(stdout())

### Name: aggregatets
### Title: Aggregate a time series
### Aliases: aggregatets
### Keywords: data manipulation

### ** Examples

#load sample price data
data("sample_tdata");
ts = sample_tdata$PRICE;

#Previous tick aggregation to the 5-minute sampling frequency:
tsagg5min = aggregatets(ts,on="minutes",k=5);
head(tsagg5min);

#Previous tick aggregation to the 30-second sampling frequency:
tsagg30sec = aggregatets(ts,on="seconds",k=30);
tail(tsagg30sec);



cleanEx()
nameEx("convert")
### * convert

flush(stderr()); flush(stdout())

### Name: convert
### Title: Convert trade or quote data into xts object saved in the RData
###   format
### Aliases: convert

### ** Examples

#In order for these examples to work, the folder datasource 
#should contain two folders named 2008-01-02 and 2008-01-03.
#These folder contain the files with the trade data,
#which are named "AAPL_trades.txt" or "AA_trades.txt".

from="2008-01-02";
to = "2008-01-03";
## Not run: 
##D datasource=datadestination="C:\data"
##D 
##D ### txt files from NYSE:
##D 
##D convert(from,to,datasource,datadestination,trades=TRUE,
##D           quotes=FALSE,ticker=c("AA","AAPL"),dir=FALSE,extension="txt",
##D           header=FALSE,tradecolnames=NULL,quotecolnames=NULL,
##D           format="%Y%m%d %H:%M:%S");
##D 
##D #Now, the folder datadestination will contain two folders
##D #named 2008-01-02 and 2008-01-03 containing 
##D #the files AAPL_trades.RData and AAPL_trades.RData containing the trades.
##D #The data can now be loaded with the TAQLoad function.
##D 
##D ########## Csv file from WRDS
##D #Suppose the datasource folder contains one csv file from WRDS 
##D #with data on IBM for multiple days.
##D #The file should be named "IBM_trades.csv" and can be easily converted into xts 
##D #and then saved in RData format by:
##D 
##D  convert(from=from, to=to, datasource=datasource, datadestination=datadestination, trades = T, 
##D                        quotes = T, ticker="IBM", dir = FALSE, extension = "csv", header = TRUE, 
##D                        tradecolnames = NULL, quotecolnames = NULL, format = format, onefile = TRUE )  
##D 
##D ####### ASC file from www.tickdata.com
##D #Suppose the datasource folder contains asc files for trades and quotes 
##D #from "www.tickdata.com" for GLP. 
##D #The files "GLP_quotes.asc" and "GLP_trades.asc" should be saved in datasource folder.
##D 
##D  convert(from=from, to=to, datasource=datasource, datadestination=datadestination, trades = T, 
##D             quotes = T, ticker="GLP", dir = TRUE, extension = "tickdatacom", header = TRUE, 
##D             tradecolnames = NULL, quotecolnames = NULL, format = "##D 
## End(Not run)



cleanEx()
nameEx("harModel")
### * harModel

flush(stderr()); flush(stdout())

### Name: harModel
### Title: HAR model estimation (Heterogeneous Autoregressive model for
###   Realized volatility)
### Aliases: harModel
### Keywords: forecasting

### ** Examples
 
 ##### Example 1: HARRVCJ ##### 
 data("sample_5minprices_jumps"); 
 data = sample_5minprices_jumps[,1];
 data = makeReturns(data); #Get the high-frequency return data
 
 x = harModel(data, periods = c(1,5,10), periodsJ=c(1,5,10), RVest = c("rCov","rBPCov"), 
              type="HARRVCJ",transform="sqrt"); # Estimate the HAR model of type HARRVCJ  
 class(x);
 x 

 ##### Example 2:  ##### 
 # Forecasting daily Realized volatility for DJI 2008 using the basic harModel: HARRV
 data(realized_library); #Get sample daily Realized Volatility data
 DJI_RV = realized_library$Dow.Jones.Industrials.Realized.Variance; #Select DJI
 DJI_RV = DJI_RV[!is.na(DJI_RV)]; #Remove NA's
 DJI_RV = DJI_RV['2008'];

 x = harModel(data=DJI_RV , periods = c(1,5,22), RVest = c("rCov"), type="HARRV",h=1,transform=NULL);
 class(x); 
 x;
 summary(x);
 plot(x);



cleanEx()
nameEx("heavyModel")
### * heavyModel

flush(stderr()); flush(stdout())

### Name: heavyModel
### Title: HEAVY Model estimation
### Aliases: heavyModel
### Keywords: forecasting

### ** Examples
 
 # Implementation of the heavy model on DJI:
 data("realized_library");
 returns =  realized_library$Dow.Jones.Industrials.Returns; 
 rk      =  realized_library$Dow.Jones.Industrials.Realized.Kernel; 
 returns = returns[!is.na(rk)];  rk = rk[!is.na(rk)]; # Remove NA's 
 data = cbind( returns^2, rk ); # Make data matrix with returns and realized measures
 backcast = matrix( c(var(returns),mean(rk)) ,ncol=1);
 
 startvalues = c(0.004,0.02,0.44,0.41,0.74,0.56); # Initial values
# output = heavyModel( data = as.matrix(data,ncol=2), compconst=FALSE, 
#                    startingvalues = startvalues, backcast=backcast); 



cleanEx()
nameEx("highfrequency-package")
### * highfrequency-package

flush(stderr()); flush(stdout())

### Name: highfrequency-package
### Title: highfrequency: Toolkit for the analysis of highfrequency
###   financial data in R.
### Aliases: highfrequency-package highfrequency
### Keywords: package

### ** Examples

# see users manual



cleanEx()
nameEx("lltc.xts")
### * lltc.xts

flush(stderr()); flush(stdout())

### Name: lltc.xts
### Title: LLTC Data
### Aliases: lltc.xts
### Keywords: datasets

### ** Examples

data(lltc.xts)
plot(lltc.xts) 



cleanEx()
nameEx("matchTradesQuotes")
### * matchTradesQuotes

flush(stderr()); flush(stdout())

### Name: matchTradesQuotes
### Title: Match trade and quote data
### Aliases: matchTradesQuotes
### Keywords: data manipulation

### ** Examples

#load data samples
data("sample_tdata");
data("sample_qdata");
#match the trade and quote data
tqdata = matchTradesQuotes(sample_tdata,sample_qdata);
#have a look
head(tqdata);



cleanEx()
nameEx("medRV")
### * medRV

flush(stderr()); flush(stdout())

### Name: medRV
### Title: medRV
### Aliases: medRV
### Keywords: volatility

### ** Examples

 data(sample_tdata);  
 medrv = medRV( rdata = sample_tdata$PRICE, align.by ="minutes", 
            align.period =5, makeReturns=TRUE); 
 medrv 



cleanEx()
nameEx("minRV")
### * minRV

flush(stderr()); flush(stdout())

### Name: minRV
### Title: minRV
### Aliases: minRV
### Keywords: volatility

### ** Examples

 data(sample_tdata); 
 
 minrv = minRV( rdata = sample_tdata$PRICE, align.by ="minutes", 
            align.period =5, makeReturns=TRUE); 
 minrv 



cleanEx()
nameEx("quotescleanup")
### * quotescleanup

flush(stderr()); flush(stdout())

### Name: quotesCleanup
### Title: Cleans quote data
### Aliases: quotesCleanup
### Keywords: cleaning

### ** Examples

#Consider you have raw quote data for 1 stock for 1 day 
data("sample_qdataraw");
head(sample_qdataraw);
dim(sample_qdataraw);
qdata_aftercleaning = quotesCleanup(qdataraw=sample_qdataraw,exchanges="N");
qdata_aftercleaning$report; 
barplot(qdata_aftercleaning$report);
dim(qdata_aftercleaning$qdata);

#In case you have more data it is advised to use the on-disk functionality
#via "from","to","datasource",etc. arguments



cleanEx()
nameEx("rAVGCov")
### * rAVGCov

flush(stderr()); flush(stdout())

### Name: rAVGCov
### Title: Realized Covariance: Average Subsample
### Aliases: rAVGCov
### Keywords: volatility

### ** Examples
 
 # Average subsampled realized variance/covariance for CTS aligned at one minute returns at 
 # 5 subgrids (5 minutes).
 data(sample_tdata); 
 data(lltc.xts);
 data(sbux.xts);
 
 # Univariate
 rvSub = rAVGCov( rdata = sample_tdata$PRICE, period = 5, align.by ="minutes", 
                   align.period=5, makeReturns=TRUE); 
 rvSub
 
 # Multivariate:
 rcSub = rAVGCov( rdata = list(lltc.xts,sbux.xts), period = 5, align.by ="minutes", 
                   align.period=5, makeReturns=FALSE); 
 rcSub



cleanEx()
nameEx("rAccumulation")
### * rAccumulation

flush(stderr()); flush(stdout())

### Name: rAccumulation
### Title: Realized Accumulation Plot
### Aliases: rAccumulation
### Keywords: methods

### ** Examples

data(sbux.xts)

cumm <- list() 
cumm[[1]] <- rCumSum(sbux.xts, period=1, align.by="seconds", align.period=60) 
cumm[[2]] <- rCumSum(sbux.xts, period=10, align.by="seconds", align.period=60) 
cumm[[3]] <- rCumSum(sbux.xts, period=20, align.by="seconds", align.period=60) 
cumm[[4]] <- rCumSum(sbux.xts, period=30, align.by="seconds", align.period=60) 
accum <- list() 
accum[[1]] <- rAccumulation(sbux.xts, period=10, align.by="seconds", align.period=60) 
accum[[2]] <- rAccumulation(sbux.xts, period=20, align.by="seconds", align.period=60) 
accum[[3]] <- rAccumulation(sbux.xts, period=30, align.by="seconds", align.period=60)

par(mfrow=c(2,1)) 
plot(cumm[[1]], xlab="", ylab="Cumulative Ruturns", main="Starbucks (SBUX)", sub='20110701', type="p", col=16, lwd=2) 
lines(cumm[[2]], col=2, lwd=2) 
lines(cumm[[3]], col=3, lwd=2) 
lines(cumm[[4]], col=4, lwd=2) 
plot(accum[[1]], xlab="", ylab="Realized Accumulation", type="l",main="Starbucks (SBUX)", sub='20110701', col=2, lwd=2) 
lines(accum[[2]], col=3, lwd=2) 
lines(accum[[3]], col=4, lwd=2) 



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("rBPCov")
### * rBPCov

flush(stderr()); flush(stdout())

### Name: rBPCov
### Title: Realized BiPower Covariance
### Aliases: rBPCov
### Keywords: volatility

### ** Examples

 # Realized Bipower Variance/Covariance for CTS aligned   
 # at 5 minutes.
 data(sample_tdata); 
 data(sample_5minprices_jumps);
 
 # Univariate: 
 rbpv = rBPCov( rdata = sample_tdata$PRICE, align.by ="minutes", 
                    align.period =5, makeReturns=TRUE); 
 rbpv 
 
 # Multivariate: 
 rbpc = rBPCov( rdata = sample_5minprices_jumps['2010-01-04'], makeReturns=TRUE,makePsd=TRUE); 
 rbpc



cleanEx()
nameEx("rCov")
### * rCov

flush(stderr()); flush(stdout())

### Name: rCov
### Title: Realized Covariance
### Aliases: rCov
### Keywords: volatility

### ** Examples

 # Realized Variance/Covariance for CTS aligned   
 # at 5 minutes.
 data(sample_tdata); 
 data(sample_5minprices_jumps);
 
 # Univariate: 
 rv = rCov( rdata = sample_tdata$PRICE, align.by ="minutes", 
                    align.period =5, makeReturns=TRUE); 
 rv 
 
 # Multivariate: 
 rc = rCov( rdata = sample_5minprices_jumps['2010-01-04'], makeReturns=TRUE); 
 rc



cleanEx()
nameEx("rCumSum")
### * rCumSum

flush(stderr()); flush(stdout())

### Name: rCumSum
### Title: Plot cummulative returns
### Aliases: rCumSum
### Keywords: methods

### ** Examples

data(sbux.xts)

cumm <- list() 
cumm[[1]] <- rCumSum(sbux.xts, period=1, align.by="seconds", align.period=60) 
cumm[[2]] <- rCumSum(sbux.xts, period=10, align.by="seconds", align.period=60) 
cumm[[3]] <- rCumSum(sbux.xts, period=20, align.by="seconds", align.period=60) 
cumm[[4]] <- rCumSum(sbux.xts, period=30, align.by="seconds", align.period=60) 
plot(cumm[[1]], xlab="", ylab="Cumulative Ruturns", main="Starbucks (SBUX)", sub='20110701', type="p", col=16, lwd=2) 
lines(cumm[[2]], col=2, lwd=2) 
lines(cumm[[3]], col=3, lwd=2) 
lines(cumm[[4]], col=4, lwd=2)



cleanEx()
nameEx("rHYCov")
### * rHYCov

flush(stderr()); flush(stdout())

### Name: rHYCov
### Title: Hayashi-Yoshida Covariance
### Aliases: rHYCov
### Keywords: volatility

### ** Examples

 # Average Realized Kernel Variance/Covariance for CTS aligned at one minute returns at 
 # 5 subgrids (5 minutes).
 data(lltc.xts); 
 data(sbux.xts); 
  # Multivariate:
 rHYCov = rHYCov( rdata = list(lltc.xts,sbux.xts), period = 5, align.by ="minutes", 
                   align.period=5, makeReturns=FALSE); 
 rHYCov 
 #Note: for the diagonal elements the rCov is used.



cleanEx()
nameEx("rKernel.available")
### * rKernel.available

flush(stderr()); flush(stdout())

### Name: rKernel.available
### Title: Available Kernels
### Aliases: rKernel.available
### Keywords: volatility

### ** Examples

rKernel.available()



cleanEx()
nameEx("rKernelCov")
### * rKernelCov

flush(stderr()); flush(stdout())

### Name: rKernelCov
### Title: Realized Covariance: Kernel
### Aliases: rKernelCov
### Keywords: volatility

### ** Examples
 
 # Average Realized Kernel Variance/Covariance for CTS aligned at one minute returns at 
 # 5 subgrids (5 minutes).
 data(sample_tdata); 
 data(lltc.xts); 
 data(sbux.xts); 
 
 # Univariate: 
 rvKernel = rKernelCov( rdata = sample_tdata$PRICE, period = 5, align.by ="minutes", 
                   align.period=5, makeReturns=TRUE); 
 rvKernel 
 
 # Multivariate:
 rcKernel = rKernelCov( rdata = list(lltc.xts,sbux.xts), period = 5, align.by ="minutes", 
                   align.period=5, makeReturns=FALSE); 
 rcKernel 



cleanEx()
nameEx("rMarginal")
### * rMarginal

flush(stderr()); flush(stdout())

### Name: rMarginal
### Title: Maginal Contribution to Realized Estimate
### Aliases: rMarginal
### Keywords: methods

### ** Examples

data(sbux.xts)
par(mfrow=c(2,1))
plot(rCumSum(sbux.xts, period=10, align.by="seconds", align.period=60), xlab="", ylab="Cumulative Ruturns", main="Starbucks (SBUX)", sub='20110701', type="p")
barplot(rMarginal(sbux.xts, period=10, align.by="seconds", align.period=60)$y, main="Marginal Contribution Plot") 



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("rOWCov")
### * rOWCov

flush(stderr()); flush(stdout())

### Name: rOWCov
### Title: Realized Outlyingness Weighted Covariance
### Aliases: rOWCov
### Keywords: volatility

### ** Examples

 # Realized Outlyingness Weighted Variance/Covariance for CTS aligned   
 # at 5 minutes.
 data(sample_tdata); 
 data(sample_5minprices_jumps);
 
 # Univariate: 
 rvoutw = rOWCov( rdata = sample_tdata$PRICE, align.by ="minutes", 
                    align.period =5, makeReturns=TRUE); 
 rvoutw 
 
 # Multivariate: 
 rcoutw = rOWCov( rdata = sample_5minprices_jumps['2010-01-04'], makeReturns=TRUE); 
 rcoutw



cleanEx()
nameEx("rRTSCov")
### * rRTSCov

flush(stderr()); flush(stdout())

### Name: rRTSCov
### Title: Robust two time scale covariance estimation
### Aliases: rRTSCov
### Keywords: volatility

### ** Examples
 
 # Robust Realized two timescales Variance/Covariance for CTS 
 data(sample_tdata); 
 data(lltc.xts); 
 data(sbux.xts); 
 
 # Univariate: 
 rvRTS = rRTSCov( pdata = sample_tdata$PRICE); 
 # Note: Prices as input
 rvRTS 
 
 # Multivariate:
 rcRTS = rRTSCov( pdata = list(cumsum(lltc.xts)+100,cumsum(sbux.xts)+100) ); 
 # Note: List of prices as input
 rcRTS 



cleanEx()
nameEx("rScatterReturns")
### * rScatterReturns

flush(stderr()); flush(stdout())

### Name: rScatterReturns
### Title: Scatterplot of aligned returns
### Aliases: rScatterReturns
### Keywords: methods

### ** Examples

data(sbux.xts)
data(lltc.xts)
par(mfrow=c(2,1))
rScatterReturns(sbux.xts,y=lltc.xts, period=1, align.period=20,ylab="LLTC",xlab="SBUX",numbers=FALSE) 
rScatterReturns(sbux.xts,y=lltc.xts, period=1, align.period=20,ylab="LLTC",xlab="SBUX",numbers=TRUE) 



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("rTSCov")
### * rTSCov

flush(stderr()); flush(stdout())

### Name: rTSCov
### Title: Two time scale covariance estimation
### Aliases: rTSCov
### Keywords: volatility

### ** Examples

 # Robust Realized two timescales Variance/Covariance for CTS 
 data(sample_tdata); 
 data(lltc.xts); 
 data(sbux.xts); 
 
 # Univariate: 
 rvTS = rTSCov( pdata = sample_tdata$PRICE); 
 # Note: Prices as input
 rvTS 
 
 # Multivariate:
 rcTS = rTSCov( pdata = list(cumsum(lltc.xts)+100,cumsum(sbux.xts)+100) ); 
 # Note: List of prices as input
 rcTS 



cleanEx()
nameEx("rThresholdCov")
### * rThresholdCov

flush(stderr()); flush(stdout())

### Name: rThresholdCov
### Title: Threshold Covariance
### Aliases: rThresholdCov
### Keywords: volatility

### ** Examples

 # Realized threshold  Variance/Covariance: 
 data(lltc.xts); 
 data(sbux.xts); 
 
 # Multivariate:
 rcThreshold = rThresholdCov(cbind(lltc.xts,sbux.xts), align.by="minutes",align.period=1); 
 rcThreshold  



cleanEx()
nameEx("rZero")
### * rZero

flush(stderr()); flush(stdout())

### Name: rZero
### Title: Calculates the percentage of co-zero returns at a specified
###   sampling period
### Aliases: rZero
### Keywords: methods

### ** Examples

data(sbux.xts)
data(lltc.xts)
rZero( rdata = list(sbux.xts, lltc.xts) , period = 60, align.by ="seconds", align.period=1)



cleanEx()
nameEx("refreshTime")
### * refreshTime

flush(stderr()); flush(stdout())

### Name: refreshTime
### Title: Synchronize (multiple) irregular timeseries by refresh time
### Aliases: refreshTime
### Keywords: data manipulation

### ** Examples

#suppose irregular timepoints:
start = as.POSIXct("2010-01-01 09:30:00")
ta = start + c(1,2,4,5,9);    	
tb = start + c(1,3,6,7,8,9,10,11);

#yielding the following timeseries:
a = as.xts(1:length(ta),order.by=ta);
b = as.xts(1:length(tb),order.by=tb);

#Calculate the synchronized timeseries:
refreshTime(list(a,b))



cleanEx()
nameEx("sbux.xts")
### * sbux.xts

flush(stderr()); flush(stdout())

### Name: sbux.xts
### Title: Starbucks Data
### Aliases: sbux.xts
### Keywords: datasets

### ** Examples

data(sbux.xts)
plot(sbux.xts) 



cleanEx()
nameEx("spotVol")
### * spotVol

flush(stderr()); flush(stdout())

### Name: spotVol
### Title: Spot volatility estimation
### Aliases: spotVol
### Keywords: volatility

### ** Examples

data("sample_real5minprices");

# Compute and plot intraday periodicity:
out = spotVol(sample_real5minprices,P1=6,P2=4,periodicvol="TML",k=5, dummies=FALSE);
head(out);



cleanEx()
nameEx("tqLiquidity")
### * tqLiquidity

flush(stderr()); flush(stdout())

### Name: tqLiquidity
### Title: Calculate numerous (23) liquidity measures
### Aliases: tqLiquidity
### Keywords: liquidity

### ** Examples

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



cleanEx()
nameEx("tradesCleanupFinal")
### * tradesCleanupFinal

flush(stderr()); flush(stdout())

### Name: tradesCleanupFinal
### Title: Perform a final cleaning procedure on trade data
### Aliases: tradesCleanupFinal
### Keywords: cleaning

### ** Examples

#Consider you have raw trade data for 1 stock for 1 day 
data("sample_qdata");    #load cleaned quote data
data("sample_tdataraw"); #load raw trade data
tdata_afterfirstcleaning = tradesCleanup(tdataraw=sample_tdataraw,
exchange="N",report=FALSE);
dim(tdata_afterfirstcleaning);
tdata_afterfinalcleaning = tradesCleanupFinal(qdata=sample_qdata,
tdata=tdata_afterfirstcleaning);
dim(tdata_afterfinalcleaning);
#In case you have more data it is advised to use the on-disk functionality
#via "from","to","datasource",etc. arguments



cleanEx()
nameEx("tradescleanup")
### * tradescleanup

flush(stderr()); flush(stdout())

### Name: tradesCleanup
### Title: Cleans trade data
### Aliases: tradesCleanup
### Keywords: cleaning

### ** Examples

#Consider you have raw trade data for 1 stock for 1 day 
data("sample_tdataraw");
head(sample_tdataraw);
dim(sample_tdataraw);
tdata_afterfirstcleaning = tradesCleanup(tdataraw=sample_tdataraw,exchanges="N");
tdata_afterfirstcleaning$report; 
barplot(tdata_afterfirstcleaning$report);
dim(tdata_afterfirstcleaning$tdata);

#In case you have more data it is advised to use the on-disk functionality
#via "from","to","datasource",etc. arguments



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

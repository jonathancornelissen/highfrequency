


#########################################################################
#
# Utility Functions from realized package Scott Payseur
#
#########################################################################
.alignedAccum <- function(x, y, period, cum = TRUE, makeReturns...) {
    x<-.accum.naive(x,x, period)
    y<-.accum.naive(y,y, period)
    if(cum)
    {
        ans <- cumsum(x*y)
    }
    else
    {
        ans <- x*y     
    }
    ans
}


.accum.naive <- function(x,y, period, ...) {
    .C("rv", 
    as.double(x), #a
    as.double(y), #b
    as.integer(length(x)), #na
    as.integer(period), #period 
    tmpa = as.double(rep(0,as.integer(length(x)/period +1))), #tmp
    as.double(rep(0,as.integer(length(x)/period +1))), #tmp
    as.integer(length(x)/period), #tmpn
    ans = double(1), 
    COPY=c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE), 
    PACKAGE="highfrequency")$tmpa     
}


alignReturns <- function(x, period, ...) {
    .C("rv", 
    as.double(x), #a
    as.double(x), #b
    as.integer(length(x)), #na
    as.integer(period), #period 
    tmpa = as.double(rep(0,as.integer(length(x)/period +1))), #tmp
    as.double(rep(0,as.integer(length(x)/period +1))), #tmp
    as.integer(length(x)/period), #tmpn
    ans = double(1), 
    COPY=c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE), 
    PACKAGE="highfrequency")$tmpa     
}

.getAlignPeriod <- function(align.period, align.by) {   
    align.by <- gsub("(^ +)|( +$)", "",align.by) # Trim White
    
    if(casefold(align.by)=="min" || casefold(align.by)=="mins" ||casefold(align.by)=="minute"||casefold(align.by)=="minutes"||casefold(align.by)=="m"){
        ans <- align.period * 60
    }
    if(casefold(align.by)=="sec" || casefold(align.by)=="secs" ||casefold(align.by)=="second"||casefold(align.by)=="seconds"||casefold(align.by)=="s"||casefold(align.by)==""){
        ans <- align.period
    }
    if(casefold(align.by)=="hour" || casefold(align.by)=="hours" ||casefold(align.by)=="h"){
        ans <- align.period * 60 * 60
    }
    return(ans)
}


.alignIndices <- function(x, period, ...) {
    .C("rvperiod", 
    as.double(x), #a
    as.double(x), #b
    as.integer(length(x)), #na
    as.integer(period), #period 
    tmpa = as.double(rep(max(x),as.integer(length(x)/period +1))), #tmp
    as.double(rep(0,as.integer(length(x)/period +1))), #tmp
    as.integer(length(x)/period), #tmpn
    ans = double(1), 
    COPY=c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE), 
    PACKAGE="highfrequency")$tmpa     
}

multixts <- function(x, y = NULL) { 
    if(is.null(y)){
        test = is.xts(x) && (ndays(x)!=1);
        return(test);}
    if(!is.null(y)){
        test = (is.xts(x) && (ndays(x)!=1)) || ( ndays(y)!=1 && is.xts(y) );
        if( test ){
            test1 = dim(y) == dim(x);
            if(!test1){ warning("Please make sure x and y have the same dimensions") }
            if(test1){  test = list( TRUE, cbind(x,y) ); return(test) }
        }
    }
}      

.convertData <- function(x, cts = TRUE, millisstart = NA, millisend = NA, makeReturns = FALSE) {
    if(is.null(x))
    {
        return(NULL)
    }
    if("realizedObject" %in% class(x))
    {
        return(x)
    }
    
    if("xts" %in% class(x))
    {
        xtmp <- x
        x <- list() 
        x$data <- as.numeric(xtmp[,1])
        
        x$milliseconds <- (as.POSIXlt(time(xtmp))$hour*60*60 + as.POSIXlt(time(xtmp))$min*60 + as.POSIXlt(time(xtmp))$sec )*1000
        if(is.na(millisstart))
        {
            millisstart = x$milliseconds[[1]]
        }
        if(is.na(millisend))
        {
            millisend = x$milliseconds[[length(x$milliseconds)]]
        }
        
        # cat(paste("xts -> realizedObject [", as.character(time(xtmp[1])), " :: ", as.character(time(xtmp[length(x$milliseconds)])), "]", sep=""),"\n")
    }
    
    if(is.na(millisstart))
    {
        millisstart=34200000
    }
    if(is.na(millisend))
    {
        millisend=57600000
    }    
    if("list" %in% class(x))
    {
        if(sum(names(x) == c("tts", "cts")) == 2) #realized obj  
        {
            if(cts)
            {
                return(x$cts)
            }
            else
            {
                return(x$tts)
            }
        }
        if(sum(names(x) == c("data", "milliseconds")) == 2) 
        {
            if(makeReturns)
            {                                           # only works on non cts prices
                errcheck <- try(.getReturns(.sameTime(x$data, x$milliseconds)))
                if(class(errcheck) != "Error")
                {
                    x$data <- errcheck
                    x$milliseconds <- intersect(x$milliseconds,x$milliseconds)
                }
                else
                {
                    warning("It appears that these are already returns.  Not creating returns")
                }
            }          
            else
            {
                x$data <- .sameTime(x$data, x$milliseconds)
                x$milliseconds <- intersect(x$milliseconds,x$milliseconds)
            }          
            if(cts)
            {
                toret <- list(data=.toCts(x=x$data, millis=intersect(x$milliseconds,x$milliseconds), millisstart=millisstart, millisend=millisend),
                milliseconds=(((millisstart/1000)+1):(millisend/1000))*1000)
                return(toret)
            }
            else
            {
                toret <- list(data=x$data, 
                milliseconds=intersect(x$milliseconds,x$milliseconds))
                return(toret)
            }
        }
    }
    
    
    if("timeSeries" %in% class(x))
    {
        stop("R timeSeries not implmented yet. Convert to realized object")
    }
    return(list(milliseconds = 1:dim(as.matrix(x))[[1]], data = as.matrix(x)))  # not an object, fake the milliseconds and return
}

.getReturns <- function(x) {
    x <- as.numeric(x)
    n <- length(x)[[1]]
    return(log(x[2:n]) - log(x[1:(n-1)]))
}

.sameTime <- function(x, millis) {
    .C("sametime", 
    as.double(x), #a
    as.integer(length(x)), #na
    as.integer(millis), #millis
    ans = double(length(union(millis,millis))), #tts
    COPY=c(FALSE,FALSE,FALSE,TRUE), 
    PACKAGE="highfrequency")$ans
}


data.toCts <- function(x, millis, millisstart=34200000, millisend=57600000) {
    .toCts(x=x, millis=millis, millisstart=millisstart, millisend=millisend)
}

.toCts <- function(x, millis, millisstart=34200000, millisend=57600000) {
    .C("tocts", 
    as.double(x), #a
    as.integer(length(x)),
    as.integer(millis), #millis
    as.integer(millisstart),
    as.integer(millisend),
    ans = double(((millisend-millisstart)/1000)), #cts
    COPY=c(FALSE,FALSE,FALSE,FALSE,TRUE), 
    PACKAGE="highfrequency")$ans
}

data.toReturns <- function(x) {
    x <- as.numeric(x)   
    n <- length(x)
    log(x[2:n]) - log(x[1:(n-1)])
}






## Percentage of zero's for given time aggregation calculator:
rZero = function( rdata, period = 1, align.by = "seconds", align.period = 1, cts = TRUE, makeReturns = FALSE, ...){
    if (is.list(rdata) == FALSE) {
        multixts = multixts(rdata);  
        if (multixts) { 
          stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")
        }
        if (is.null(dim(rdata))){  
          n = 1
        } else { 
          n = dim(rdata)[2] 
        }
        if (n == 1) { 
            L = .makeROlist(rdata=list(rdata), align.period=align.period, align.by=align.by,cts=cts,makeReturns=makeReturns);#make objects list     
            result = rv.zero( L[[1]], period=period ); 
            return(result)  }
        if( n >  1 ){ stop('The rdata input is not a list. Please provide a list as input for this function. Each list-item should contain the series for one asset.') }
    }
    if(is.list(rdata)){
        multixts = multixts(rdata[[1]]);  
        if(multixts){ stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")}
        n = length(rdata)
        if( n == 1 ){ 
            L = .makeROlist(rdata=rdata, align.period=align.period, align.by=align.by,cts=cts,makeReturns=makeReturns);#make objects list     
            result = rv.zero(L[[1]], period=period ); 
            return(result) }
        if( n > 1){      
            cov = matrix(rep(0, n * n), ncol = n); 
            diagonal = c(); 
            L = .makeROlist(rdata=rdata, align.period=align.period, align.by=align.by,cts=cts,makeReturns=makeReturns);#make objects list     
            
            for(i in 1:n){ 
                diagonal[i] = rv.zero( L[[i]], period=period );
            } 
            diag(cov) = diagonal;
            for (i in 2:n){
                for (j in 1:(i - 1)){
                    cov[i, j] = cov[j, i] = rc.zero( x=L[[i]], y=L[[j]], period=period);       
                }
            }
            return(cov)
        }  #List-length > 1
    }  #If-list condition
}   #end rAVGCov

# 
# # Accumulation:
# rAccumulation <- function(x, period = 1, y = NULL, align.by = "seconds",align.period = 1, plotit = FALSE, cts=TRUE, makeReturns = FALSE) {
#   multixts = multixts(x) || multixts(y)
#   if( multixts ){ stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")}     
#   
#   align.period = .getAlignPeriod(align.period, align.by)   
#   ans <- list(x=NULL, y=NULL)
#   ans$y <- cumsum(rMarginal(x=x, y=y, period=period, align.period=align.period, cts=cts, makeReturns=makeReturns)$y)
#   #    ans$x <- .alignIndices(1:length(x), align.period)
#   #    ans$x <- .alignIndices(ans$x, period)
#   ans$x <- rCumSum(x=x, period=period, align.period=align.period, cts=cts, makeReturns=makeReturns)$x
#   #ans$x <- ans$x[-length(ans$x)]
#   if(plotit)
#   {
#     plot(ans, xlab="", ylab="Realized Accumulation")
#     return(NULL)
#   }
#   ans
# } 
# 
# # Marginal distribution:
# rMarginal <- function(x, y = NULL, period, align.by = "seconds", align.period = 1, plotit=FALSE, cts=TRUE, makeReturns=FALSE) {
#   multixts = multixts(x) || multixts(y);
#   if( multixts ){ stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")}     
#   
#   align.period = .getAlignPeriod(align.period, align.by)   
#   ans <- list(x = NULL, y = NULL)
#   ans$x <- .alignIndices(1:length(x), align.period)
#   ans$x <- .alignIndices(ans$x, period)
#   
#   if (is.null(y))
#     y <- x   
#   x <- alignReturns(.convertData(x, cts=cts, makeReturns=makeReturns)$data, align.period)
#   y <- alignReturns(.convertData(y, cts=cts, makeReturns=makeReturns)$data, align.period)
#   ans$y <- .alignedAccum(x=x, y=y, period=period, cum=FALSE)
#   
#   if (plotit) {
#     plot(ans, xlab="", ylab="Realized Marginals")
#     return(NULL)
#   }
#   ans
# }

# # Cumulative sum of returns:
# rCumSum <- function(x, period = 1, align.by = "seconds", align.period = 1, plotit = FALSE, type='l', cts = TRUE, makeReturns = FALSE) {
#   multixts = multixts(x);
#   if( multixts ){ 
#     stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")
#   }     
#   
#   align.period = .getAlignPeriod(align.period, align.by)   
#   ans <- list(x = NULL, y = NULL)
#   ans$x <- .alignIndices(1:length(.convertData(x, cts=cts, makeReturns=makeReturns)$data), align.period)
#   ans$x <- .alignIndices(ans$x, period)
#   
#   x<- alignReturns(.convertData(x, cts=cts, makeReturns=makeReturns)$data, align.period)
#   x<- alignReturns(.convertData(x, cts=cts, makeReturns=makeReturns)$data, period)
#   
#   ans$y <- cumsum(x)
#   if (plotit) {
#     plot(cumsum(x), xlab="Time", ylab="Cummulative Returns", type=type)
#     return(NULL)
#   }
#   ans
# } 

# #Scatter returns:
# rScatterReturns <- function(x, y, period, align.by = "seconds", align.period = 1, numbers = FALSE, xlim = NULL, ylim = NULL, 
#                             plotit = TRUE, pch = NULL, cts = TRUE, makeReturns = FALSE, scale.size = 0, col.change = FALSE,...) {
#   multixts = multixts(x) || multixts(y)
#   if (multixts == TRUE) { 
#     stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")
#   }     
#   
#   align.period = .getAlignPeriod(align.period, align.by) 
#   y <- alignReturns(.convertData(y, cts=cts, makeReturns=makeReturns)$data, align.period)
#   x <- alignReturns(.convertData(x, cts=cts, makeReturns=makeReturns)$data, align.period)
#   
#   x <- .accum.naive(x, x, period)
#   y <- .accum.naive(y, y, period)
#   if (is.null(pch)) {
#     pch <- 1
#   }
#   
#   it <- table(round(x,4),round(y,4))
#   xs <- as.numeric(dimnames(it)[[1]])
#   ys <- as.numeric(dimnames(it)[[2]])
#   
#   if (is.null(ylim) == TRUE) {
#     ylim <- c(min(ys), max(ys))
#   }
#     
#   if (is.null(xlim)) {
#     xlim <- c(min(xs), max(xs))
#   }
#   
#   mat <- matrix(it, nrow=length(xs), ncol=length(ys))
#   
#   if (plotit == TRUE) {
#     plot(0,0, xlim=xlim, ylim=ylim , type='n',...)
#     lines(c(0,0), c(-1,2), col="grey", lty=3, lwd=2)
#     lines(c(-1,2), c(0,0), col="grey", lty=3, lwd=2)
#     
#     maxed <- max(mat)
#     
#     for (i in 1:length(xs)) {
#       for(j in 1:length(ys)) {
#         if(mat[i,j]!=0) {
#           if(col.change) {
#             thecol <- round(runif(1)*100,0)
#           } else {
#             thecol = 1
#           }
#           if(numbers) {
#             if(scale.size ==0)
#               text(xs[i], ys[j],as.character(mat[i,j]), cex=.7, col=thecol)         
#             else
#               text(xs[i], ys[j], as.character(mat[i,j]), cex = (mat[i,j]/maxed) * scale.size, col=thecol)
#           } else {
#             if(scale.size ==0)
#               points(xs[i], ys[j], pch=pch, cex=.7, col=thecol)         
#             else
#               points(xs[i], ys[j], pch=pch, cex = (mat[i,j]/maxed) * scale.size, col=thecol)
#           }
#         }
#         
#       }
#     }
#     return(NULL)
#   }     
#   mat
# }











##################################################################################################
######################################### FORMER RTAQ FUNCTIONS ##################################
########## HELPFUNCTION ####
readdata = function(path=NULL, extension="txt",header=FALSE,dims=0){
  #extention should either be "txt" or "csv"
  if(!(extension=="txt"|extension=="csv")){print("Please select a supported extension")}
  colnames = rep("x",dims);
  #load txt
  if(extension == "txt"){
    fullpath = paste(path,".txt",sep="");
    data = try(read.delim(fullpath,sep="",header=header,dec=",",col.names=colnames,colClasses = "character"),silent=TRUE);
    
    if(is.null(dim(data))){
      data = try(read.delim(fullpath,sep="",header=header,dec=",",col.names=c(colnames,"EXTRA"),colClasses = "character"),silent=TRUE);
      if(is.null(dim(data))){data=matrix(nrow=0,ncol=9);
      }else{data=data[,(-dim(data)[2])]}
    }
  }
  
  if(extension == "csv"){
    fullpath = paste(path,".csv",sep="");
    data = try(read.delim(fullpath,sep=",",header=header,dec=".",col.names=colnames,colClasses = "character"),silent=TRUE);
    
    if(is.null(dim(data))){
      data = try(read.delim(fullpath,sep=",",header=header,dec=".",col.names=c(colnames,"EXTRA"),colClasses = "character"),silent=TRUE);
      if(is.null(dim(data))){data=matrix(nrow=0,ncol=9);
      }else{data=data[,(-dim(data)[2])]}
    }
  }
  return(data);
}


convert_trades = function (datasource, datadestination, ticker, extension = "txt", 
                           header = FALSE, tradecolnames = NULL, format = "%Y%M%D %H:%M:%S") {  
  missingt <- matrix(ncol=2,nrow=0)
  
  dir.create(datadestination, showWarnings = FALSE)
  dir.create(datasource, showWarnings = FALSE)
  cur.dir <- getwd()
  
  setwd(datasource)
  adjtime = function(z) {
    zz = unlist(strsplit(z, ":"))
    if (nchar(zz[1]) != 2) {
      return(paste(paste(0, zz[1], sep = ""), zz[2], zz[3], 
                   sep = ":"))
    }
    return(z)
  }
  for (i in 1:length(ticker)) {
    tfile_name = paste(datasource, "/", ticker[i], "_trades", 
                       sep = "")
    tdata = try(readdata(path = tfile_name, extension = extension, 
                                header = header, dims = length(tradecolnames)), silent = TRUE)
    
    error = dim(tdata)[1] == 0
    if (error) {
      print(paste("no trades for stock", ticker[i]))
      missingt = rbind(missingt, c(datasource, ticker[i]))
    }
    if (error == FALSE) {
      if (is.null(tradecolnames)) {
        tradecolnames = c("SYMBOL", "DATE", "EX", "TIME", 
                          "PRICE", "SIZE", "COND", "CORR", "G127")
        colnames(tdata) = tradecolnames
      }else {
        colnames(tdata) = tradecolnames
      }
      cond = tdata$COND[is.na(tdata$G127)];
      cr = tdata$CORR[is.na(tdata$G127)];
      
      tdata$COND[is.na(tdata$G127)] = 0
      tdata$CORR[is.na(tdata$G127)] = as.character(cond)
      tdata$G127[is.na(tdata$G127)] = as.character(cr)
      rm(cond, cr)
      oldtime = as.matrix(as.vector(tdata$TIME))
      newtime = apply(oldtime, 1, adjtime)
      tdata$TIME = newtime
      rm(oldtime, newtime); 
      tdobject = as.POSIXct(paste(as.vector(tdata$DATE), as.vector(tdata$TIME)), format=format, tz="GMT")       
      tdata = xts(tdata, order.by = tdobject)
      tdata = tdata[, c("SYMBOL", "EX", "PRICE", "SIZE", 
                        "COND", "CORR", "G127")]
      rm(tdobject)
    }
    xts_name = paste(ticker[i], "_trades.RData", sep = "")
    setwd(datadestination)
    save(tdata, file = xts_name)
  }
  setwd(cur.dir)
}


convert_quotes = function (datasource, datadestination, ticker, extension = "txt", 
                           header = FALSE, quotecolnames = NULL, format = "%Y%M%D %H:%M:%S") {
  missingq=matrix(ncol=2,nrow=0);
  
  dir.create(datadestination, showWarnings = FALSE)
  dir.create(datasource, showWarnings = FALSE)
  cur.dir <- getwd()
  setwd(datasource)
  adjtime = function(z) {
    zz = unlist(strsplit(z, ":"))
    if (nchar(zz[1]) != 2) {
      return(paste(paste(0, zz[1], sep = ""), zz[2], zz[3], 
                   sep = ":"))
    }
    return(z)
  }
  for (i in 1:length(ticker)) {
    qfile_name = paste(datasource, "/", ticker[i], "_quotes", 
                       sep = "")
    qdata = try(readdata(path = qfile_name, extension = extension, 
                         header = header, dims = length(quotecolnames)), silent = FALSE)
    error = dim(qdata)[1] == 0
    if (error) {
      print(paste("no quotes for stock", ticker[i]))
      missingq = rbind(missingq, c(datasource, ticker[i]))
    }
    if (error == FALSE) {
      if (is.null(quotecolnames)) {
        quotecolnames = c("SYMBOL", "DATE", "EX", "TIME", 
                          "BID", "BIDSIZ", "OFR", "OFRSIZ", "MODE")
        colnames(qdata) = quotecolnames
      }
      else {
        colnames(qdata) = quotecolnames
      }
      qdata = qdata[qdata$SYMBOL == ticker[i], ]
      oldtime = as.matrix(as.vector(qdata$TIME))
      newtime = apply(oldtime, 1, adjtime)
      qdata$TIME = newtime
      rm(oldtime, newtime)
      test = paste(as.vector(qdata$DATE), as.vector(qdata$TIME))
      tdobject = as.POSIXct(test, format=format, tz="GMT")                       
      qdata = xts(qdata, order.by = tdobject)
      qdata = qdata[, c("SYMBOL", "EX", "BID", "BIDSIZ", 
                        "OFR", "OFRSIZ", "MODE")]
    }
    xts_name = paste(ticker[i], "_quotes.RData", sep = "")
    setwd(datadestination)
    save(qdata, file = xts_name)
  }
  setwd(cur.dir)
}

# NEW CODE GSoC 2012 #
makeXtsTrades = function(tdata,format=format){
  adjtime = function(z) {
    zz = unlist(strsplit(z, ":"))
    if (nchar(zz[1]) != 2) {
      return(paste(paste(0, zz[1], sep = ""), zz[2], zz[3], 
                   sep = ":"))  } 
    return(z) }
  tradecolnames = colnames(tdata)
  if (is.null(tradecolnames)){
    tradecolnames = c("SYMBOL", "DATE", "EX", "TIME", 
                      "PRICE", "SIZE", "COND", "CORR", "G127");
    colnames(tdata) = tradecolnames; }  
  
  cond = tdata$COND[is.na(tdata$G127)];
  cr = tdata$CORR[is.na(tdata$G127)];
  tdata$COND[is.na(tdata$G127)] = 0;
  tdata$CORR[is.na(tdata$G127)] = as.character(cond);
  tdata$G127[is.na(tdata$G127)] = as.character(cr);
  rm(cond, cr);
  oldtime = as.matrix(as.vector(tdata$TIME));
  newtime = apply(oldtime, 1, adjtime);
  tdata$TIME = newtime;
  rm(oldtime, newtime);
  tdobject = as.POSIXct(paste(as.vector(tdata$DATE), as.vector(tdata$TIME)), format=format, tz="GMT")       
  tdata  = xts(tdata, order.by = tdobject);
  tdata  = tdata[, c("SYMBOL", "EX", "PRICE", "SIZE","COND", "CORR", "G127")];
  rm(tdobject)
  return(tdata)  
}  

####
makeXtsQuotes <- function(qdata, format = format) { 
  adjtime = function(z) {    
    zz = unlist(strsplit(z, ":")); if (nchar(zz[1]) != 2) {return(paste(paste(0, zz[1], sep = ""), zz[2], zz[3], sep = ":"))}; return(z) 
  }
  quotecolnames = colnames(qdata);
  
  if (is.null(quotecolnames)) {
    quotecolnames = c("SYMBOL", "DATE", "EX", "TIME", "BID", "BIDSIZ", "OFR", "OFRSIZ", "MODE")
    colnames(qdata) = quotecolnames;
  }else{ colnames(qdata) = quotecolnames }
  
  oldtime = as.matrix(as.vector(qdata$TIME));
  newtime = apply(oldtime, 1, adjtime);
  qdata$TIME = newtime;
  rm(oldtime, newtime);
  test = paste(as.vector(qdata$DATE), as.vector(qdata$TIME))
  tdobject = as.POSIXct(test, format=format, tz="GMT")
  qdata = xts(qdata, order.by = tdobject)
  qdata = qdata[, c("SYMBOL", "EX", "BID", "BIDSIZ","OFR", "OFRSIZ", "MODE")]
  rm(tdobject)
  return(qdata)
}

################ The real conversion starts here ;)

convert <- function(from, to, datasource, datadestination, trades = TRUE, 
                   quotes = TRUE, ticker, dir = FALSE, extension = "txt", header = FALSE, 
                   tradecolnames = NULL, quotecolnames = NULL, format = "%Y%m%d %H:%M:%S", onefile = FALSE) {  

  #############  1.A the data is in the "RTAQ folder" sturcture ##############
  if (onefile == FALSE) {
    
    # Create trading dates:
    dates = timeDate::timeSequence(from, to, format = "%Y-%m-%d", FinCenter = "GMT")
    dates = dates[timeDate::isBizday(dates, holidays = timeDate::holidayNYSE(1960:2050))];
    
    # Create folder structure for saving:
    if (dir == TRUE) { 
      dir.create(datadestination, showWarnings = FALSE) 
      for (i in 1:length(dates)) {
        dirname = paste(datadestination, "/", as.character(dates[i]), sep = "")
        dir.create(dirname, showWarnings = FALSE)    
      } 
    }
    for (i in 1:length(dates)){ #Loop over days  
      #Get the day-specific path
      datasourcex = paste(datasource, "/", dates[i], sep = "")
      datadestinationx = paste(datadestination, "/", dates[i], sep = "")
      
      if(trades == TRUE){ 
        if(extension=="txt"|extension=="csv"){ convert_trades(datasourcex, datadestinationx, ticker, extension = extension, header = header, tradecolnames = tradecolnames, format = format) }
      }
      
      if (quotes == TRUE) { 
        if(extension=="txt"|extension=="csv"){ convert_quotes(datasourcex, datadestinationx, ticker, extension = extension, header = header, quotecolnames = quotecolnames,format = format)}
      } 
    }#End loop over days
  }#End "not oneday" if
  
  #############  1.B The data is in one file: ###########
  if( onefile == TRUE ){
    # Load the data: ############################ This depends on the data provider
    if(trades == TRUE){ 
      if( extension == "txt"){ 
        dataname = paste(datasource,"/",ticker,"_trades",sep="") 
        readdata(path = datasource, extension = "txt", header = FALSE, dims = 0)
      } 
      if (extension == "csv"){ 
        dataname = paste(datasource,"/",ticker,"_trades.csv",sep="") 
        data = read.csv(dataname)
      }
      if (extension=="tickdatacom") { 
        dataname   = paste(datasource,"/",ticker,"_trades.asc",sep="")
        colnames   = c("DATE","TIME","PRICE","SIZE","EX","COND","CORR","SEQN","SOURCE","TSTOP","G127","EXCL","FPRICE")
        alldata    = read.delim(dataname, header=F, sep=",",dec=".",col.names=colnames)
        taqnames   = c("DATE","EX","TIME","PRICE","SIZE","COND","CORR","G127")
        data = alldata[,taqnames]
        data = cbind(rep(ticker,dim(data)[1]),data); colnames(data)[1] = "SYMBOL"
      }
      alldata = suppressWarnings(makeXtsTrades(tdata=data,format=format))
    }
    
    if (quotes == TRUE){ 
      if( extension=="txt"){ 
        dataname = paste(datasource,"/",ticker,"_quotes",sep="") 
        readdata(path = datasource, extension = "txt", header = FALSE, dims = 0)
      } 
      if( extension=="csv"){ 
        dataname = paste(datasource,"/",ticker,"_quotes.csv",sep="") 
        data = read.csv(dataname)
      }
      if( extension=="tickdatacom"){ 
        dataname   = paste(datasource,"/",ticker,"_quotes.asc",sep="")
        colnames   = c("DATE","TIME","EX","BID","OFR","BIDSIZ","OFRSIZ","MODE","MMID","SEQN","EXB", "EXO","NBBOID","NBBOID","CORR","QSO")
        alldata    = read.delim(dataname, header=F, sep=",",dec=".",col.names=colnames)
        taqnames   = c("DATE","TIME","EX","BID","BIDSIZ","OFR","OFRSIZ","MODE")
        data = alldata[,taqnames]
        data = cbind(rep(ticker,dim(data)[1]),data)
        colnames(data)[1] = "SYMBOL"
        format = "%d/%m/%Y %H:%M:%S"; # Tickdata always has this format
      } 
      alldata <- suppressWarnings(makeXtsQuotes(qdata = data, format = format))
    }
    
    # Save the data: ############################ This is the same irrespective of the data provider
    # Create trading dates: 
    
    dates <- unique(as.Date(index(alldata)));
    
    # Create folder structure for saving : 
    suppressWarnings( 
      if (dir == TRUE) { 
        dir.create(datadestination, showWarnings = FALSE)
        for (i in 1:length(dates)) {
          dirname = paste(datadestination, "/", as.character(dates[i]), sep = "") 
          dir.create(dirname, showWarnings = FALSE) 
        }
      })
    
    for(i in 1:length(dates) ){ # Loop over days
      datadestinationx = paste(datadestination, "/", dates[i], sep = ""); 
      
      if (trades == TRUE) { 
        tdata        = alldata[as.character(dates[i])];
        xts_name     = paste(ticker, "_trades.RData", sep = "")
        destfullname = paste(datadestinationx,"/",xts_name,sep="");          
        save(tdata, file = destfullname); # Save daily in right folder:
      } 
      
      if( quotes == TRUE ){ 
        qdata        = alldata[as.character(dates[i])]; 
        xts_name     = paste(ticker, "_quotes.RData", sep = ""); 
        destfullname = paste(datadestinationx,"/",xts_name,sep=""); 
        save(qdata, file = destfullname); # Save daily in right folder: 
      }#End quotes if
    } #End save loop over days
  } #End oneday   
} #End convert function

### Manipulation functions:
#MANIPULATION FUNCTIONS:
TAQLoad <- function(tickers, from, to, trades = TRUE, quotes = FALSE, datasource = NULL, variables = NULL){ 
  if (is.null(datasource) == TRUE) {
    print("Please provide the argument 'datasource' to indicate in which folder your data is stored")
  }
  
  if ((trades&quotes) == FALSE) {#not both trades and quotes demanded
    for (ticker in tickers) {
      out <- uniTAQload( ticker = ticker , from = from, to = to , trades=trades,quotes=quotes,datasource = datasource,variables=variables);
      if (ticker == tickers[1]){ 
        totalout <- out 
      } else { 
        totalout <- merge(totalout,out) }
    }
  } else {#in case both trades and quotes
    totalout <- list()
    totalout[[1]] = TAQLoad( tickers = tickers , from = from, to = to , trades=TRUE, quotes=FALSE, datasource = datasource,variables=variables);
    totalout[[2]] = TAQLoad( tickers = tickers , from = from, to = to , trades=FALSE, quotes=TRUE, datasource = datasource,variables=variables);
  }
  return(totalout);
}

uniTAQload <- function(ticker,from,to,trades=TRUE,quotes=FALSE,datasource=NULL,variables=NULL){
  ##Function to load the taq data from a certain stock 
  #From&to (both included) should be in the format "%Y-%m-%d" e.g."2008-11-30"
 
  dates = timeDate::timeSequence(as.character(from),as.character(to), format = "%Y-%m-%d", FinCenter = "GMT")
  dates = dates[timeDate::isBizday(dates, holidays = timeDate::holidayNYSE(1960:2050))];
  
  if (trades) { 
    tdata <- NULL
    totaldata <-NULL;
    for(i in 1:length(dates)){
      datasourcex = paste(datasource,"/",dates[i],sep="");
      filename = paste(datasourcex,"/",ticker,"_trades.RData",sep="");
      
      
      ifmissingname = paste(datasourcex,"/missing_",ticker,".RData",sep="");  
      
      if(file.exists(ifmissingname)){warning(paste("No trades available on ",dates[i],sep="")); next;}
      if(!file.exists(filename)){warning(paste("The file ",filename," does not exist. Please read the documentation.",sep="")); next;}
      if(file.exists(ifmissingname)==FALSE){
        load(filename);
        if(i==1)  { 
          if (is.null(variables)) {
            totaldata=tdata
          } else {
            allnames <- as.vector(colnames(tdata))
            selection <- allnames%in%variables
            qq <- (1:length(selection))[selection]
            totaldata <- tdata[,qq]
          }    
        };
        if(i>1){
          if( is.null(variables)){totaldata=rbind(totaldata,tdata)
          }else{
            totaldata=rbind(totaldata,tdata[,qq])}
        }
        rm(tdata)
      }
    }
  }
  
  if(quotes){ qdata=NULL;
              totaldataq=NULL;
              for(i in 1:length(dates)){
                datasourcex = paste(datasource,"/",dates[i],sep="");
                filename = paste(datasourcex,"/",ticker,"_quotes.RData",sep="");
                ifmissingname = paste(datasourcex,"/missingquotes_",ticker,".RData",sep="");
                
                if(file.exists(ifmissingname)){warning(paste("no quotes available on ",dates[i],sep="")); next;}
                if(!file.exists(filename)){warning(paste("The file ",filename," does not exist. Please read the documentation.",sep="")); next;}
                if(file.exists(ifmissingname)==FALSE){
                  load(filename);
                  
                  if(i==1)  {
                    if( is.null(variables)){totaldataq=qdata;
                    }else{
                      allnames=as.vector(colnames(qdata));
                      selection = allnames%in%variables;
                      qq=(1:length(selection))[selection];
                      totaldataq=qdata[,qq];
                    }	  
                  }
                  if(i>1){
                    if( is.null(variables)){totaldataq=rbind(totaldataq,qdata);
                    }else{
                      totaldataq=rbind(totaldataq,qdata[,qq])};
                  }
                  rm(qdata);
                }
              }
  }
  
  if(trades&quotes){return(list(trades = totaldata,quotes=totaldataq))}
  if(trades==TRUE & quotes==FALSE){return(totaldata)}
  if(trades==FALSE & quotes==TRUE){return(totaldataq)}
}





LeeMyklandCV = function( beta = 0.999 , M = 78 ) {
  # Critical value for Lee-Mykland jump test statistic
  # Based on distribution of Maximum of M absolute normal random variables
  a = function(n){ a1=sqrt(2*log(n)) ; a2= (log(pi)+log(log(n))  )/( 2*sqrt(2*log(n))   ); return(a1-a2)             };
  b = function(n){ return( 1/sqrt(2*log(n) )  ) ; return(b)} ;
  return( -log(-log(beta))*b(M) + a(M)     )
}

 ###### end SPOTVOL FUNCTIONS formerly in periodicityTAQ #########

 ###### Liquidity functions formerly in in RTAQ  ######


tdatacheck = function(tdata){ 
  if(!is.xts(tdata)){stop("The argument tdata should be an xts object")}
  if(!any(colnames(tdata)=="PRICE")){stop("The argument tdata should have a PRICE column")}
}

tqdatacheck = function(tqdata){ 
  if(!is.xts(tqdata)){stop("The argument tqdata should be an xts object")}
  if(!any(colnames(tqdata)=="PRICE")){ stop("The argument tqdata should have a column containing the PRICE data. Could not find that column.")}
  if(!any(colnames(tqdata)=="BID")){    stop("The argument tqdata should have a column containing the BID. Could not find that column")}
  if(!any(colnames(tqdata)=="OFR")){    stop("The argument tqdata should have a column containing the ASK / OFR. Could not find that column")}
} 

rdatacheck = function(rdata,multi=FALSE){
  #if(!is.xts(rdata)){stop("The argument rdata should be an xts object")} CAN PERFECTLY BE A MATRIX FOR ALL FUNCTIONS SO FAR...
  if((dim(rdata)[2] < 2) & (multi)){stop("Your rdata object should have at least 2 columns")}
}


getTradeDirection <- function(tqdata,...) {
  if (hasArg(data) == TRUE) { 
    tqdata = data
    rm(data) 
  }
  tqdata = .check_data(tqdata);
  tqdatacheck(tqdata); 
  
  ##Function returns a vector with the inferred trade direction:
  ##NOTE: the value of the first (and second) observation should be ignored if price=midpoint for the first (second) observation.
  bid = as.numeric(tqdata$BID);
  offer = as.numeric(tqdata$OFR);
  midpoints = (bid + offer)/2;
  price = as.numeric(tqdata$PRICE);
  
  buy1 = price > midpoints; #definitely a buy
  equal = price == midpoints;
  dif1 = c(TRUE,0 < price[2:length(price)]-price[1:(length(price)-1)]);#for trades=midpoints: if uptick=>buy
  equal1 = c(TRUE,0 == price[2:length(price)]-price[1:(length(price)-1)]);#for trades=midpoints: zero-uptick=>buy
  dif2 = c(TRUE,TRUE,0 < price[3:length(price)]-price[1:(length(price)-2)]);
  
  buy = buy1 | (dif1 & equal) | (equal1 & dif2 & equal);
  
  buy[buy==TRUE]=1;
  buy[buy==FALSE]=-1;
  
  return(buy);
}

es = function(data){
  data = .check_data(data);
  #returns the effective spread as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  midpoints = (bid + offer)/2;
  price = as.numeric(data$PRICE);
  d = getTradeDirection(data);
  
  es=xts(2*d*(price-midpoints),order.by=index(data));
  return(es);
}

rs <- function(data, tdata, qdata) {
  data  <- .check_data(data)
  qdata <- .check_data(qdata)
  tdata <- .check_data(tdata)
  
  ###Function returns the realized spread as an xts object
  #Please note that the returned object can contain less observations than the original "data"
  #because of the need to find quotes that match the trades 5 min ahead
  
  #arguments
  #data=> xts object containing matched trades and quotes
  #tdata and qdata, the xts object containing the trades and quotes respectively
  
  ##First part solves the problem that unequal number of obs (in data and data2) is possible when computing the RS
  data2 <- matchTradesQuotes(tdata, qdata, adjustment = 300)
  if (dim(data2)[1]>dim(data)[1]) {
    condition = as.vector(as.character(index(data2)))%in%as.vector(as.character(index(data)))
    data2 = subset(data2,condition,select=1:(dim(data)[2]))
    data = subset(data,as.vector(as.character(index(data)))%in%as.vector(as.character(index(data2))),select=1:(dim(data2)[2]))
  }
  
  if(dim(data2)[1]<dim(data)[1]){
    condition = as.vector(as.character(index(data)))%in%as.vector(as.character(index(data2)))
    data = subset(data,condition,select=1:(dim(data2)[2]))
    data2 = subset(data2,as.vector(as.character(index(data2)))%in%as.vector(as.character(index(data))),select=1:(dim(data)[2]))
  }
  
  bid = as.numeric(data2$BID)
  offer = as.numeric(data2$OFR)
  midpoints = (bid + offer)/2
  price = as.numeric(data$PRICE)
  d = getTradeDirection(data)
  rs = 2*d*(price-midpoints)
  
  rs_xts = xts(rs,order.by=index(data))
  return(rs_xts)
}

value_trade = function(data) {
  data = .check_data(data)
  #returns the trade value as xts object
  price = as.numeric(data$PRICE)
  size = as.numeric(data$SIZE)
  
  value = xts(price*size,order.by=index(data))
  return(value)
}

signedValueTrade <- function(data){
  data <- .check_data(data);
  #returns the signed trade value as xts object
  price <- as.numeric(data$PRICE)
  size <- as.numeric(data$SIZE)
  d <- getTradeDirection(data)
  
  value = xts(d*price*size,order.by=index(data))
  return(value);
}


signed_trade_size = function(data){
  data = .check_data(data);
  #returns the signed size of the trade as xts object
  size = as.numeric(data$SIZE);
  d = getTradeDirection(data);
  
  value = xts(d*size,order.by=index(data));
  return(value);
}

di_diff = function(data){
  data = .check_data(data);
  #returns the depth imbalance (as a difference) as xts object
  bidsize = as.numeric(data$BIDSIZ);
  offersize = as.numeric(data$OFRSIZ);
  
  d = getTradeDirection(data);
  di = (d*(offersize-bidsize))/(offersize+bidsize);
  di_xts = xts(di,order.by=index(data));
  return(di_xts);
}

di_div = function(data){
  data = .check_data(data);
  #returns the depth imbalance (as a ratio) as xts object
  bidsize = as.numeric(data$BIDSIZ);
  offersize = as.numeric(data$OFRSIZ);
  d = getTradeDirection(data);
  
  di = (offersize/bidsize)^d;
  di_xts = xts(di,order.by=index(data));
  return(di_xts);
}

pes = function(data){
  data = .check_data(data);
  #returns the Proportional Effective Spread as xts object
  es = es(data);
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  midpoints = (bid + offer)/2;
  
  pes = es/midpoints
  pes_xts = xts(pes,order.by=index(data));
  return(pes_xts);
}

prs = function(data,tdata,qdata){
  data  = .check_data(data);
  tdata = .check_data(tdata);
  qdata = .check_data(qdata);
  
  #returns the Proportional Realized Spread as xts object
  rs = rs(data,tdata,qdata);
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  midpoints = (bid + offer)/2;
  prs = rs/midpoints
  prs_xts = xts(prs,order.by=index(data));
  return(prs_xts);
}

price_impact = function(data,tdata,qdata){
  data = .check_data(data);
  #returns the Price impact as xts object
  rs = rs(data,tdata,qdata);
  es = es(data);
  
  pi = (es-rs)/2;
  pi_xts = xts(pi,order.by=index(data));
  return(pi_xts);
}

prop_price_impact = function(data,tdata,qdata){
  data = .check_data(data);
  #returns the Proportional Price impact as xts object
  rs = rs(data,tdata,qdata);
  es = es(data);
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  midpoints = (bid + offer)/2;
  
  prop_pi = ((es-rs)/2)/midpoints;
  prop_pi_xts = xts(prop_pi,order.by=index(data));
  return(prop_pi_xts);
}

tspread = function(data){
  data = .check_data(data);
  #returns the half traded spread as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  midpoints = (bid + offer)/2;
  price = as.numeric(data$PRICE);
  d = getTradeDirection(data);
  
  ts = xts(d*(price-midpoints),order.by=index(data));
  return(ts);
}

pts = function(data){
  data = .check_data(data);
  #returns the proportional half traded spread as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  midpoints = (bid + offer)/2;
  price = as.numeric(data$PRICE);
  d = getTradeDirection(data);
  pts = (d*(price-midpoints))/midpoints;
  
  pts_xts = xts(pts,order.by=index(data));
  return(pts_xts);
}

p_return_sqr = function(data){
  data = .check_data(data);
  #returns the squared log return on Trade prices as xts object
  price = as.numeric(data$PRICE);
  return = c(0,log(price[2:length(price)])-log(price[1:length(price)-1]));
  sqr_return = return^2;
  
  sqr_return_xts = xts(sqr_return,order.by=index(data));
  return(sqr_return_xts);
}

qs = function(data){
  data = .check_data(data);
  #returns the quoted spread as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  qs = offer-bid;
  
  qs_xts = xts(qs,order.by=index(data));
  return(qs_xts);
}

pqs = function(data){
  data = .check_data(data);
  #returns the proportional quoted spread as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  midpoints = (bid + offer)/2;
  qs = offer-bid;
  pqs = qs/midpoints;
  
  pqs_xts = xts(pqs,order.by=index(data));
  return(pqs_xts);
}

logqs = function(data){
  data = .check_data(data);
  #returns the logarithm of the quoted spread as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  logqs = log(offer/bid);
  
  logqs_xts = xts(logqs,order.by=index(data));
  return(logqs_xts);
}

logsize = function(data){
  data = .check_data(data);
  #returns the log quoted size as xts object
  bidsize = as.numeric(data$BIDSIZ);
  offersize = as.numeric(data$OFRSIZ);
  logsize = log(bidsize)+log(offersize);
  
  logsize_xts = xts(logsize,order.by=index(data));
  return(logsize_xts);
}

qslope = function(data){
  data = .check_data(data);
  #returns the quoted slope as xts object
  logsize = logsize(data);
  qs = qs(data);
  
  qslope = qs/logsize;
  
  qslope_xts = xts(qslope,order.by=index(data));
  return(qslope_xts);
}

logqslope = function(data){
  data = .check_data(data);
  #returns the log quoted slope as xts object
  logqs = logqs(data);
  logsize = logsize(data);
  
  logqslope = logqs/logsize;
  
  logqslope_xts = xts(logqslope,order.by=index(data));
  return(logqslope_xts);
}

mq_return_sqr = function(data){
  data = .check_data(data);
  #returns midquote squared returns slope as xts object
  mq_return = mq_return(data);
  
  mq_return_sqr = mq_return^2;
  
  mq_return_sqr_xts = xts(mq_return_sqr,order.by=index(data));
  return(mq_return_sqr_xts);
}

mq_return_abs = function(data){ 
  data = .check_data(data);
  #returns absolute midquote returns slope as xts object
  mq_return = mq_return(data);
  
  mq_return_abs = abs(mq_return);
  
  mq_return_abs_xts = xts(mq_return_abs,order.by=index(data));
  return(mq_return_abs_xts);
}

tqLiquidity <- function(tqdata=NULL, tdata = NULL, qdata = NULL, type, ...) {
  if (hasArg(data)){ 
    tqdata = data 
  }
  if (!is.null(tqdata)) {
    tqdatacheck(tqdata)
  }
  if (!is.null(qdata)) {
    qdatacheck(qdata)
  }
  if (!is.null(tdata)) {
    tdatacheck(tdata)
  }
  
  result=switch(type,
                es = es(tqdata),
                rs = rs(tqdata,tdata,qdata),
                value_trade = value_trade(tqdata),
                signed_value_trade = signedValueTrade(tqdata),
                di_diff = di_diff(tqdata),
                pes = pes(tqdata),
                prs = prs(tqdata,tdata,qdata),
                price_impact = price_impact(tqdata,tdata,qdata),
                prop_price_impact = prop_price_impact(tqdata,tdata,qdata),
                tspread = tspread(tqdata),
                pts = pts(tqdata),
                p_return_sqr = p_return_sqr(tqdata),
                p_return_abs = p_return_abs(tqdata),
                qs = qs(tqdata),
                pqs = pqs(tqdata),
                logqs = logqs(tqdata),
                logsize = logsize(tqdata),
                qslope = qslope(tqdata),
                logqslope = logqslope(tqdata),
                mq_return_sqr = mq_return_sqr(tqdata),
                mq_return_abs = mq_return_abs(tqdata),
                signed_trade_size = signed_trade_size(tqdata)
  )
  
  colnames(result)=type;
  return(result);
}

##help_function:
mq_return = function(data){
  data = .check_data(data);
  #function returns the midquote logreturns as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  midpoints = (bid + offer)/2;
  mq_return = c(0,log(midpoints[2:length(midpoints)])-log(midpoints[1:length(midpoints)-1]));
  
  mq_return_xts = xts(mq_return,order.by=index(data));
  return(mq_return_xts);
}

p_return_abs <- function (data) {
  price = as.numeric(data$PRICE)
  return = c(0, log(price[2:length(price)]) - log(price[1:length(price) -
    1]))
  abs_return = abs(return)
  abs_return_xts = xts(abs_return, order.by = index(data))
  return(abs_return_xts)
}
             

##################### Total cleanup functions formerly in RTAQ ################################
quotesCleanup <- function(from, to, datasource, datadestination, ticker, exchanges, qdataraw = NULL, report = TRUE, 
                          selection="median", maxi = 50, window = 50, type = "advanced", rmoutliersmaxi = 10, ...) {
  nresult = rep(0,7)
  if(is.null(qdataraw)){
    dates = timeDate::timeSequence(from,to, format = "%Y-%m-%d", FinCenter = "GMT")
    dates = dates[timeDate::isBizday(dates, holidays = timeDate::holidayNYSE(1960:2050))]
    
    for(j in 1:length(dates)){
      datasourcex = paste(datasource,"/",dates[j],sep="");
      datadestinationx = paste(datadestination,"/",dates[j],sep="");
      
      for(i in 1:length(ticker)){
        dataname = paste(ticker[i],"_quotes.RData",sep="");
        load(paste(datasourcex,"/",dataname,sep=""));
        
        if(class(qdata)[1]!="try-error"){
          exchange = exchanges[i];  
          if(exchange=="Q"){exchange="T"}
          
          qdata = .check_data(qdata); nresult[1] = nresult[1]+dim(qdata)[1];
          ##actual clean-up:
          ##general:
          qdata = try(nozeroquotes(qdata)); nresult[2] = nresult[2]+dim(qdata)[1]
          qdata = try(selectexchange(qdata,exch=exchange)); nresult[3] = nresult[3]+dim(qdata)[1]
          
          ##quote specific:
          qdata = try(rmnegspread(qdata)); nresult[4] = nresult[4]+dim(qdata)[1]
          qdata = try(rmlargespread(qdata,maxi=maxi)); nresult[5] = nresult[5]+dim(qdata)[1]
          
          qdata = try(mergequotessametimestamp(qdata,selection=selection))
          nresult[6] = nresult[6] + dim(qdata)[1]
          qdata = try(rmoutliers(qdata,maxi=rmoutliersmaxi,window=window,type=type)); nresult[7] = nresult[7]+dim(qdata)[1];
          
          save(qdata, file = paste(datadestinationx,"/",dataname,sep=""));
        }
        
        if(class(qdata)=="try-error"){
          abc=1;
          save(abc, file = paste(datadestinationx,"/missingquotes_",ticker[i],".RData",sep=""));
        }
      }
    }
  }
  
  if(!is.null(qdataraw)){
    if(class(qdataraw)[1]!="try-error"){
      if(length(exchanges)>1){print("The argument exchanges contains more than 1 element. Please select a single exchange, in case you provide qdataraw.")}
      if(class(qdataraw)[1]!="try-error"){
        exchange=exchanges;
        qdata = qdataraw; rm(qdataraw) 
        
        qdata = .check_data(qdata); nresult[1] = nresult[1]+dim(qdata)[1];
        ##actual clean-up:
        ##general:
        qdata = try(nozeroquotes(qdata));                                           nresult[2] = nresult[2]+dim(qdata)[1];
        qdata = try(selectexchange(qdata,exch=exchange));                           nresult[3] = nresult[3]+dim(qdata)[1];
        
        ##quote specific:
        qdata = try(rmnegspread(qdata));                                            nresult[4] = nresult[4]+dim(qdata)[1];
        qdata = try(rmlargespread(qdata,maxi=maxi));                                nresult[5] = nresult[5]+dim(qdata)[1];
        
        qdata = try(mergequotessametimestamp(qdata,selection=selection));           nresult[6] = nresult[6]+dim(qdata)[1];
        qdata = try(rmoutliers(qdata,maxi=rmoutliersmaxi,window=window,type=type)); nresult[7] = nresult[7]+dim(qdata)[1];
        
        if(report==TRUE){
          names(nresult) = c("initial number","no zero quotes","select exchange",
                             "remove negative spread","remove large spread","merge same timestamp","remove outliers");
          return(list(qdata=qdata,report=nresult))
        }
        if(report!=TRUE){return(qdata)}
      }
    }
  }
}


tradesCleanupFinal = function(from,to,datasource,datadestination,ticker,tdata=NULL,qdata=NULL,...){
  if(is.null(tdata)&is.null(qdata)){
    dates = timeDate::timeSequence(from,to, format = "%Y-%m-%d", FinCenter = "GMT");
    dates = dates[timeDate::isBizday(dates, holidays = timeDate::holidayNYSE(1960:2050))];
    
    for(j in 1:length(dates)){
      datasourcex = paste(datasource,"/",dates[j],sep="");
      datadestinationx = paste(datadestination,"/",dates[j],sep="");
      
      for(i in 1:length(ticker)){
        dataname = paste(ticker[i],"_trades.RData",sep="");
        dataname2 = paste(ticker[i],"_quotes.RData",sep="");
        
        # Missing file??
        m1 = paste(datasourcex,"/missing_",ticker[i],".RData",sep="");
        m2 = paste(datasourcex,"/missingquotes_",ticker[i],".RData",sep="");
        miscondition = file.exists(m1)|file.exists(m1);
        a=FALSE;#check whether tried to clean
        
        if(!miscondition){
          # load trades and quotes
          load(paste(datasourcex,"/",dataname,sep=""));
          load(paste(datasourcex,"/",dataname2,sep=""));
          
          tdata = .check_data(tdata);
          qdata = .check_data(qdata);
          
          #1 cleaning procedure that needs cleaned trades and quotes
          tdata = try(rmtradeoutliers(tdata,qdata));
          
          #save
          save(tdata, file = paste(datadestinationx,"/",dataname,sep=""));
          a=TRUE;
        }
        
        if(a==TRUE){a=(class(tdata)=="try-error")}
        
        if(miscondition|a)  {
          abc=1;
          save(abc, file = paste(datadestinationx,"/missing_",ticker[i],".RData",sep=""));
        }
      }
    }
  }
  
  if((!is.null(tdata))&(!is.null(qdata))){
    tdata = .check_data(tdata);
    qdata = .check_data(qdata);
    
    #1 cleaning procedure that needs cleaned trades and quotes
    tdata = try(rmtradeoutliers(tdata, qdata))
    return(tdata)
  }
}

##################### Specific cleanup functions formerly in RTAQ ################################
## Help functions : 
## Help function to make all time notation consistent
adjtime = function(z){ 
  zz = unlist(strsplit(z,":")); 
  if(nchar(zz[1])!=2){
    return(paste(paste(0,zz[1],sep=""),zz[2],zz[3],sep=":"))}
  return(z);
}


autoSelectExchangeTrades = function(tdata){
  tdata = .check_data(tdata);
  tdatacheck(tdata);
  ## AUTOSELECT EXCHANGE WITH HIGHEST NUMBER OF SHARES TRADED (for trades) ON:
  #function returns ts with obs of only 1 exchange
  #searches exchange with a maximum on the variable "SIZE"
  nobs <- c()
  
  exchanges = c("Q","A","P","B","C","N","D","X","I","M","W","Z");
  exchangenames = c("NASDAQ","AMEX","ARCA","Boston","NSX","NYSE","NASD ADF and TRF","Philadelphia","ISE","Chicago","CBOE","BATS");
  
  
  z1 = sum(as.numeric(selectexchange(tdata,"Q")$SIZE));
  z2 = sum(as.numeric(selectexchange(tdata,"T")$SIZE));
  z = max(z1,z2);
  watchout = z == z2;
  nobs = cbind(nobs,z);
  
  for(i in 2:length(exchanges)) {
    z = sum(as.numeric(selectexchange(tdata,exchanges[i])$SIZE));
    nobs = cbind(nobs,z); 
  }
  
  exch = exchanges[max(nobs)==nobs];
  
  as.character(tdata$EX[1]) == exchanges;
  namechosen = exchangenames[exch==exchanges];
  print(paste("The information of the",namechosen,"exchange was collected"));
  
  if(exch=="Q"&watchout){exch="T"}
  filteredtdata = tdata[tdata$EX==exch];
}


##### TRADE DATA SPECIFIC FUNCTIONS: ###################################


##Merge same timestamp:
sumN = function(a){
  a = sum(as.numeric(a));
  return(a)
}

medianN = function(a){
  a = median(as.numeric(a));
  return(a)
}

maxvol = function(a){
  p <- as.numeric(a[,1])
  s <- as.numeric(a[,2])
  
  b <- median(p[s == max(s)])
  return(b)
}

waverage <- function(a) {
  p <- as.numeric(a[,1])
  s <- as.numeric(a[,2])
  
  b <- sum(p * s / sum(s))
  return(b)
}

rmTradeOutliers = function(tdata,qdata){
  tdata = .check_data(tdata);
  qdata = .check_data(qdata);
  qdatacheck(qdata);
  tdatacheck(tdata);
  
  ##Function to delete entries with prices that are above the ask plus the bid-ask
  ##spread. Similar for entries with prices below the bid minus the bid-ask
  ##spread.
  data = matchTradesQuotes(tdata,qdata);
  price = as.numeric(data$PRICE);
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  spread = offer - bid;
  
  upper = offer+spread;
  lower = bid-spread;
  
  tdata = tdata[(price<upper) & (price>lower)];
  return(tdata);
}

#################       QUOTE SPECIFIC FUNCTIONS:       #################

autoSelectExchangeQuotes = function(qdata){
  qdata = .check_data(qdata);
  qdatacheck(qdata);
  ####Autoselect exchange with highest value for (bidsize+offersize)
  nobs=c();
  exchanges = c("Q","A","P","B","C","N","D","X","I","M","W","Z");
  exchangenames = c("NASDAQ","AMEX","ARCA","Boston","NSX","NYSE","NASD ADF and TRF","Philadelphia","ISE","Chicago","CBOE","BATS");
  
  selected1 = selectexchange(qdata,"Q");
  selected2 = selectexchange(qdata,"T");
  z1 = sum(as.numeric(selected1$BIDSIZ)+as.numeric(selected1$OFRSIZ));
  z2 = sum(as.numeric(selected2$BIDSIZ)+as.numeric(selected2$OFRSIZ));
  z = max(z1,z2);
  watchout = z == z2;
  nobs = cbind(nobs,z);
  
  for(i in 2:length(exchanges)) {
    selected = selectexchange(qdata,exchanges[i]);
    z = sum(as.numeric(selected$BIDSIZ)+as.numeric(selected$OFRSIZ));
    nobs = cbind(nobs,z); 
  }
  
  exch=exchanges[max(nobs)==nobs];
  
  namechosen = exchangenames[exch==exchanges];  
  print(paste("The information of the",namechosen,"exchange was collected"));
  
  if(exch=="Q"&watchout){exch="T"}
  
  filteredts = qdata[qdata$EX==exch];
  return(filteredts);
}





# rmNegativeSpread = function(qdata){
#   qdata = .check_data(qdata);
#   qdatacheck(qdata);
#   ##function to remove observations with negative spread
#   condition = as.numeric(qdata$OFR)>as.numeric(qdata$BID);
#   qdata[condition];
# }


# Zivot : 
correctedTrades <- function (tdata){ 
  tdatacheck(tdata);
  filteredts = tdata[tdata$CR == " 0"];
  return(filteredts)
}

autoselectexchange = function(...){autoSelectExchangeTrades(...)};        
autoselectexchangeq = function(...){autoSelectExchangeQuotes(...)};       
ExchangeHoursOnly = function(...){exchangeHoursOnly(...)};                
mergequotessametimestamp = function(...){mergeQuotesSameTimestamp(...)};  
mergesametimestamp = function(...){mergeTradesSameTimestamp(...)};        
nozeroprices = function(...){noZeroPrices(...)};                          
nozeroquotes = function(...){noZeroQuotes(...)};                          
rmlargespread = function(...){rmLargeSpread(...)};                        
rmnegspread = function(...){rmNegativeSpread(...)};                       
rmoutliers = function(...){rmOutliers(...)};                              
rmtradeoutliers = function(...){rmTradeOutliers(...)};                    
salescond = function(...){salesCondition(...)};                           
selectexchange = function(...){selectExchange(...)};                      







####### Aggregation functions that were formerly in RTAQ ######################

previoustick = function(a){
  a = as.vector(a);
  b = a[length(a)];
  return(b)
}

weightedaverage = function(a){
  aa = as.vector(as.numeric(a[,1]));
  bb = as.vector(as.numeric(a[,2]));
  c = weighted.mean(aa,bb);
  return(c)
}

period.apply2 = function (x, INDEX, FUN2, ...) 
{
  x <- try.xts(x, error = FALSE)
  FUN <- match.fun(FUN2)
  xx <- sapply(1:(length(INDEX) - 1), function(y) {
    FUN(x[(INDEX[y] + 1):INDEX[y + 1]], ...)
  })
  reclass(xx, x[INDEX])
}

## AGGREGATION;
aggregatets = function (ts, FUN = "previoustick", on = "minutes", k = 1, weights = NULL,dropna=FALSE) {
  makethispartbetter = ((!is.null(weights))| on=="days"|on=="weeks"| (FUN!="previoustick")|dropna);
  if(makethispartbetter)  {
    
    FUN = match.fun(FUN);
    
    if (is.null(weights)) {
      ep = endpoints(ts, on, k)
      if(dim(ts)[2]==1){ ts2 = period.apply(ts, ep, FUN) }
      if(dim(ts)[2]>1){  ts2 = xts(apply(ts,2,FUN=period.apply2,FUN2=FUN,INDEX=ep),order.by=index(ts)[ep],)}
    }
    if (!is.null(weights)) {
      tsb = cbind(ts, weights)
      ep = endpoints(tsb, on, k)
      ts2 = period.apply(tsb, ep, FUN = match.fun(weightedaverage) )
    }
    if (on == "minutes" | on == "mins" | on == "secs" | on == 
      "seconds") {
      if (on == "minutes" | on == "mins") {
        secs = k * 60
      }
      if (on == "secs" | on == "seconds") {
        secs = k
      }
      a = .index(ts2) + (secs - .index(ts2)%%secs)
      ts3 = .xts(ts2, a,tzone="GMT")
    }
    if (on == "hours") {
      secs = 3600
      a = .index(ts2) + (secs - .index(ts2)%%secs)
      ts3 = .xts(ts2, a,tzone="GMT")
    }
    if (on == "days") {
      secs = 24 * 3600
      a = .index(ts2) + (secs - .index(ts2)%%secs) - (24 * 
        3600)
      ts3 = .xts(ts2, a,tzone="GMT")
    }
    if (on == "weeks") {
      secs = 24 * 3600 * 7
      a = (.index(ts2) + (secs - (.index(ts2) + (3L * 86400L))%%secs)) - 
        (24 * 3600)
      ts3 = .xts(ts2, a,tzone="GMT")
    }
    
    if (!dropna) {
      if (on != "weeks" | on != "days") {
        if (on == "secs" | on == "seconds") {
          tby = "s"
        }
        if (on == "mins" | on == "minutes") {
          tby = "min"
        }
        if (on == "hours") {
          tby = "h"
        }
        by = paste(k, tby, sep = " ")
        allindex = as.POSIXct(base::seq(start(ts3), end(ts3), 
                                         by = by))
        xx = xts(rep("1", length(allindex)), order.by = allindex)
        ts3 = merge(ts3, xx)[, (1:dim(ts)[2])]
      }
    }
    
    index(ts3) = as.POSIXct(index(ts3));
    return(ts3);
  }
  
  if(!makethispartbetter){
    if (on == "secs" | on == "seconds") { secs = k; tby = paste(k,"sec",sep=" ")}
    if (on == "mins" | on == "minutes") { secs = 60*k; tby = paste(60*k,"sec",sep=" ")}
    if (on == "hours") {secs = 3600*k; tby = paste(3600*k,"sec",sep=" ")}
    
    FUN = match.fun(FUN);
    
    g = base::seq(start(ts), end(ts), by = tby);
    rawg = as.numeric(as.POSIXct(g,tz="GMT"));
    newg = rawg + (secs - rawg%%secs);
    g    = as.POSIXct(newg,origin="1970-01-01",tz="GMT");
    ts3  = na.locf(merge(ts, zoo(, g)))[as.POSIXct(g,tz="GMT")]
    return(ts3) 
  }
}

#PRICE (specificity: opening price and previoustick)

aggregatePrice = function (ts, FUN = "previoustick", on = "minutes", k = 1, marketopen = "09:30:00", marketclose = "16:00:00") {
  ts2 = aggregatets(ts, FUN = FUN, on, k)
  date = strsplit(as.character(index(ts)), " ")[[1]][1]
  
  #open
  a = as.POSIXct(paste(date, marketopen),tz="GMT")
  b = as.xts(matrix(as.numeric(ts[1]),nrow=1), a)
  ts3 = c(b, ts2)
  
  #close
  aa = as.POSIXct(paste(date, marketclose),tz="GMT")
  condition = index(ts3) < aa
  ts3 = ts3[condition]
  bb = as.xts(matrix(as.numeric(last(ts)),nrow=1), aa)
  ts3 = c(ts3, bb)
  
  return(ts3)
}

#VOLUME: (specificity: always sum)
aggVolume <- function(ts, FUN = "sumN", on = "minutes", k = 5, includeopen = FALSE, marketopen = "09:30:00", marketclose = "16:00:00") {
  if (includeopen == FALSE) {
    ts3 <- aggregatets(ts, FUN = FUN, on, k)
  } else {
    ts2 <- aggregatets(ts, FUN = FUN, on, k)
    date <- strsplit(as.character(index(ts)), " ")[[1]][1]
    a <- as.POSIXct(paste(date, marketopen), tz = "GMT")
    b <- as.xts(matrix(as.numeric(ts[1]),nrow=1), a)
    ts3 <- c(b, ts2)
  }
  
  aa <- as.POSIXct(paste(date, marketclose),tz="GMT")
  condition <- {index(ts3) < aa}
  ts4 <- ts3[condition]
  
  lastinterval <- matrix(colSums(matrix(ts3[!condition],ncol=dim(ts3)[2])),ncol=dim(ts3)[2])
  bb <- xts(lastinterval, aa)
  ts4 <- c(ts4, bb)
  
  return(ts4)
}

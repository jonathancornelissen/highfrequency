#' library(data.table)
#' library(xts)
#' library(anytime)
#' ### Rank test at jump events
#' rm(list = ls())
#' 
#' 
#' dat <- fread("/home/emil/Dropbox/GSOC/code_from_papers/OnlineSoftware/dz.csv")
#' dx <- fread("/home/emil/Dropbox/GSOC/code_from_papers/OnlineSoftware/dx.csv")
#' 
#' 
#' nDays <- nrow(dat)/390
#' timestamps <- rep(seq(34260, 57600, 60), nDays) + rep(0:(nDays-1) * 86400, each = nrow(dat)/nDays)
#' marketReturns <- xts(dat$V1, anytime(timestamps))
#' 
#' stockReturns <- xts(dx, anytime(timestamps))
#' 
#' alpha = c(7, 4)
#' 
#' 
#' timeOfDayAdjustments <- function(returns, n, m, polyOrder){
#'   
#'   
#'   timePolyMatrix <- matrix(rep(1:nrow(returns), each = polyOrder + 1)^(0:polyOrder), nrow = nrow(returns), ncol = polyOrder + 1, byrow = TRUE)
#'   
#'   # 1.249531 is a constant from the author's implementation.
#'   #timeOfDayScatter <- matrix(1.249531 * rowMeans(abs(returns[,1:(m-2)]) ^ (2/3) * abs(returns[,2:(m-1)])^ (2/3) * abs(returns[,3:m])^ (2/3)))
#'   #
#'   
#'   timeOfDayScatter <- 1.249531 * rowMeans(abs(returns[,1:(m-2)]* returns[,2:(m-1)] * returns[,3:m])^ (2/3))
#' 
#'   
#'   
#'   timeOfDayBeta <- as.numeric(solve(t(timePolyMatrix) %*% timePolyMatrix) %*% t(timePolyMatrix) %*% timeOfDayScatter)
#'   
#'   timeOfDayFit <- timePolyMatrix %*% timeOfDayBeta
#'   
#'   # Normalize the fit
#'   timeOfDayFit <- timeOfDayFit / mean(timeOfDayFit)
#'   
#'   
#'   timeOfDayScatter <- timeOfDayScatter/mean(timeOfDayScatter)
#'   out <- list("timeOfDayScatter" = timeOfDayScatter, "timeOfDayFit" = timeOfDayFit, "timeOfDayBeta" = timeOfDayBeta, "timePolyMatrix" = timePolyMatrix) 
#'   
#'   return(out)
#'   
#'   # plot(timeOfDayScatter)
#'   # lines(timeOfDayFit, col = 2)
#' }
#' 
#' 
#' #' @importFrom zoo coredata
#' jumpDetection <- function(returns, alpha, nRets, nDays){
#'   
#'   returns <- matrix(coredata(returns), nrow = nRets, ncol = nDays, byrow = FALSE) #remap returns
#'   bpv <- pi/2 * colSums(abs(returns[1:(nRets-1),]) * abs(returns[2:nRets,]))
#'   rv <- colSums(returns^2)
#'   TODadjustments <- timeOfDayAdjustments(returns, n=nRets,  m = nDays, polyOrder = 2)
#'   
#'   Un <- alpha * sqrt(kronecker(pmin(bpv,rv), TODadjustments$timeOfDayFit)) * (1/nRets) ^0.49
#'   
#'   jumpIndices <- which(abs(as.numeric(returns)) > Un) # Where does a jump in the market occur?
#'   
#'   out <- list("jumpIndices" = jumpIndices, "Un" = Un, "timeOfDayADJ" = TODadjustments$timeOfDayFit)
#'   return(out)
#'   
#' }
#' 
#' #find better name?
#' BoxCox__ <- function(x, lambda){
#'   if(!lambda)
#'     return(log(1+x))
#'   else 
#'     return(((1+x)^lambda - 1)/lambda)
#' }
#' 
#' 
#' 
#' 
#' #' @importFrom zoo coredata
#' rankJumpTest <- function(marketReturns, stockReturns, alpha = c(7,4), K = 10, kn = 30, r = 1, BoxCox = 1, nBoot = 1000, dontTestAtBoundaries = TRUE){
#'   
#'   
#'   if(!is.numeric(alpha)){
#'     stop("alpha must be a numeric and should be of length one or two")
#'   }
#'   if(length(alpha) == 1){
#'     alpha = rep(alpha,2)
#'   } else if(length(alpha) > 2){
#'     warning("alpha should be a numeric of length one or two")
#'     alpha = alpha[1:2]
#'   }
#'   nDays <- ndays(marketReturns)
#'   nRets <- nrow(marketReturns)/nDays
#'   marketJumpDetection <- jumpDetection(marketReturns, alpha[1], nRets = nRets, nDays = nDays)
#'   jumpIndices <- marketJumpDetection$jumpIndices
#'   
#'   nAssets <- ncol(stockReturns)
#'   stockJumpDetections <- stockReturns
#'   for(j in 1:nAssets){
#'     stockJumpDetections[, j] <- jumpDetection(stockJumpDetections[,j], alpha[2], nRets = nRets, nDays = nDays)[["Un"]]
#'   }
#'   
#'   
#'   jumps <- matrix(coredata(stockReturns)[jumpIndices,], ncol = ncol(stockReturns), byrow = FALSE)
#'   
#'   for(i in 1:(K-1)){
#'     jumps <- jumps + matrix(stockReturns[jumpIndices + i, ], ncol = ncol(stockReturns), byrow = FALSE)
#'   }
#'   
#'   jumps <- t(jumps) # Transpose here so we don't have to transpose every time in the loop.
#'   
#'   # Set nu and nv because we need full SVD (see https://stackoverflow.com/questions/41972419/different-svd-result-in-r-and-matlab)
#'   decomp <- svd(jumps, nu = nrow(jumps), nv = ncol(jumps)) 
#'   U2 <- decomp$u[, (r+1):ncol(decomp$u)]
#'   V2 <- decomp$v[, (r+1):ncol(decomp$v)]
#'   
#'   singularValues <- decomp$d[(r+1):length(decomp$d)]^2
#'   
#'   testStatistic <- numeric(length(BoxCox))
#'   for (i in 1:length(BoxCox)) {
#'     a <- BoxCox[i]
#'     testStatistic[i] <- sum(BoxCox__(singularValues, a))
#'   }
#'   
#'   
#'   
#'   ## Start bootstrapping of the critical values
#'   
#'   p <- ncol(jumps)
#'   dxc <- pmax(pmin(coredata(stockReturns), coredata(stockJumpDetections)), -coredata(stockJumpDetections))
#'   siumulatedTestStatistics <- numeric(nBoot)
#'   
#'   for (b in 1:nBoot) {
#'     zetaStar <- matrix(0, nrow = nAssets, ncol = p)
#'     for (i in 1:p) {
#'       jmp <- jumpIndices[i] 
#'       
#'       if(dontTestAtBoundaries){
#'         # We need to make sure that we don't take data from the previous day
#'         pos <- ((jmp - 1) %% nRets) + 1
#'         leftKN <- min(kn, pos-1)
#'         rightKN <- min(kn, nRets - pos)
#'       } else {
#'         leftKN <- kn
#'         rightKN <- kn
#'       }
#'       
#'       jumpLeft <- ceiling(runif(1) * leftKN)
#'       jumpRight <- ceiling(runif(1) * rightKN)
#'       
#'       dxcLeft <- t(dxc[jmp - jumpLeft,])
#'       dxcRight <- t(dxc[jmp + jumpRight,])
#'       
#'       kappaStar <- runif(1)
#'       
#'       zetaStar[,i]  <- sqrt(kappaStar) * dxcLeft + sqrt(K - kappaStar) * dxcRight
#'       
#'     }
#'     
#'     tmp <- t(U2) %*% zetaStar %*% V2
#'     siumulatedTestStatistics[b] <- t(as.numeric(tmp)) %*% as.numeric(tmp)
#'     
#'   }
#'   
#'   criticalValues <- quantile(siumulatedTestStatistics, c(0.9, 0.95, 0.99))
#'   
#'   out <- list("criticalValues" = criticalValues, "testStatistic" = testStatistic, "dimensions" = dim(jumps),
#'               "marketJumpDetections" = marketJumpDetection, "stockJumpDetections" = stockJumpDetections, "jumpIndices" = jumpIndices)
#'   
#'   return(out)
#' }
#' 
#' 
#' 
#' jumpTest <- rankJumpTest(marketReturns, stockReturns, alpha, K, kn, r, BoxCox, nBoot = 1000, dontTestAtBoundaries = TRUE)
#' 
#' jumpTest$criticalValues
#' 
#' for (jmp in 1:length(jumpTest$jumpIndices)) {
#'   print(plot(cbind(marketReturns[(jumpTest$jumpIndices[jmp]-10):(jumpTest$jumpIndices[jmp]+10)], 
#'              stockReturns[(jumpTest$jumpIndices[jmp]-10):(jumpTest$jumpIndices[jmp]+10),])))
#'   
#' }
#' 

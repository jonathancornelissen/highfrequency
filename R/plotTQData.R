#' Plot Trade and Quote data
#' 
#' @description 
#' Plot trade and quote data, trades are marked by crosses, and quotes are plotted as boxes denoting the bid-offer spread for all the quotes.
#' 
#' @param tData cleaned trades data
#' @param qData cleaned quotes data
#' @param xLim timestamps for the start and the end of the plots.
#' @param tradeCol color in which to paint the trade crosses.
#' @param quoteCol color in which to fill out the bid-offer spread.
#' @param ... passed to \code{plot} and \code{points}.
#' 
#' @examples 
#' cleanedQuotes = quotesCleanup(qDataRaw = sampleQDataRaw, report = FALSE, printExchange = FALSE)
#' cleanedTrades <- tradesCleanupUsingQuotes(
#'         tData = tradesCleanup(tDataRaw = sampleTDataRaw, report = FALSE, printExchange = FALSE),
#'         qData = quotesCleanup(qDataRaw = sampleQDataRaw, report = FALSE, printExchange = FALSE)
#'         )[as.Date(DT) == "2018-01-03"]
#' xLim <- range(as.POSIXct(c("2018-01-03 15:30:00", "2018-01-03 16:00:00"), tz = "EST"))
#' plotTQData(cleanedTrades, cleanedQuotes, xLim = xLim, 
#'            main = "Raw trade and quote data from NYSE TAQ")
#' @importFrom data.table %between%
#' @importFrom graphics rect
#' @export
plotTQData <- function(tData, qData = NULL, xLim = NULL, tradeCol = "black", quoteCol = "darkgray", ...){
  DT_END <- BID <- OFR <- DT <- NULL
  if(is.null(qData)){
    qData <- copy(tData)
  } else {
    qData <- checkColumnNames(qData)
  }
  if(is.null(xLim)) xLim <- range(tData$DT, qData$DT)
  
  yLim <- range(c(tData[DT %between% xLim]$PRICE, qData[DT %between% xLim & BID!=0 & OFR != 0, list(BID, OFR)]))
  qData <- copy(qData)
  qData <- qData[BID != 0 & OFR != 0]
  
  qData[, DT_END := shift(DT, type = 'lead')]
  qData <- qData[DT %between% xLim]
  plot(0, 0, col = "white", ylim = yLim, xlim = xLim, xaxs = "i", lwd = 1, xaxt = "n", ...)
  rect(qData$DT, qData$BID, qData$DT_END, qData$OFR, border = quoteCol, col = quoteCol)
  rect(qData$DT, qData$OFR, qData$DT_END, qData$BID, border = quoteCol, col = quoteCol)
  
  points(tData$DT, tData$PRICE, ylab = "price", pch = "x", xaxs = "i", ylim = yLim, col = tradeCol, ...)
  axis.POSIXct(1, x = seq(xLim[1], xLim[2], length.out = 7))
}

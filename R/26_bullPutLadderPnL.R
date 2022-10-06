#'Calculates per share Profit and Loss at expiration for Bull Put Ladder and draws its graph in the Plots tab.
#'@description
#'This is a vertical spread consisting of a short position in (usually) a close to ATM put option with a strike price X3H, a long position in an OTM put option with a strike price X2M, and a long position in another OTM put option with a lower strike price X1L. A bull put ladder typically arises when a bull put spread (a bullish strategy) goes wrong (that is the stock trades lower), so the trader buys another OTM put option (with the lower strike price X1L) to adjust the position to bearish. On initiation, this is a net credit Strategy and results in net cash inflow as premium received on shorting a put (at higher strike) is more than premium paid on buying two puts (buying one put at t somewhat middle priced strike X2M and then one more put at lower strike X1L).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed-form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration for Bull Put Ladder and draw its graph in the Plots tab. EXAMPLE: Shorting HypoQuant December 24 put at $2.40 , buy HypoQuant December 21 put at $1.00, and then again buy HypoQuant December 19 put at $0.40. The graph gets displayed in Plots tab (Kakushadze & Serur, 2018).
#'@param ST Spot Price at time T.
#'@param X1L Lower Strike Price or eXercise price.
#'@param X2M Medium Strike Price or eXercise price.
#'@param X3H Higher Strike Price or eXercise price.
#'@param PX1L Put Premium paid for the bought Put at Lower Strike.
#'@param PX2M Put Premium paid for the bought Put at Medium Strike.
#'@param PX3H Put Premium received for the sold Put at higher Strike .
#'@param hl lower bound value for setting lower-limit of x-axis displaying spot price.
#'@param hu upper bound value for setting upper-limit of x-axis displaying spot price.
#'@param xlab X-axis label.
#'@param ylab Y-axis label.
#'@param main Title of the Graph.
#'@return Returns a graph of the strategy.
#'@importFrom graphics abline
#'@importFrom graphics points
#'@importFrom graphics text
#'@importFrom graphics lines
#'@importFrom graphics par
#'@importFrom graphics plot
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Cohen, G. (2015). The Bible of Options Strategies (2nd ed.). Pearson Technology Group. https://bookshelf.vitalsource.com/books/9780133964448\cr
#'Kakushadze, Z., & Serur, J. A. (2018, August 17). 151 Trading Strategies. Palgrave Macmillan. https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3247865
#'@examples
#'bullPutLadderPnLatExpiration(19,19,21,24,0.40,1.00,2.40)
#'bullPutLadderPnLatExpiration(200,200,205,209,2,5,7,hl=0.95,hu=1.1)
#'@export
bullPutLadderPnLatExpiration<-function (ST,X1L,X2M,X3H,PX1L,PX2M,PX3H,hl=0,hu=1.5,xlab="Spot Price ($) at Expiration",ylab=" Profit / Loss [PnL] at Expiration ($)",main="Bull Put Ladder [ PnL ]"){
  V0Cr= PX3H-PX1L-PX2M
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$Val <-  (pmax(0,(X1L-myData$spot))+pmax(0,(X2M-myData$spot))-pmax(0,(X3H-myData$spot))+ V0Cr)
  myData$Val = round(myData$Val, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1))
  plot(myData$spot, myData$Val, pch=21, bg="chartreuse2",col="chartreuse2",cex=1.1, xlab = xlab, ylab = ylab, col.lab="blue",main = main)
  points(x=(X3H-V0Cr), y=0,cex = 2, pch = 23, col ="red",bg="gold")
  points(x=(X1L+X2M-X3H+V0Cr), y=0,cex = 2, pch = 23, col ="red",bg="gold")
  text (myData$spot, myData$Val, labels = as.character(myData$Val), adj = 1.2,
        pos = NULL, offset = 0.5, vfont = NULL,
        cex = 0.7, col = "red", font = NULL )
  abline(h = 0,col = "gray")
  abline(v = X1L,col = "gray", lty=5,lwd=2)
  abline(v = X2M,col = "gray", lty=5,lwd=2)
  abline(v = X3H,col = "gray", lty=5,lwd=2)
  abline(v = ,col = "gray",lty=5,lwd=2)
  legend("topright", legend = c("PnL","BEP"),text.col ="snow",  bg ="darkorchid4", pch=c(16,18), col=c("chartreuse2","gold"),cex = 1.1)
  lines(myData$spot,myData$Val,col = "blue")
}


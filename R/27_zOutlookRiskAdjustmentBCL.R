#'Calculates per share Profit and Loss at expiration for Bear Call Ladder and draws its graph in the Plots tab.
#'@description
#'This is a vertical spread consisting of a short position in (usually) a close to ATM (at-the-money) call option with a strike price X1L, a long position in an OTM (out-of-the-money) call option with a strike price X2M, and a long position in another OTM call option with a higher strike price X3H. A bear call ladder typically arises when a bear call spread (a bearish strategy) goes wrong (the stock trades higher), so the trader buys another OTM call option (with the strike price X1L) to reverse from the initial bearish outlook to emerging bullish trends. On initiation, this is a net credit Strategy and results in net cash inflow as premium received on shorting a call (at lower strike) is more than premium paid on buying two calls (buying one call at somewhat middle priced strike X2M and then buying one more call at higher strike X3H) (Kakushadze & Serur, 2018).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed-form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration for Bear Call Ladder and draw its graph in the Plots tab. EXAMPLE, Shorting  HypoFintech December 24 put at $2.40, buying HypoFintech December 21 put at $1.00, and then again buying HypoFintech December 19 put at $0.40. This is used when Bear call Spread goes wrong and the Trader is trying to Adjust Initial Bearish outlook to now a Bullish. The graph gets displayed in Plots tab.
#'@param ST Spot Price at time T.
#'@param X1L Lower Strike Price or eXercise price.
#'@param X2M Medium Strike Price or eXercise price.
#'@param X3H Higher Strike Price or eXercise price.
#'@param CX1L Call Premium received for the sold Call at Lower Strike.
#'@param CX2M Call Premium paid for the bought Call at Medium Strike.
#'@param CX3H Call Premium paid for the bought Call at higher Strike .
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
#'zOutlookRiskAdjustmentBCL(19,19,20,24,4.20,2.40,0.80)
#'zOutlookRiskAdjustmentBCL(200,200,201,204,7,4,2,hl=0.95,hu=1.1)
#'@export
zOutlookRiskAdjustmentBCL<-function (ST,X1L,X2M,X3H,CX1L,CX2M,CX3H,hl=0,hu=1.5,xlab="Spot Price ($) at Expiration",ylab=" Profit / Loss [PnL] at Expiration ($)",main="Bear Call Ladder [ PnL ]"){
  V0Cr= CX1L-CX2M-CX3H
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$Val <-  (pmax(0,(myData$spot-X3H))+pmax(0,(myData$spot-X2M))-pmax(0,(myData$spot-X1L))+V0Cr)
  myData$Val = round(myData$Val, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1))
  plot(myData$spot, myData$Val, pch=21, bg="chartreuse2",col="chartreuse2",cex=1.1, xlab = xlab, ylab = ylab, col.lab="blue",main = main)
  points(x=(X1L+V0Cr), y=0,cex = 2, pch = 23, col ="red",bg="gold")
  points(x=(X3H+X2M-X1L-V0Cr), y=0,cex = 2, pch = 23, col ="red",bg="gold")
  text (myData$spot, myData$Val, labels = as.character(myData$Val), adj = -1.1,
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


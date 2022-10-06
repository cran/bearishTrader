#'Calculates per share Profit and Loss at expiration for Put Ratio Backspread and draws its graph in the Plots tab.
#'@description
#'This strategy consists of a shorting one (or two) put options with a strike price X2H, and a buying two (or three) put options with a lower strike price X1L. The outlook of the trader is strongly bearish. On initiation, this is a net Debit Strategy and results in net cash outflow as premium paid on buying 2 puts (at lower strike) is more than premium received from selling one put (at higher strike) (Kakushadze & Serur, 2018).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed-form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration for Put Ratio Backspread and draw its graph in the Plots tab. EXAMPLE, Buying two HypoERP December 25 Put at $2.15 and buy HypoERP December 30 put at $4.20. The graph gets displayed in Plots tab.
#'@param ST Spot Price at time T.
#'@param X1L Lower Strike Price or eXercise price.
#'@param X2H Higher Strike Price or eXercise price.
#'@param PX1L Put Premium paid for the bought Puts at Lower Strike.
#'@param PX2H Put Premium received for the sold Put at higher Strike.
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
#'putRatioBackspreadPnL(25,25,30,2.15,4.20)
#'@export
putRatioBackspreadPnL<-function (ST,X1L,X2H,PX1L,PX2H,hl=0,hu=1.4,xlab="Spot Price ($) at Expiration",ylab=" Profit / Loss [PnL] at Expiration ($)",main=" Put Ratio Backspread [ PnL ]"){
  V0Dr= 2*PX1L-PX2H
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$Val <-  ((2*(pmax(0,(X1L-myData$spot)))-pmax(0,(X2H-myData$spot))-V0Dr))
  myData$Val = round(myData$Val, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1))
  plot(myData$spot, myData$Val, pch=21, bg="chartreuse2",col="chartreuse2",cex=1.1, xlab = xlab, ylab = ylab, col.lab="blue",main = main)
  points(x=(X2H+V0Dr/1), y=0,cex = 2, pch = 23, col ="red",bg="gold")
  points(x=(2*X1L-1*X2H-V0Dr)/(2-1), y=0,cex = 2, pch = 23, col ="red",bg="gold")
  text (myData$spot, myData$Val, labels = as.character(myData$Val), adj = -1.1,
        pos = NULL, offset = 0.5, vfont = NULL,
        cex = 0.7, col = "red", font = NULL )
  abline(h = 0,col = "gray")
  abline(v = X1L,col = "gray", lty=5,lwd=2)
   abline(v = X2H,col = "gray", lty=5,lwd=2)
  abline(v = ,col = "gray",lty=5,lwd=2)
  legend("topright", legend = c("PnL","BEP"),text.col ="snow",  bg ="darkorchid4", pch=c(16,18), col=c("chartreuse2","gold"),cex = 1.1)
  lines(myData$spot,myData$Val,col = "blue")
}


#'Calculates per share Profit and Loss (pnL) at expiration for Bear Put Spread and draws its graph in the Plots tab.
#'@description
#'A bear put spread is established by buying a put option with a higher strike price and writing a put option on the same underlying stock with the same expiration date, but with a lower strike price. Like bull call spreads, the purchased option tends to be at-the-money and the written option out-of-the-money (Kakushadze & Serur, 2018). Suppose HypoMart stock is trading at $17. An investor creates a bear put spread by buying the HypoMart May 17 put at $4 and selling the HypoMart May 14 put at $3, at a net debit (net cash outflow) of $1. If the stock price is trading at $17 or higher, both puts will expire worthless and the investor will lose the net premium paid for the spread.
#'@details
#'According to conceptual details given by Cohen (2015), and a closed-form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss (pnL) at expiration for Bear Put Spread and draw its graph in the Plots tab. EXAMPLE, Buy HypoPharma December 17 put at $4.00 and Write HypoPharma December 14 put at $3.00. This is a vertical spread consisting of a long position in a put option (close to at- the-money) with a strike price XH, and a short position in an put option (out-of-the-money) with a lower strike price XL. This is a net debit trade. The outlook of the trader (investor) is bearish and trader profits if the stock price falls. The graph gets displayed in Plots tab.
#'@param ST Spot Price at time T.
#'@param XH higher Strike Price or eXercise price.
#'@param XL lower Strike Price or eXercise price.
#'@param PH Put Premium on higher Strike.
#'@param PL Put Premium on lower Strike.
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
#'bearPutSpreadPnLatExpiration(17,17,14,4,3)
#'bearPutSpreadPnLatExpiration(40,40,35,1.85,0.50,hl=0.8,hu=1.2)
#'bearPutSpreadPnLatExpiration(500,500,495,9,7,hl=0.95,hu=1.02)
#'@export
bearPutSpreadPnLatExpiration<-function (ST,XH,XL,PH,PL,hl=0,hu=1.5,xlab="Spot Price ($) at Expiration",ylab=" Profit / Loss [PnL] at Expiration ($)",main="Bear Spread using Puts [ PnL ]"){
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$Val <- (pmax(0,(XH-myData$spot))-pmax(0,(XL-myData$spot))-(PH-PL))
  myData$Val = round(myData$Val, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1))
  plot(myData$spot, myData$Val, pch=21, bg="chartreuse2",col="chartreuse2",cex=1.2, xlab = xlab, ylab = ylab, col.lab="blue",main = main)
  points(x=(XH-(PH-PL)),y=0,cex = 2, pch = 23, col ="red",bg="gold")
  text (myData$spot, myData$Val, labels = as.character(myData$Val), adj = 1.1,
        pos = NULL, offset = 0.5, vfont = NULL,
        cex = 0.6, col = "red", font = NULL )
  text(XL+2.25,0.25, labels=as.character("PnL= VT + V0Cr"), adj = 1,col="darkblue")
  text(XL+3,-0.25, labels=as.character("Bearish OUTLOOK"), adj = 1,col = "brown")
  abline(h = 0,col = "gray")
  abline(v = XH,col = "gray", lty=5,lwd=2)
  abline(v = XL,col = "gray",lty=5,lwd=2)
  legend("topright", legend = c("PnL","BEP"),text.col ="snow",  bg ="darkorchid4", pch=c(16,18), col=c("chartreuse2","gold"),cex = 1.1)
  lines(myData$spot,myData$Val,col = "blue")
}


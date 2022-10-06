#'Calculates Profit and Loss at expiration per share (or unit of the underlying) for Covered Put and draws its graph in the Plots tab.
#'@description
#'A covered put is the bearish equivalent of a covered call. It is achieved by short-selling the underlying stock and writing an equivalent put against it. By writing a put against a short sale, the trader is now in the position to buy the stock, if exercised, when the market price fell below the put’s strike price. The covered put writer is, in effect, forgoing the opportunity to participate in the decrease in stock price under the strike price in exchange for premium received for selling the put for the same (Kakushadze & Serur, 2018).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed-form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute Profit and Loss at expiration per share (or unit of the underlying) for Covered Put and draw its graph in the Plots tab. EXAMPLE, Short HypoGamma stock at $20.00 (inflow) and short HypoGamma December 18 call at $2.00 (inflow). Selling HypoGamma stock and writing the HypoGamma December 18 put. As the price of HypoGamma is currently $20, the investor has an opportunity to profit from a decline in the stock from $20 to the strike price of $18. By writing the 18 put, the investor has given up the right to participate in any decline in the stock below this level. If the stock falls under $18 at expiration, the option will be exercised by the put holder, forcing the writer to buy the stock at $18 regardless of what the market price is at that time. Buying the stock will offset the short position. The graph gets displayed in Plots tab.
#'@param ST Spot Price at time T.
#'@param S0 Initial Stock Price.
#'@param X Strike Price or eXercise price..
#'@param P Put Premium.
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
#'coveredPutPnLatExpiration(18,18,2,20)
#'coveredPutPnLatExpiration(110,110,4,120,hl=0.9,hu=1.2)
#'coveredPutPnLatExpiration(1090,1090,10,1100,hl=0.995,hu=1.03)
#'@export
coveredPutPnLatExpiration<-function (ST,X,P,S0,hl=0,hu=1.5,xlab="Spot Price ($) at Expiration",ylab=" Profit / Loss [PnL] at Expiration ($)",main="Covered Put / Married Put [ PnL ]"){
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$Val <- (S0-myData$spot-pmax((X-myData$spot),0)+P)
  # ALTERNATIVELY: myData$pl <- (S0-X-pmax((myData$spot-X),0) + P)
  myData$Val = round(myData$Val, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1))
  plot(myData$spot, myData$Val, pch=21, bg="chartreuse2",col="chartreuse2",cex=1.1, xlab = xlab, ylab = ylab, col.lab="blue",main = main)
  points(x=(S0+P), y=0,cex = 2, pch = 23, col ="red",bg="gold")
  text (myData$spot, myData$Val, labels = as.character(myData$Val), adj =1.4 ,
        pos = NULL, offset = 0.5, vfont = NULL,
        cex = 0.7, col = "red", font = NULL )
   text(S0-3,-1.25, labels=as.character("PnL= VT + V0Cr"), adj = 1,col="darkblue")
   text(S0-3,-0.25, labels=as.character("Bearish OUTLOOK"), adj = 1,col = "brown")
  abline(h = 0,col = "gray")
  abline(v = X,col = "gray", lty=5,lwd=2)
  legend("topright", legend = c("PnL","BEP"),text.col ="snow",  bg ="darkorchid4", pch=c(16,18), col=c("chartreuse2","gold"),cex = 1.1)
  lines(myData$spot,myData$Val,col = "blue")
}


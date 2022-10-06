#'Calculates Value/Payoff at expiration per share or unit of the underlying for Covered Put and draws its graph in the Plots tab.
#'@description
#'A covered put is the bearish equivalent of a covered call. It is achieved by short-selling the underlying stock and writing an equivalent put against it. By writing a put against a short sale, the trader is now in the position to buy the stock, if exercised, when the market price fell below the put’s strike price. The covered put writer is, in effect, forgoing the opportunity to participate in the decrease in stock price under the strike price in exchange for premium received for selling the put for the same (Kakushadze & Serur, 2018).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed-form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute Value/Payoff at expiration per share or unit of the underlying for Covered Put and draw its graph in the Plots tab. EXAMPLE, Short HypoGamma stock at $20.00 (inflow) and short HypoGamma December 18 call at $2.00 (inflow). Selling HypoGamma stock and writing the HypoGamma December 18 put. As the price of HypoGamma is currently $20, the investor has an opportunity to profit from a decline in the stock from $20 to the strike price of $18. By writing the 18 put, the investor has given up the right to participate in any decline in the stock below this level. Graph shows that the risk that can be incurred is uncapped because, theoretically, the price of HypoGamma stock could rise to any extent (infinity).
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
#'@importFrom graphics text
#'@importFrom graphics lines
#'@importFrom graphics par
#'@importFrom graphics plot
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Cohen, G. (2015). The Bible of Options Strategies (2nd ed.). Pearson Technology Group. https://bookshelf.vitalsource.com/books/9780133964448\cr
#'Kakushadze, Z., & Serur, J. A. (2018, August 17). 151 Trading Strategies. Palgrave Macmillan. https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3247865
#'@examples
#'coveredPutExpirationValueVT(18,18,2,20)
#'coveredPutExpirationValueVT(100,100,4,120,hl=0.9,hu=1.1)
#'coveredPutExpirationValueVT(1000,1000,20,1100,hl=0.995,hu=1.01)
#'@export
coveredPutExpirationValueVT<-function (ST,X,P,S0,hl=0,hu=1.5,xlab="Spot Price ($) at Expiration",ylab=" Value / Payoff [ VT ] at Expiration ($)",main="Covered Put / Married Put [ VT]"){
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  myData$Val <- (-myData$spot-pmax((X-myData$spot),0))
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$Val = round(myData$Val, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1))
  plot(myData$spot, myData$Val, pch=21, bg="cyan1",col="cyan1",cex=0.7, xlab = xlab, ylab = ylab, col.lab="blue",main = main)
  text (myData$spot, myData$Val, labels = as.character(myData$Val), adj = 1,
        pos = NULL, offset = 0.5, vfont = NULL,
        cex = 0.7, col = "red", font = NULL )
  text(S0-3,(-X-1.25), labels=as.character("VT@Expiration"), adj = 1,col="darkblue")
  text(S0-3,(-X-0.75), labels=as.character("Bearish OUTLOOK"), adj = 1,col = "brown")
  abline(h = 0,col = "gray")
  abline(v = X,col = "gray", lty=5,lwd=2)
  abline(v = S0,col = "gold1", lty=5,lwd=2)
  legend("topright", legend = "VT ",text.col ="snow",  bg ="midnightblue", pch=16, col="cyan1",cex = 1.1)
  lines(myData$spot,myData$Val,col = "blue")
}


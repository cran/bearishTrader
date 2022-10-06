#'Calculates  the Value/Payoff per share at expiration for Protective Call and draws its graph in the Plots tab.
#'@description
#'This strategy is also known as married call or synthetic put or a protected short sale. It consists of a call purchase against a short sale of the underlying stock. An increase in the  price of the stock over the strike price of the call will prompt the investor to exercise the right to buy the stock. As a result, the investor is protected against an increase in the stock price over the strike price (Kakushadze & Serur, 2018).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed-form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute the Value/Payoff per share at expiration for Protective Call and draw its graph in the Plots tab. EXAMPLE, Short HypoMedia stock at $14.00 (inflow) and buy HypoMedia December 15 call at $2.00 (outflow).
#'@param ST Spot Price at time T.
#'@param S0 Initial Stock Price.
#'@param X Strike Price or eXercise price.
#'@param C Call Premium.
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
#'protectiveCallExpirationValueVT(15,15,2,14)
#'protectiveCallExpirationValueVT(50,50,3,48,hl=0.8,hu=1.2)
#'protectiveCallExpirationValueVT(1000,1000,4,998,hl=0.98,hu=1.01)
#'@export
protectiveCallExpirationValueVT <- function (ST,X,C,S0,hl=0,hu=1.5,xlab="Spot Price ($) at Expiration",ylab="Value / Payoff [ VT ] at Expiration ($)",main="Protective Call / Married Call / SyntheticPut [VT]"){
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$pl <- (-myData$spot+pmax((myData$spot-X),0))
  myData$pl = round(myData$pl, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1))
  plot(myData$spot, myData$pl, pch=21, bg="cyan1",col="cyan1", xlab = xlab, ylab = ylab,col.lab="blue", main = main)
  text (myData$spot, myData$pl, labels = as.character(myData$pl), adj =-1,
        pos = NULL, offset = 0.3, vfont = NULL,
        cex = 0.7, col = "red", font = NULL )
  text(S0-C+2,-ST+3, labels=as.character("Bearish OUTLOOK"),adj = 1,col="darkblue")
  text(X-C+0.5,-ST+4, labels=as.character("VT @ Expiration"),  adj = 1,col = "brown" )
  abline(h = 0,col = "gray")
  abline(v = X,col = "gray", lty=5,lwd=2)
  abline(v = S0,col = "gold1", lty=5,lwd=2)
  legend("topright", legend = "VT ",text.col ="snow",  bg ="midnightblue", pch=16, col="cyan1",cex = 1.1)
  lines(myData$spot,myData$pl,col ="blue")
}

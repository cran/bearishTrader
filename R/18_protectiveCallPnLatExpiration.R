#'Calculates per share Profit and Loss at expiration for Protective Call and draws its graph in the Plots tab.
#'@description
#'This strategy is also known as married call or synthetic put or a protected short sale. It consists of a call purchase against a short sale of the underlying stock. An increase in the  price of the stock over the strike price of the call will prompt the investor to exercise the right to buy the stock. As a result, the investor is protected against an increase in the stock price over the strike price. In effect, the call acts as insurance against a rise in the stock price. As with any kind of insurance; however, there is a price to pay. In the case of options, the price is the premium which in effect lowers the price received on the short sale of the stock (Kakushadze & Serur, 2018).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed-form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration for Protective Call and draw its graph in the Plots tab. EXAMPLE, Short HypoMedia stock at $14.00 (inflow) and buy HypoMedia December 15 call at $2.00 (outflow). Protective Call results from shorting stock and buying an ATM (at-the money) or OTM (out-of-the-money) call option with a strike price X greater than or equal to S0. The outlook of trader or investor is bearish. This strategy has the same profile as a long put. In fact, this strategy is often referred to as a synthetic put. The graph gets displayed in Plots tab.
#'@param ST Spot Price at time T.
#'@param S0 Initial Stock Price.
#'@param X Strike Price or eXercise price.
#'@param C Call Premium.
#'@param hl lowe0r bound value for setting lower-limit of x-axis displaying spot price.
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
#'protectiveCallPnLatExpiration(15,15,2,14)
#'protectiveCallPnLatExpiration(50,50,3,48,hl=0.8,hu=1.2)
#'protectiveCallPnLatExpiration(1000,1000,7,998,hl=0.98,hu=1.01)
#'@export
protectiveCallPnLatExpiration <- function (ST,X,C,S0,hl=0,hu=1.5,xlab="Spot Price ($)",ylab="Profit / Loss ($)",main="Protective Call / Married Call / SyntheticPut"){
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$pl <- (S0-myData$spot+pmax((myData$spot-X),0)-C)
  myData$pl = round(myData$pl, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1))
  plot(myData$spot, myData$pl, pch=21, bg="chartreuse2",col="chartreuse2", xlab = xlab, ylab = ylab,col.lab="blue", main = main)
  points(x=(S0-C), y=0,cex = 2, pch = 23, col ="red",bg="gold")
  text (myData$spot, myData$pl, labels = as.character(myData$pl), adj =-1,
        pos = NULL, offset = 0.3, vfont = NULL,
        cex = 0.7, col = "red", font = NULL )
  text(S0-C+2,1, labels=as.character("Bearish OUTLOOK"),adj = 1,col="darkblue")
  text(X-C+0.5,X-C-4, labels=as.character("PnL= VT + V0Cr"),  adj = 1,col = "brown" )
  abline(h = 0,col = "gray")
  abline(v = X,col = "gray", lty=5,lwd=2)
  abline(v = S0,col = "gold1", lty=5,lwd=2)
  legend("topright", legend = c("PnL","BEP"),text.col ="snow",  bg ="darkorchid4", pch=c(16,18), col=c("chartreuse2","gold"),cex = 1.1)
  lines(myData$spot,myData$pl,col ="blue")
}

#'Calculates Profit and Loss (pnL) at expiration per share or unit of the underlying for European Put Buyer and draws its graph in the Plots tab.
#'@description
#'A Put option allows the holder, or long, to receive amount \code{X} by allowing the holder to sell the underlying asset at the exercise price. Thus, the holder should exercise the put at expiration if the underlying asset is worth less than the exercise price (ST < X). In that case, the put is said to be in-the-money. If the underlying asset is worth the same as the exercise price (ST = X), meaning the put is at-the-money, or more than the exercise price (ST > X), meaning the put is out-of-the-money, the option holder would not exercise it and it would expire with zero value. Thus, the Profit and Loss(PnL) to the Put Buyer (Holder) is equal to Maximum of (0, X - ST) minus Net Debit (V0Dr) (Chance, 2019).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed-form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute Profit and Loss (pnL) at expiration per share or unit of the underlying for European Put Buyer and draw its graph in the Plots tab. EXAMPLE, Buying HypoTech December 10 put at $2.00. Here \code{X} is the exercise price (strike price) of the option, \code{ST} is the price of the underlying at time T, and  \code{P} is the price (also called premium) paid by the European Put Buyer to the the Put Seller at time 0. The graph gets displayed in Plots tab.
#'@param ST Spot Price at time T.
#'@param X Strike Price or eXercise price.
#'@param P Put Premium or Put price.
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
#'Chance,D.M.(2019). Basics of Derivative Pricing and Valuation. In CFA Program Curriculum 2020 Level I Volumes 1-6. (Vol. 5, pp. 385-453). Wiley Professional Development (P&T). ISBN 9781119593577, https://bookshelf.vitalsource.com/books/9781119593577\cr
#'Cohen, G. (2015). The Bible of Options Strategies (2nd ed.). Pearson Technology Group. https://bookshelf.vitalsource.com/books/9780133964448\cr
#'Kakushadze, Z., & Serur, J. A. (2018, August 17). 151 Trading Strategies. Palgrave Macmillan. https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3247865
#'@examples
#'longPutPnLatExpiration(10,9,2)
#'longPutPnLatExpiration(40,38,3,hl=0.8,hu=1.2)
#'longPutPnLatExpiration(1000,1020,14,hl=0.995,hu=1.03)
#'@export
longPutPnLatExpiration<-function (ST,X,P,hl=0,hu=1.5,xlab="Spot Price ($) at Expiration",ylab=" Profit / Loss [PnL] at Expiration ($)",main="Long Put / Put Buyer [ PnL ]"){
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$Val <- ((pmax(0,(X-myData$spot))-P))
  myData$Val = round(myData$Val, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1))
  plot(myData$spot, myData$Val, pch=21, bg="chartreuse2",col="chartreuse2",cex=1.1, xlab = xlab, ylab = ylab, col.lab="blue",main = main)
  points(x=(X-P), y=0,cex = 2, pch = 23, col ="red",bg="gold")
  text (myData$spot, myData$Val, labels = as.character(myData$Val), adj = -1,
        pos = NULL, offset = 0.5, vfont = NULL,
        cex = 0.7, col = "red", font = NULL )
  text(X+2.25,1.5, labels=as.character("PnL= VT - V0Dr"), adj = 1,col="darkblue")
  text(X+3,2.5, labels=as.character("Bearish OUTLOOK"), adj = 1,col = "brown")
  abline(h = 0,col = "gray")
  abline(v = X,col = "gray")
  legend("topright", legend = c("PnL","BEP"),text.col ="snow",  bg ="darkorchid4", pch=c(16,18), col=c("chartreuse2","gold"),cex = 1.1)
    lines(myData$spot,myData$Val,col = "blue")
}


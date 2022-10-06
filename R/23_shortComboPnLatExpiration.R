#'Calculates per share Profit and Loss at expiration for Short Combo and draws its graph in the Plots tab.
#'@description
#'Short Combo results from buying a put option with a strike price X1L and selling a call option with a strike price X2H with the same expiration date. Here, X2H > X1L. Outlook of the trader (investor) is bearish (Kakushadze & Serur, 2018).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed-form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration for Short Combo and draw its graph in the Plots tab. EXAMPLE, Buy HypoCRM December 8 Put at $1.50 and shortHypoCRM December 12 call at $2.00. This is a net credit trade as premium received on shorted call (CX1H) at a higher strike is more than the premium paid on the bought put (P1XL) at a lower strike.
#'@param ST Spot Price at time T.
#'@param X1L Lower Strike Price or eXercise price.
#'@param X2H Higher Strike Price or eXercise price.
#'@param PX1L Put Premium paid for the bought Put at Lower Strike.
#'@param CX2H Call Premium received for the sold Call at higher Strike .
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
#'shortComboPnLatExpiration(19,18,20,1.50,2.00)
#'@export
shortComboPnLatExpiration<-function (ST,X1L,X2H,PX1L,CX2H,hl=0,hu=1.2,xlab="Spot Price ($) at Expiration",ylab=" Profit / Loss [PnL] at Expiration ($)",main="Short Combo [ PnL ]"){
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$Val <-  (pmax(0,(X1L-myData$spot))-pmax(0,(myData$spot-X2H))+(CX2H-PX1L))
  myData$Val = round(myData$Val, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1))
  plot(myData$spot, myData$Val, pch=21, bg="chartreuse2",col="chartreuse2",cex=1.1, xlab = xlab, ylab = ylab, col.lab="blue",main = main)
  points(x=(X2H +(CX2H-PX1L)), y=0,cex = 2, pch = 23, col ="red",bg="gold")
  text (myData$spot, myData$Val, labels = as.character(myData$Val), adj = -1.1,
        pos = NULL, offset = 0.5, vfont = NULL,
        cex = 0.7, col = "red", font = NULL )
   text(X1L-2.25,0.5, labels=as.character("PnL= VT + V0Cr"), adj = 1,col="darkblue")
  text(X1L-3,-0.5, labels=as.character("Bearish OUTLOOK"), adj = 1,col = "brown")
  abline(h = 0,col = "gray")
  abline(v = X1L,col = "gray", lty=5,lwd=2)
  abline(v = X2H,col = "gray", lty=5,lwd=2)
  abline(v = ,col = "gray",lty=5,lwd=2)
  legend("topright", legend = c("PnL","BEP"),text.col ="snow",  bg ="darkorchid4", pch=c(16,18), col=c("chartreuse2","gold"),cex = 1.1)
  lines(myData$spot,myData$Val,col = "blue")
}


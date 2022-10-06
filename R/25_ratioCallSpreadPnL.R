#'Calculates per share Profit and Loss at expiration for Ratio Call Spread and draws its graph in the Plots tab.
#'@description
#'This strategy consists of a shorting two calls with a higher strike price X2H, and a buying one (or three) calls with a lower strike price X1L. The graph shows that investor or trader is exposed to uncapped risk and can only make a limited reward and therefore an undesirable strategy (Kakushadze & Serur, 2018).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed-form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration for Ratio Call Spread and draw its graph in the Plots tab. EXAMPLES, Shorting two HypoQuill December 27.50 calls at $1.40 and buying HypoQuill December 25 put at $3.20 OR Shorting two HypoQuill December 27.50 calls at $1.90 and buying HypoQuill December 25 put at $3.20. In the first example, trades are net Debit on the Initiation day as Premium paid is more then the premium received. (In this situation, as displayed in the graph, there are two Breakeven points). However, as shown in the second example the trades can be structured to have net credit on initiation. (In this case, as displayed in the graph, there will be only one Breakeven Point). The graph gets displayed in Plots tab.
#'@param ST Spot Price at time T.
#'@param X1L Lower Strike Price or eXercise price.
#'@param X2H Higher Strike Price or eXercise price.
#'@param CX1L Call Premium paid for the bought Call at Lower Strike.
#'@param CX2H Call Premium received for the sold Calls at higher Strike.
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
#'ratioCallSpreadPnL(25,25,27.50,3.20,1.40)
#'ratioCallSpreadPnL(25,25,27.50,3.20,1.90)
#'ratioCallSpreadPnL(25,25,27.50,3.20,1.60)
#'@export
ratioCallSpreadPnL<-function (ST,X1L,X2H,CX1L,CX2H,hl=0.8,hu=1.4,xlab="Spot Price ($) at Expiration",ylab=" Profit / Loss [PnL] at Expiration ($)",main=" Ratio Call Spread [ PnL ]"){
  V0Cr= 2*CX2H-CX1L
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$Val <-  ((pmax(0,(myData$spot-X1L))-(2*pmax(0,(myData$spot-X2H)))+V0Cr))
  myData$Val = round(myData$Val, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1))
  plot(myData$spot, myData$Val, pch=21, bg="chartreuse2",col="chartreuse2",cex=1.1, xlab = xlab, ylab = ylab, col.lab="blue",main = main)
  if (V0Cr<= 0)
  {points(x=(X1L-V0Cr/1), y=0,cex = 2, pch = 23, col ="red",bg="gold")}
    points(x=(2*X2H-1*X1L+V0Cr)/(2-1), y=0,cex = 2, pch = 23, col ="red",bg="gold")
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


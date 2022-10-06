#'Plots per share Initial cost for Short Synthetic Future in the Plots tab.
#'@description
#'This strategy results from shorting a call of the option on Future and buying a put of the option on the Future of the same strike price with the same expiration. On initiation, this is a net credit Strategy and results in net cash inflow as premium received on shorting a call of the option on Future is more than premium paid on buying a put of the option on Future (Kakushadze & Serur, 2018).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed-form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created to plot per share Initial cost for Short Synthetic Future in the Plots tab.
#'@param STF  Future contract price at time T.
#'@param XF  Strike Price of Option on Future.
#'@param COF Call Premium received from Option on Future.
#'@param POF Put premium paid on Option on Future.
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
#'zShortSyntheticFutureV0(20,20,2.80,2.60)
#'@export
zShortSyntheticFutureV0<-function (STF,XF,COF,POF,hl=0.5,hu=1.5,xlab="Future Contract Price ($) at Expiration of Options on Future",ylab=" Initial Value [ V0] ($)",main="Short Synthetic Future V0 [Dr/Cr]"){
  myData <- data.frame (spot = c((STF*hl):(STF*hu)))
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$Val <- (pmax(0,(COF-POF)))
  myData$Val = round(myData$Val, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1))
  plot(myData$spot, myData$Val, pch=21, bg="gold",col="gold",cex=1.1, xlab = xlab, ylab = ylab, col.lab="blue",main = main, xlim=c(STF*hl,STF*hu), ylim=c(-1,COF+1))
  text (myData$spot, myData$Val, labels = as.character(myData$Val), adj = 1,
        pos = NULL, offset = 0.5, vfont = NULL,
        cex = 0.7, col = "red", font = NULL )
   text(XF+2.5,(COF-POF+0.45), labels=as.character("V0Cr = Net Credit "), adj = 1,pos = NULL, offset = 0.5, vfont = NULL,
   cex = 0.9, col = "darkblue", font = NULL )
text(XF+2.7,(POF-0.25), labels=as.character("Bearish OUTLOOK"), adj = 1,pos = NULL, offset = 0.5, vfont = NULL,
   cex = 1, col = "brown", font = NULL )
  abline(h = 0,col = "gray")
  abline(h = COF,col = "darkseagreen")
  abline(h = POF,col = "coral")
  abline(h = COF-POF,col = "blue")
  abline(v = XF,col = "gray", lty=5)
}

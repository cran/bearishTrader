#'Displays per share Initial Net Debit (V0Dr) on initiation day for Strip Option Strategy and draws its graph in the Plots tab.
#'@details
#'According to conceptual details given by Cohen (2015), and a closed-form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Initial Net Debit (V0Dr) on initiation day for Strip Option Strategy and draw its graph in the Plots tab. EXAMPLE, Buy HypoBeta December 9 call at $1.40 (outflow) and buy two HypoBeta December 9 Puts at $0.80 (outflow). This Strategy consists of a long call position (in an at-the-money call option) and a long position in two put options (at-the-money) with a strike price X. This is a net debit trade and involves three cash outflows in the form of premiums paid for buying one call option and two put options.
#'@param ST Spot Price at time T.
#'@param X Strike Price or eXercise price.
#'@param C Call Premium or Call Price.
#'@param P1 Put Premium on first Put.
#'@param P2 Put Premium on second Put.
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
#'stripInitialValueV0(9,9,1.4,0.80,0.80)
#'stripInitialValueV0(40,40,2,1.25,1.25,hl=0.9,hu=1.2)
#'stripInitialValueV0(1000,1000,8,5.50,6.50,hl=0.99,hu=1.015)
#'@export
stripInitialValueV0<-function (ST,X,C,P1,P2,hl=0.5,hu=1.5,xlab="Spot Price ($) at Expiration",ylab=" Initial Value [ V0] ($)",main="Strip V0 [Dr/Cr]"){
  V0Dr=C+P1+P2
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$Val <- (pmax(0,V0Dr))
  myData$Val = round(myData$Val, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1))
  plot(myData$spot, myData$Val, pch=21, bg="mistyrose",col="mistyrose",cex=1.1, xlab = xlab, ylab = ylab, col.lab="blue",main = main,xlim=c(ST*hl,ST*hu), ylim=c(-1,(V0Dr+2)))
  text (myData$spot, myData$Val, labels = as.character(myData$Val), adj = 2,
        pos = NULL, offset = 0.5, vfont = NULL,
        cex = 0.7, col = "red", font = NULL )
  text(X+C,V0Dr+1, labels=as.character("V0Dr = Net Debit"), adj = 1,pos = NULL, offset = 0.5, vfont = NULL,
       cex = 1.2, col = "darkblue", font = NULL )
  text(X+C,V0Dr-1.4, labels=as.character("Bearish / Neutral OUTLOOK"), adj = 1,pos = NULL, offset = 0.5, vfont = NULL,
       cex = 1, col = "brown", font = NULL )
  abline(h = 0,col = "gray")
  abline(h = V0Dr, col = "blue")
  abline(v = X,col = "gray")
  abline(h = P1,col = "coral",lty=2,lwd=1.5)
  abline(h = P2,col = "coral",lty=5,lwd=1.25)
  legend("topright", legend = "V0 ",text.col ="lavender",  bg ="violetred4", pch=16, col="mistyrose",cex = 1.2)
  abline(h = C,col = "lightseagreen",lty=5,lwd=1.5)
}


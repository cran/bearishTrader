#'Calculates per share Profit and Loss at expiration for Strip Option Strategy and draws its graph in the plots tab.
#'@description
#'This Strategy consists of a long call position (in an at-the-money call option) and a long position in two put options (at-the-money) with a strike price X. The Strip is a simple adjustment to the Straddle to make it more biased toward the downside. In buying a second put, the strategy retains its preference for high volatility but now with a more bearish slant (Cohen, 2016).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed-form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration for Strip Option Strategy and draw its graph in the plots tab. EXAMPLE, Buy HypoBeta December 9 call at $1.40 (outflow) and Buy two HypoBeta December 9 Puts at $0.80 (outflow). This is a net debit trade and involves three cash outflows. The graph gets displayed in Plots tab.
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
#'@importFrom graphics points
#'@importFrom graphics text
#'@importFrom graphics legend
#'@importFrom graphics lines
#'@importFrom graphics par
#'@importFrom graphics plot
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Cohen, G. (2015). The Bible of Options Strategies (2nd ed.). Pearson Technology Group. https://bookshelf.vitalsource.com/books/9780133964448\cr
#'Kakushadze, Z., & Serur, J. A. (2018, August 17). 151 Trading Strategies. Palgrave Macmillan. https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3247865
#'@examples
#'stripPnLatExpiration(9,9,1.4,0.80,0.80)
#'stripPnLatExpiration(40,40,2,1.25,1.25,hl=0.9,hu=1.2)
#'stripPnLatExpiration(1000,1000,8,5.50,6.50,hl=0.985,hu=1.022)
#'@export
stripPnLatExpiration <- function (ST,X,C,P1,P2,hl=0,hu=2,xlab="Spot Price ($) at Expiration",ylab="Profit / Loss ($) at Expiration", main="Strip [PnL]"){
  V0Dr=C+P1+P2
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$pl <- (pmax((myData$spot-X),0)+2*pmax((X-myData$spot),0)-V0Dr)
  myData$pl = round(myData$pl, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1))
  plot(myData$spot, myData$pl, pch=21, bg="chartreuse2",col="chartreuse2", xlab = xlab, ylab = ylab,col.lab="blue", main = main)
  text (myData$spot, myData$pl, labels = as.character(myData$pl), adj =-0.4,
        pos = NULL, offset = 0.3, vfont = NULL,
        cex = 0.6, col = "red", font = NULL )
  points(x=(X+V0Dr), y=0,cex = 2, pch = 23, col ="red",bg="gold")
  points(x=(X-(V0Dr/2)), y=0,cex = 2, pch = 23, col ="red",bg="gold")
  text(X+4,C+1, labels=as.character("Bearish / Neutral OUTLOOK"),adj = 1,col="darkblue")
  text(X+4,C, labels=as.character("PnL= VT - V0Dr"),  adj = 1,col = "brown" )
  abline(h = 0,col = "gray")
  abline(v = X,col = "gray")
  legend("topright", legend = c("PnL","BEP"),text.col ="snow",  bg ="darkorchid4", pch=c(16,18), col=c("chartreuse2","gold"),cex = 1.1)
  lines(myData$spot,myData$pl,col ="blue")
}

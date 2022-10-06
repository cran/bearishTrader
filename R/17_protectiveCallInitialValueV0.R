#'Displays per share Net Credit (V0Cr) on initiation day for Protective Call and draws its graph in the Plots tab.
#'@details
#'According to conceptual details given by Cohen (2015), and a closed-form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Net Credit (V0Cr) on initiation day for Protective Call and draw its graph in the Plots tab. EXAMPLE, Short HypoMedia stock at $14.00 (inflow) and buy HypoMedia December 15 call at $2.00 (outflow). This is a net credit trade as the net cash inflow equals shorted price realized minus call premium (call price) paid on call purchase.
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
#'protectiveCallInitialValueV0(15,15,2,14)
#'protectiveCallInitialValueV0(50,50,3,48,hl=0.8,hu=1.2)
#'protectiveCallInitialValueV0(1000,1000,20,998,hl=0.995,hu=1.01)
#'@export
protectiveCallInitialValueV0 <- function (ST,X,C,S0,hl=0.5,hu=1.5,xlab="Spot Price ($)",ylab="Initial Value V0 ($)",main="Protective Call / Married Call / SyntheticPut"){
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$pl <- (S0-C)
  myData$pl = round(myData$pl, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1))
  plot(myData$spot, myData$pl, pch=21, bg="gold",col="gold", xlab = xlab, ylab = ylab,col.lab="blue", main = main, xlim=c(ST*hl,ST*hu), ylim=c(-1,S0+1))
  text (myData$spot, myData$pl, labels = as.character(myData$pl), adj =-1,
        pos = NULL, offset = 0.3, vfont = NULL,
        cex = 0.7, col = "red", font = NULL )
  text(X+5,C+6, labels=as.character("Bearish OUTLOOK"),adj = 1,col="darkblue")
  text(X+1,C-2, labels=as.character("V0Cr = Net Credit "),  adj = 1,col = "brown" )
  abline(h = 0,col = "gray")
  abline(h = S0-C,col = "blue")
  abline(h = S0,col = "darkseagreen")
  abline(h = C,col = "coral")
  lines(myData$spot,myData$pl,col ="blue")
  legend("topright", legend = "V0Cr ",text.col ="snow",  bg ="violetred4", pch=16, col="gold",cex = 1.1)
}

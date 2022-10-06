#'Displays graph of the Initial Value as Net Credit (V0Cr) per share on initiation day for shorting the Call in the Plots tab.
#'@description
#'As we know that options involve two parties: the buyer and the seller (sometimes known as the writer); each of whom agrees to do something for the other, either now or later. The buyer, who purchases the derivative, is referred to as the long or the holder because he owns (pr holds) the derivative and holds a long position. The seller is referred to as the short because he holds a short position and has, upon exercise, an obligation to deliver (Chance, 2019).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed-form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to display the Initial Value as Net Credit (V0Cr) per share on initiation day for shorting the Call. EXAMPLE, Writing HypoInc December 10 call at $3.00. Here, \code{X} is the exercise price (strike price) of the option, \code{ST} is the price of the underlying at time T, and \code{C} is the price (also called premium) paid by the European Call Buyer to the the Call Seller. For a call Seller there is inflow of cash in the form of premium received for writing the call and hence it is a Net Credit Position that is represented by V0Cr. The graph gets displayed in Plots tab. Horizontal Straight Line on the graph represents that V0Cr is same irrespective of spot price at expiration.
#'@param ST Spot Price at time T.
#'@param X Strike Price or eXercise price.
#'@param C Call Premium or call price.
#'@param hl lower bound value for setting lower-limit of x-axis displaying spot price.
#'@param hu upper bound value for setting upper-limit of x-axis displaying spot price.
#'@param xlab X-axis label.
#'@param ylab Y-axis label.
#'@param main Title of the Graph.
#'@return Returns a graph of the strategy.
#'@importFrom graphics abline
#'@importFrom graphics text
#'@importFrom graphics legend
#'@importFrom graphics lines
#'@importFrom graphics par
#'@importFrom graphics plot
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Chance,D.M.(2019). Basics of Derivative Pricing and Valuation. In CFA Program Curriculum 2020 Level I Volumes 1-6. (Vol. 5, pp. 385-453). Wiley Professional Development (P&T). ISBN 9781119593577, https://bookshelf.vitalsource.com/books/9781119593577\cr
#'Cohen, G. (2015). The Bible of Options Strategies (2nd ed.). Pearson Technology Group. https://bookshelf.vitalsource.com/books/9780133964448\cr
#'Kakushadze, Z., & Serur, J. A. (2018, August 17). 151 Trading Strategies. Palgrave Macmillan. https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3247865
#'@examples
#'aShortCallinitialValueV0(10,10,3)
#'aShortCallinitialValueV0(50,50,3,0.8,1.2)
#'@export
aShortCallinitialValueV0<-function (ST,X,C,hl=0,hu=1.5,xlab="Spot Price ($) at Expiration",ylab=" Initial Value [ V0] ($)",main="Short Call / Call Seller V0 [Dr/Cr] "){
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$Val <- (pmax(0,C))
  myData$Val = round(myData$Val, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1))
  plot(myData$spot, myData$Val, pch=21, bg="gold",col="gold",cex=1.1, xlab = xlab, ylab = ylab, col.lab="blue",main = main,xlim=c(ST*hl,ST*hu), ylim=c(-1,C+2))
  text (myData$spot, myData$Val, labels = as.character(myData$Val), adj = 2,
        pos = NULL, offset = 0.5, vfont = NULL,
        cex = 0.7, col = "red", font = NULL )
  text(X+2.5,C+0.25, labels=as.character("V0Cr = Net Credit ( Premium Received )"), adj = 1,pos = NULL, offset = 0.5, vfont = NULL,
       cex = 0.9, col = "darkblue", font = NULL )
  text(X+1.75,0.25, labels=as.character("Bearish OUTLOOK"), adj = 1,pos = NULL, offset = 0.5, vfont = NULL,
       cex = 1, col = "brown", font = NULL )
  abline(h = 0,col = "gray")
  abline(h = C,col = "blue")
  abline(v = X,col = "gray")
  legend("topright", legend = "V0Cr ",text.col ="snow",  bg ="violetred4", pch=16, col="gold",cex = 1.1)
 }

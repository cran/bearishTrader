#'Displays graph of per share Initial Value Net Credit (V0Cr) on initiation day for Bear Call Spread in the Plots tab.
#'@description
#'The trader writes a call option at a lower strike price and purchases a call option at a higher strike price on the same underlying stock with the same expiration date. Since the lower-strike call trades at a higher price, the trader receives a net credit when the spread is established. The bear call spreader hopes the price of the underlying stock will drop below the strike price of the written option, in which case both options will expire worthless, and he or she can keep the net credit received when the position was initiated (Kakushadze & Serur, 2018).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed-form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to display per share Initial Value Net Credit (V0Cr) on initiation day for Bear Call Spread. EXAMPLE, Buy HypoPharma December 19 call at $2.00 and Write HypoPharma December 16 call at $3.00. So, a trader paid a call premium of $2 per share on bought call at $19 (XH) and received a Call Premium of $3 per share sold call at $16 (lower strike represented by XL). This is a vertical spread consisting of a long position call option with a strike price XH, and a short position in another OTM call option with a lower strike price XL. This is a net credit trade involving a net cash inflow. The outlook of the trader is bearish. The graph gets displayed in Plots tab. Horizontal Straight Line on the graph represents that V0Cr is same irrespective of spot price at expiration.
#'@param ST Spot Price at time T.
#'@param XH higher Strike Price or eXercise price.
#'@param XL lower Strike Price or eXercise price.
#'@param CH Call Premium on higher Strike.
#'@param CL Call Premium on lower strike.
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
#'bearCallSpreadInitialValueV0(19,19,16,2,3)
#'bearCallSpreadInitialValueV0(45,45,40,0.50,2,hl=0.8,hu=1.2)
#'bearCallSpreadInitialValueV0(500,500,492,3,8,hl=0.95,hu=1.02)
#'@export
bearCallSpreadInitialValueV0<-function (ST,XH,XL,CH,CL,hl=0,hu=1.5,xlab="Spot Price ($) at Expiration",ylab=" Initial Value [ V0] ($)",main="Bear Spread using Calls V0 [Dr/Cr]"){
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$Val <- (pmax(0,(CL-CH)))
  myData$Val = round(myData$Val, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1))
  plot(myData$spot, myData$Val, pch=21, bg="gold",col="gold",cex=1.1, xlab = xlab, ylab = ylab, col.lab="blue",main = main, xlim=c(ST*hl,ST*hu), ylim=c(-1,CL+1))
  text (myData$spot, myData$Val, labels = as.character(myData$Val), adj = 1,
        pos = NULL, offset = 0.5, vfont = NULL,
        cex = 0.7, col = "red", font = NULL )
   text(XL+2.5,(CL-CH+0.45), labels=as.character("V0Cr = Net Credit "), adj = 1,pos = NULL, offset = 0.5, vfont = NULL,
   cex = 0.9, col = "darkblue", font = NULL )
text(XL+2.7,(CL-CH-0.25), labels=as.character("Bearish OUTLOOK"), adj = 1,pos = NULL, offset = 0.5, vfont = NULL,
   cex = 1, col = "brown", font = NULL )
  abline(h = 0,col = "gray")
  abline(h = CH,col = "coral")
  abline(h = CL,col = "darkseagreen")
  abline(h = CL-CH,col = "blue")
  abline(v = XH,col = "gray", lty=5)
  abline(v = XL,col = "gray",lty=5)
  legend("topright", legend = "V0Cr ",text.col ="snow",  bg ="violetred4", pch=16, col="gold",cex = 1.1)
  }

#'Calculates Profit and Loss (PnL) per share or unit of the underlying at expiration for European Call Seller and draws its graph in the Plots tab.
#'@description
#'Using the payoff value and the price paid for the option, we can determine the profit (or loss) from the strategy, which is denoted with the \code{PnL}. Let us say the call option buyer paid \code{C} for the option at time 0. Then, the profit to the call buyer is \code{Maximum of (0,ST minus X) minus C}. For call seller the payoff is \code{minus} maximum of \code{(0,ST minus X) plus net Credit that is represented by V0Cr}. Further, it is explained that the fixed price at which the underlying asset can be purchased is called the exercise price (also called the strike price or the strike). This price at which the underlying will be purchased or sold if the option is exercised. The strike price of the option is chosen by the participants and is fixed at the predetermined amount. The actual price or value of the option is an altogether different concept and will differ from time-to-time. The option buyer pays the seller (writer) a sum of money called the option price (option premium, or just the premium). It represents a fair price of the option, and in a well-functioning market, it would be the value of the option. An option is also designated both as exercisable early (before expiration) or only at expiration. Options that can be exercised early are referred to as American-style. Options that can be exercised only at expiration are referred to as European-style. It is extremely important that you do not associate these terms with where these options are traded. Both types of options trade on all continents (Chance, 2019).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed-form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute Profit and Loss (PnL) per share or unit of the underlying at expiration for European Call Seller and draw its graph in the Plots tab. EXAMPLE, Writing  HypoInc December 10 call at $3.00. Here,  \code{X} is the exercise price (strike price) of the option, \code{ST} is the price of the underlying at time T, and  \code{C} is the price (also called premium) paid by the European Call Buyer to the the Call Seller. The graph gets displayed in Plots tab.
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
#'aShortCallPnLatExpiration(10,10,3)
#'aShortCallPnLatExpiration(50,50,3,hl=0.8,hu=1.2)
#'aShortCallPnLatExpiration(1000,1000,14,hl=0.995,hu=1.025)
#'@export
aShortCallPnLatExpiration<-function (ST,X,C,hl=0,hu=1.5,xlab="Spot Price ($) at Expiration",ylab=" Profit / Loss [PnL] at Expiration ($)",main="Short Call / Call Seller [ PnL]"){
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$Val <- ((-pmax(0,(myData$spot-X))+C))
  myData$Val = round(myData$Val, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1))
  plot(myData$spot, myData$Val, pch=21, bg="chartreuse2",col="chartreuse2",cex=1.1, xlab = xlab, ylab = ylab, col.lab="blue",main = main)
  points(x=(X+C), y=0,cex = 2, pch = 23, col ="red",bg="gold")
  text (myData$spot, myData$Val, labels = as.character(myData$Val), adj = 2,
        pos = NULL, offset = 0.5, vfont = NULL,
        cex = 1, col = "red", font = NULL )
  text(X+1.75,-3, labels=as.character("PnL= VT + V0Cr"), adj = 1,cex = 1.2, col="darkblue")
  text(X+1.75,-5, labels=as.character("Bearish OUTLOOK"), adj = 1,col = "brown")
  abline(h = 0,col = "gray")
  abline(v = X,col = "gray")
  legend("topright", legend = c("PnL","BEP"),text.col ="snow",  bg ="darkorchid4", pch=c(16,18), col=c("chartreuse2","gold"),cex = 1.1)
  lines(myData$spot,myData$Val,col = "blue")
}


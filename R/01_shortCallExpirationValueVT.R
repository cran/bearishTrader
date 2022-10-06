#'Calculates Payoff (VT) at expiration per share or unit of the underlying for European Call Seller and draws its graph in the Plots tab.
#'@description
#'An implemented trading strategy with an expectation that the price of the stock or the underlying will decline in future is called a bearish strategy. As explained by Chance (2019), an option is a derivative contract in which one party, the buyer, pays a sum of money to the other party, the seller or writer, and receives the right to either buy (call option) or sell (put option) an underlying asset at a fixed price either on a specific expiration date (European option) or at any time prior to the expiration date (American Option). So the right to buy is one type of option, referred to as a call or call option, whereas the right to sell is another type of option, referred to as a put or put option. Further, it is explained by Chance (2019) that a derivative is a financial instrument that derives its performance from the performance of an underlying asset. Derivatives can be used as insurance that allows for the transfer of risk from one party to another. As everyone knows, insurance is a financial contract that provides protection against loss. The party bearing the risk purchases an insurance policy, which transfers the risk to the other party, the insurer, for a specified period of time. The risk itself does not change, but the party bearing it does. Derivatives allow for this same type of transfer of risk. Derivatives are associated with an underlying asset. As such, the so-called underlying asset is often simply referred to as the underlying, whose value is the source of risk. Derivatives are created in the form of legal contracts. They involve two parties: the buyer and the seller (sometimes known as the writer); each of whom agrees to do something for the other, either now or later. The buyer, who purchases the derivative, is referred to as the long or the holder because he owns (or holds) the derivative and holds a long position. The seller is referred to as the short because he holds a short position (Chance, 2019).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed-form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute Payoff (VT) at expiration per share or unit of the underlying for European Call Seller and draw its graph in the Plots tab. EXAMPLE, Writing HypoCorp December 50 call at $3.00. Using the same notation, let \code{ST} be the price of the underlying at the time T, \code{exp}, and \code{X} are the exercise price (strike price) of the option. Remember that a call option requires the holder, or long, to pay amount \code{X} and receive the underlying upon exercise. It should be obvious that the long would exercise the option at expiration, if ST is greater than X, meaning that the underlying value is greater than what he would pay to obtain the underlying. Otherwise, the call option buyer would simply let the option expire. Thus, on the expiration date European call option buyer has a payoff amount which is Maximum of (0,ST minus X). This payoff is like a Gross Profit (or Gross Loss) as the price (also called premium) paid to buy the European call (denoted by C) has not yet been deducted by call option buyer. For call seller the payoff is \code{minus} Maximum of (0,ST minus X) (Chance, 2019). Here, \code{X} is the exercise price (strike price) of the option, \code{ST} is the price of the underlying at time T, and  \code{C} is the price (also called premium) paid by the European Call Buyer to the the Call Seller.par The graph gets displayed in the Plots tab.
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
#'aShortCallExpirationValueVT(10,10,3)
#'aShortCallExpirationValueVT(50,50,3,0.8,1.2)
#'@export
aShortCallExpirationValueVT<-function (ST,X,C,hl=0,hu=1.5,xlab="Spot Price ($) at Expiration",ylab=" Value / Payoff [ VT ] at Expiration ($)",main="Short Call / Call Seller [ VT ]"){
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$Val <- (-pmax(0,(myData$spot-X)))
  myData$Val = round(myData$Val, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1))
  plot(myData$spot, myData$Val, pch=21, bg="cyan1",col="cyan1",cex=1.1, xlab = xlab, ylab = ylab, col.lab="blue", main = main,col.main="goldenrod3")

    text (myData$spot, myData$Val, labels = as.character(myData$Val), adj = 2,
        pos = NULL, offset = 0.5, vfont = NULL,
        cex = 1, col = "red", font = NULL )
  text(X+1.5,-3, labels=as.character("Value [VT]@Expiration"), adj = 1,col="darkblue")
  text(X+1.75,-5, labels=as.character("Bearish OUTLOOK"), adj = 1,col = "brown")
  abline(h = 0,col = "gray")
  abline(v = X,col = "gray")
  legend("topright", legend = "VT ",text.col ="snow",  bg ="midnightblue", pch=16, col="cyan1",cex = 1.1)
  lines(myData$spot,myData$Val,col = "blue")
}


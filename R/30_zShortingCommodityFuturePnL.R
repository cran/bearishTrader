#'Calculates Profit and Loss at expiration for Shorting a Commodity Future (per unit of the underlying commodity) and draws its graph in the Plots tab.
#'@description
#'On initiation, this is a net credit Strategy and results in net cash inflow in the form of receiving the price of Future contract (per unit of the underlying). At the time of offset the trader or investor has to buy the underlying commodity of future contract at the price on the day of offset. If the bearish outlook of the trader holds good and the price of the commodity (underlying) falls then the trader makes the profit as shown in the graph (TD Ameritrade, 2019).
#'@details
#'According to the information provided by TD Ameritrade (2019), this method is developed, and the given examples are created to compute Profit and Loss at expiration for Shorting a Commodity Future (per unit of the underlying commodity) and draws its graph in the Plots tab.
#'@param STF Commodity price at time T.
#'@param F0 Commodity Initial Price .
#'@param C Cost involved in establishing Future Contract.
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
#'TD Ameritrade. (2019, July 26). Micro E-Mini Futures Contracts. YouTube.https://youtu.be/SShGjJepCdA\cr
#'Hull, J. C. (2022). Options, Futures, and Other Derivatives (11th ed.). Pearson Education (US). https://bookshelf.vitalsource.com/books/9780136940043
#'@examples
#'zShortingCommodityFuture(430,430,0)
#'zShortingCommodityFuture(2900,2910,0,hl=0.9995,hu=1.005)
#'@export
zShortingCommodityFuture<-function (STF,F0,C,hl=0.985,hu=1.005,xlab="Commodity Price ($) at Offset",ylab=" Profit / Loss [PnL] at Offset ($)",main="Shorting a Commodity Future [ PnL]"){
  myData <- data.frame (spot = c((STF*hl):(STF*hu)))
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$Val <- (F0-myData$spot)+ C
  myData$Val = round(myData$Val, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1))
  plot(myData$spot, myData$Val, pch=21, bg="chartreuse2",col="chartreuse2",cex=1.1, xlab = xlab, ylab = ylab, col.lab="blue",main = main)
  points(x=(STF=(F0+C)), y=0,cex = 2, pch = 23, col ="red",bg="gold")
  text (myData$spot, myData$Val, labels = as.character(myData$Val), adj = 2,
        pos = NULL, offset = 0.5, vfont = NULL,
        cex = 0.6, col = "red", font = NULL )
  text(F0+1.25,2, labels=as.character("PnL @ Offset"), adj = 1,cex = 1.2, col="darkblue")
  text(F0+1.75,4, labels=as.character("Bearish OUTLOOK"), adj = 1,col = "brown")
  abline(h = 0,col = "gray")
  abline(v = F0,col = "gray")
  legend("topright", legend = c("PnL","BEP"),text.col ="snow",  bg ="darkorchid4", pch=c(16,18), col=c("chartreuse2","gold"),cex = 1.1)
  lines(myData$spot,myData$Val,col = "blue")
}


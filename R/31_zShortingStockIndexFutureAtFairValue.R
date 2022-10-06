#'Calculates  Profit and Loss at closeout of shorted a Stock Index Future (like Micro E-mini) and draws its graph in the Plots tab.
#'@description
#'On initiation, this is a net credit Strategy and results in net cash inflow in the form of receiving the amount of shorted Stock Index Future. On the day of the closeout the trader or investor has to buy the underlying at the price at the time of closeout. If the bearish outlook of the trader is as expected and the Stock Index Future (like Micro E-mini) falls then the trader makes the profit as shown in the graph (TD Ameritrade, 2019).
#'@details
#'According to the information provided by TD Ameritrade (2019) and  Hull (2022), this method is developed, and the given examples are created, to compute Profit and Loss at closeout of shorted a Stock Index Future (like Micro E-mini) and draw its graph in the Plots tab.
#'@param SIT Stock Index at time T.
#'@param SI0 Stock Index Initial Value.
#'@param R annualized financing rate
#'@param d dividend yield of the index.
#'@param n represents days in Future like 90day Stock Index Future.
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
#'Hull, J. C. (2022). Options, Futures, and Other Derivatives (11th ed.). Pearson Education (US). https://bookshelf.vitalsource.com/books/9780136940043.
#'@examples
#'zShortingStockIndexFutureAtFairValue(3700,3709,0.0275,0.02,90)
#'zShortingStockIndexFutureAtFairValue(2900,2910,0.025,0.03,90,hl=0.9995,hu=1.005)
#'@export
zShortingStockIndexFutureAtFairValue<-function (SIT,SI0,R,d,n,hl=0.9995,hu=1.006,xlab="Stock Index at closeout",ylab=" Profit / Loss [PnL] at closeout ($)",main="Shorting a Stock Index like Micro E-mini S & P 500 [ PnL]")
  {
  FV0 <- SI0*(1+(R-d)*(n/360))
  myData <- data.frame (spot = c((SIT*hl):(SIT*hu)))
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$Val <- (FV0-myData$spot)
  myData$Val = round(myData$Val, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1))
  plot(myData$spot, myData$Val, pch=21, bg="chartreuse2",col="chartreuse2",cex=1.1, xlab = xlab, ylab = ylab, col.lab="blue",main = main)
  points(x=(SIT=FV0), y=0,cex = 2, pch = 23, col ="red",bg="gold")
  text (myData$spot, myData$Val, labels = as.character(myData$Val), adj = 2,
        pos = NULL, offset = 0.5, vfont = NULL,
        cex = 0.6, col = "red", font = NULL )
  text(FV0+1.25,-3, labels=as.character("PnL @ at closeout"), adj = 1,cex = 1.2, col="darkblue")
  text(FV0+1.75,-5, labels=as.character("Bearish OUTLOOK"), adj = 1,col = "brown")
  abline(h = 0,col = "gray")
  abline(v = FV0,col = "gray")
  legend("topright", legend = c("PnL","BEP"),text.col ="snow",  bg ="darkorchid4", pch=c(16,18), col=c("chartreuse2","gold"),cex = 1.1)
  lines(myData$spot,myData$Val,col = "blue")

}


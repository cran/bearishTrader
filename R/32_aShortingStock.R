#'Calculates per share Profit and Loss at covering of the shorted stock and draws its graph in the Plots tab.
#'@description
#'On initiation, this is a net credit Strategy and results in net cash inflow in the form of receiving the unit price of shorted share. On the day of the covering the trader or investor has to buy the underlying stock at the price at the time of covering. If the bearish outlook of the trader is as expected and the stock price falls then the trader makes the profit as shown in the graph (Hull, 2022).
#'@details
#'According to the information provided by Hull (2019), this method is developed, and the given examples are created to compute per share Profit and Loss at covering of the shorted stock and draws its graph in the Plots tab.
#'@param ST a number.
#'@param S0 a number.
#'@param C a number.
#'@param hl a number.
#'@param hu a number.
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
#'Hull, J. C. (2022). Options, Futures, and Other Derivatives (11th ed.). Pearson Education (US). https://bookshelf.vitalsource.com/books/9780136940043
#'@examples
#'aShortStock(51,52)
#'aShortStock(1000,1009,hl=0.995,hu=1.015)
#'@export
aShortStock<-function (ST,S0,C=0,hl=0.8,hu=1.1,xlab="Share Price  @ covering",ylab=" Profit / Loss [PnL] at covering ($)",main="Shorting a Stock [ PnL]"){
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$Val <- (S0-myData$spot)+ C
  myData$Val = round(myData$Val, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1))
  plot(myData$spot, myData$Val, pch=21, bg="chartreuse2",col="chartreuse2",cex=1.1, xlab = xlab, ylab = ylab, col.lab="blue",main = main)
  points(x=(ST=S0), y=0,cex = 2, pch = 23, col ="red",bg="gold")
  text (myData$spot, myData$Val, labels = as.character(myData$Val), adj = -1,
        pos = NULL, offset = 0.5, vfont = NULL,
        cex = 0.6, col = "red", font = NULL )
  text(S0+1.25,-3, labels=as.character("PnL @ covering"), adj = 1,cex = 1.2, col="darkblue")
  text(S0+1.75,+5, labels=as.character("Bearish OUTLOOK"), adj = 1,col = "brown")
  abline(h = 0,col = "gray")
  abline(v = S0,col = "gray")
  legend("topright", legend = c("PnL","BEP"),text.col ="snow",  bg ="darkorchid4", pch=c(16,18), col=c("chartreuse2","gold"),cex = 1.1)
  lines(myData$spot,myData$Val,col = "blue")
}


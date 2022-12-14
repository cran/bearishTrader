% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/18_protectiveCallPnLatExpiration.R
\name{protectiveCallPnLatExpiration}
\alias{protectiveCallPnLatExpiration}
\title{Calculates per share Profit and Loss at expiration for Protective Call and draws its graph in the Plots tab.}
\usage{
protectiveCallPnLatExpiration(
  ST,
  X,
  C,
  S0,
  hl = 0,
  hu = 1.5,
  xlab = "Spot Price ($)",
  ylab = "Profit / Loss ($)",
  main = "Protective Call / Married Call / SyntheticPut"
)
}
\arguments{
\item{ST}{Spot Price at time T.}

\item{X}{Strike Price or eXercise price.}

\item{C}{Call Premium.}

\item{S0}{Initial Stock Price.}

\item{hl}{lowe0r bound value for setting lower-limit of x-axis displaying spot price.}

\item{hu}{upper bound value for setting upper-limit of x-axis displaying spot price.}

\item{xlab}{X-axis label.}

\item{ylab}{Y-axis label.}

\item{main}{Title of the Graph.}
}
\value{
Returns a graph of the strategy.
}
\description{
This strategy is also known as married call or synthetic put or a protected short sale. It consists of a call purchase against a short sale of the underlying stock. An increase in the  price of the stock over the strike price of the call will prompt the investor to exercise the right to buy the stock. As a result, the investor is protected against an increase in the stock price over the strike price. In effect, the call acts as insurance against a rise in the stock price. As with any kind of insurance; however, there is a price to pay. In the case of options, the price is the premium which in effect lowers the price received on the short sale of the stock (Kakushadze & Serur, 2018).
}
\details{
According to conceptual details given by Cohen (2015), and a closed-form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration for Protective Call and draw its graph in the Plots tab. EXAMPLE, Short HypoMedia stock at $14.00 (inflow) and buy HypoMedia December 15 call at $2.00 (outflow). Protective Call results from shorting stock and buying an ATM (at-the money) or OTM (out-of-the-money) call option with a strike price X greater than or equal to S0. The outlook of trader or investor is bearish. This strategy has the same profile as a long put. In fact, this strategy is often referred to as a synthetic put. The graph gets displayed in Plots tab.
}
\examples{
protectiveCallPnLatExpiration(15,15,2,14)
protectiveCallPnLatExpiration(50,50,3,48,hl=0.8,hu=1.2)
protectiveCallPnLatExpiration(1000,1000,7,998,hl=0.98,hu=1.01)
}
\references{
Cohen, G. (2015). The Bible of Options Strategies (2nd ed.). Pearson Technology Group. https://bookshelf.vitalsource.com/books/9780133964448\cr
Kakushadze, Z., & Serur, J. A. (2018, August 17). 151 Trading Strategies. Palgrave Macmillan. https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3247865
}
\author{
MaheshP Kumar, \email{maheshparamjitkumar@gmail.com}
}

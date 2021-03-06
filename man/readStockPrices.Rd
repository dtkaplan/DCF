% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/readStockPrices.R
\name{readStockPrices}
\alias{readStockPrices}
\title{Read stock and dividends from yahoo.finance}
\usage{
readStockPrices(symbols = "F", what = c("prices", "daily", "weekly",
  "monthly", "dividends"), startYear = 1972, endYear = 2015)
}
\arguments{
\item{symbols}{a single character string or a vector of character strings specifying
a stock listed on yahoo.finance.}

\item{what}{whether to read prices or dividends.  For prices, use one of \code{"daily"}, \code{"weekly"}, \code{"monthly"}.
For dividends, use \code{"dividends"}.}

\item{startYear}{the lower end of the time interval for which to download data}

\item{endYear}{the upper end of the time interval}
}
\value{
A data frame.  One column in the data frame lists the stock symbol.  This
is useful when reading data for more than one stock.
}
\description{
This is a convenience function for exercises in the DCF book. It reads stock prices
and dividends over a specified time interval.
}
\details{
This function relies on the finance.yahoo.com API as it exists in early 2015.
}
\examples{
\dontrun{CarStocks <- readStockPrices("F", "monthly", 1995, 2012)}
}


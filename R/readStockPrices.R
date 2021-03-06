#' Read stock and dividends from yahoo.finance
#' 
#' This is a convenience function for exercises in the DCF book. It reads stock prices
#' and dividends over a specified time interval.
#' 
#' @details This function relies on the finance.yahoo.com API as it exists in early 2015.
#' 
#' @param symbols a single character string or a vector of character strings specifying
#' a stock listed on yahoo.finance.
#' @param what whether to read prices or dividends.  For prices, use one of \code{"daily"}, \code{"weekly"}, \code{"monthly"}.
#' For dividends, use \code{"dividends"}.
#' @param startYear the lower end of the time interval for which to download data
#' @param endYear the upper end of the time interval
#' 
#' @return A data frame.  One column in the data frame lists the stock symbol.  This
#' is useful when reading data for more than one stock.
#' 
#' @examples 
#' \dontrun{CarStocks <- readStockPrices("F", "monthly", 1995, 2012)}
#' @export
readStockPrices <- function(symbols='F',
                            what=c("prices","daily","weekly", "monthly", "dividends"),
                            startYear=1972,endYear=2015) {
  
  if (!require(RCurl)) stop("Must install RCurl package.")
  what <- match.arg(what)
  yahooCode <- switch(what,
                      prices = ,
                      daily = "d",
                      weekly = "w",
                      monthly = "m",
                      dividends = "v",
                      "unknown")
  
  stockURL <- "http://real-chart.finance.yahoo.com/table.csv?s=%s&a=05&b=1&c=%d&d=01&e=25&f=%d&g=%s&ignore=.csv"
  
  output <- NULL # for collecting output
  
  for (symbol in symbols) {
    thisURL <- sprintf(stockURL, symbol, startYear, endYear, yahooCode)
    con <- try(textConnection(getURLContent( thisURL )))
    if (inherits(con, what = "try-error"))
      stop(paste("Symbol", symbol, "not found in years", 
                 startYear, "to", endYear, "on Yahoo finance."))
    res <- read.csv(con)
    res$company <- symbol
    close(con)
    output <- rbind(output, res)
  }
  output
}
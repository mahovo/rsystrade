## Notes
## 1)
## Decimal fractions are used instead of percentages throughout.
## Example: 87 percent is expressed as 0.87.
##
## 2)
## "LT" refers to Robert Carver: Leveraged Trading
## "ST" refers to Robert Carver: Systematic Trading
## "F" for "formula".



## Alpha ====
## LT F12
#' Moving Average
#'
#' Calculates average of n items prior to time t in price vector.
#' N (length of price vector) smaller than or equal to n is accepted.
#' This will not be the desired moving average, but will also not fail.
#'
#' @param prices A vector of prices in currency. Newest first. Top to bottom:
#'   Newer to older.
#' @param n Window length.
#' @param t Time index.
#'
#' @returns A number.
#' @export
#'
#' @example
#'
moving_average <- function(prices, n, t = NA) {
  N <- length(prices)
  if(is.na(t)) {t = N} ## Set t to last item if no t is provided

  #stopifnot(N >= n)
  ma <- NA
  if(t <= n) { ## Handle t less than or equal to n
    ma <- mean(prices[1:t])
  } else {
    ma <- sum(prices[(t - n + 1):N]) / n
  }
  ma
}


## LT F12
#' Moving Average vector
#'
#' Calculates moving average for each row in a price series data frame.
#' N (length of price vector) smaller than or equal to n is accepted.
#' This will not be the desired moving average, but will also not fail.
#'
#' @param prices A vector of prices in currency. Newest first. Top to bottom: Newer to older.
#' @param n Moving Average period.
#'
#' @returns A vector of prices.
#' @export
#'
#' @example
#'

moving_average_vector <- function(prices, n) {
  N <- length(prices)

  #stopifnot(N >= n)
  ma_prices <- rep(NA, N)
  for(t in 1:n) { ## Handle t less than or equal to n
    ma_prices[t] <- mean(prices[1:t])
  }
  if(N >= n) {
    for(t in (n + 1):N) {
      ma_prices[t] <- sum(prices[(t - n + 1):t]) / n
    }
  } else {warning("Length of price vector is shorter than or equal to desired MA window length.\n")}
  ma_prices
}


#' Moving Average Crossover
#'
#' Returns TRUE when ma_fast > ma_slow.
#' 1 indicates uptrend ie. go long.
#' -1 indicates downtrend ie. go short.
#'
#' @param ma_fast Fast moving average
#' @param ma_slow Slow moving average
#' @param gap Gap.
#'
#' @returns A boolean.
#' @export
#'
#' @example
#'
moving_average_crossover <- function(ma_fast, ma_slow, gap = 0) {
  ## Is the absolute difference bigger than the gap?
  ## And is ma_fast bigger than ma_slow?
  x <- abs(ma_fast - ma_slow) > gap
  y <- ma_fast > ma_slow
  x * (y - (1 - y))
}

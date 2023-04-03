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
#' @description
#' Calculates average of n items prior to time t in price vector.
#' N (length of price vector) smaller than or equal to n is accepted.
#' This will not be the desired moving average, but will also not fail.
#'
#' f_moving_average() doesn't actually calculate a "moving" average. It only
#' calculates one step of the moving average, i.e. the mean of the past n
#' prices.
#'
#' If the length of the price vector is shorter than the window length, the
#' window length will be set to the length of the price vector, and a warning
#' will be given. The function will not fail or abort in this case.
#'
#' @param prices A vector of prices in currency. Newest first. Top to bottom:
#'   Newer to older.
#' @param t Time index.
#' @param window_length Window length.
#'
#' @returns A single number. One step of a moving average.
#' @export
#'
#' @example
#'
f_moving_average <- function(prices, t = NA, window_length) {
  #N <- length(prices)
  #stopifnot(N >= window_length)
  if(t > window_length) {
    #ma <- sum(prices[(t - window_length + 1):N]) / window_length
    ma <- mean(prices[(t - window_length + 1):t])
  } else { ## Handle t less than or equal to window_length
    ma <- mean(prices[1:t])
    warning("Price vector is shorter than MA window. Window length modified.")
  }
  ma
}


# §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
# fix§0019
# See also zoo::rollmean
# Maybe use if depending on `zoo` anyway.
# §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
## LT F12
#' Calculate Moving Average Vector
#'
#' Calculates moving average for each row in a price series data frame.
#' N (length of price vector) smaller than or equal to n is accepted.
#' This will not be the desired moving average, but will also not fail.
#'
#' @param prices A vector of prices in currency. Newest first. Top to bottom: Newer to older.
#' @param n Moving Average lookback period.
#'
#' @returns A vector of prices.
#' @export
#'
#' @example
#'
f_ma_vector <- function(prices, n) {
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




## Stop Loss ====

## LT F24
#' Stop loss level
#'
#' @param hwm High Water Mark.
#' @param lwm Low Water Mark.
#' @param stop_loss_gap Stop loss gap.
#' @param direction Is current trade long or short? 1 for long, -1 for short.
#' @param rnd If TRUE, add small random amount to stop loss level. Negative if
#'   short.
#'
#' @returns
#' @export
#'
#' @example
#'
f_stop_loss_level <- function(
    hwm,
    lwm,
    stop_loss_gap,
    direction = 0,
    rnd = TRUE) {
  if(direction == 1){ ## If long
    hwm - stop_loss_gap + stats::runif(1, 0.01, 0.03) * rnd
  } else {lwm + stop_loss_gap - stats::runif(1, 0.01, 0.03) * rnd}
}

## LT F23
#' Calculate Stop Loss Gap
#'
#' @description
#' Calculate stop loss gap. Stop loss only takes effect if the difference
#'   between the current price and the stop loss level is bigger than the stop
#'   loss gap.
#'
#' @param price_unit_vol Volatility of returns in price units.
#' @param stop_loss_fraction Stop loss fraction.
#'
#' @returns
#' @export
#'
#' @example
#'
f_stop_loss_gap <- function(price_unit_vol, stop_loss_fraction) {
  price_unit_vol * stop_loss_fraction
}

## F15, LT, p. 113
#' Decimal fraction of capital at risk per trade for system with one instrument
#'   only.
#'
#' @description
#' When using stop loss to exit trades using while trading one instrument only.
#'
#' @param risk_target Annualized system risk target in percentage terms given
#'   as a decimal fraction
#' @param stop_loss_fraction Stop loss fraction
#'
#' @returns
#' @export
#'
#' @example
#'
f_stop_loss_risky_capital_pct <- function(risk_target, stop_loss_fraction) {
  risk_target * stop_loss_fraction
}







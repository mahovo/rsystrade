## Notes
## 1)
## Decimal fractions are used instead of percentages throughout.
## Example: 87 percent is expressed as 0.87.
##
## 2)
## "LT" refers to Robert Carver: Leveraged Trading
## "ST" refers to Robert Carver: Systematic Trading
## "F" for "formula".



## Basic calculations ====

##LT F4.1a
#' Growth Rate With Leverage
#'
#' @param L Leverage factor.
#' @param r Return rate on investment.
#' @param b Borrowing rate.
#'
#' @returns Growth rate as a decimal fraction.
#' @export
#'
#' @examples
#'
f_growth_rate_lvrg <- function(L, r, b) {L * r - (L - 1) * b}


## LT F4.1b
#' Growth With Leverage
#'
#' @param V Value (V_t, value at time t)
#' @param L Leverage factor.
#' @param r Return rate on investment.
#' @param b Borrowing rate.
#'
#' @returns Value in price currency.
#' @export
#'
#' @examples
#'
f_growth_lvrg <- function(V, L, r, b) {(1 + (L * r - (L - 1) * b)) * V}


## LT F4.1c
#' Growth With Leverage
#'
#' @param V Value (\eqn{V_t}, value at time \eqn{t})
#' @param L Leverage factor.
#' @param r Return rate on investment.
#' @param b Borrowing rate.
#'
#' @return Profit in price currency.
#' @export
#'
#' @examples
#'
f_profit_lvrg <- function(V, L, r, b) {(L * r - (L - 1) * b) * V}

#' Calculate Net Returns Vector From Price Vector
#'
#' @param prices A vector of prices in currency. Oldest first. Top to bottom:
#'   Older to newer. The last observation is time t.
#'
#' @return Percentage returns
#' @export
f_returns_from_prices <- function(prices) {
  N <- length(prices)
  (utils::tail(prices, N - 1) / utils::head(prices, N - 1)) - 1
}

#' Calculate Price Vector From Returns Vector
#'
#' @param returns Percentage returns
#' @param initial_price Initial price
#'
#' @returns Vector of prices in currency of initial price.
#' @export
#'
#' @examples
#'
f_prices_from_returns <- function(returns, initial_price) {
  n <- length(returns) + 1 ## Length of price vector
  cumprod(c(initial_price, returns[2:(n - 1)] + 1))
}


#' Calculate Exponentially Weighted Average
#'
#' Calculate EWA of vector \eqn{x} at time \eqn{t} based on a lookback window.
#'   If the length of the lookback window is \eqn{L}, the range of the lookback
#'   window is \eqn{[t-L, t-1]}.
#'
#' @param x vector
#' @param lookback Lookback window length as positive integer. If no `lookback`
#'   is provided, the entire \eqn{x} vector will be used.
#'
#' @return Single exponentially weighted average value
#' @export
#'
#' @examples
f_ewa <- function(x, lookback = NA) {
  if(is.na(lookback)) {
    L <- length(x)
    x_window <- x
  } else {
    L <- lookback
    x_window <- utils::tail(x, L)
  }
  if(!is.integer(L)) {stop("lookback must be an integer (e.g. 25L).")}
  if(!(L >= 0L)) {stop("lookback must be zero or positive.")}

  lambda <- 2 / (1 + L)
  w <- lambda^((0):(L - 1))
  (sum(w %*% x_window)) / sum(w)
}

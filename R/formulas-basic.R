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
#' Growth rate with leverage
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
growth_rate_lvrg <- function(L, r, b) {L * r - (L - 1) * b}


## LT F4.1b
#' Growth with leverage
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
growth_lvrg <- function(V, L, r, b) {(1 + (L * r - (L - 1) * b)) * V}


## LT F4.1c
#' Growth with leverage
#'
#' @param V Value (V_t, value at time t)
#' @param L Leverage factor.
#' @param r Return rate on investment.
#' @param b Borrowing rate.
#'
#' @return Profit in price currency.
#' @export
#'
#' @examples
#'
profit_lvrg <- function(V, L, r, b) {(L * r - (L - 1) * b) * V}

#'Generate net returns vector from price vector
#'
#' @param prices A vector of prices in currency. Newest first. Top to bottom:
#'   Newer to older.
returns_from_prices <- function(prices) {
  N <- length(prices)
  (head(prices, N - 1) / tail(prices, N - 1)) - 1
}

#' Generate price vector from returns vector
#'
#' @param returns Returns.
#' @param initial_price Initial price.
#'
#' @returns Vector of prices in currency of initial price.
#' @export
#'
#' @examples
#'
prices_from_returns <- function(returns, initial_price) {
  n <- length(returns) + 1 ## Length of price vector
  cumprod(c(initial_price, returns[2:(n - 1)] + 1))
}

#' Simulate returns
# sim_returns <- function(n) {
#
# }

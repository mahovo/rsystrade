## Notes
## 1)
## Decimal fractions are used instead of percentages throughout.
## Example: 87 percent is expressed as 0.87.
##
## 2)
## "LT" refers to Robert Carver: Leveraged Trading
## "ST" refers to Robert Carver: Systematic Trading
## "F" for "formula".


## Risk ====

## LT F14
#' Notional exposure
#'
#' @param risk_target Risk target as decimal fraction
#' @param capital Trading capital in currency
#' @param instrument_risk Instrument risk as decimal fraction
#'
#' @returns
#' @export
#'
#' @example
#'
notional_exposure <- function(risk_target, capital, instrument_risk) {
  (risk_target * capital) / instrument_risk
}

## F15, LT, p. 113
#' Decimal fraction of capital at risk per trade for Starter System
#'
#' @param risk_target Risk target as decimal fraction
#' @param stop_loss_fraction Stop loss fraction
#'
#' @returns
#' @export
#'
#' @example
#'
risky_capital_pct <- function(risk_target, stop_loss_fraction) {
  risk_target * stop_loss_fraction
}

#' Instrument risk, annualized
#'
#' Standard deviations of returns.
#'
#' @param prices A time series of prices in currency. Newest first.
#'   Top to bottom: Newer to older.
#' @param window_length Window length.
#' @param t Time index.
#'
#' @returns
#' @export
#'
#' @example
#'
instr_risk <- function(prices, t, window_length = NA) {
  if(is.na(window_length)) {window_length = t} ## Include all if window_length is NA
  if(t > window_length) {
    sd_ <- sd(returns_from_prices(prices[(t - window_length + 1):t])) * 15.87451 ## 252 is the number of business days in a year. sqrt(252) = 15.87451
  } else if (t > 1) {
    sd_ <- sd(returns_from_prices(prices[1:t])) * 15.87451
    warning("Window length for instrument risk calculation is bigger than length of price vector.\n")
  } else {
    sd_ <- NA
    warning("instr_risk=NA. Careful! This might break something.\n")
  }
  sd_
}

#' Minimum exposure
#'
#' @param min_exposure Notional exposure of the minimum trade
#' @param instr_risk Instrument risk as decimal fraction
#' @param target_risk Target risk as decimal fraction
#'
#' @returns
#' @export
#'
#' @example
#'
min_exposure <- function(min_exposure, instr_risk, target_risk) {
  (min_exposure * instr_risk) / target_risk
}

## LT F21
#' Minimum capital
#'
#' @param min_exposure
#' @param instr_risk
#' @param target_risk
#'
#' @returns
#' @export
#'
#' @example
#'
minimum_capital <- function(min_exposure, instr_risk, target_risk) {
  (min_exposure * instr_risk) / target_risk
}

## LT F22
#' Price unit volatility (instrument risk in price units)
#'
#' @param instr_risk Instrument risk.
#' @param price A vector of prices in currency. Newest first. Top to bottom:
#'   Newer to older.
#'
#' @returns
#' @export
#'
#' @example
#'
price_unit_vol <- function(instr_risk, price) {
  instr_risk * price
}

## LT F23
#' Stop loss gap
#'
#' @param price_unit_vol
#' @param stop_loss_fraction
#'
#' @returns
#' @export
#'
#' @example
#'
stop_loss_gap <- function(price_unit_vol, stop_loss_fraction) {
  price_unit_vol * stop_loss_fraction
}

## LT F24
#' High water mark
#'
#' @param prices
#' @param t
#' @param t_trade_open
#'
#' @returns
#' @export
#'
#' @example
#'
hwm <- function (prices, t, t_trade_open) {
  max(prices[t_trade_open:t])
}

## LT F24
#' Low water mark
#'
#' @param prices A vector of prices in currency. Newest first. Top to bottom:
#'   Newer to older.
#' @param t Time index.
#' @param t_trade_open Time index for the time when the trade is opened.
#'
#' @returns
#' @export
#'
#' @example
#'
lwm <- function (prices, t, t_trade_open) {
  min(prices[t_trade_open:t])
}

## LT F24
#' Stop loss level
#'
#' @param hwm High Water Mark.
#' @param lwm Low  Water Mark.
#' @param stop_loss_gap Stop loss gap.
#' @param direction Is current trade long or short? 1 for long, -1 for short.
#' @param rnd If TRUE, add small random amount to stop loss level. Negative if
#'   short.
#'
#'@returns
#' @export
#'
#' @example
#'
stop_loss_level <- function(hwm, lwm, stop_loss_gap, direction = 0, rnd = TRUE) {
  if(direction == 1){ ## If long
    hwm - stop_loss_gap + runif(1, 0.01, 0.03) * rnd
  } else {lwm + stop_loss_gap - runif(1, 0.01, 0.03) * rnd}
}

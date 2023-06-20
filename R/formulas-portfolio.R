#' Calculate Position
#'
#' @description
#' Calculate Position.
#'
#' @return Number
#' @export
#'
#' @examples
f_position <- function() {
  warning("calculate_position() is not yet implemented.")
}


#' Calculate Portfolio Weighted Position
#'
#' @description
#' Calculate weighted position.
#'
#' @return Number
#' @export
#'
#' @examples
f_portfolio_weighted_position <- function() {
  warning(" is not yet implemented.")
}

#' Calculate Final Position To Trade In Units Of Contracts
#'
#' @description
#' Calculate final rounded position to trade in units of contracts. Truncates
#'   the target position to the nearest integer towards 0.
#'
#' @param target_position_size_units Target position to trade in units of
#'   contracts.
#'
#' @return Number of contracts
#' @export
#'
#' @examples
f_position_in_units <- function(
    target_position_size_units
) {
  trunc(target_position_size_units)
}


## "enter_or_exit" is a column in the "positions" table.
## Should this flag be entered by a function or is it just a state in a
## variable?
#' Calculate enter_or_exit Flag
#'
#' @description
#' Calculate `enter_or_exit` flag.
#'
#' @return Character string
#' @export
#'
#' @examples
f_enter_or_exit <- function() {
  warning("calculate_enter_or_exit() is not yet implemented.")
}

## "trade_on" is a column in the "positions" table.
## Should this flag be entered by a function or is it just a state in a
## variable?
#' Calculate trade_on Flag
#'
#' @description
#' Calculate `trade_on` flag
#'
#' @return `0` or `1`
#' @export
#'
#' @examples
f_trade_on <- function() {
  warning("calculate_trade_on() is not yet implemented.")
}

## "direction" is a column in the "positions" table.
## Should this flag be entered by a function or is it just a state in a
## variable?
#' Calculate Direction Flag
#'
#' @description
#' Calculate direction flag
#'
#' @return `-1`, `0` or `1`
#' @export
#'
#' @examples
f_direction <- function() {
  warning("calculate_direction() is not yet implemented.")
}

#' Calculate Leverage Factor
#'
#' @description
#' Calculates leverage factor from any two provided valid parameters. At least
#'   two valid parameters must be provided.
#'
#' @param cash Amount of liquid available.
#' @param invested Amount to invest in an instrument.
#' @param borrowed Amount needed to borrow.
#'
#' @return Number
#' @export
#'
#' @examples
f_leverage_factor <- function(cash = NA, invested = NA, borrowed = NA) {
  I <- invested
  D <- borrowed
  K <- cash
  if(!is.na(I * D)) {
    leverage_factor <- I / (I - D)
  } else if (!is.na(I * K)) {
    leverage_factor <- I / K
  } else if(!is.na(K * D)) {
    leverage_factor <- (K + D) / K
  } else {
    warning("At least two of parameter cash, invested and borrowed must be
    a valid amount (not NA)")
  }
  leverage_factor
}


#' Calculate Target Position In Units Of Contracts
#'
#' @description
#' Calculate the target position in units of contracts. Works for position size
#'   as well as position change.
#'
#' @param notional_exposure Notional exposure.
#' @param price Single price observation in currency of account.
#' @param fx_rate FX rate.
#'
#' @return Number
#' @export
#'
#' @examples
f_target_position_in_units <- function(
    notional_exposure,
    price,
    fx_rate = 1
) {
  notional_exposure / (price * fx_rate)
}


#' Calculate Position In Account Currency
#'
#' @description
#' Calculate position in price units of the account currency. Works for position
#'   size as well as position change.
#'
#' @param price Price in currency of account.
#' @param position_size_units Position size in units.
#'
#' @return Amount in price units of account currency
#' @export
#'
#' @examples
f_position_in_ccy <- function(
    price,
    position_size_units
    ) {
  price * position_size_units
}


#' Calculate Units Of Asset Borrowed
#'
#' @description
#'
#' @return Number
#' @export
#'
#' @examples
f_borrowed_asset <- function() {
  warning("calculate_borrowed_asset() is not yet implemented.")
}


#' Calculate Amount Of Cash Borrowed
#'
#' @return
#' @export
#'
#' @examples
f_borrowed_cash <- function() {
  warning("calculate_borrowed_cash() is not yet implemented.")
}



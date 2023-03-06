#' Calculate Position
#'
#' @return
#' @export
#'
#' @examples
f_position <- function() {
  warning("calculate_position() is not yet implemented.")
}


#' Calculate Portfolio Weighted Position
#'
#' @return
#' @export
#'
#' @examples
f_portfolio_weighted_position <- function() {
  warning(" is not yet implemented.")
}


## "enter_or_exit" is a column in the "positions" table.
## Should this flag be entered by a function or is it just a state in a
## variable?
#' Calculate enter_or_exit flag
#'
#' @return
#' @export
#'
#' @examples
f_enter_or_exit <- function() {
  warning("calculate_enter_or_exit() is not yet implemented.")
}

## "trade_on" is a column in the "positions" table.
## Should this flag be entered by a function or is it just a state in a
## variable?
#' Calculate trade_on flag
#'
#' @return
#' @export
#'
#' @examples
f_trade_on <- function() {
  warning("calculate_trade_on() is not yet implemented.")
}

## "direction" is a column in the "positions" table.
## Should this flag be entered by a function or is it just a state in a
## variable?
#' Calculate direction flag
#'
#' @return
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
#' @return
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


#' Calculate Target Position Size In Units Of Contracts
#'
#' @param notional_exposure
#' @param price
#' @param fx_rate
#'
#' @return
#' @export
#'
#' @examples
f_target_position_size_units <- function(
    notional_exposure,
    price,
    fx_rate = 1
) {
  notional_exposure / (price * fx_rate)
}


#' Calculate Position Size In Account Currency
#'
#' @description
#'
#' @param price Price in currency of account
#' @param position_size_units
#'
#' @return Amout in currency of account
#' @export
#'
#' @examples
f_position_size_ccy <- function(
    price,
    position_size_units
    ) {
  warning("calculate_position_size_ccy() is not yet implemented.")
  price * position_size_units
}


#' Calculate Units Of Asset Borrowed
#'
#' @return
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



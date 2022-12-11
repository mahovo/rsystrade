## Functions shared between live and simulation execution modes.

#' Open trade for Moving Average Crossover
#'
#' Returns 1 for going long and -1 for going short.
#' Returns 0 for no change.
#'
#' 1 if latest_trade_direction = -1 and moving_average_crossover = TRUE.
#' 1 if latest_trade_direction = 0 and moving_average_crossover = TRUE.
#' -1 if latest_trade_direction = 1 and moving_average_crossover = FALSE.
#' -1 if latest_trade_direction = 0 and moving_average_crossover = FALSE.
#' 0 if latest_trade_direction = 1 and moving_average_crossover = TRUE.
#' 0 if latest_trade_direction = -1 and moving_average_crossover = FALSE.
#'
#' No trade will ever open when direction = 0, so latest_trade_direction can only
#' ever be 0 before the first trade.
#'
#' moving_average_crossover:
#' TRUE indicates uptrend ie. go long.
#' FALSE indicates downtrend ie. go short.
#'
#' @param moving_average_crossover 1 for long, 0 for short.
#' @param latest_trade_direction 1 for long, 0 for short.
#'
#' @returns
#' @export
#'
#' @example
#'
# open_trade_mac <- function(moving_average_crossover, latest_trade_direction = 0) {
##  --- TODO ---
#   next_trade_direction <-
#   next_trade_direction
# }


#' Close trade
#'
#' @param prices A vector of prices in currency. Newest first. Top to bottom:
#'   Newer to older.
#' @param t Time index.
#' @param t_trade_open Time index of the time when trade is opened.
#' @param stop_loss_gap Stop loss gap.
#' @param rnd If TRUE, add small random amount to stop loss level. Negative if
#'   short.
#' @param direction Is current trade long or short? 1 for long, -1 for short.
#'
#' @returns -1, 0 or 1.
#' @export
#'
#' @example
#'
close_trade_stop_loss <- function(prices, t, t_trade_open, stop_loss_gap, direction = 0, rnd = FALSE) {
  stop_loss_level(hwm(prices, t, t_trade_open), lwm(prices, t, t_trade_open), stop_loss_gap, direction, rnd)
}


#' Position size in units
#'
#' @param price A vector of prices in currency. Newest first. Top to bottom:
#'   Newer to older.
#' @param risk_target Risk target.
#' @param capital Capital.
#' @param instrument_risk Instrument risk.
#'
#' @returns Position size in units.
#' @export
#'
#' @example
#'
position_size_units <- function(price, risk_target, capital, instrument_risk) {
  floor(notional_exposure(risk_target, capital, instrument_risk) / price)
}

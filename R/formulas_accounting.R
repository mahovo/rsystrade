
#' Calculate Total Positions Value
#'
#' @description
#' Current aggregate value of all open positions.
#'
#' @param position_tables List of all position tables.
#' @param t Time index ("now")
#'
#' @return Single number in units of price
#' @export
#'
#' @examples
f_total_positions_value <- function(position_tables, t) {
  sum(get_all_position_sizes_ccy(position_tables, t))
}

#' Calculate Total Price Returns
#'
#' @description
#' Calculate total price returns across all instruments in system.
#'
#' @param position_tables List of all position tables.
#' @param t Time index ("now")
#'
#' @return Aggregate return in units of price
#' @export
#'
#' @examples
f_total_returns <-function(position_tables, t) {
  sum(get_all_returns(position_tables, t))
}

#' Calculate Account Profit/Loss
#'
#' @description
#' Calculate account profit/loss.
#'
#' Calculate account profit and loss at time \eqn{t}.
#'
#' @param position_tables List of all position tables.
#' @param t Time index ("now")
#'
#' @return List of profits/losses
#' @export
#'
#' @examples
f_account_pandl <- function(position_tables, t) {
  sum(get_all_subsystem_pandls(position_tables, t))
  #new_broker_account_value - prev_broker_account_value
}

#' Calculate Sum Of All Position Changes
#'
#' @description
#' Calculate sum of all position changes across all subsystems at time eqn{t}.
#'
#' @param position_tables List of all position tables.
#' @param t Time index ("now")
#'
#' @return Amount in price units
#' @export
#'
#' @examples
f_total_positions_changes_ccy <- function(position_tables, t) {
  sum(get_all_position_changes_ccy(position_tables, t))
}

#' Calculate Cash Account Total
#'
#' @description
#' Calculate cash account total.
#'
#' @param position_tables List of position tables.
#' @param account_table Account table.
#' @param t Time index ("now")
#'
#' @return Amount in price units
#' @export
#'
#' @examples
f_total_cash <- function(position_tables, system_account_table, t) {
  system_account_table$cash[t - 1] - f_total_positions_changes_ccy(position_tables, t)
}

# calculate_total_cash <- function(prev_cash, trad_amount, open_trade, close_trade) {
# See https://qoppac.blogspot.com/2016/06/capital-correction-pysystemtrade.html
#   if(open_trade) {
#     cash <- prev_cash - trad_amount
#   } else if(close_trade) {
#     cash <- prev_cash + trad_amount
#   } else {cash <- prev_cash}
#
#   cash
# }

#' Calculate Account Value
#'
#' @description
#' Current value of all open positions plus cash.
#'
#' @param position_tables List of all position tables.
#' @param cash Amount in cash account.
#' @param t Time index ("now").
#'
#' @return Single number in units of price
#' @export
#'
#' @examples
f_account_value <- function(position_tables, cash, t) {
  sum(get_all_position_sizes_ccy(position_tables, t)) + cash
}

#' Calculate Trading Capital
#'
#' @description
#' The trading capital is the initial trading capital plus the sum of all
#'   profits/losses. Profits/losses are mark to market, not just realized.
#'
#' @param account_table Account table.
#' @param account_pandl Total account profit/loss at time \eqn{t}.
#' @param t
#'
#' @return Single amount in units of price
#' @export
#'
#' @examples
f_capital <- function(system_account_table, account_pandl, t) {
  system_account_table$capital[t - 1] + account_pandl
}

#' Calculate Subsystem Profit And Loss
#'
#' @description
#' Calculate subsystem profit and loss (subsystem returns).
#'
#' @param position_table Position table.
#' @param instrument_returns Instrument price returns. This is typically the
#'   difference between today's and yesterday's price.
#' @param t Time index ("now").
#'
#' @return A single number in price units.
#' @export
#'
#' @examples
f_subsystem_pandl <- function(position_table, instrument_return, t) {
  position_table$position_size_units[t - 1] * instrument_return
}


#' Calculate Price Returns
#'
#' @description
#' Calculate price returns.
#'
#' @param prices A price vector containing at least the two prices we need for
#'   calculating returns.
#' @param t Time index ("now")
#'
#' @return A single number in price units.
#' @export
#'
#' @examples
f_price_returns <- function(prices, t) {
  prices[t] - prices[t - 1]
}

#' Calculate Percentage Returns
#'
#' @description
#' Calculate percentage returns.
#'
#' @param prices A price vector containing at least the two prices we need for
#'   calculating returns.
#'
#' @return Percentage as decimal fraction
#' @export
#'
#' @examples
f_percentage_returns <- function(prices, t) {
  (prices[t] - prices[t - 1]) / prices[t - 1]
}

#' Calculate Position Change
#'
#' @description
#' Calculate position change in price units.
#'
#' @param position_table Position table
#' @param position_size_ccy Current (time \eqn{t}) position size in price units.
#' @param t Time index ("now")
#'
#' @return Amount in price units
#' @export
#'
#' @examples
f_position_change_ccy <- function(position_table, position_size_ccy, t) {
  position_size_ccy - position_table$position_size_ccy[t - 1]
}

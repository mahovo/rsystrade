## Accounting ====


update_account_table <- function() {
  stop("\"Live mode is not implemented yet.\"")
}

#' Account value
# account_value <- function() {
#   # <------ TODO
# }

#' Cash
#' --- DON'T USE ---
#'
#' @param prev_capital Value of capital at t - 1.
#' @param trade_amount Amount of account currency for current trade.
#' @param open_trade Boolean. A trade is being opened at time t.
#' @param open_trade Boolean. A trade is being closed at time t.
# cash <- function(prev_cash, trad_amount, open_trade, close_trade) {
#   # <------ TODO
#   # See https://qoppac.blogspot.com/2016/06/capital-correction-pysystemtrade.html
#   if(open_trade) {
#     cash <- prev_cash - trad_amount
#   } else if(close_trade) {
#     cash <- prev_cash + trad_amount
#   } else {cash <- prev_cash}
#
#   cash
# }


#' Capital
#'
# capital <- function() {
#   # <------ TODO
# }


#' Profit and loss
# pnl <- function() {
#   # <------ TODO
#   new_broker_account_value - prev_broker_account_value
# }

#' Accumulated profit and loss
# cum_pnl <- function() {
#   # <------ TODO
# }





#' Binary Moving Average Crossover
#'
#' @description
#' Returns
#' * 1 when ma_fast > ma_slow, and abs(ma_fast - ma_slow) > gap.
#' * -1 when ma_fast < ma_slow, and abs(ma_fast - ma_slow) > gap.
#' * 0 when abs(ma_fast - ma_slow) < gap.
#'
#' 1 indicates uptrend i.e. go long.
#' -1 indicates downtrend i.e. go short.
#'
#' If `ma_fast` or `ma_slow` are not provided, both values are calculated from
#' price vector. In this case `n_fast` and `n_slow` _must_ be provided.
#'
#' If both `ma_fast` and `ma_slow` are provided, `n_slow` and `n_slow` will be
#'   ignored.
#'
#' If `strict=FALSE` the function will not fail if `n_fast > n_slow`, but a
#'   warning will be given. Also, if `strict=FALSE` and the length of the price
#'   vector is smaller than `n_slow`, `n_slow` will be set to the length of
#'   the price vector.
#'
#' `ma_fast` and `ma_slow` inputs are typically single steps of moving averages,
#' but can be vectors.
#'
#' On a side note, `mac_rule()` doesn't check that `ma_fast` and `ma_slow`
#' inputs are in fact calculated as a moving averages. Any number or vector will
#' work.
#'
#' @param prices. A vector of prices.
#' @param ma_fast A number. Fast _moving average_. Vector or single numeric.
#' @param ma_slow A number. Slow _moving average_. Vector or single numeric.
#' @param n_fast A positive integer. It is the responsibility of the user to
#'   check that the input value makes sense.
#' @param n_slow A positive integer. It is the responsibility of the user to
#'   check that the input value makes sense.
#' @param gap A positive integer. Gap size in same unit as the parameters above
#'   (typically days).
#' @param strict Boolean. If `strict=TRUE`, `n_slow` must be smaller than the
#'   number of prices in the `prices` vector, and `n_slow` must be greater than
#'   `n_fast`.
#'
#' @returns A boolean.
#' @export
#'
#' @example
#'
apply_binary_mac_rule <- function(
    prices,
    ma_fast = NA,
    ma_slow = NA,
    n_fast = 20L,
    n_slow = 80L,
    gap = 0L,
    strict = TRUE) {

  ## If ma_fast or ma_slow is NA, calculate both values
  if((!is.na(ma_fast)) * (!is.na(ma_slow)) == 0) {
    if(n_fast > n_slow) {
      warning("n_fast is greater than n_slow.")
    }
    if(n_fast == n_slow) {
      warning("n_fast is equal to n_slow.")
    }
    if(strict == TRUE) {
      stopifnot(nrow(prices) > n_slow + 1)
      stopifnot(n_slow > n_fast)
    }
    ma_fast <- moving_average(
      prices,
      n_fast
    )
    ma_slow <- ma_slow <- moving_average(
      prices,
      n_slow
    )
  }

  ## Is the absolute difference bigger than the gap?
  ## And is ma_fast bigger than ma_slow?
  x <- abs(ma_fast - ma_slow) > gap
  y <- ma_fast > ma_slow

  ## Returns
  ## 1 when ma_fast > ma_slow, and abs(ma_fast - ma_slow) > gap.
  ## -1 when ma_fast < ma_slow, and abs(ma_fast - ma_slow) > gap.
  ## 0 when abs(ma_fast - ma_slow) < gap.
  x * (2 * y - 1)
}




apply_stop_loss_rule <- function(prices, instrument_risk_, stop_loss_fraction, t_trade_open) {
  price_unit_vol_ <- price_unit_vol(instrument_risk_, price)
  stop_loss_gap_ <- stop_loss_gap(price_unit_vol_, stop_loss_fraction)

  ## Close position if stop loss level was breached yesterday [LT, p. 138].
  ## Close position even if price has recovered. [LT, p. 141]
  stop_loss_signal <- calculate_stop_loss_signal(
    prices = prices,
    t = t - 1,
    t_trade_open = t_trade_open,
    stop_loss_gap = stop_loss_gap_,
    direction = direction,
    rnd = FALSE
  )

  if(direction == 1) { ## If long
    if(price < close_trade_stop_loss_) { ## Close position if...
      open_close <- "CLOSE"
    } else {open_close <- "OPEN"}
  } else if(direction == -1) { ## If short
    open_close <- "OPEN"
    if(price > close_trade_stop_loss_) { ## Close position if...
      open_close <- "CLOSE"
    } else {open_close <- "OPEN"}
  } else {open_close <- "CLOSE"} ## Should be redundant...
  open_close
}


#' Stop loss signal
#'
#' @description Calculate stop loss signal.
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
calculate_stop_loss_signal <- function(prices, t, t_trade_open, stop_loss_gap, direction = 0, rnd = FALSE) {
  stop_loss_level(hwm(prices, t, t_trade_open), lwm(prices, t, t_trade_open), stop_loss_gap, direction, rnd)
}




##################################################################

#ToDo
#Instead of apply_mac_rule use apply_rule in sys_frwk.R.
#apply_rule takes whatever rule and applies it.
#Note, mac_rule works for vectors (including a vector of length 1).

##################################################################

#' Moving Average
#'
#' @description
#'
#' Calculates the _moving average crossover signal_ for the last element in the
#'   vector `prices`.
#'
#' Typically the number of rows in the input data frame `ma_data` will be
#'   `length(prices) - 1`. `apply_mac_rule()` will append a row to
#'   `ma_data`. If for some reason `ma_data` has fewer rows than
#'   `length(prices) - 1`, `apply_mac_rule` will still insert new row at
#'   row number `length(prices) - 1` in ` ma_data`. The rows between the last
#'   row in the input data frame and the new row will be filled with NAs.
#'
#' @param prices
#' @param ma_data Vector of previous moving averages passed to output
#' @param n_fast
#' @param n_slow
#' @param gap
#'
#' @return
#' @export
#'
#' @examples
# apply_mac_rule <- function(
#     prices,
#     ma_data, ## Data frame
#     n_fast = 16,
#     n_slow = 64,
#     gap = 0) {
#
#   #stopifnot(nrow(prices) > n_slow + 1)
#   #ToDo
#   #Have a look at this
#   stopifnot(n_slow > n_fast)
#
#   t <- length(prices)
#
#   ## We need to skip AT LEAST two days because:
#   ## sd needs two data points as input.
#   ## To get two returns, we need three prices.
#   ##
#   ## However, we should definitely make sure we have enough data for a full
#   ## ma_slow window and instrument_risk.
#   min_periods <- max(n_slow, risk_window_length)
#
#
#   # ma_data <- data.frame(
#   #   ma_fast = prices[1:min_periods],
#   #   ma_slow = prices[1:min_periods],
#   #   mac = rep(FALSE, min_periods)
#   # )
#
#   if(t < min_periods + 1) {
#     ma_data[t, ] <- list(
#       ma_fast = NA,
#       ma_slow = NA,
#       mac_signal = FALSE
#     )
#   }
#
#   #ToDo:
#   #Could we do something like:
#   #  ma_fast_slow <- moving_average(prices, c(n_fast, n_slow))
#   ma_fast <- moving_average(
#     prices,
#     n_fast
#   )
#   ma_slow <- moving_average(
#     prices,
#     n_slow
#   )
#
#   ## mac == 1 indicates long, mac == -1 indicates short
#   ## mac == 0 indicates on signal.
#
#   #ToDo
#   #Could we do something like
#   # moving_average_crossover <- mac(ma_fast_slow[1], ma_fast_slow[2])
#
#
#   mac_signal <- mac_rule(ma_fast, ma_slow, gap = gap)
#
#   # cat("ma_fast =", ma_fast, "\n")
#   # cat("ma_slow =", ma_slow, "\n")
#   # cat("mac =", mac, "\n")
#   # cat("latest_trade_direction =", latest_trade_direction, "\n")
#   # cat("instrument_risk =", instrument_risk_, "\n")
#   # cat("notional_exposure =", notional_exposure_, "\n")
#
#   #direction <- open_trade_mac(
#   #  moving_average_crossover = mac,
#   #  latest_trade_direction = latest_trade_direction
#   #) ## 0 if no change
#   #direction <- mac
#   #cat("direction =", direction, "\n")
#
#   mac_data[t, ] <-list(
#     ma_fast = ma_fast,
#     ma_slow = ma_slow,
#     mac_signal = mac_signal
#   )
# }





#' Calculate a moving average crossover signal.
#'
#' @description
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
#'   but can be vectors.
#'
#' On a side note, `r_mac()` doesn't check that `ma_fast` and `ma_slow`
#'   inputs are in fact calculated as a moving averages. Any number or vector
#'   will work.
#'
#' @param price A vector of prices in currency. Oldest first. Top to bottom:
#'   Older to newer. The last observation is time t.
#' @param ma_fast A number. Fast _moving average_. Vector or single numeric.
#' @param ma_slow A number. Slow _moving average_. Vector or single numeric.
#' @param n_fast A positive integer. It is the responsibility of the user to
#'   check that the input value makes sense.
#' @param n_slow A positive integer. It is the responsibility of the user to
#'   check that the input value makes sense.
#' @param gap A positive integer. Gap size in same unit as the parameters above
#'   (typically days).
#' @param strict Boolean. If `strict=TRUE`, `n_slow` must be smaller than the
#'   number of prices in the `price` vector, and `n_slow` must be greater than
#'   `n_fast`.
#' @param binary If `TRUE`: Binary mode. If `FALSE`: Proportional signal.
#' In binary mode returns
#' * 1 when ma_fast > ma_slow, and abs(ma_fast - ma_slow) > gap.
#' * -1 when ma_fast < ma_slow, and abs(ma_fast - ma_slow) > gap.
#' * 0 when abs(ma_fast - ma_slow) < gap.
#'
#' 1 indicates uptrend i.e. go long. -1 indicates downtrend i.e. go short.
#'
#' @returns A named list containing:
#'   - `signal` Moving average crossover signal
#'   - `ma_fast` Fast moving average price
#'   - `ma_slow` Slow moving average price
#' @export
#'
#' @example
r_mac <- function(
    t = NA,
    price,
    ma_fast = NA,
    ma_slow = NA,
    n_fast = 20L,
    n_slow = 80L,
    gap = 0,
    strict = TRUE,
    binary = FALSE) {

  if(is.na(t)) {t = length(price)} ## Set t to last item if no t is provided

  ## If ma_fast or ma_slow is NA, calculate both values
  if((!is.na(ma_fast)) * (!is.na(ma_slow)) == 0) {
    if(n_fast > n_slow) {
      warning("n_fast is greater than n_slow.")
    }
    if(n_fast == n_slow) {
      warning("n_fast is equal to n_slow.")
    }
    if(strict == TRUE) {
      stopifnot(length(price) > n_slow + 1)
      stopifnot(n_slow > n_fast)
    }
    ma_fast <- f_moving_average(
      price,
      t,
      n_fast
    )
    ma_slow <- f_moving_average(
      price,
      t,
      n_slow
    )
  }

  if(binary == FALSE) {
    ## Signal equals fast signal minus slow signal if the difference is bigger
    ## than the gap value. Otherwise the signal is 0.
    signal <- (ma_fast - ma_slow) * (abs(ma_fast - ma_slow) > gap)
  } else { ## If binary mode, return -1, 0 or 1
    ## Is the absolute difference bigger than the gap?
    ## And is ma_fast bigger than ma_slow?
    x <- abs(ma_fast - ma_slow) > gap
    y <- ma_fast > ma_slow

    ## Returns
    ## 1 when ma_fast > ma_slow, and abs(ma_fast - ma_slow) > gap.
    ## -1 when ma_fast < ma_slow, and abs(ma_fast - ma_slow) > gap.
    ## 0 when abs(ma_fast - ma_slow) < gap.
    signal <- x * (2 * y - 1)
  }
  list(
    signal = signal,
    ma_fast = ma_fast,
    ma_slow = ma_slow
  )
}




#' Stop Loss Rule
#'
#' @description
#' Return 0 if price level breached stop loss threshold. Return 1 if stop loss
#'   threshold was not breached.
#'
#' 0 indicates that an open position should be exited. 1 indicates that an open
#'   position should remain open. If a position is closed, and the stop loss
#'   returns value 1, the position will remain closed.
#'
#' Note that `instrument_risk`, `t_last_position_entry` and `direction` are
#'   calculated at time `t - 1`. If we want these parameters at time `t`, we
#'   need to apply the stop loss rule after `update_position_table_row`.
#'
#' @param t Time index.
#' @param prices A vector of prices in currency. Oldest first. Top to bottom:
#'   Older to newer.
#' @param instrument_risk Instrument risk at time t-1.
#' @param stop_loss_fraction Stop loss fraction.
#' @param t_trade_entry Time index of the time when current trade was entered.
#' @param direction Was current trade long or short at time t-1? 1 for long, -1
#'   for short.
#' @param rnd If TRUE, add small random amount to stop loss level. Negative if
#'   short.
#'
#' @return A named list containing:
#'   - `signal` Trade on/off indicator: 1 or 0
#'   - `stop_loss_gap` Stop loss gap
#' @export
#'
#' @examples
r_stop_loss <- function(
    t,
    prices,
    instrument_risk, # at time t-1
    stop_loss_fraction,
    t_last_position_entry, # at time t-1
    direction, # at time t-1
    rnd = FALSE
) {
  price_unit_vol <- f_price_unit_vol(prices[t], instrument_risk)
  stop_loss_gap <- f_stop_loss_gap(price_unit_vol, stop_loss_fraction)

  ## Exit position if stop loss level was breached yesterday [LT, p. 138].
  ## Exit position even if price has recovered. [LT, p. 141]
  stop_loss_level <- f_stop_loss_level(
    f_high_water_mark(prices, t - 1, t_last_position_entry),
    f_low_water_mark(prices, t - 1, t_last_position_entry),
    stop_loss_gap = stop_loss_gap,
    direction = direction,
    rnd  = rnd
  )

  signal <- 1 ## bypass by default
  stop_loss <- "---"
  if(direction == 1) { ## If long
    if(prices[t] < stop_loss_level) { ## Exit position if...
      signal <- 0 ## Exit
      stop_loss <- "stop_loss"
    }
  } else if(direction == -1) { ## If short
    if(prices[t] > stop_loss_level) { ## Exit position if...
      signal <- 0 ## Exit
      stop_loss <- "stop_loss"
    }
  } #else {signal <- 0} ## Should be redundant...

  list(
    signal = signal,
    stop_loss_gap = stop_loss_gap,
    stop_loss = stop_loss
  )
}

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
#' @param prices A vector of prices in currency. Oldest first. Top to bottom:
#'   Older to newer. The last observation is time t.
#' @param ma_data Vector of previous moving averages passed to output
#' @param n_fast
#' @param n_slow
#' @param gap
#'
#' @return


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




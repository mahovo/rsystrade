

#' Moving Average Crossover Stop Loss
#'
#' @param t Time index after the latest price in the windows. (Note that
#'   the latest price is the same for both fast and slow window.)
#' @param price A vector of prices in currency. Oldest first. Top to bottom:
#'   Older to newer. The last observation is time t.
#' @param signal_table Signal table
#' @param position_table Position table
#' @param config Config list
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
#' @param binary If `TRUE`: Binary mode. If `FALSE`: Proportional signal.
#'   In binary mode returns
#'   * 1 when ma_fast > ma_slow, and abs(ma_fast - ma_slow) > gap.
#'   * -1 when ma_fast < ma_slow, and abs(ma_fast - ma_slow) > gap.
#'   * 0 when abs(ma_fast - ma_slow) < gap.
#'
#' @return
#' @export
#'
#' @examples
s_mac_stoploss <- function(
    t = t,
    price = price,
    signal_table,
    position_table,
    config,
    ma_fast = NA,
    ma_slow = NA,
    n_fast = 25L,
    n_slow = 100L,
    gap = 0,
    strict = TRUE,
    binary = FALSE
) {

  combine_rules <- function() {
    mac_signal <- r_mac(
      t = t,
      price = price,
      ma_fast = NA,
      ma_slow = NA,
      n_fast = 25L,
      n_slow = 100L,
      gap = 0,
      strict = TRUE,
      binary = FALSE
    )

    stop_loss_signal <- r_stop_loss(
      price,
      t = NA,
      position_table$instrument_risk[t - 1],
      config$stop_loss_fraction,
      position_table$t_last_position_entry[t - 1],
      position_table$direction[t - 1],
      rnd = FALSE
    )

    signal <- mac_signal[[1]] * stop_loss_signal[[1]]
    c(
      list(signal = signal), ## s_mac_25_100_stoploss signal
      mac_signal[-1], ## Additional output from mac_rule()
      stop_loss_signal[-1] ## Additional output from stop_loss_rule()
    )
  }

  if(signal_table$stop_loss[t - 1] == "stop_loss") {
    signal <- combine_rules()[[1]]
    ## Don't allow entering trade in the same direction after stop was
    ## triggered.
    ## I.e. if
    ##  * position was long on day t - 3, and
    ##  * threshold was breached on day t - 2,
    ##  * then stop was triggered on day t - 1.
    ## In this case is is not allowed to enter a long position on day t or later
    ## until a special condition has been met.
    if(sign(signal) == position_table$direction[position_table$t_last_position_entry[t - 1]]) {
      signal[[1]] <- 0
      stop_loss <- "same_dir"
    }
  } else {
    signal <- combine_rules()
  }

}

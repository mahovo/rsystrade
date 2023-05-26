#' Stop Loss Position Modifier
#'
#' @description
#' Return 0 if price level breached stop loss threshold. Return input
#'   position_size_ccy if stop loss threshold was not breached.
#'
#' @param t Time index.
#' @param price A vector of prices in currency. Oldest first. Top to bottom:
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
p_stop_loss <- function(
    t,
    position_size_ccy,
    price,
    instrument_risk, # at time t
    t_last_position_entry, # at time t
    direction, # at time t
    stop_loss_fraction = 0.5,
    rnd = FALSE
  ) {

  price_unit_vol <- f_price_unit_vol(price[t], instrument_risk)
  stop_loss_gap <- f_stop_loss_gap(price_unit_vol, stop_loss_fraction)

  ## Exit position if stop loss level was breached today [LT, p. 138].
  ## Exit position even if price has recovered. [LT, p. 141]
  stop_loss_level <- f_stop_loss_level(
    f_high_water_mark(price, t, t_last_position_entry),
    f_low_water_mark(price, t, t_last_position_entry),
    stop_loss_gap = stop_loss_gap,
    direction = direction,
    rnd  = rnd
  )

  signal <- 1 ## bypass by default
  stop_loss <- "---"
  if(direction == 1) { ## If long
    if(price[t] < stop_loss_level) { ## Exit position if...
      signal <- 0 ## Exit
      stop_loss <- "stop_loss"
    }
  } else if(direction == -1) { ## If short
    if(price[t] > stop_loss_level) { ## Exit position if...
      signal <- 0 ## Exit
      stop_loss <- "stop_loss"
    }
  } #else {signal <- 0} ## Should be redundant...
  modified_position_size_ccy <- position_size_ccy * signal

  list(
    modified_position_size_ccy = modified_position_size_ccy,
    stop_loss_gap = stop_loss_gap,
    stop_loss = stop_loss
  )
}

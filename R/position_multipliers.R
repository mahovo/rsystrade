

#' Block Entry In Same Direction
#'
#' @description
#' Position multiplier
#'
#' @param t Time index.
#' @param price (Variable param) Price vector.
#' @param direction (Variable param) Direction. 1 for long, -1 for short, 0 for flat.
#' @param t_last_position_entry (Variable param) Time index of last position entry.
#' @param position_table (Variable param) Position table.
#' @param mode (Fixed param) Mode. See details.
#'
#' @details
#' `mode=1` is `block_same_dir_entry()`.
#'
#' `mode=2` is `block_same_dir_entry_inside_watermarks()`.
#'
#' @return
#' @export
#'
#' @examples
m_block_same_direction_entry <- function(
    t,
    price,
    direction, ## at time t
    t_last_position_entry,
    position_table,
    mode = 1 ## Fixed param
  ) {

  previous_entry_dir <- if(t_last_position_entry == 0) {
      0
    } else {
      position_table$direction[t_last_position_entry]
    }

  true_if_entering <- function() {
    previous_entry_dir == 0 && abs(direction) == 1
  }

  block_same_dir_entry <- function() {
    if(direction == previous_entry_dir && true_if_entering()) {
      0
    } else {
      1
    }
  }

  block_same_dir_entry_inside_watermarks <- function() {
    hwm <- f_high_water_mark(price, t, t_last_position_entry)
    lwm <- f_low_water_mark(price, t, t_last_position_entry)

    block_below_hwm <- function() {
      if(price[t] < hwm) {
        block_same_dir_entry()
      } else {
        1
      }
    }

    block_above_lwm <- function() {
      if(price[t] > lwm) {
        block_same_dir_entry()
      } else {
        1
      }
    }

    if(direction == 1) {
      block_below_hwm()
    } else if(direction == -1) {
      block_above_lwm()
    } else {1} ## Should be redundant as position will be 0
  }

  multiplier_value <- switch(
    mode,
    "1" = block_same_dir_entry(),
    "2" = block_same_dir_entry_inside_watermarks()
  )

  list(
    multiplier_value = multiplier_value
  )
}

## AFTS, p. 581

## ## ## ## ## ## ##
## TODO
## Should the instrument risk be limited directly instead of adjusting the
## position?
## Implement as a soft limit function where min is calculated by m_min_limit_risk()
## (not using that name).
## Max risk could be determined by percentile, e.g. 99th...?
## But don't we only want to limit risk downwards?
## ## ## ## ## ## ##

m_min_limit_risk <- function(
    t,
    instrument_risk,
    inst_div_mul,
    instrument_weight,
    instrument_risk_target,
    config,
    max_leverage = 2, ## Fixed param
    mode = 1 ## Fixed param
  ) {
  min_risk <- config$max_signal * inst_div_mul * instrument_weight *
    instrument_risk_target / (config$normalization_factor_target * max_leverage)

  multiplier_value <- min(1, instrument_risk / min_risk)

  list(
    multiplier_value = multiplier_value
  )
}


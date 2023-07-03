

#' Block Entry In Same Direction
#'
#' @description
#' Position multiplier
#'
#' @param t Time index.
#' @param prices Price vector.
#' @param direction Direction. 1 for long, -1 for short, 0 for flat.
#' @param t_last_position_entry Time index of last position entry.
#' @param position_table Position table.
#' @param mode Mode.
#'
#' @return
#' @export
#'
#' @examples
m_block_same_direction_entry <- function(
    t,
    prices,
    direction, ## at time t
    t_last_position_entry,
    position_table,
    mode = 1
  ) {

  previous_entry_dir <- position_table$direction[t_last_position_entry]

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
    hwm <- f_high_water_mark(prices, t, t_last_position_entry)
    lwm <- f_low_water_mark(prices, t, t_last_position_entry)

    block_below_hwm <- function() {
      if(prices[t] < hwm) {
        block_same_dir_entry()
      } else {
        1
      }
    }

    block_above_lwm <- function() {
      if(prices[t] > lwm) {
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






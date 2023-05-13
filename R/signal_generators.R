mac_2_8_signal <- function(
    prices,
    signal_table,
    position_table,
    t
  ) {
  mac_rule(
    prices,
    t = t,
    ma_fast = NA,
    ma_slow = NA,
    n_fast = 2L,
    n_slow = 8L,
    gap = 0,
    strict = TRUE,
    binary = FALSE
  )
}



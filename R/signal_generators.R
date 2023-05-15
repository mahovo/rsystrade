s_mac_25_100 <- function(
    prices,
    signal_table,
    position_table,
    t,
    config
  ) {
  mac_rule(
    prices,
    t = t,
    ma_fast = NA,
    ma_slow = NA,
    n_fast = 25L,
    n_slow = 100L,
    gap = 0,
    strict = TRUE,
    binary = FALSE
  )
}

s_mac_25_100_stoploss <- function(
    prices,
    signal_table,
    position_table,
    t,
    config
) {

  combine_rules <- function() {
    mac_signal <- mac_rule(
      prices,
      t = t,
      ma_fast = NA,
      ma_slow = NA,
      n_fast = 25L,
      n_slow = 100L,
      gap = 0,
      strict = TRUE,
      binary = FALSE
    )

    stop_loss_signal <- stop_loss_rule(
      prices,
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

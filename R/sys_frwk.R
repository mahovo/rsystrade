
## System framework

# §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
# fix§0029
# Write help explaining structure of input algos. (See help for `parse_algos`)
# §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
#' Make System
#'
#' @param algos A list of algos.
#' @param init_capital Initial capital.
#' @param system_risk_target System risk target as decimal fraction (percent
#'   divided by 100).
#' @param risk_window_length Risk window length.
#' @param stop_loss_fraction Stop loss fraction.
#' @param min_periods Minimum number of periods (rows) in the data sets.
#'   Defaults to 1.
#' @param mode `sim` for _simulation mode_, `live` for _live mode_.
#' @param instrument_data_folder_path _Instrument data sets folder_ path. Path
#'   of folder containing all the instrument csv-files for the system. The
#'   _instrument data sets folder_ can not be nested.
#' @param ...
#'
#' @return A list containing
#'    * a list of instrument data sets as data frames. The instrument name is
#'      assigned to each data frame as a comment. So we can get the data frame
#'      name:
#'      ```R
#'      comment(instrument_data[[1]])
#'      [1] "instrument1"
#'      ```
#'    * a list of parsed algos, which in turn contains an instrument name and a
#'      list of two trade rule functions (one for entering and on for exiting).
#'    * a list `signal_tables`, each containing a data frame.
#'    * a list `position_table`, each containing a data frame.
#'    * a dataframe `system_account_table`.
#'    * a list `config` containing system configuration.
#'    ```
#'    system <- list(
#'      instrument_data_sets <- list(
#'        data.frame(),
#'        data.frame(),
#'        ...,
#'        data.frame()
#'      )
#'      algos <- list(
#'        list(
#'          instrument = <instrument_name_1>,
#'          algo = list(
#'            <entering_rule_function>,
#'            <exiting_rule_function>
#'          )
#'        ),
#'        list(
#'          instrument = <instrument_name_1>,
#'          algo = list(
#'            <entering_rule_function>,
#'            <exiting_rule_function>
#'          )
#'        ),
#'        ...,
#'        list(
#'          instrument = <instrument_name_1>,
#'          algo = list(
#'            <entering_rule_function>,
#'            <exiting_rule_function>
#'          )
#'        ),
#'      ),
#'      signal_tables <- list(
#'        data.frame(),
#'        data.frame(),
#'        ...,
#'        data.frame()
#'      ),
#'      position_tables <- list(
#'        data.frame(),
#'        data.frame(),
#'        ...,
#'        data.frame()
#'      ),
#'      account_table <- data.frame()
#'    )
#'    ```
#' @export
#'
#' @examples
make_system <- function(
    algos = list(),
    init_capital = 100000,
    risk_target = 0.12,
    risk_window_length = 25,
    stop_loss_fraction = 0.5,
    min_periods = 1,
    min_signal = -2,
    max_signal = 2,
    instrument_data_folder_path,
    ...) {

    config <- list(
      init_capital = init_capital,
      risk_target = risk_target,
      risk_window_length = risk_window_length,
      stop_loss_fraction = stop_loss_fraction,
      min_periods = min_periods,
      min_signal = min_signal,
      max_signal = max_signal,
      min_cor = 0,
      max_sdm = 2.5,
      same_direction_trade_allowed = FALSE,
      instrument_data_folder_path = instrument_data_folder_path
    )
  ##////////////////////
  ## Initialize system
  ##////////////////////


  # ## One table for each algo (i.e each instrument + rule combination)
  # signal <- data.frame(
  #   time = data$time[1:min_periods],
  #   price = data$price[1:min_periods],
  #   raw_signal = rep(NA, min_periods),
  #   normalized_signal = rep(NA, min_periods),
  #   capped_signal = rep(NA, min_periods),
  #   signal_weight = rep(NA, min_periods)
  # )
  #
  # ## One table for each instrument
  # position_table <- data.frame(
  #   combined_signal = rep(0, min_periods),
  #   subsystem_position = rep(0, min_periods),
  #   portfolio_weighted_position = rep(0, min_periods),
  #   enter_or_exit = rep(NA, min_periods),
  #   trade_on = rep(FALSE, min_periods),
  #   direction = rep(0, min_periods),
  #   instrument_risk = rep(NA, min_periods),
  #   leverage_factor = rep(0, min_periods),
  #   inst_div_mult = rep(0, min_periods),leverage_factor
  #   notional_exposure = rep(0, min_periods),
  #   position_size_units = rep(0, min_periods),
  #   position_size_ccy = rep(0, min_periods),
  #   stop_loss_gap = rep(0,  min_periods),
  #   exit_trade_stop_loss = rep(0, min_periods),
  #   borrowed_asset = rep(0, min_periods),
  #   borrowed_cash = rep(0, min_periods),
  #   cash = rep(init_capital, min_periods), ## Uninvested money.
  #   pnl_of_position = rep(0, min_periods)
  # )
  #
  # ## Totals for entire system
  # system_account_table <- data.frame(
  #   pnl = rep(0, min_periods), #not implemented yet
  #
  #   account_value = rep(init_capital, min_periods),
  #   capital = rep(init_capital, min_periods),
  #
  #   total_borrowed_cash = rep(0, min_periods), #not implemented yet
  #
  #   ## Uninvested money
  #   cash = rep(init_capital, min_periods)  #not implemented yet
  # )



  ## A nested list of data sets and rules to be applied to each data set.
  ## This loads all the specified instrument data and rule functions into the
  ## parsed_algos list.
  parsed_algos <- parse_algos(algos)

  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0011
  # More flexible way of getting data from parsed_algos[[algo_ID]]$data
  # - what if we want other types of data instead?
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0022
  # Create tables in a `make_system_tables()` function?
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

  instrument_data_sets <- load_instrument_data_sets(
    expanded_algos = parsed_algos,
    instrument_data_folder_path = instrument_data_folder_path
  )

  ## Get number of signals
  num_signals <- length(parsed_algos)

  ## Build list of signals. To be filled with one signal for each algo.
  signals <- list()
  for(i in 1:num_signals) {
    ## One table for each algo (i.e each instrument + rule combination)
    signals[[i]] <- data.frame(
      time = parsed_algos[[i]]$data$time[1:min_periods],
      price = parsed_algos[[i]]$data$price[1:min_periods],
      raw_signal = rep(NA, min_periods),
      normalized_signal = rep(NA, min_periods),
      clamped_signal = rep(NA, min_periods),
      signal_weight = rep(NA, min_periods)
    )
  }

  ## Get number of instuments
  num_instruments <- get_num_inst_from_parsed_algos_list(parsed_algos)

  ## Build list of positions tables.
  position_tables <- list()
  for(i in 1:num_instruments) {
    ## One table for each instrument
    position_tables[[i]] <- data.frame(
      instrument_risk = rep(NA, min_periods),
      combined_signal = rep(0, min_periods),
      leverage_factor = rep(0, min_periods),
      inst_div_mult = rep(0, min_periods),
      notional_exposure = rep(0, min_periods),
      position_size_units = rep(0, min_periods),
      position_size_ccy = rep(0, min_periods),
      direction = rep(0, min_periods),
      stop_loss_gap = rep(0,  min_periods),
      exit_trade_stop_loss = rep(0, min_periods),
      subsystem_position = rep(0, min_periods),
      portfolio_weighted_position = rep(0, min_periods),
      enter_or_exit = rep("---", min_periods),
      trade_on = rep(FALSE, min_periods),
      subsystem_returns = rep(0, min_periods),
      borrowed_asset = rep(0, min_periods),
      borrowed_cash = rep(0, min_periods),

      ## Cash is uninvested money.
      # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
      # fix§0001
      # Delete cash
      # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
      cash = rep(init_capital, min_periods),

      pnl_of_position = rep(0, min_periods)
    )
  }

  ## Totals for entire system
  system_account_table <- data.frame(

    # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
    # fix§0023
    # Calculate total `pnl` and save in `system_account_table`.
    # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
    total_returns = rep(0, min_periods),
    pnl = rep(0, min_periods),

    account_value = rep(init_capital, min_periods),
    capital = rep(init_capital, min_periods),

    total_borrowed_cash = rep(0, min_periods), #not implemented yet

    ## Uninvested money
    cash = rep(init_capital, min_periods)  #not implemented yet
  )

  instrument_correlations <-
    matrix(rep(NA, num_instruments * num_instruments), nrow = num_instruments)

  list(
    instrument_data_sets = instrument_data_sets,
    algos = parsed_algos,
    signal_tables = signal_tables,
    position_tables = position_tables,
    system_account_table = system_account_table,
    # signal_cor_mat = signal_cor_mat,
    subsystem_ret_cor_mat = subsystem_ret_cor_mat,
    config = config
  )
}



# §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
# fix§0003:
# - update_system() should perform daily update of entire system:
#    - for each algo: update signal data frame with apply_rule().
#    - for each instrument/subsystem: update position_table with f_position()
# §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§



#update_system <- function(signal_table, position_table, algos, t,
# t_trade_enter) {
update_system <- function(
    system,
    t,
    ...) {

  ## Structure of input "system" list:
  ##   system <- list(
  ##       instrument_data_sets = instrument_data_sets,
  ##       algos = parsed_algos,
  ##       signal_tables,
  ##       position_tables,
  ##       system_account_table,
  ##       instrument_data_folder_path = instrument_data_folder_path
  ##   )

  ## Structure of input "algos" list, taken from parsed algos output from
  ## make_system():
  ##   algos <- list(
  ##     list( ## algo 1: algos[[1]]
  ##       instrument = "<inst_1_data_frame>",
  ##       rule = rule1
  ##     ),
  ##     list( ## algo 2: algos[[2]]
  ##       instrument = "<inst_name_2>",
  ##       rule = rule1
  ##     )
  ##     list( ## algo 3: algos[[3]]
  ##       instrument = "<inst_1_data_frame>",
  ##       data = <data_frame>,
  ##       rule = rule2
  ##     ),
  ##     list( ## algo 4: algos[[4]]
  ##       instrument = "<inst_2_data_frame>",
  ##       data = <data_frame>,
  ##       rule = rule2
  ##     )
  ##   )

  ## "signal" table columns:
  ## * time
  ## * price
  ## * raw_signal
  ## * normalized_signal
  ## * signal_weight

  cat("Updating system...\n")


  ## This kind of structure would only work if number of algos is same as
  ## number of instruments.
  # apply_rule() %>% ## depends on dplyr
  #   normalize_signal() %>%
  #   f_position() %>%
  #   execute_trade() %>%
  #   save_tables()

  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0021
  # For each row in the simulated data set, make a system update for each algo
  # (for each instrument + rule combination)
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0012
  # We need to get the data from algos list first
  # Note: The data is already loaded into the list by parse_algos()
  # Access the data directly from the system when needed.
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0024
  # In live mode, we need to load the new data into parsed_data.
  # In sim mode we itearate over the data that is already loaded into the list
  # by parse_algos()
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  ## Load instrument data
  # instrument_data_sets <- load_instrument_data_sets(
  #   expanded_algos = system$algos,
  #   instrument_data_folder_path = system$instrument_data_folder_path
  # )


  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0013
  # We need to get the tables (`signal_table`, `position_table`,
  # `system_account_table`).
  # Access the tables directly from the system when needed.
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§




  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0030
  ## Update data for each instrument
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0031
  # Make sure algos use the updated data
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0032
  # Update each signal_table with new data.
  # Time and price in each signal_table should link to the instrument data
  # object and reload when the instrument data has been updated.
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0033
  # Update each position_table with new data.
  # Time and price in each position_table should link to the instrument data
  # object and reload when the instrument data has been updated.
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§


  ## Apply rule.
  ## system$algos[[i]]$rule is a list containing one entering rule and one
  ## exiting rule.
  ## apply_rule() will pick the appropriate rule

  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0028
  # Benchmark against lapply()
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§



  ## Update new row in each signal_table
  for(i in seq_along(system$algos)) {
    system$signal_tables[[i]][t, ] <- update_signal_table_row(
      system$signal_tables[[i]],
      system$algos[[i]],
      t
    )
  }

  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0004
  # Sort out arguments to the rules.
  # - Use tables as inputs?
  # - No input? Define apply_rule() inside system()?
  # - Assign separate variables and assemble tables in the end?
  # - apply_rule() needs access to position_table and
  #   system_account_table (?)
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§



  #   # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  #   # fix§0025
  #   # - separate functions that get these values
  #   # - check validity
  #   # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  #   raw_signal <- apply_rule(
  #     system$algos[[i]],
  #     system$signals[[i]],
  #     system$position_tables[[system$positions ==
  #       system$algos[[i]]$instrument]]
  #   )
  #
  #   signal_weight <-
  #
  #
  #   normalized_signal <-
  #
  #   system$signals[[i]] <- list(
  #     price = price,
  #     raw_signal = raw_signal,
  #     signal_weight = signal_weight,
  #     normalized_signal = normalized_signal
  #   )
  # }




  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0006
  # "position" only needs to be updated for each instrument/subposition,
  # not for each algo.
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

  # *----
  ## Calculate instrument correlations.
  ## No need to do this every time we run update_position_table_row.
  #price_vectors <- data.frame(
  #  sapply(system$instrument_data_sets, function(x) list(price = x$price))
  #)
  #instrument_list <- sapply(system$instrument_data_sets, function(x) comment(x))
  #names(price_vectors) <- instrument_list

  # TODO
  instrument_weights <- NA

  ## Get all past subsystem from the system as vectors in a data frame
  min_periods <- system$config$min_periods
  subsystem_returns_vectors <- data.frame(
    sapply(
      system$position_tables,
      function(x) list(returns = x$subsystem_returns[(min_periods + 1):(t - 1)])
    )
  )

  ## Rename the columns in the subsystem returns data frame with the
  ## corresponding instrument names.
  instrument_list <- sapply(system$instrument_data_sets, function(x) comment(x))
  names(subsystem_returns_vectors) <- instrument_list

  ## Correlations of returns
  subsystem_ret_cor_mat <-
    f_subsystem_ret_cor_mat(subsystem_returns_vectors)

  for(i in seq_along(system$position_tables)) {
    system$position_tables[[i]][t, ] <- update_position_table_row(
      system$signal_tables[[i]], ## signal_table
      system$position_tables[[i]], ## position_table
      # signal_cor_mat,
      subsystem_ret_cor_mat,
      system$system_account_table,
      t,
      config = system$config
    )
  }



  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0007
  # Update "accounts" table
  # Doesn't need update for each algo, only once for each system update.
  # Do this in combine_signals()
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§




  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0008
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  execute_trade()

  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0009
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  save_tables()

  ## Return updated system
  list(
    instrument_data_sets = instrument_data_sets,
    algos = parsed_algos,
    signal_tables = signal_tables,
    position_tables = position_tables,
    system_account_table = system_account_table,
    # signal_cor_mat = signal_cor_mat,
    subsystem_ret_cor_mat = subsystem_ret_cor_mat,
    config = config
  )
}



#' Apply Entering Or CExiting Rule To Data
#'
#' @description
#' Apply a single rule to a single instrument.
#'
#' The `algo` list contains a rule function and data for a single instrument.
#'
#' @param algo
#' @param signal_table
#' @param position_table
#' @param t
#'
#' @return
#' @export
#'
#' @examples
apply_rule <- function(
    algo, ## Single algo from the algos list
    signal_table, ## signal table for the instrument
    position_table, ## position table for the instrument
    t) { ## Time at which to apply the rule

  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0014:
  # Get the needed variables from the system
  # Pass the calculated variables back to system
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

# *----
  if(trade_on == FALSE) {
    #if(latest_trade_direction == direction) {direction = 0}

    # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
    # fix§0015
    # Make sure that rule is applied correctly to data here
    # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
    raw_signal <- algo$rule[[1]](algo$data) ## entering rule
    # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
    # fix§0027:
    # Move the rest of this if condition to update_position_table_row() after
    # `combine_signals() and update_account(), as appropriate.
    # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

    if(direction != 0) {
      update_position_table_row(
        # price,
        # direction,
        # latest_trade_direction,
        # system_account_table,
        # risk_target,
        # instrument_risk,
        # position_size_ccy,
        # t
      )
    } else { ## direction == 0: no position change (don't enter trade)
      ## We don't have any trade on, and nothing changes
      ## Still 0
      position_size_units <- position_table$position_size_units[t - 1]
      ## Still 0
      position_size_ccy <- position_table$position_size_ccy[t - 1]

      ## How much did we borrow when we entered the trade?
      borrowed_cash <- system_account_table$borrowed_cash[t_trade_enter]
      borrowed_asset <- system_account_table$borrowed_asset[t_trade_enter]

      cash <- system_account_table$cash[t - 1]
      capital <- system_account_table$capital[t - 1]


      # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
      # fix§0016
      # Calculate `cash` for entire system and move `cash` from
      # `position_tables` to `system_account_table`.
      # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
      account_value <- capital
    }
  } else { ## If trade is on, check stop loss
    ## Starter System: stop_loss_gap should be fixed for duration of trade
    ## [LT p. 138]
    ## Instead I use a new calculation of price volatility (instrument risk)
    ## each day, which I already calculated above.

    #inst_risk_at_entry <- trade_data$instrument_risk[t_trade_enter]
    #price_at_entry <- trade_data$price[t_trade_enter]
    #price_vol_at_entry <- price_unit_vol(inst_risk_at_entry, price_at_entry)

    ## Exit position if stop loss level was breached yesterday [LT, p. 138].
    ## Exit position even if price has recovered. [LT, p. 141]
    # exit_trade_stop_loss_ <- f_exit_trade_stop_loss(
    #   prices = signal_table$price,
    #   t = t - 1,
    #   t_trade_enter = t_trade_enter,
    #   stop_loss_gap = stop_loss_gap_,
    #   direction = direction,
    #   rnd = FALSE
    # )

    ## NOTE:
    ## position_size_units will never be negative, as long as capital is not
    ## allowed to reach 0 or below.
    ## The break below takes care of that.

    ## While long trade is on:
    ## cash[t] = cash[t - 1]
    ##
    #### borrowed_cash[t] = borrowed_cash[t_trade_enter]
    #### borrowed_asset[t] = 0
    #### capital[t] = position_size_ccy[t] - borrowed_cash[t_trade_enter] + cash

    ## While short trade is on:
    ## cash[t] = cash[t - 1]
    ## borrowed_cash[t] = 0
    ## borrowed_asset[t] = position_size_ccy[t]
    ## capital[t] = cash[t_trade_enter - 1] + (position_size_ccy[t] - position_size_ccy[t_trade_enter])

    ## When long trade has just been exited:
    ## cash[t] = cash[t - 1] + position_size_ccy[t] - borrowed_cash[t_trade_enter]
    ## borrowed_cash[t] = 0
    ## borrowed_asset[t] = 0
    ## capital[t] = cash[t]

    ## When short trade has just been exited:
    ## cash[t] = cash[t - 1] - position_size_ccy[t]
    ## borrowed_cash[t] = 0
    ## borrowed_asset[t] = 0
    ## capital[t] = cash[t]

    position_size_units <- position_table$position_size_units[t - 1]
    position_size_ccy <- position_size_units * price

    ## 0 if short
    borrowed_cash =
      system_account_table$borrowed_cash[t_trade_enter] * (direction > 0) * trade_on

    ## 0 if short
    borrowed_asset = position_size_ccy * (1 - (direction > 0)) * trade_on

    ## Exiting rule
    raw_signal <- algo$rule[[2]](algo$data)

    if(direction == 1) { ## If long
      cash <- system_account_table$cash[t - 1]
      capital <-
        cash + position_size_ccy - system_account_table$borrowed_cash[t_trade_enter]
      if(enter_or_exit == "EXIT") {
        trade_on <- FALSE ## Exit trade
        direction <- 0
        cash <- system_account_table$cash[t - 1] + position_size_ccy -
          system_account_table$borrowed_cash[t_trade_enter]
        capital <- cash
      }
    } else if(direction == -1) { ## If short
      cash <- system_account_table$cash[t - 1]
      capital <- system_account_table$cash[t_trade_enter - 1] +
        (position_table$position_size_ccy[t_trade_enter] - position_size_ccy)
      if(enter_or_exit <- "EXIT") {
        trade_on <- FALSE ## Exit trade
        direction <- 0
        cash <- system_account_table$cash[t - 1] - position_size_ccy
        capital <- cash
      }
    } else { ## Should be redundant, since we know a trade is on, and a trade
             ## will either be long or short...
      cash <- system_account_table$cash[t - 1]
      capital <- system_account_table$capital[t - 1]
    }
  }
  list(
    time = time,
    price = price,
    raw_signal = raw_signal,
    normalized_signal,
    signal_weight = signal_weight
  )
}



update_signal_table_row <- function(algo, signal_table, t) {

  #TODO
  #time_t <- update_time(t)

  #TODO
  #price_t <- update_price(t)

  raw_signal <- apply_rule(
    system$algos[[i]],
    system$signals[[i]],
    t
  )

  risk_adjusted_signal <- f_risk_adjusted_signal(
    raw_signal,
    price_t,
    inst_risk
  )

  normalized_signal <- normalize_signal(risk_adjusted_signal)

  clamped_signal <- clamp_signal(normalized_signal, min_signal, max_signal)

  ## Equal signal weights
  signal_weight <- calculate_equal_signal_weight(system$algos)

  c(time,
    price,
    raw_signal,
    normalized_signal,
    clamped_signal,
    signal_weight
  )
}



#' Update Position Table
#'
#' @description
#'
#' Based on signal:
#'   * If a trade is not on: Determine whether to enter a trade, and if so
#'     in which direction.
#'   * If a trade is on: Determine whether to exit the trade or to switch
#'     direction.
#'   * It is not allowed to enter a trade in the same direction as the
#'     previous trade.
#'
#' Update the position table accordingly.
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
update_position_table_row <- function(
  signal_table,
  position_table,
  # signal_cor_mat,
  subsystem_ret_cor_mat,
  system_account_table,
  t,
  config
) {

  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0026
  # This should not apply in general:
  ## It is not allowed to enter a trade in the same direction as the
  ## previous trade.
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  #*----
  raw_combined_signal <- combine_signals(
    signal_tables,
    algos,
    min_cor = config$min_cor,
    max_sdm = config$max_sdm,
    # signal_cor_mat,
    t
  )

  ## Calculate trade direction from combined signal at time t
  direction <- sign(raw_combined_signal)


  if(same_direction_trade_allowed) {
    ## set to something other than -1, 0 or 1 so that
    ## (last_direction != direction) will always return TRUE
    last_direction <- 2
  } else {latest_trade_direction <- position_table$direction[t - 1]}

  if(latest_trade_direction != direction) {

    instrument_risk <- f_inst_risk(
      #c(position_table$price[1:(t - 1)], price),
      position_table$price,
      t = t,
      window_length = risk_window_length
    )

    idm <- f_inst_div_mult(subsystem_ret_cor_mat, instrument_weights)
    instrument_risk_target <- f_instrument_risk_target(system_risk_target, idm)

    # ST, p. 133
    rescaled_combined_signal <- raw_combined_signal * idm

    clamped_combined_signal <- clamp_signal(
      rescaled_combined_signal,
      min_signal = min_signal,
      max_signal = max_signal
    )

    #
    required_leverage_factor <- f_required_leverage_factor(
      instrument_risk_target,
      instrument_risk
    )

    subsystem_position <- calculate_subsystem_position(
      clamped_combined_signal,
      required_leverage_factor
    )

    required_leverage_factor <- f_required_leverage_factor(
      instrument_risk_target,
      instrument_risk
    )

    ## Notional exposure in account currency
    notional_exposure <- f_notional_exposure(
      combined_signal,
      system_account_table$capital[t - 1],
      required_leverage_factor
    )

    ## position_size_units
    # position_size_units <- f_position_size_units(
    #   price,
    #   risk_target,
    #   system_account_table$capital[t - 1],
    #   instrument_risk
    # )
    target_position_size_units <- f_target_position_size_units(
      notional_exposure,
      price #,
      # TODO
      # Uncomment when implementing fx rates:
      #fx_rate # default 1
    )

    ## Actual traded position in rounded number of contracts
    # TODO
    position_size_units <-
      calculate_position_units(target_position_size_units)

    ## Actual position size in units account currency
    position_size_ccy <- position_size_ccy(
      price,
      position_size_units
    )


    # stop_loss_gap,
    # exit_trade_stop_loss,

    ## Starter System: No position adjustment.
    #prev_position_size_units <- trades_data$position_size_units[t - 1]
    #prev_position_size_ccy <- trades_data$position_size_ccy[t - 1]

    #trade_amount_units <- (position_size_units - prev_position_size_units) * direction
    #trade_amount_ccy <- (position_size_ccy - prev_position_size_ccy) * direction


    # TODO:
    # Based on signal:
    #   * If a trade is not on: Determine whether to enter a trade, and if so
    #     in which direction.
    #   * If a trade is on: Determine whether to exit the trade or to switch
    #     direction.
    #   * It is not allowed to enter a trade in the same direction as the
    #     previous trade.
    trade_on <- TRUE
    enter_or_exit <- "ENTER"
    t_trade_enter <- t

    ## Simple System:
    ## When entering a trade, amount of cash and capital should always be
    ## the same, as we only trade one instrument, and always exit the trade
    ## before entering a new.
    ## Only list amount borrowed.
    ## Don't list negative amount when leverage is <1,
    ## ie. position_size_ccy[t] < cash[t - 1].

    ## When long trade has just been entered:
    ## cash[t] = cash[t - 1] - position_size_ccy[t] + borrowed_cash[t]
    ##
    #### borrowed_cash[t] = max[0, position_size_ccy[t] - cash[t - 1]]
    #### borrowed_asset[t] = 0
    #### capital[t] = cash[t] + position_size_ccy[t] - borrowed_cash[t]

    ## When short trade has just been entered:
    ## cash[t] = cash[t - 1] + borrowed_asset[t]
    ## borrowed_cash[t] = 0
    ## borrowed_asset[t] = position_size_ccy[t]
    ## capital[t] = cash[t - 1]


    # TODO
    instrument_returns <- NA

    # borrowed_cash
    borrowed_cash <- max(0, position_size_ccy - system_account_table$cash[t - 1])

    # borrowed_asset,
    borrowed_asset <- position_size_ccy * (direction < 0) ## 0 if long

    cash <- system_account_table$cash[t - 1] - ((direction > 0) * position_size_ccy)
      + borrowed_cash + borrowed_asset
    if(direction > 0) {
      capital <- cash + position_size_ccy - borrowed_cash
    } else {
      capital <- system_account_table$cash[t - 1]
    }

    # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
    # fix§0017
    # Calculate `cash` for entire system and move `cash` from `position_tables`
    # to `system_account_table`.
    # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
    account_value <- capital
  } else { ## latest_trade_direction == direction: no change (don't enter
    ## trade)
    position_size_units <- position_table$position_size_units[t - 1] ## Still 0
    position_size_ccy <- position_table$position_size_ccy[t - 1] ## Still 0

    borrowed_cash <- 0 ## No trade is on
    borrowed_asset <- 0

    ## While no trade on:
    ## capital[t] = cash[t]

    cash <- system_account_table$cash[t - 1]
    capital <- system_account_table$capital[t - 1]

    # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
    # fix§0018
    # Calculate `cash` for entire system and move `cash` from `position_tables`
    # to `system_account_table`.
    # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
    account_value <- capital
  }

  ## Return updated row for time t
  c(
    f_instrument_risk,
    combined_signal,
    leverage_factor,
    inst_div_mult,
    notional_exposure,
    position_size_units,
    position_size_ccy,
    direction,
    stop_loss_gap,
    exit_trade_stop_loss,
    subsystem_position,
    portfolio_weighted_position,
    enter_or_exit,
    trade_on,
    subsystem_returns,
    borrowed_asset,
    borrowed_cash,
    cash,
    pnl_of_position
  )
}






## Load data into system$instrument_data_sets list.
#' Load Instrument Data Sets
#'
#' @description
#' Takes character string instrument names from each algo in a system created by
#' `make_system()` and loads the corresponding csv file into a list.
#'
#' @param expanded_algos
#' @param instrument_data_folder_path
#'
#' @return List of data frames
#' @export
#'
#' @examples
load_instrument_data_sets <- function(
    expanded_algos,
    instrument_data_folder_path
) {
  unique_instrument_paths <-
    get_unique_inst_paths_from_expanded_algos_list(
      expanded_algos,
      instrument_data_folder_path
    )
  unique_instrument_names <-
    get_unique_inst_names_from_parsed_algos_list(
      expanded_algos
    )
  unique_instrument_datasets <- list()
  for(i in seq_along(unique_instrument_paths)) {
    unique_instrument_datasets[[i]] <- data.frame(
      read.csv(unique_instrument_paths[[i]])
    )
    ## Assign name as comment to data frame.
    comment(unique_instrument_datasets[[i]]) <-  unique_instrument_names[[i]]
  }
  unique_instrument_datasets
}

#' Run System
#'
#' @param system List containing a full system.
#' @param mode "live" for live trading or "sim" for simulation mode.
#'
#' @return List containing full updated system.
#' @export
#'
#' @examples
run_system <- function(
    system,
    min_periods = 1,
    mode = "sim") {

  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0020
  # Should be able to handle data frames with different numbers of rows.
  #
  ## Get number of rows in instrument data sets.
  ## For now the system expects all data frames for all instruments to have
  ## same number of rows.
  ## We get the number of rows from the data of the first algo (temporary
  ## solution).
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  num_rows <- nrow(system$instrument_data_sets[[1]])
  if(!(num_rows > min_periods)){
    stop(paste(
      "At least one data set must contain at least", min_periods + 1, "rows."
    ))
  }
  if(mode == "live") {
    stop("\"Live mode is not implemented yet.\"")
  } else if(mode == "sim") {
    ## Load instrument data
    system$instrument_data_sets <- load_instrument_data_sets(
      expanded_algos = system$algos,
      instrument_data_folder_path = "~/git/rsystrade/misc/temp/data/"
    )
    for(t in (min_periods + 1):num_rows) {
      system <- update_system(system, t)
    }
  } else {
    stop(
      paste("\nProvide a valid mode when running make_system().",
            "\nValid modes are \"sim\" (default) or \"live\".")
    )
  }
}



#' Calculate Equal Signal Weight
#'
#' @param algos
#'
#' @return
#' @export
#'
#' @examples
calculate_equal_signal_weight <- function(algos) {
  num_algos <- length(algos)
  if(!is.na(num_algos) && num_algos >= 1) {
    1/num_algos
  } else {
    stop("At least one algo must be provided to the system.")
  }
}





#' Combine Signals For Instrument Subsystem
#'
#' @description
#' For each instrument subsystem calculates a weighted sum of all signals in
#'   that subsystem.
#'
#' A subsystem is one or more rules applied to one instrument. A signal is the
#'   output from a rule.
#'
#' `combine_signals()` returns a single combined signal for each instrument.
#'   Now the subsystem consists of exactly one rule applied to one instrument.
#'
#' @param signal_tables
#' @param t
#'
#' @return Single number
#' @export
#'
#' @examples
combine_signals <- function(
    signal_tables,
    algos,
    min_cor,
    max_sdm,
    t) {

  inst_names_by_algo <- get_inst_names_by_parsed_algo(algos)
  unique_inst_names <-
    get_unique_inst_names_from_parsed_algos_list(algos)

  n <- length(unique_inst_names)
  combined_signals <- numeric(n)
  signal_cor_mats <- list()

  for(i in 1:n) {
    ## A subsystem is all the algos for an individual instrument
    subsystem_IDs <- which(
      inst_names_by_algo[inst_names_by_algo == unique_inst_names[[i]]]
    )

    ## Collect all clamped signals in subsystem i
    signals <- sapply(
      signal_tables[subsystem_IDs], function(x) x$clamped_signal[t]
    )
    ## Collect all signal weights in subsystem i
    signal_weights <- sapply(
      signal_tables[subsystem_IDs], function(x) x$signal_weight[t]
    )


    #Use matrix instead of df
    # signal_vectors <- data.frame(
    #   sapply(signal_tables, function(x) list(sig = x$clamped_signal))
    # )

    ## Make vector of signal vectors
    signal_vectors <- sapply(signal_tables, function(x) {x$clamped_signal})

    ## Calculate signal correlations.
    ## No need to do this every time we run update_position_table_row.
    signal_cor_mats[i] <- f_signal_cor_mat(signal_vectors)

    #TODO:
    #Collect sdm for positions tables
    ## Signal Diversification Multiplier
    sdm <- f_sig_div_mult(
      signal_cor_mats[i],
      signal_weights,
      min_cor,
      max_sdm
      )

    combined_signals[i] <- signals %*% signal_weights * sdm
  }
  combined_signals
}



#' Calculate Subsystem Position
#'
#' @param combined_signal Typically normalized and clamped
#' @param sdm Signal Diversification Multiplier
#'
#' @return Number of contracts
#' @export
#'
#' @examples
calculate_subsystem_position <- function(
    combined_signal,
    required_leverage_factor) {
  combined_signal * required_leverage_factor
}


## Final position to trade in units of contracts
calculate_position_units <- function(
    target_position_size_units
    ) {
  warning("calculate_instrument_position_units() is not yet implemented.")
  floor(target_position_size_units)
}


#' Execute Trade
#'
#' @return
#' @export
#'
#' @examples
execute_trade <- function() {
  warning("execute_trade() is not yet implemented.")
}

#' Save Tables
#'
#' @return
#' @export
#'
#' @examples
save_tables <- function() {
  warning("save_tables() is not yet implemented.")
}




# apply_rule <- function() {
#
#   ## We need to skip AT LEAST two days because:
#   ## sd needs two data points as input.
#   ## To get two returns, we need three prices.
#   ##
#   ## However, we should definitely make sure we have enough data for a full
#   ## ma_slow window and instrument_risk.
#   #min_periods <- max(n_slow, risk_window_length)
#
#
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
#   # Could we do something like:
#   #  ma_fast_slow <- calculate_moving_average(prices, c(n_fast, n_slow))
#   ma_fast <- alculate_moving_average(
#     prices,
#     n_fast
#   )
#   ma_slow <- alculate_moving_average(
#     prices,
#     n_slow
#   )
#
#   ## mac == 1 indicates long, mac == -1 indicates short
#   ## mac == 0 indicates on signal.
#
#   #Could we do something like
#   # moving_average_crossover <- calculate_mac(ma_fast_slow[1], ma_fast_slow[2])
#
#
#   mac_signal <- mac_rule(ma_fast, ma_slow, gap = gap)
#
#   # cat("ma_fast =", ma_fast, "\n")
#   # cat("ma_slow =", ma_slow, "\n")
#   # cat("mac =", mac, "\n")
#   # cat("latest_trade_direction =", latest_trade_direction, "\n")
#   # cat("instrument_risk =", instrument_risk_, "\n")
#   # cat("notional_exposure =", notional_exposure, "\n")
#
#   #direction <- enter_trade_mac(
#   #  moving_average_crossover = mac,
#   #  latest_trade_direction = latest_trade_direction
#   #) ## 0 if no change
#   #direction <- mac
#   #cat("direction =", direction, "\n")
#
#   rule_output[t, ] <- rule()
#
# }








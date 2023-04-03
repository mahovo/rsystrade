
## Trade system framework

# §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
# fix§0029
# Write help explaining structure of input algos. (See help for `parse_algos`)
# §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
#' Make Trade System
#'
#' @param algos A list of algos.
#' @param init_capital Initial capital.
#' @param system_risk_target System risk target as decimal fraction (percent
#'   divided by 100).
#' @param risk_window_length Risk window length.
#' @param stop_loss_fraction Stop loss fraction.
#' @param min_periods Minimum number of periods (rows) in the data sets.
#'   Defaults to 1.
#' @param min_signal Minimum signal value.
#' @param max_signal Maximum signal value.
#' @param instrument_data_folder_path _Instrument data sets folder_ path. Path
#'   of folder containing all the instrument csv-files for the trade system. The
#'   _instrument data sets folder_ can not be nested.
#' @param ... Additional arguments.
#'
#' @return A list containing
#'    * a list of instrument data sets as data frames. The instrument name is
#'      assigned to each data frame as a comment. So we can get the data frame
#'      name:
#'      ```R
#'      comment(inst_data[[1]])
#'      [1] "instrument1"
#'      ```
#'    * a list of parsed algos, which in turn contains an instrument name and a
#'      list of two trade rule functions: One trade signal generating rule and
#'      one optional stop loss rule.
#'      If present, the exit rule is applied in the positions stage.
#'    * a list `signal_tables`, each containing a data frame.
#'    * a list `position_table`, each containing a data frame.
#'    * a dataframe `system_account_table`.
#'    * a list `config` containing system configuration.
#'    ```
#'    trade_system <- list(
#'      inst_data <- list(
#'        data.frame(),
#'        data.frame(),
#'        ...,
#'        data.frame()
#'      )
#'      algos <- list(
#'        list(
#'          instrument = <instrument_name>,
#'          algo = list(
#'            <signal_rule_function>,
#'            <stop_loss_rule_function>
#'          )
#'        ),
#'        list(
#'          instrument = <instrument_name>,
#'          algo = list(
#'            <signal_rule_function>,
#'            <stop_loss_rule_function>
#'          )
#'        ),
#'        ...,
#'        list(
#'          instrument = <instrument_name>,
#'          algo = list(
#'            <signal_rule_function>,
#'            <stop_loss_rule_function>
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
    system_risk_target = 0.12,
    risk_window_length = 25,
    stop_loss_fraction = 0.5,
    min_periods = 1,
    min_signal = -2,
    max_signal = 2,
    instrument_data_folder_path,
    ...) {

    config <- list(
      init_capital = init_capital,
      system_risk_target = system_risk_target,
      risk_window_length = risk_window_length,
      stop_loss_fraction = stop_loss_fraction,
      min_periods = min_periods,
      min_signal = min_signal,
      max_signal = max_signal,
      min_cor = 0,
      max_sdm = 2.5,
      same_direction_trade_allowed = FALSE,
      instrument_data_folder_path = instrument_data_folder_path,
      signal_weight_calculation_method = "equal",
      inst_weight_calculation_method = "equal",
      normalization_target = 1,
      signal_normalization_factors_method = "equal"
    )
  ##////////////////////
  ## Initialize system
  ##////////////////////

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


  inst_data <- load_instrument_data_sets(
    parsed_algos = parsed_algos,
    instrument_data_folder_path = instrument_data_folder_path
  )
  names(inst_data) <- unlist(get_unique_inst_names_from_parsed_algos_list(parsed_algos))

  ## The way to access a data set:
  ## inst_data[[parsed_algos[[i]]$instrument]]
  ## For instance, get prices:
  ## inst_data[[parsed_algos[[i]]$instrument]]$price

  rule_functions <-load_rule_functions(parsed_algos)
  names(rule_functions) <- get_unique_rule_function_names_by_parsed_algo(parsed_algos)

  ## The way to access a specific rule function:
  ## rule_functions[[ parsed_algos[[i]]$rule[[1]] ]] # Signal rule
  ## rule_functions[[ parsed_algos[[i]]$rule[[2]] ]] # Stop loss rule

  ## Get number of signals
  num_signals <- length(parsed_algos)

  ## Build list of signal tables. To be filled with one signal table for each
  ## algo.
  signal_tables <- list()

  for(i in 1:num_signals) {
    ## One table for each algo (i.e each instrument + rule combination)
    signal_tables[[i]] <- data.frame(
      time = inst_data[[ parsed_algos[[i]]$instrument ]]$time[1:min_periods],
      price = inst_data[[ parsed_algos[[i]]$instrument ]]$price[1:min_periods],
      ## Fill with small random values to avoid zero-sd
      raw_signal = rnorm(min_periods, 0, 0.001), #nnumeric(min_periods), #rep(NA, min_periods),
      normalized_signal = rnorm(min_periods, 0, 0.001), #nnumeric(min_periods), #rep(NA, min_periods),
      clamped_signal = rnorm(min_periods, 0, 0.001), #nnumeric(min_periods), #rep(NA, min_periods),
      signal_weight = numeric(min_periods) #rep(NA, min_periods)
    )
  }

  ## Get number of instuments
  num_instruments <- get_num_inst_from_parsed_algos_list(parsed_algos)

  ## Build list of positions tables.
  position_tables <- list()
  for(i in 1:num_instruments) {
    ## One table for each instrument
    position_tables[[i]] <- data.frame(
      time = signal_tables[[i]]$time,
      price = signal_tables[[i]]$price,
      instrument_risk = rep(NA, min_periods),
      instrument_risk_target = rep(NA, min_periods),
      raw_combined_signal = numeric(min_periods),
      rescaled_combined_signal = numeric(min_periods),
      clamped_raw_combined_signal = numeric(min_periods),
      required_leverage_factor = numeric(min_periods),
      signal_div_mult = numeric(min_periods),
      instrument_weight = numeric(min_periods),
      inst_div_mult = numeric(min_periods),
      notional_exposure = numeric(min_periods),
      target_position_size_units = numeric(min_periods),
      position_size_units = numeric(min_periods),
      position_size_ccy = numeric(min_periods),
      direction = numeric(min_periods),
      stop_loss = numeric(min_periods),
      stop_loss_gap = numeric(min_periods),
      subsystem_position = numeric(min_periods),
      enter_or_exit = rep("---", min_periods),
      t_last_position_entry = rep(0, min_periods),
      trade_on = rep(FALSE, min_periods),
      ## Fill with small random values to avoid zero-sd
      instrument_returns = rnorm(min_periods, 0, 0.001),
      subsystem_pandl = rnorm(min_periods, 0, 0.001), #numeric(min_periods),
      borrowed_asset_ccy = numeric(min_periods),
      position_change_ccy = numeric(min_periods)
    )
  }

  ## Totals for entire system
  system_account_table <- data.frame(

    # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
    # fix§0023
    # Calculate total `account_pandl` and save in `system_account_table`.
    # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
    total_positions_value = numeric(min_periods),
    total_returns = numeric(min_periods),
    account_pandl = numeric(min_periods),
    cash = rep(init_capital, min_periods),
    account_value = rep(init_capital, min_periods),
    capital = rep(init_capital, min_periods)
  )

  subsystem_ret_cor_mat <-
    matrix(rep(NA, num_instruments * num_instruments), nrow = num_instruments)

  ## Calculate normalization factors for the system
  number_of_unique_rules <- get_num_rules_from_parsed_algos_list(parsed_algos)
  signal_normalization_factors <- update_signal_normalization_factors(
    parsed_algos,
    signal_tables,
    instrument_data_sets,
    target = 1,
    method = config$signal_normalization_factors_method,
    equal_norm_factor = 1
  )
  names(signal_normalization_factors) <- unlist(get_unique_rule_names_from_parsed_algos_list(parsed_algos))

  list(
    inst_data = inst_data,
    rule_functions = rule_functions,
    subsystem_ret_cor_mat = subsystem_ret_cor_mat,
    algos = parsed_algos,
    signal_normalization_factors = signal_normalization_factors,
    signal_tables = signal_tables,
    position_tables = position_tables,
    system_account_table = system_account_table,
    # signal_cor_mat = signal_cor_mat,
    config = config
  )
}


# §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
# fix§0003:
# - update_system() should perform daily update of entire system:
#    - for each algo: update signal data frame with generate_signal().
#    - for each instrument/subsystem: update position_table with f_position()
# §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

#' Update System
#'
#' Config options
#'   `signal_weight_calculation_method`
#'   * `"equal"` (default)
#'   * `"estimate"`
#'
#' @param system Trade system list containing a complete system.
#' @param t Time index.
#' @param ... Additional arguments.
#'
#' @return
#' @export
#'
#' @examples
update_system <- function(
    trade_system,
    t,
    ...) {

  ## Structure of input "trade_system" list:
  ##   trade_system <- list(
  ##        inst_data = inst_data,
  ##        instrument_correlations = instrument_correlations,
  ##        algos = parsed_algos,
  ##        signal_normalization_factors = signal_normalization_factors,
  ##        signal_tables = signal_tables,
  ##        position_tables = position_tables,
  ##        system_account_table = system_account_table,
  ##        # signal_cor_mat = signal_cor_mat,
  ##        subsystem_ret_cor_mat = subsystem_ret_cor_mat,
  ##        config = config
  ##   )

  ## Structure of input "algos" list, taken from parsed algos output from
  ## make_system():
  ##   algos <- list(
  ##     list( ## algo 1: algos[[1]]
  ##       instrument = "<inst_1_name>",
  ##       data = <data_frame>,
  ##       rule = rule1
  ##     ),
  ##     list( ## algo 2: algos[[2]]
  ##       instrument = "<inst_2_name>",
  ##       data = <data_frame>,
  ##       rule = rule1
  ##     )
  ##     list( ## algo 3: algos[[3]]
  ##       instrument = "<inst_1_name>",
  ##       data = <data_frame>,
  ##       rule = rule2
  ##     ),
  ##     list( ## algo 4: algos[[4]]
  ##       instrument = "<inst_2_name>",
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

  cat("Updating trade system at t =", t,"...\n")

  ## This kind of structure would only work if number of algos is same as
  ## number of instruments:
  # generate_signal() %>% ## depends on dplyr
  #   normalize_signal() %>%
  #   f_position() %>%
  #   execute_trade() %>%
  #   save_tables()

  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0021
  # For each row in the simulated data set, make a trade system update
  # for each algo (for each instrument + rule combination)
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0012
  # We need to get the data from algos list first
  # Note: The data is already loaded into the list by parse_algos()
  # Access the data directly from the trade system when needed.
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0024
  # In live mode, we need to load the new data into parsed_algos.
  # In sim mode we itearate over the data that is already loaded into the list
  # by parse_algos()
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  ## Load instrument data
  # inst_data <- load_inst_data(
  #   expanded_algos = trade_system$algos,
  #   instrument_data_folder_path = trade_system$instrument_data_folder_path
  # )


  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0013
  # We need to get the tables (`signal_table`, `position_table`,
  # `system_account_table`).
  # Access the tables directly from the trade system when needed.
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
  ## trade_system$algos[[i]]$rule is a list containing one signal rule and one
  ## optional stop loss rule.
  ## generate_signal() will pick the appropriate rule

  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0028
  # Benchmark against lapply()
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

  ## Gets vector of weights, one weight for each algo.
  signal_weights_all_algos <- calculate_signal_weights(
    trade_system$algos,
    trade_system$config$signal_weight_calculation_method
  )

  ## List of signal normalization factors in the order of the parsed algos
  ## list
  sig_norm_fact_by_algos <- get_signal_normalization_factors_by_algos(
    trade_system$signal_normalization_factors,
    parsed_algos
  )

  ## Update new row in each signal_table
  signal_tables <- trade_system$signal_tables
  for(i in seq_along(trade_system$algos)) {
    signal_tables[[i]][t, ] <- update_signal_table_row(
      trade_system$inst_data[ trade_system$algos[[i]]$instrument ],
      trade_system$algos[[i]],

      #TODO
      #change rule[[1]] when rule changed to only ever include 1 element
      trade_system$rule_functions[ trade_system$algos[[i]]$rule[[1]] ],
      trade_system$signal_tables[[i]],
      signal_weights_all_algos[[i]],
      sig_norm_fact_by_algos[[i]],
      t,
      trade_system$config
    )
  }

  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0004
  # Sort out arguments to the rules.
  # - Use tables as inputs?
  # - No input? Define generate_signal() inside make_system()?
  # - Assign separate variables and assemble tables in the end?
  # - generate_signal() needs access to position_table and
  #   system_account_table (?)
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§


  #price_vectors <- data.frame(
  #  sapply(trade_system$inst_data, function(x) list(price = x$price))
  #)
  #instrument_list <- sapply(trade_system$inst_data, function(x) comment(x))
  #names(price_vectors) <- instrument_list



  ## Get all past subsystem returns from the tradesystem as vectors in a data
  ## frame.
  min_periods <- trade_system$config$min_periods
  subsystem_pandl_vectors <- data.frame(
    sapply(
      trade_system$position_tables,
      #function(x) list(returns = x$subsystem_pandl[(min_periods + 1):(t - 1)])
      function(x) list(returns = x$subsystem_pandl[1:(t - 1)])
    )
  )

  ## Rename the columns in the subsystem returns data frame with the
  ## corresponding instrument names.
  instrument_list <- sapply(trade_system$inst_data, function(x) comment(x))
  names(subsystem_pandl_vectors) <- instrument_list

  ## Correlations of subsystem returns.
  subsystem_ret_cor_win_len <- trade_system$config$subsystem_ret_cor_win_len
  subsystem_ret_cor_mat <-
    f_subsystem_ret_cor_mat(subsystem_pandl_vectors)

  ## Vector of weights
  instrument_weights <- calculate_equal_inst_weights(trade_system$algos)

  combined_signals_and_sdm <- combine_signals(
    signal_tables,
    trade_system$algos,
    min_cor = trade_system$config$min_cor,
    max_sdm = trade_system$config$max_sdm,
    # signal_cor_mat,
    t
  )

  raw_combined_signals <- combined_signals_and_sdm$combined_signals

  ## Vector of SDM values
  signal_div_mult_vect <- combined_signals_and_sdm$signal_div_mult_vect

  inst_div_mult <- f_inst_div_mult(
    subsystem_ret_cor_mat,
    instrument_weights
  )

  instrument_risk_target <- f_instrument_risk_target(
    trade_system$config$system_risk_target,
    inst_div_mult
  )

  position_tables <- trade_system$position_tables

  for(i in seq_along(position_tables)) {
    position_tables[[i]][t, ] <- update_position_table_row(
      trade_system$inst_data[[i]],
      #trade_system$algos[[i]], ## algo
      trade_system$rule_functions,
      #signal_tables[[i]], ## Current signal_table (not yet in system)
      trade_system$position_tables[[i]], ## position_table
      raw_combined_signals[[i]],
      signal_div_mult_vect[i], ## SDM value
      instrument_weights[i],
      inst_div_mult,
      instrument_risk_target,
      subsystem_ret_cor_mat,
      trade_system$system_account_table,
      t,
      config = trade_system$config
    )
  }

  system_account_table <- trade_system$system_account_table

  system_account_table[t, ] <- update_system_account_table_row(
    position_tables,
    system_account_table,
    t
  )



  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0007
  # Update "accounts" table
  # Doesn't need update for each algo, only once for each trade system
  # update.
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0008
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  #execute_trade()

  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0009
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  #save_tables()

  ## Return updated trade system
  list(
    inst_data = trade_system$inst_data,
    rule_functions = trade_system$rule_functions,
    subsystem_ret_cor_mat = subsystem_ret_cor_mat,
    algos = trade_system$algos,
    signal_normalization_factors = trade_system$signal_normalization_factors,
    signal_tables = signal_tables,
    position_tables = position_tables,
    system_account_table = system_account_table,
    # signal_cor_mat = signal_cor_mat,
    config = trade_system$config
  )
}


#' Update Signal Table Row
#'
#' @param algo Algo list for a single expanded algo.
#' @param signal_table Signal table data frame.
#' @param signal_weight Signal weight.
#' @param t Time index.
#' @param config List of trade system configuration values.
#'
#' @return List of signal table row values
#' @export
#'
#' @examples
update_signal_table_row <- function(
    inst_data,
    algo,
    rule_function,
    signal_table,
    signal_weight, ## Weight is just passed to the output vector
    signal_normalization_factor,
    t, ## Time index
    config
  ) {

  time_t <- update_time(inst_data, algo, t)

  price_t <- update_price(inst_data, algo, t)
  prices <- c(signal_table$price, price_t)

  raw_signal <- generate_signal(
    prices, ## For now only price data is allowed as input for rules
    algo,
    rule_function
    #trade_system$signal,
  )

  normalized_signal <- f_normalize_signal(
    raw_signal,
    signal_normalization_factor
  )

  clamped_signal <- clamp_signal(
    normalized_signal,
    config$min_signal,
    config$max_signal
  )

  list(time_t,
    price_t,
    raw_signal,
    normalized_signal,
    clamped_signal,
    signal_weight
  )
}



#' Update Position Table Row
#'
#' @description
#'
#' Based on signal:
#'   * If a trade is not on: Determine whether to enter a trade, and if so
#'     in which direction.
#'   * If a trade is on: Determine whether to modify the position size, exit the
#'     trade or to switch direction.
#'
#' Update the position table accordingly.
#'
#' @param algo Single algo from the expanded algos list.
#' @param signal_table Signal table.
#' @param position_table Position table.
#' @param raw_combined_signal Raw combined signal value.
#' @param signal_div_mult Signal diversification multiplier.
#' @param subsystem_ret_cor_mat Subsystem returns correlation matrix.
#' @param system_account_table System account table.
#' @param t Time index.
#' @param config Configuration list.
#'
#' @return List
#' @export
#'
#' @examples
update_position_table_row <- function(
  #algo,
  inst_data,
  rule_functions,
  #signal_table,
  position_table,
  raw_combined_signal,
  signal_div_mult,
  instrument_weight,
  inst_div_mult,
  instrument_risk_target,
  subsystem_ret_cor_mat,
  system_account_table,
  t,
  config
) {

  prices <- inst_data$price[1:t]

  ## Calculate trade direction from combined signal at time t
  direction <- sign(raw_combined_signal)

  latest_trade_direction <- position_table$direction[t - 1]

  instrument_risk <- f_inst_risk(
    #c(position_table$price[1:(t - 1)], price),
    prices,
    t = t,
    window_length = config$risk_window_length
  )

  #if(latest_trade_direction != direction) {

  # TODO:
  # Based on signal:
  #   * If a trade is not on: Determine whether to enter a trade, and if so
  #     in which direction.
  #   * If a trade is on: Determine whether to exit the trade or to switch
  #     direction.
  #   * It is not allowed to enter a trade in the same direction as the
  #     previous trade.

    ## ** NOTE **
    ## This is the same for all instruments...
    ## Right...?
    ## So: Calculate in update_system()
    ## *** ** ***
    # inst_div_mult <- f_inst_div_mult(
    #   subsystem_ret_cor_mat,
    #   instrument_weights
    # )
    #
    # instrument_risk_target <- f_instrument_risk_target(
    #   trade_system$config$system_risk_target,
    #   inst_div_mult
    # )

    # ST, p. 133
    rescaled_combined_signal <- raw_combined_signal * inst_div_mult

    clamped_combined_signal <- clamp_signal(
      rescaled_combined_signal,
      min_signal = config$min_signal,
      max_signal = config$max_signal
    )

    required_leverage_factor <- f_required_leverage_factor(
      instrument_risk_target,
      instrument_risk
    )

    subsystem_position <- calculate_subsystem_position(
      clamped_combined_signal,
      required_leverage_factor
    )

    if(latest_trade_direction == direction) { ## No change in direction
      enter_or_exit <- "---"
      trade_on <- abs(direction) ## Note TRUE == 1, FALSE == 0
      t_last_position_entry <- position_table$t_last_position_entry[t - 1]
    } else if(latest_trade_direction == 0 && abs(direction) == 1) { ## If entering a position
      enter_or_exit <- "enter"
      t_last_position_entry <- t
      trade_on <- TRUE
    } else if(direction == 0 && abs(latest_trade_direction) == 1) { ## If closing an open position
      enter_or_exit <- "exit"
      trade_on <- FALSE
      t_last_position_entry <- "---"
    } else if(latest_trade_direction * direction == -1) { ## If changing direction
      enter_or_exit <- "reverse"
      trade_on <- TRUE
      t_last_position_entry <- t
    } else {
      enter_or_exit <- NA
      t_last_position_entry <- NA
      trade_on <- NA
    }

    stop_loss <- 1
    stop_loss_gap <- NA

    # if(trade_on == TRUE) {
    #   ## apply stop loss rule
    #   if(length(algo$rule) == 2) {
    #     stop_loss <- rule_functions[[ algo$rule[[2]] ]](
    #       prices,
    #       t,
    #       instrument_risk,
    #       config$stop_loss_fraction,
    #       t_last_position_entry,
    #       direction,
    #       rnd = false
    #     )
    #
    #     ## for now the stop loss rule must return a list of a stop loss signal
    #     ## and a stop loss gap. gop extracted for book keeping.
    #     ## extract separate variables
    #     stop_loss_gap <- stop_loss$stop_loss_gap
    #     stop_loss <- stop_loss$stop_loss
    #
    #     subsystem_position <- subsystem_position * stop_loss
    #   }
    # }


    ## Notional exposure in account currency
    notional_exposure <- f_notional_exposure(
      clamped_combined_signal,
      system_account_table$capital[t - 1],
      required_leverage_factor,
      instrument_weight
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
      prices[t] #,
      # TODO
      # Uncomment when implementing fx rates:
      #fx_rate # default 1
    )


    ## Actual traded position in rounded number of contracts
    position_size_units <- f_position_units(target_position_size_units)

    ## Actual position size in units account currency
    position_size_ccy <- f_position_size_ccy(
      prices[t],
      position_size_units
    )

    ## Starter System: No position adjustment.
    #prev_position_size_units <- trades_data$position_size_units[t - 1]
    #prev_position_size_ccy <- trades_data$position_size_ccy[t - 1]

    #trade_amount_units <- (position_size_units - prev_position_size_units) * direction
    #trade_amount_ccy <- (position_size_ccy - prev_position_size_ccy) * direction


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

    instrument_return <- f_price_returns(prices, t)

    # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
    # fix§0040
    # Calculate `subsystem_pandl`
    # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
    ## Update P&L for subsystem
    subsystem_pandl <- f_subsystem_pandl(position_table, instrument_return, t)

    # borrowed_asset,
    borrowed_asset_ccy <- position_size_ccy * (direction < 0) ## 0 if long

    position_change_ccy <- f_position_change_ccy(position_table, position_size_ccy, t)

  #} else { ## latest_trade_direction == direction: no change (don't enter
    ## trade)
    #position_size_units <- position_table$position_size_units[t - 1] ## Still 0
    #position_size_ccy <- position_table$position_size_ccy[t - 1] ## Still 0


    #subsystem_pandl <- NA

    #borrowed_cash <- 0 ## No trade is on
    #borrowed_asset <- 0

    ## While no trade on:
    ## capital[t] = cash[t]

    #cash <- system_account_table$cash[t - 1]
    #capital <- system_account_table$capital[t - 1]

    # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
    # fix§0018
    # Calculate `cash` for entire trade system and move `cash` from
    # `position_tables` to `system_account_table`.
    # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
    #account_value <- capital

    # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
    # fix§0037
    # Fill unmodified columns for this row
    # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  #} ## closure of "if(latest_trade_direction != direction) {"

  ## Return updated row in position_tables for time t
  list(
    inst_data$time[t],
    prices[t],
    instrument_risk,
    instrument_risk_target,
    raw_combined_signal,
    rescaled_combined_signal,
    clamped_combined_signal,
    required_leverage_factor,
    signal_div_mult,
    instrument_weight,
    inst_div_mult,
    notional_exposure,
    target_position_size_units,
    position_size_units,
    position_size_ccy,
    direction,
    stop_loss,
    stop_loss_gap,
    subsystem_position,
    enter_or_exit,
    t_last_position_entry,
    trade_on,
    instrument_return,
    subsystem_pandl,
    borrowed_asset_ccy,
    position_change_ccy
  )
}

## capital =
## account_value =
## cash =
update_system_account_table_row <- function(
    position_tables,
    system_account_table,
    t) {

  # cash <- system_account_table$cash[t - 1] - ((direction > 0) * position_size_ccy)
  # + borrowed_cash + borrowed_asset
  # if(direction > 0) {
  #   capital <- cash + position_size_ccy - borrowed_cash
  # } else {
  #   capital <- system_account_table$cash[t - 1]
  # }

  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0017
  # Calculate `cash` for entire trade system and move `cash` from
  # `position_tables` to `system_account_table`.
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

  ## Sum of current value of all current positions
  total_positions_value <- f_total_positions_value(position_tables, t)

  ## Sum of all price returns at time t for all instruments with an open position
  total_returns <- f_total_returns(position_tables, t)

  ## Sum of P&L at time t for all open positions
  account_pandl <- f_account_pandl(position_tables, t)

  ## Cash in account at time t
  cash <- f_total_cash(position_tables, system_account_table, t)

  ## Cash plus account P&L at time t
  account_value <- f_account_value(position_tables, cash, t)

  ## Trading capital: Initial capital plus sum of all P&L for all instruments
  ## up until and including time t
  capital <- f_capital(system_account_table, account_pandl, t)

  list(
    total_positions_value = total_positions_value,
    total_returns = total_returns,
    account_pandl = account_pandl,
    cash = cash,  #not implemented yet
    account_value = account_value,
    capital = capital
  )
}



## Load data into trade_system$inst_data list.
#' Load Instrument Data Sets
#'
#' @description
#' Takes character string instrument names from each algo in a trade system
#' created by `make_system()` and loads the corresponding csv file into a list.
#'
#' @param parsed_algos Expanded list of algos.
#' @param instrument_data_folder_path Path to instrument data folder.
#'
#' @return List of data frames
#' @export
#'
#' @examples
load_instrument_data_sets <- function(
    parsed_algos,
    instrument_data_folder_path
) {
  unique_instrument_paths <-
    get_unique_inst_paths_from_expanded_algos_list(
      parsed_algos,
      instrument_data_folder_path
    )
  unique_instrument_names <-
    get_unique_inst_names_from_parsed_algos_list(
      parsed_algos
    )
  unique_instrument_data_sets <- list()
  for(i in seq_along(unique_instrument_paths)) {
    unique_instrument_data_sets[[i]] <- data.frame(
      utils::read.csv(unique_instrument_paths[[i]])
    )
    ## Assign name as comment to data frame.
    comment(unique_instrument_data_sets[[i]]) <-  unique_instrument_names[[i]]
  }
  names(unique_instrument_data_sets) <- get_unique_inst_names_from_parsed_algos_list(parsed_algos)
  unique_instrument_data_sets
}

#' Load Rule Functions
#'
#' @description
#' Load rule functions as specified by function names as character strings in
#'   parsed algos into a list. The functions must be loaded into the enclosing
#'   (typically global) environment.
#'
#' @param parsed_algos
#'
#' @return
#' @export
#'
#' @examples
load_rule_functions <- function(parsed_algos) {
  unique_rule_names <- get_unique_rule_function_names_by_parsed_algo(parsed_algos)
  unique_rule_functions <- lapply(unique_rule_names,
    function(x) {eval(parse(text = x))}
  )
  unique_rule_functions
}

#' Evaluate Instrument Data Set
#'
#' @description
#' Evaluate an instrument data set given a name as a character string.
#'
#' @param rule_name Instrument data set name as a character string.
#'
#' @return Data frame
#' @export
#'
#' @examples
eval_inst <- function(instrument_name) {
  eval(parse(text = instrument_name))
}

#' Evaluate Rule Function
#'
#' @description
#' Evaluate a rule function given a function name as a character string.
#'
#' @param rule_name Rule function name as a character string.
#'
#' @return Function
#' @export
#'
#' @examples
eval_rule <- function(rule_name) {
  eval(parse(text = rule_name))
}

#' Run Trade System
#'
#' @param system List containing a full trade system.
#' @param min_periods Minimum number of observations before first position is
#'   calculated.
#' @param mode "live" for live trading or "sim" for simulation mode.
#' @param instrument_data_folder_path Path of instrument data folder.
#'
#' @return List containing full updated trade system
#' @export
#'
#' @examples
run_system <- function(
    trade_system,
    min_periods = 1,
    mode = "sim",
    instrument_data_folder_path = "~/git/rsystrade/misc/temp/data/") {

  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0020
  # Should be able to handle data frames with different numbers of rows.
  #
  ## Get number of rows in instrument data sets.
  ## For now the trade system expects all data frames for all instruments to
  ## have same number of rows.
  ## We get the number of rows from the data of the first algo (temporary
  ## solution).
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  num_rows <- nrow(trade_system$inst_data[[1]])
  if(!(num_rows > min_periods)){
    stop(paste(
      "At least one data set must contain at least", min_periods + 1, "rows."
    ))
  }
  if(mode == "live") {
    stop("\"Live mode is not implemented yet.\"")
  } else if(mode == "sim") {
    ## Load instrument data
    trade_system$inst_data <- load_instrument_data_sets(
      parsed_algos = trade_system$algos,
      instrument_data_folder_path = instrument_data_folder_path
    )
    for(t in (min_periods + 1):num_rows) {
      trade_system <- update_system(trade_system, t)
    }
    trade_system
  } else {
    stop(
      paste("\nProvide a valid mode when running make_system().",
            "\nValid modes are \"sim\" (default) or \"live\".")
    )
  }
}

#' Calculate Signal Weights For All Algos
#'
#' @description
#' Calculate signal weights for all algos.
#'
#' Within each subsystem weights must sum to one. Note, the sum of signal
#'   weights for all algos need not sum to one.
#'
#' @param algos List of all exoanded algos.
#' @param method Method for calculating weights.
#'
#' @return Numeric vector of weights
#' @export
#'
#' @examples
calculate_signal_weights <- function(algos, method = "equal") {
  weights <- switch(
    method,
    "equal" = calculate_equal_signal_weights(algos),
    "estimate" = NA
  )
  weights
}


#' Generate Trade Signal From Rule
#'
#' @description
#' Apply a single signal rule to a single instrument.
#'
#' The `algo` list contains a rule list and data for a single instrument. The
#'   rule list contains one signal generating rule function and optionally one
#'   stop loss rule function.
#'   `generate_signal()` applies the signal rule and produces a trade signal.
#'
#' A positive signal value indicates a long position.
#' A negative signal indicates a short position.
#' A signal of value 0 indicates no position.
#'
#' NOTE
#' For now `generate_signal()` only accepts a price vector as input. The last
#'   observation is time $t$ ("now").
#'
#' @param prices Vector of prices. The last observation is time t.
#' @param algo Single algo from the expanded algos list.
#'
#' @return Single signal value
#' @export
#'
#' @examples
generate_signal <- function(
    prices, ## Vector of prices
    algo, ## Single algo from the algos list
    rule_function ## Rule function
    #signal_table, ## signal table for the instrument
) {

  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0014:
  # Get the needed variables from the trade system.
  # Pass the calculated variables back to trade system.
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

  ## Signal rule
  #raw_signal <- algo$rule[[1]](algo$data)

  #raw_signal <- rule_functions[[ algo$rule[[1]] ]](prices)
  raw_signal <- rule_function[[1]](prices)

  # # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # # fix§0027:
  # # Move the rest of this if condition to update_position_table_row() after
  # # `combine_signals() and update_account(), as appropriate.
  # # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # if(trade_on == FALSE) {
  #   #if(latest_trade_direction == direction) {direction = 0}
  #
  #   if(direction != 0) {
  #     update_position_table_row(
  #       # price,
  #       # direction,
  #       # latest_trade_direction,
  #       # system_account_table,
  #       # risk_target,
  #       # instrument_risk,
  #       # position_size_ccy,
  #       # t
  #     )
  #   } else { ## direction == 0: no position change (don't enter trade)
  #     ## We don't have any trade on, and nothing changes
  #     ## Still 0
  #     position_size_units <- position_table$position_size_units[t - 1]
  #     ## Still 0
  #     position_size_ccy <- position_table$position_size_ccy[t - 1]
  #
  #     ## How much did we borrow when we entered the trade?
  #     borrowed_cash <- system_account_table$borrowed_cash[t_trade_enter]
  #     borrowed_asset <- system_account_table$borrowed_asset[t_trade_enter]
  #
  #     cash <- system_account_table$cash[t - 1]
  #     capital <- system_account_table$capital[t - 1]
  #
  #
  #     # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  #     # fix§0016
  #     # Calculate `cash` for entire trade system and move `cash` from
  #     # `position_tables` to `system_account_table`.
  #     # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  #     account_value <- capital
  #   }
  # } else { ## If trade is on, check optional stop loss rule.
  #
  #   #inst_risk_at_entry <- trade_data$instrument_risk[t_trade_enter]
  #   #price_at_entry <- trade_data$price[t_trade_enter]
  #   #price_vol_at_entry <- price_unit_vol(inst_risk_at_entry, price_at_entry)
  #
  #
  #   ## NOTE:
  #   ## position_size_units will never be negative, as long as capital is not
  #   ## allowed to reach 0 or below.
  #   ## The break below takes care of that.
  #
  #   ## While long trade is on:
  #   ## cash[t] = cash[t - 1]
  #   ##
  #   #### borrowed_cash[t] = borrowed_cash[t_trade_enter]
  #   #### borrowed_asset[t] = 0
  #   #### capital[t] = position_size_ccy[t] - borrowed_cash[t_trade_enter] + cash
  #
  #   ## While short trade is on:
  #   ## cash[t] = cash[t - 1]
  #   ## borrowed_cash[t] = 0
  #   ## borrowed_asset[t] = position_size_units[t]
  #   ## capital[t] = cash[t_trade_enter - 1] + (position_size_ccy[t] - position_size_ccy[t_trade_enter])
  #
  #   ## When long trade has just been exited:
  #   ## cash[t] = cash[t - 1] + position_size_ccy[t] - borrowed_cash[t_trade_enter]
  #   ## borrowed_cash[t] = 0
  #   ## borrowed_asset[t] = 0
  #   ## capital[t] = cash[t]
  #
  #   ## When short trade has just been exited:
  #   ## cash[t] = cash[t - 1] - position_size_ccy[t]
  #   ## borrowed_cash[t] = 0
  #   ## borrowed_asset[t] = 0
  #   ## capital[t] = cash[t]
  #
  #   position_size_units <- position_table$position_size_units[t - 1]
  #   position_size_ccy <- position_size_units * price
  #
  #   ## (direction > 0) == 0 if short
  #   borrowed_cash =
  #     system_account_table$borrowed_cash[t_trade_enter] * (direction > 0)
  #
  #   ## (1 - (direction > 0)) == 1 if short
  #   borrowed_asset = position_size_units * (1 - (direction > 0))
  #
  #   ## Stop loss rule
  #   #raw_signal <- algo$rule[[2]](algo$data)
  #
  #   if(direction == 1) { ## If long
  #     cash <- system_account_table$cash[t - 1]
  #     capital <-
  #       cash + position_size_ccy - system_account_table$borrowed_cash[t_trade_enter]
  #     if(enter_or_exit == "EXIT") {
  #       trade_on <- FALSE ## Exit trade
  #       direction <- 0
  #       cash <- system_account_table$cash[t - 1] + position_size_ccy -
  #         system_account_table$borrowed_cash[t_trade_enter]
  #       capital <- cash
  #     }
  #   } else if(direction == -1) { ## If short
  #     cash <- system_account_table$cash[t - 1]
  #     capital <- system_account_table$cash[t_trade_enter - 1] +
  #       (position_table$position_size_ccy[t_trade_enter] - position_size_ccy)
  #     if(enter_or_exit <- "EXIT") {
  #       trade_on <- FALSE ## Exit trade
  #       direction <- 0
  #       cash <- system_account_table$cash[t - 1] - position_size_ccy
  #       capital <- cash
  #     }
  #   } else { ## Should be redundant, since we know a trade is on, and a trade
  #            ## will either be long or short...
  #     cash <- system_account_table$cash[t - 1]
  #     capital <- system_account_table$capital[t - 1]
  #   }
  # }
  # list(
  #   time = time,
  #   price = price,
  #   raw_signal = raw_signal,
  #   normalized_signal
  # )

  raw_signal
}


get_signal_weight <- function() {
  warning("get_signal_weight() not implemented yet.")
}


#' Get All Past Raw Signals
#'
#' @param signal_tables List
#'
#' @return List of raw signals
#' @export
#'
#' @examples
get_all_past_raw_signals <- function(signal_tables) {
  lapply(signal_tables, function(signal_table) {
    signal_table$raw_signal
  })
}


#' Calculate Equal Signal Weights For All Algos
#'
#' First calculates the equal weights for each instrument. Then outputs a vector
#'   of weights, one weight for each parsed algo. The sum of all weights for
#'   each subsystem is 1.
#'
#' @param parsed_algos List of expanded algos
#'
#' @return Vector of weights.
#' @export
#'
#' @examples
calculate_equal_signal_weights <- function(parsed_algos) {
  num_signals <- get_num_rules_per_inst_from_parsed_algos(parsed_algos)
  weights_by_instrument <- numeric(length(num_signals))
  for(i in seq_along(num_signals)) {
    if(!is.na(num_signals[[i]]) && num_signals[[i]] >= 1) {
      weights_by_instrument[i] <- 1/num_signals[[i]]
    } else {
      stop("At least one rule must be provided to the trade system.")
    }
  }

  weights_by_algo <- numeric(length(parsed_algos))

  id <- 1
  for(i in seq_along(num_signals)) {
    for(j in 1:num_signals[[i]]) {
      weights_by_algo[[id]] <- weights_by_instrument[[i]]
      id <- id + 1
    }
  }
  weights_by_algo
}

#' Calculate Equal Instrument Weights From Parsed Algos
#'
#' @description
#' Calculates a vector of weights, one weight for each instrument. The sum of
#'   all weights is 1.
#'
#' @param parsed_algos List of expanded algos.
#'
#' @return Vector of weights.
#' @export
#'
#' @examples
calculate_equal_inst_weights <- function(parsed_algos) {
  n <- get_num_inst_from_parsed_algos_list(parsed_algos)
  rep(1/n, n)
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
#' @param signal_tables List of data frames. One data frame for each signal.
#' @param t Time index.
#' @param algos List of parsed (expanded) algos.
#' @param min_cor Minimum correlation.
#' @param max_sdm Maximum signal diversification multiplier.
#'
#' @return List containing a vector of combined signals and a vector of signal
#'   diversification multiplier values
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
  signal_div_mult_vect <-numeric(n)

  for(i in 1:n) {
    ## A subsystem is all the algos for an individual instrument
    subsystem_IDs <- which(
      inst_names_by_algo == unique_inst_names[[i]]
    )

    ## Collect all clamped signals in subsystem i
    signals <- sapply(
      signal_tables[subsystem_IDs], function(x) x$clamped_signal[t]
    )
    ## Collect all signal weights in subsystem i
    signal_weights <- sapply(
      signal_tables[subsystem_IDs], function(x) x$signal_weight[t]
    )

    ## Make vector of signal vectors
    signal_vectors <- sapply(signal_tables[subsystem_IDs], function(x) {x$clamped_signal})

    ## Calculate signal correlations.
    ## No need to do this every time we run update_position_table_row.

    signal_cor_mats[[i]] <- f_signal_cor_mat(signal_vectors)

    ## Signal Diversification Multiplier
    signal_div_mult_vect[i] <- f_sig_div_mult(
      signal_cor_mats[[i]],
      signal_weights,
      min_cor,
      max_sdm
    )

    combined_signals[i] <- signals %*% signal_weights * signal_div_mult_vect[i]
  }

  list("combined_signals" = combined_signals, "signal_div_mult_vect" = signal_div_mult_vect)
}



# §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
# fix§0036
# Input param in help is sdm, but required_leverage_factor in function.
# So which one is it?
# §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

#' Calculate Subsystem Position
#'
#' @param combined_signal Typically normalized and clamped
#' @param required_leverage_factor Required leverage factor
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



#' Update Price
#'
#' @description
#' Get latest price from data set in trade system list according to algo.
#'
#' @param inst_data List of unique data frames.
#' @param algo Algo list for a single expanded algo.
#' @param t Time index.
#'
#' @return Single price
#'
#' @examples
update_price <- function(inst_data, algo, t) {
  inst_data[[ algo$instrument ]]$price[t]
}

#' Update time
#'
#' Get latest time from data in `algo` list.
#'
#' @param inst_data List of unique data frames.
#' @param algo Algo list for a single expanded algo.
#' @param t Time index.
#'
#' @return
#' @export
#'
#' @examples
update_time <- function(inst_data, algo, t) {
  inst_data[[ algo$instrument ]]$time[t]
}


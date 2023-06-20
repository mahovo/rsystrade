
## Trade system framework

# §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
# fix§0029
# Write help explaining structure of input algos. (See help for `parse_algos`)
# §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
#' Make Trade System
#'
#' @description
#' `config` contains
#'
#'   * `init_capital`
#'   * `system_risk_target`
#'   * `risk_window_length`
#'   * `min_periods`
#'   * `min_signal`
#'   * `max_signal`
#'   * `min_cor` Default is `0`.
#'   * `max_sdm` Default is `2.5`.
#'   * `same_direction_trade_allowed` Default is `FALSE`.
#'   * `instrument_data_folder_path`
#'   * `signal_weight_calculation_method` Default is `"equal"`.
#'   * `inst_weight_calculation_method` Default is `"equal"`.
#'   * `target_normalization_factor` Default is `1`,
#'   * `signal_normalization_factors_method` Default is `"equal"`.
#'   * `signal_normalization_factors_args` A list of additional arguments for
#'     each method in `update_signal_normalization_factors()`.
#'     * Additional argument for method `"equal"` is `equal_norm_factor`.
#'     * Additional argument for method `"median_pool_all"` is
#'       `min_periods_median_pool_all`.
#'
#' @param algos A list of algos.
#' @param init_capital Initial capital.
#' @param system_risk_target System risk target as decimal fraction (percent
#'   divided by 100).
#' @param risk_window_length Risk window length.
#' @param position_modifiers Named list of position modifier functions. Names
#'   should be the names of the corresponding instruments.
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
#'    * a list of rule functions.
#'    * a matrix of correlations between subsystem returns.
#'    * a list of parsed algos, which in turn contains an instrument name and a
#'      list of two trade rule functions: One trade signal generating rule and
#'      one optional stop loss rule.
#'      If present, the stop loss rule is applied in the positions stage.
#'    * a list of signal normalization factors
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
#'          rule_function = <rule_function_name>
#'        ),
#'        list(
#'          instrument = <instrument_name>,
#'          rule_function = <rule_function_name>
#'        ),
#'        ...,
#'        list(
#'          instrument = <instrument_name>,
#'          rule_function = <rule_function_name>
#'        )
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
    position_modifiers = list(),
    position_multipliers = list(),
    min_periods = 1,
    min_signal = -2,
    max_signal = 2,
    instrument_data_folder_path,
    ...) {

    config <- list(
      init_capital = init_capital,
      system_risk_target = system_risk_target,
      risk_window_length = risk_window_length,
      min_periods = min_periods,
      min_signal = min_signal,
      max_signal = max_signal,
      correlation_method = "Pearson",
      min_cor = 0,
      max_sdm = 2.5,
      same_direction_trade_allowed = FALSE,
      instrument_data_folder_path = instrument_data_folder_path,
      signal_weight_calculation_method = "equal",
      inst_weight_calculation_method = "equal",
      normalization_factor_target = 1,
      signal_normalization_factors_method = "equal",
      ## A list of additional arguments for each method in
      ## update_signal_normalization_factors()
      signal_normalization_factors_args = list(
        equal = list(equal_norm_factor = 1),
        median_pool_all = list(min_periods_median_pool_all = 250)
      )
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
  inst_names <- unlist(get_unique_inst_names_from_parsed_algos_list(parsed_algos))
  names(inst_data) <- inst_names

  ## The way to access a data set:
  ## inst_data[[parsed_algos[[i]]$instrument]]
  ## For instance, get prices:
  ## inst_data[[parsed_algos[[i]]$instrument]]$price

  ## We don't need to load rule functions anymore, as they are stored in each
  ## parsed algo
  #rule_functions <-load_rule_functions(parsed_algos)
  #names(rule_functions) <- get_unique_rule_function_names_by_parsed_algo(parsed_algos)

  ## Get number of signals
  num_signals <- length(parsed_algos)

  ## Get algo name for each signal
  signal_names <- get_rule_variation_names_by_parsed_algo(parsed_algos)

  ## Build list of signal tables. To be filled with one signal table for each
  ## algo.
  signal_tables <- list()

  for(i in 1:num_signals) {
    ## One table for each algo (i.e each instrument + rule combination)
    signal_tables[[i]] <- data.frame(
      time = inst_data[[ parsed_algos[[i]]$instrument ]]$time[1:min_periods],
      price = inst_data[[ parsed_algos[[i]]$instrument ]]$price[1:min_periods],
      ## Fill with small random values to avoid zero-sd
      raw_signal = numeric(min_periods),  #rnorm(min_periods, 0, 0.001)
      normalized_signal = numeric(min_periods),  #rnorm(min_periods, 0, 0.001)
      clamped_signal = numeric(min_periods),  #rnorm(min_periods, 0, 0.001)
      signal_weight = numeric(min_periods) #rep(NA, min_periods)
    )

    ## Algo names as comments in each data frame
    comment(signal_tables[[i]]) <- signal_names[[i]]
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
      #stop_loss = numeric(min_periods),
      #stop_loss_gap = numeric(min_periods),
      subsystem_position = numeric(min_periods),
      enter_or_exit = rep("---", min_periods),
      t_last_position_entry = rep(0, min_periods),
      trade_on = rep(FALSE, min_periods),
      ## Fill with small random values to avoid zero-sd
      instrument_return = c(
        0,
        f_price_returns(
          signal_tables[[i]]$price[2:min_periods],
          2:min_periods
          )
      ),#numeric(min_periods),  #rnorm(min_periods, 0, 0.001)
      subsystem_pandl = numeric(min_periods),  #rnorm(min_periods, 0, 0.001)
      final_target_pos_size_units = numeric(min_periods),
      final_pos_size_units = numeric(min_periods),
      final_pos_size_ccy = numeric(min_periods),
      final_pos_change_ccy = numeric(min_periods),
      final_target_pos_change_units = numeric(min_periods),
      final_pos_change_units = numeric(min_periods),
      borrowed_asset_ccy = numeric(min_periods)#,
      #position_change_ccy = numeric(min_periods)
    )

    ## Instrument names as comments in each data frame
    comment(position_tables[[i]]) <- inst_names[[i]]
  }

  names(position_tables) <- inst_names

  ## Initialize list of position modifiers.
  ## User may provide a named list where the names are instrument names and
  ## values are position modifier functions.
  ## make_position_modifiers_list() creates a named list. Each element corresponds
  ## to an instrument, in the order instruments occur in the inst_data list.
  ## The name of each element is the name of the instrument.
  ## User provided position modifier functions will be assigned to the
  ## appropriate element in the list.
  ## Any position modifier function for which the name doesn't match any
  ## instrument in the system, will be ignored.
  ## If no position modifier function is assigned to an instrument, the position
  ## of that instrument will not be modified.
  position_modifiers <- parse_position_modifiers_list(position_modifiers, inst_names)


  position_multipliers <- parse_position_multipliers_list(position_multipliers, inst_names)

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
    inst_data,
    target = config$normalization_factor_target,
    method = config$signal_normalization_factors_method,
    ## Get any additional arguments from config.
    ## Looks up method in config, then gets the associated argument values.
    args = config$signal_normalization_factors_args[[config$signal_normalization_factors_method]]
  )

  names(signal_normalization_factors) <- unlist(get_unique_rule_names_from_parsed_algos_list(parsed_algos))

  ## Generate initial system list
  list(
    inst_data = inst_data,
    #rule_functions = rule_functions,
    subsystem_ret_cor_mat = subsystem_ret_cor_mat,
    algos = parsed_algos,
    signal_normalization_factors = signal_normalization_factors,
    signal_tables = signal_tables,
    position_tables = position_tables,
    position_modifiers = position_modifiers,
    position_multipliers = position_multipliers,
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
  ##       rule = rule1
  ##     ),
  ##     list( ## algo 2: algos[[2]]
  ##       instrument = "<inst_2_name>",
  ##       rule = rule1
  ##     )
  ##     list( ## algo 3: algos[[3]]
  ##       instrument = "<inst_1_name>",
  ##       rule = rule2
  ##     ),
  ##     list( ## algo 4: algos[[4]]
  ##       instrument = "<inst_2_name>",
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
    trade_system$algos
  )

  ## Update new row in each signal_table
  signal_tables <- trade_system$signal_tables

  for(i in seq_along(trade_system$algos)) {
    signal_name <- comment(signal_tables[[i]])

    new_signal_row <- update_signal_table_row(
      t = t,
      inst_data = trade_system$inst_data[ trade_system$algos[[i]]$instrument ][[1]],
      algo = trade_system$algos[[i]],
      #trade_system$rule_functions[ trade_system$algos[[i]]$rule[[1]] ],
      signal_table = trade_system$signal_tables[[i]],
      signal_weight = signal_weights_all_algos[[i]],
      signal_normalization_factor = sig_norm_fact_by_algos[[i]],
      position_table = trade_system$position_tables[[i]],
      config = trade_system$config
    )

    ## Additional output from rules will be added to signal tables append
    ## additional columns.
    ## The first time a rule produces additional output, the appropriate new
    ## columns will be appended to the signal table. Previous rows will be
    ## backfilled with NAs.
    ## (This code assumes, that if the length of signal list is not equal to
    ## the number of columns in the signal table, it must be because there is
    ## additional output (">"). The "<" case should not be possible.)

    if(length(new_signal_row) == ncol(signal_tables[[i]])) {
      signal_tables[[i]][t, ] <- new_signal_row
    } else {
      signal_tables[[i]] <- backfill_table(
        table = signal_tables[[i]],
        new_row = new_signal_row
      )
    }

    ## Transfer comment to updated data frame
    comment(signal_tables[[i]]) <- signal_name
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
    f_subsystem_ret_cor_mat(
      subsystem_pandl_vectors,
      method = trade_system$config$correlation_method, #"Pearson"
      min_cor = trade_system$config$min_cor
    )

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
    inst_name <- comment(position_tables[[i]])

    new_position_row <- update_position_table_row(
      t = t,
      inst_data = trade_system$inst_data[[i]],
      position_table = trade_system$position_tables[[i]], ## position_table
      ## Signular because each position_modifier list may only contain one
      ## modifier.
      position_modifier = trade_system$position_modifiers[[i]],
      ## Plural because each position_multipliers may contain several
      ## multipliers.
      position_multipliers = trade_system$position_multipliers[[i]],
      raw_combined_signal = raw_combined_signals[[i]],
      signal_div_mult = signal_div_mult_vect[i], ## SDM value
      instrument_weight = instrument_weights[i],
      inst_div_mult = inst_div_mult,
      instrument_risk_target = instrument_risk_target,
      subsystem_ret_cor_mat = subsystem_ret_cor_mat,
      system_account_table = trade_system$system_account_table,
      config = trade_system$config
    )
    if(length(new_position_row) == ncol(position_tables[[i]])) {
      position_tables[[i]][t, ] <- new_position_row
    } else {
      position_tables[[i]] <- backfill_table(
        table = position_tables[[i]],
        new_row = new_position_row
      )
    }

    ## Transfer comment to updated data frame
    comment(position_tables[[i]]) <- inst_name
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
    #rule_functions = trade_system$rule_functions,
    subsystem_ret_cor_mat = subsystem_ret_cor_mat,
    algos = trade_system$algos,
    signal_normalization_factors = trade_system$signal_normalization_factors,
    signal_tables = signal_tables,
    position_tables = position_tables,
    position_modifiers = trade_system$position_modifiers,
    position_multipliers = trade_system$position_multipliers,
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
    t, ## Time index
    inst_data,
    algo,
    #rule_function,
    signal_table,
    signal_weight, ## Weight is just passed to the output vector
    signal_normalization_factor,
    position_table,
    config
  ) {

  time_t <- inst_data$time[t] #update_time(inst_data, algo, t)

  ## Only prices are currently supported as variable params.
  ## This should be generalized.
  prices <-  inst_data$price[1:t]
  price_t <- prices[t]

  signal_list <- generate_signal(
    variable_param_vals = get_variable_param_vals(t, inst_data, algo),
    #signal_table = signal_table,
    #position_table = position_table,
    algo = algo#,
    #t
    #config = config
  )

  ## Everything in signal list except signal
  secondary_rule_output <- signal_list[-1]

  ## Signal from signal list
  raw_signal <- signal_list[[1]]

  normalized_signal <- f_normalize_signal(
    raw_signal,
    signal_normalization_factor
  )

  clamped_signal <- clamp_signal(
    normalized_signal,
    config$min_signal,
    config$max_signal
  )

  standard_columns <- list(
    time = time_t,
    price = price_t,
    raw_signal = raw_signal,
    normalized_signal = normalized_signal,
    clamped_signal = clamped_signal,
    signal_weight = signal_weight
  )

  c(
    standard_columns,
    secondary_rule_output
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
#' @param inst_data Instrument data table.
#' @param signal_table Signal table.
#' @param position_table Position table.
#' @param position_modifier Position modifier function.
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
  t,
  inst_data,
  #rule_functions,
  #signal_table,
  position_table,
  position_modifier,
  position_multipliers,
  raw_combined_signal,
  signal_div_mult,
  instrument_weight,
  inst_div_mult,
  instrument_risk_target,
  subsystem_ret_cor_mat,
  system_account_table,
  config
) {

  prices <- inst_data$price[1:t]

  instrument_risk <- f_inst_risk(
    #c(position_table$price[1:(t - 1)], price),
    prices,
    t = t,
    window_length = config$risk_window_length
  )

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
  target_position_size_units <- f_target_position_in_units(
    notional_exposure,
    prices[t] #,
    # TODO
    # Uncomment when implementing fx rates:
    #fx_rate # default 1
  )

  ## Position in rounded number of contracts
  position_size_units <- f_position_in_units(target_position_size_units)

  ## Position size in units account currency
  position_size_ccy <- f_position_in_ccy(
    prices[t],
    position_size_units
  )

  t_last_position_entry <- position_table$t_last_position_entry[t - 1]

  latest_trade_direction <- position_table$direction[t - 1]

  trade_on <- abs(latest_trade_direction)

  ## We calculate direction based on combined signal here, so that direction is
  ## avaiable for any position modifiers.
  direction <- sign(raw_combined_signal)

  position_modifier_output <- modify_position(
    position_modifier = position_modifier,
    mod_variable_param_vals = get_pos_mod_var_param_vals(
      t = t,
      position_size_ccy = position_size_ccy,
      inst_data = inst_data,
      position_modifier = position_modifier,
      pos_table_vars = as.list(environment())
    ),
    position_multipliers = position_multipliers,
    mul_variable_param_vals = get_pos_mul_var_param_vals(
      t = t,
      position_size_ccy = position_size_ccy,
      inst_data = inst_data,
      position_multipliers = position_multipliers,
      pos_table_vars = as.list(environment())
    ),

    position_size_ccy = position_size_ccy
  )

  final_pos_target_ccy <- position_modifier_output[[1]]

  ## We calculate direction again based on modified position. E.g. if a position
  ## modifier invoked a stop loss, the position this will change the direction
  ## to 0.
  direction <- sign(final_pos_target_ccy)

  if(latest_trade_direction == direction) { ## No change in direction
    enter_or_exit <- "---"
    trade_on <- abs(direction) ## Note TRUE == 1, FALSE == 0
    t_last_position_entry <- position_table$t_last_position_entry[t - 1]
  } else if(latest_trade_direction == 0 && abs(direction) == 1) { ## If entering a position
    enter_or_exit <- "enter"
    t_last_position_entry <- t
    trade_on <- TRUE
  } else if(direction == 0 && trade_on == TRUE) { ## If closing an open position
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

  instrument_return <- f_price_returns(prices, t)

  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0040
  # Calculate `subsystem_pandl`
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  ## Update P&L for subsystem
  subsystem_pandl <- f_subsystem_pandl(position_table, instrument_return, t)


  ## Recalculate target position post position modifier
  final_target_pos_size_units <- f_target_position_in_units(
    final_pos_target_ccy,
    prices[t] #,
    # TODO
    # Uncomment when implementing fx rates:
    #fx_rate # default 1
  )

  ## Actual traded final position in rounded number of contracts
  final_pos_size_units <- f_position_in_units(final_target_pos_size_units)

  ## Recalculate actual final traded position size in units account currency
  ## post position modifier
  final_pos_size_ccy <- f_position_in_ccy(
    prices[t],
    final_pos_size_units
  )

  final_pos_change_ccy <- f_position_change_ccy(
    position_table,
    final_pos_size_ccy,
    t
  )

  ## Recalculate target position post position modifier
  final_target_pos_change_units <- f_target_position_in_units(
    final_pos_change_ccy,
    prices[t] #,
    # TODO
    # Uncomment when implementing fx rates:
    #fx_rate # default 1
  )

  final_pos_change_units <- f_position_in_units(
    final_target_pos_change_units
  )

  ## borrowed_asset (total after position change)
  borrowed_asset_ccy <- final_pos_size_units * (direction < 0) ## 0 if long

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


  standard_columns <- list(
    time = inst_data$time[t],
    price = prices[t],
    instrument_risk = instrument_risk,
    instrument_risk_target = instrument_risk_target,
    raw_combined_signal = raw_combined_signal,
    rescaled_combined_signal = rescaled_combined_signal,
    clamped_raw_combined_signal = clamped_combined_signal,
    required_leverage_factor = required_leverage_factor,
    signal_div_mult = signal_div_mult,
    instrument_weight = instrument_weight,
    inst_div_mult = inst_div_mult,
    notional_exposure = notional_exposure,
    target_position_size_units = target_position_size_units,
    position_size_units = position_size_units,
    position_size_ccy = position_size_ccy,
    direction = direction,
    subsystem_position = subsystem_position,
    enter_or_exit = enter_or_exit,
    t_last_position_entry = t_last_position_entry,
    trade_on = trade_on,
    instrument_return = instrument_return,
    subsystem_pandl = subsystem_pandl,
    final_target_pos_size_units = final_target_pos_size_units,
    final_pos_size_units = final_pos_size_units,
    final_pos_size_ccy = final_pos_size_ccy,
    final_pos_change_ccy = final_pos_change_ccy,
    final_target_pos_change_units = final_target_pos_change_units,
    final_pos_change_units = final_pos_change_units,
    borrowed_asset_ccy = borrowed_asset_ccy#,
    #position_change_ccy = position_change_ccy
  )

  c(
    standard_columns,
    position_modifier_output
  )


  ## Return updated row in position_tables for time t

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
#' DEPRECATED: This depends on a previous structure of parsed algos, where
#'   rule functions were given by their name as a character string!
#'
#' Load rule functions into a list. Functions are specified as functions
#'   assigned to variables or by function names as character strings in parsed
#'   algos. The functions must be loaded into the enclosing (typically global)
#'   or package environment.
#'
#' @param parsed_algos
#'
#' @return
#' @export
#'
#' @examples
load_rule_functions <- function(parsed_algos) {
  unique_rule_names <- get_unique_rule_variation_names_by_parsed_algo(parsed_algos)
  unique_signal_generators <- lapply(
    unique_rule_names,
    function(x) {
      if(is.character(x)) {
        #eval(parse(text = x))
        eval_function_from_string(x)
      } else {
        x
      }
    }
  )
  unique_signal_generators
}

#' Evaluate Instrument Data Set
#'
#' @description
#' Evaluate an instrument data set given a name as a character string.
#'
#' @param instrument_name Instrument data set name as a character string.
#'
#' @return Data frame
#' @export
#'
#' @examples
eval_inst <- function(instrument_name) {
  eval(parse(text = instrument_name))
}

#' Evaluate Function From String
#'
#' @description
#' Evaluate a function given a function name as a character string.
#'
#' @param function_name Function name as a character string.
#'
#' @return Function
#' @export
#'
#' @examples
eval_function_from_string <- function(function_name) {
  eval(parse(text = function_name))
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

#' Update Signal Normalization Factors
#'
#' @description
#' Each raw signal is scaled by a normalization factor. This normalization
#'   factor is the _required leverage target_.
#'
#' `update_signal_normalization_factors()` calculates the normalization factors
#'   and updates the `signal_normalization_factors` list.
#'
#' @param parsed_algos Parsed algos list from trade system.
#' @param signal_tables Signal tables list from trade system.
#' @param instrument_data_sets Instrument data sets list from trade system.
#' @param target The target expected value of the signal scaled by the
#'   normalization factor. Default is 1.
#' @param method Method.
#' * `"equal"` All rules use the same normalization factor, passed as
#'   `target`.
#' * `"pool_traded"` Calculate the normalization factor for each rule based on
#'   actual past signals for that rule across all the instruments to which
#'   the individual rule has been applied.
#'   We are pooling all the instruments for each rule. We are not taking the
#'   cross section median across instruments, as we do for `pool_all()`,
#'   because the number of instruments per rule is likely to be small - taking
#'   the median of two values doesn't make much sense.
#' * `"pool_all"` Calculate the normalization factor for each rule based on
#'   signals simulated by applying each rule to all available instruments and
#'   pooling the resulting signals.
#'   The normalization factor for each rule is the target divided by the mean
#'   absolute value of the pooled signals.
#'   Additional argument:
#'   * `min_period`. A value of `250` (ca. 1 year of
#'     daily data) might be a good starting point. It is up to the user to
#'     provide data sets with enough data.
#' * `"median_pool_all"` Calculate the normalization factor for each rule
#'   based on signals simulated by applying each rule to all available
#'   instruments.
#'   The normalization factor for each rule is the target divided by the mean
#'   absolute value of all cross section medians.
#'   Additional argument:
#'   * `min_period`. A value of `250` (ca. 1 year of
#'     daily data) might be a good starting point. It is up to the user to
#'     provide data sets with enough data.
#' * `"pool_class"` Calculate the normalization factor for each rule based on
#'   signals simulated by applying each rule to all available instruments in
#'   a relevant asset class.
#' @param ... Additional method specific arguments.
#'
#' @return Named list of normalization factors
#' @export
#'
#' @examples
update_signal_normalization_factors <- function(
    parsed_algos,
    signal_tables,
    instrument_data_sets,
    target = 1,
    method = "equal",
    ...) {

  ## Check that a single valid method is provided.
  valid_methods <- c("equal", "pool_traded", "pool_all", "median_pool_all", "pool_class")
  if(length(method) != 1 || sum(method == valid_methods) != 1) {
    stop("A single valid method must be provided to update_normalization_factors()")
  }

  equal <- function(
    parsed_algos,
    args = list(equal_norm_factor = 1)
  ) {
    n <- get_num_rules_from_parsed_algos_list(parsed_algos)
    factors <- as.list(rep(args$equal_norm_factor, n))
    names(factors) <- get_unique_rule_names_from_parsed_algos_list(parsed_algos)
    factors
  }

  pool_traded <- function(
    signal_tables,
    parsed_algos
  ) {
    ## Get rule name for each algo in the order they appear in the parsed algos
    ## list
    rule_names_by_algo <- get_rule_names_by_parsed_algo(parsed_algos)

    ## Get all unique rule variation names in a list
    all_unique_rule_names <- get_unique_rule_names_from_parsed_algos_list(parsed_algos)

    ## For each unique rule name, get the IDs of that rule in the
    ## rule_names_by_algo list
    IDs_grouped_by_rule_names <- split(
      seq_along(rule_names_by_algo),
      unlist(rule_names_by_algo)
    )

    ## For each unique rule name, get all signals produced by that rule.
    ## We are assuming that algos are in the same order as signal tables
    raw_signals_list <- list()
    for(rule in unlist(all_unique_rule_names)) {
      all_raw_signals <- lapply(
        signal_tables[unlist(IDs_grouped_by_rule_names[rule])],
        function(x) {x$raw_signal}
      )
      df <- data.frame(all_raw_signals)
      names(df) <- NULL
      raw_signals_list[[rule]] <- df
    }

    ## When we unlist the raw signal data frame, all the column vectors in that
    ## data frame are concatenated to one vector. So in effect we are pooling all
    ## the instruments for each rule. We are not taking the cross section median
    ## across instruments, as we do for median_pool_all(), because the number of
    ## instruments per rule is likely to be small - taking the median of two
    ## values doesn't make much sense.
    lapply(raw_signals_list,
           function(raw_signal_vector) {
             f_indiv_normalization_factor(
               unlist(raw_signal_vector),
               target = 1
             )
           }
    )
  }

  # TODO
  # Same as median_pool_all(), but instead of median(unlist(raw_signals_df[j, ]))
  # do mean(abs(unlist(raw_signals_df))) for each rule.
  pool_all <- function(signal_tables, parsed_algos) {
    stop("pool_all() not implemented yet")
  }

  median_pool_all <- function(
    parsed_algos, ## parsed (expanded) algos
    data_sets, ## data frames
    args = list(min_periods_median_pool_all = 250)
  ) {

    ## Get number of rows from first data set.
    ## Brave assuming that all data sets have same number of rows!
    num_rows <- nrow(data_sets[[1]])
    num_data_sets <- length(data_sets)
    min_periods <- args$min_periods_median_pool_all

    if(num_rows < min_periods) {
      stop("Not enough data to estimate normalization factor with pool_all().")
    }

    ## Get rule name for each algo in the order they appear in the algos list
    rule_names_by_algo <- get_rule_names_by_parsed_algo(parsed_algos)

    ## Get all unique rule names in a list
    all_unique_rule_names <-get_unique_rule_names_from_parsed_algos_list(parsed_algos)

    ## Get instrument name for each algo in the order they appear in the algos list
    inst_names_by_algo <-  get_inst_names_by_parsed_algo(parsed_algos)

    ## Get all unique instrument names in a list
    all_unique_inst_names <- get_unique_inst_names_from_parsed_algos_list(parsed_algos)

    instruments <- lapply(all_unique_inst_names,
                              function(x) {
                                #ID = which(x == rule_names_by_algo)[1]
                                #This should be equivalent:
                                ID = match(x, inst_names_by_algo)
                                ## Get functions at each ID in algos list
                                parsed_algos[[ID]]$instrument
                                # c(
                                #  parsed_algos[[ID]]$rule[1],
                                #  parsed_algos[[ID]]$rule[2]
                                # )
                              }
    )


    ## For each unique rule name, get the first ID of that rule in the
    ## rule_names_by_algo list
    rule_variations <- lapply(all_unique_rule_names,
                             function(x) {
                               #ID = which(x == rule_names_by_algo)[1]
                               #This should be equivalent:
                               ID = match(x, rule_names_by_algo)
                               ## Get functions at each ID in algos list
                               parsed_algos[[ID]]$rule
                               # c(
                               #  parsed_algos[[ID]]$rule[1],
                               #  parsed_algos[[ID]]$rule[2]
                               # )
                             }
    )

    ## Make list of algos for simulation
    sim_algos <- list()
    k <- 1
    for(i in seq_along(rule_variations)) {
      for(j in seq_along(instruments)) {
        sim_algos[[k]] <- list(
          #"data" = data_sets[[j]],
          instrument = instruments[[j]],
          rule = rule_variations[[i]]
          #list(
            #rule_functions[[i]][[1]],
            #rule_functions[[i]]#[[2]]
          #)
        )
        k <- k + 1
      }
    }


    ## For each unique rule, apply that rule to all instruments.
    ## (sim_algos contains one algo per unique rule.)
    ## One raw signals data frame for each rule.
    ## In each raw signals data frame one column for each instrument.
    ## Each column in raw_signals_df is a signal vector.
    # raw_signals_df <- data.frame()
    # for(i in seq_along(sim_algos)) {
    #   for(t in (min_periods + 1):num_rows) {
    #     raw_signals_df[i, t] <- generate_signal(
    #       variable_param_vals = get_variable_param_vals(
    #         t = t,
    #         inst_data = data_sets[[i]],
    #         algo = sim_algos[[i]]
    #       ),
    #       #prices, ## For now only price data is allowed as input for rules
    #       algo = sim_algos[[i]]#,
    #       #rule_function
    #       #trade_system$signal,
    #     )
    #     # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
    #
    #   }
    #
    # }

    raw_signals_dfs <- list()
    for(i in seq_along(rule_variations)) {
      raw_signals_df <- data.frame()
      for(j in seq_along(instruments)){
        for(t in (min_periods + 1):num_rows) {
          ID <- length(instruments) * (i - 1) + j
          raw_signals_df[t, j] <- generate_signal(
            variable_param_vals = get_variable_param_vals(
              t = t,
              inst_data = data_sets[[j]],
              algo = sim_algos[[ID]]
            ),
            #prices, ## For now only price data is allowed as input for rules
            algo = sim_algos[[ID]]#,
            #rule_function
            #trade_system$signal,
          )[[1]]
        }
      }
      raw_signals_dfs[[i]] <- raw_signals_df
    }



    ## Each raw_signals data frame contains columns of signal vectors for one
    ## rule applied to all instruments, one column for each instrument.
    ## Calculate cross section medians across all instruments for each
    ## rule.
    ## Columns are vectors of cross section medians across all instruments for
    ## each rule.
    ## Each column in cs_medians corresponds to a rule.
    ## Rows are observations (oldest to newest, so higher row numbers mean newer).
    cs_medians <- data.frame()
    for(i in seq_along(raw_signals_dfs)) {
      for(j in (min_periods + 1):num_rows) {
        k <- j - min_periods
        cs_medians[k, i] <- stats::median(unlist(raw_signals_dfs[[i]][j, ])) ## Row median
      }
    }


    ## Take mean absolute value of each vector of medians
    mav_for_each_rule <- lapply(
      cs_medians,
      #function(x) {unlist(mean(abs(x)))}
      function(x) {mean(abs(x))}
    )

    factors <- as.list(target / unlist(mav_for_each_rule, use.names = FALSE))
    #target / mav_for_each_rule

    names(factors) <- unlist(get_unique_rule_names_from_parsed_algos_list(parsed_algos))
    factors
  }

  pool_class <-  function() {
    "pool_class() not yet implemented."
  }

  switch(
    method,
    "equal" = equal(
      parsed_algos,
      ...),
    "pool_traded" = pool_traded(
      signal_tables,
      parsed_algos),
    "pool_all" = pool_all(
      instrument_data_sets,
      ...),
    "median_pool_all" = median_pool_all(
      parsed_algos,
      instrument_data_sets,
      ...),
    "pool_class" = pool_class()
  )
}


#' Get Values Of Variable Paramaters
#'
#' @param t
#' @param inst_data Instrument data set as data frame
#' @param algo Algo
#'
#' @return Named list of parameter values
#' @export
#'
#' @examples
get_variable_param_vals <- function(t, inst_data, algo) {
  c(
    ## t must be the first variable param
    list(t = t),
    ## Exclude t, which we get as the system is running
    inst_data[names(algo$rule$variable_params[-1])]
  )
}


#' Get Position Midifier Variable Parameter Values
#'
#' @param t Time index
#' @param position_size_ccy Position size in currency
#' @param inst_data Instrument data frame
#' @param position_modifier Position modifier
#' @param pos_table_vars Variables passed from update_position_table_row()
#'
#' @return Named list of param values
#' @export
#'
#' @examples
get_pos_mod_var_param_vals <- function(
    t,
    position_size_ccy,
    inst_data,
    position_modifier,
    pos_table_vars
  ) {

  ## If a variable param in position_modifier exists as a column in the
  ## instrument data, get the data from that column. Otherwise the param
  ## must match a variable assigned inside update_posisition_table_row()
  ## before modify_position().

  ## Get the variable params which match data columns
  pos_mod_params_in_data <- intersect(
    names(position_modifier$variable_params[-c(1, 2)]),
    colnames(inst_data)
  )

  ## Get the variable param names which don't match any data columns
  pos_mod_param_names_not_in_data <- setdiff(
    names(position_modifier$variable_params[-c(1, 2)]),
    colnames(inst_data)
  )

  ## Create variables from the param names
  load_pos_mod_params_not_in_data <- function(
      pos_mod_param_names_not_in_data,
      pos_table_vars
    ) {
    vars <- lapply(
      pos_mod_param_names_not_in_data,
      function(x) {eval(parse(text = paste0("pos_table_vars$", x)))}
    )
    names(vars) <- pos_mod_param_names_not_in_data
    vars
  }

  pos_mod_params_not_in_data <- load_pos_mod_params_not_in_data(
    pos_mod_param_names_not_in_data,
    pos_table_vars
  )

  c(
    ## t must be the first variable param
    ## position_size_ccy must be the second variable param
    list(t = t, position_size_ccy = position_size_ccy),
    ## t and position_size_ccy, which we get as the system is running, are
    ## excluded
    inst_data[pos_mod_params_in_data],
    #inst_data[names(position_modifier$variable_params[-c(1, 2)])]
    ## Variable params which don't correspond to columns in the instrument
    ## data frame, must be available inside update_position_table_row()
    pos_mod_params_not_in_data
  )
}


#' Get Position Multiplier Variable Parameter Values
#'
#' @param t Time index
#' @param position_size_ccy Position size in currency
#' @param inst_data Instrument data frame
#' @param position_multiplier Position multiplier from parsed multiplier list
#' @param pos_table_vars Variables passed from update_position_table_row()
#'
#' @return Nested list of param values for each multiplier function
#' @export
#'
#' @examples
get_pos_mul_var_param_vals <- function(
    t,
    position_size_ccy,
    inst_data,
    position_multipliers,
    pos_table_vars
) {
  ## If a variable param in position_multiplier exists as a column in the
  ## instrument data, get the data from that column. Otherwise the param
  ## must match a variable assigned inside update_posisition_table_row()
  ## before modify_position() - and if it is available inside
  ## update_posisition_table_row() before modify_position(), it should also be
  ## available inside modify_position() before multiply_position().

  pos_mul_var_param_vals <- list()
  for(i in seq_along(position_multipliers)) {
    ## Get the variable params which match data columns
    position_multiplier <- position_multipliers[[i]]
    pos_mul_params_in_data <- intersect(
      names(position_multiplier$variable_params[-c(1)]),
      colnames(inst_data)
    )

    ## Get the variable param names which don't match any data columns
    pos_mul_param_names_not_in_data <- setdiff(
      names(position_multiplier$variable_params[-c(1)]),
      colnames(inst_data)
    )

    ## Create variables from the param names
    load_pos_mul_params_not_in_data <- function(
      pos_mul_param_names_not_in_data,
      pos_table_vars
    ) {
      vars <- lapply(
        pos_mul_param_names_not_in_data,
        function(x) {eval(parse(text = paste0("pos_table_vars$", x)))}
      )
      names(vars) <- pos_mul_param_names_not_in_data
      vars
    }

    pos_mul_params_not_in_data <- load_pos_mul_params_not_in_data(
      pos_mul_param_names_not_in_data,
      pos_table_vars
    )

    pos_mul_var_param_vals[[i]] <- c(
      ## t must be the first variable param
      # ## position_size_ccy must be the second variable param
      # list(t = t, position_size_ccy = position_size_ccy),
      list(t = t),
      ## t and position_size_ccy, which we get as the system is running, are
      ## excluded
      inst_data[pos_mul_params_in_data],
      #inst_data[names(position_multipliers[[i]]$variable_params[-c(1, 2)])]
      ## Variable params which don't correspond to columns in the instrument
      ## data frame, must be available inside update_position_table_row()
      pos_mul_params_not_in_data
    )
  }


  ## TODO
  ## names??
  pos_mul_var_param_vals
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
#' @param prices A vector of prices in currency. Oldest first. Top to bottom:
#'   Older to newer. The last observation is time t.
#' @param signal_table Signal table. Last row is time t-1.
#' @param position_table Position table. Last row is time t-1.
#' @param algo Single algo from the expanded algos list.
#' @param rule_function
#' @param t
#'
#' @return Single signal value
#' @export
#'
#' @examples
# generate_signal <- function(
#     prices,
#     signal_table,
#     position_table,
#     algo, ## Single algo from the algos list
#     rule_function, ## Rule function
#     t,
#     config
#     #signal_table, ## signal table for the instrument
# ) {
#
#   # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
#   # fix§0014:
#   # Get the needed variables from the trade system.
#   # Pass the calculated variables back to trade system.
#   # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
#
#   ## Signal rule
#   raw_signal <- rule_function[[1]](
#     prices,
#     signal_table,
#     position_table,
#     t,
#     config
#   )
#
#   raw_signal
# }
generate_signal <- function(
    variable_param_vals, ## list. The first param must be t
    #signal_table,
    #position_table,
    algo#, ## Single parsed algo from the algos list
    #config
    #signal_table, ## signal table for the instrument
  ) {

  if(names(variable_param_vals[1]) != "t") {
    stop("The first variable parameter must be t.")
  }

  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
  # fix§0014:
  # Get the needed variables from the trade system.
  # Pass the calculated variables back to trade system.
  # §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

  ## Pass values from instrument data to variable params
  variable_params <- variable_param_vals
  ## Assign the variable names from algo to appropriate values
  ## (These names are actually not used, so redundant.)
  names(variable_params) <- algo$rule$variable_params

  params <- c(
    variable_params, ## assign variable params
    algo$rule$fixed_params ## fixed params
  )

  raw_signal <- do.call(
    algo$rule$signal_generator,
    params
  )

  raw_signal
}


#' Backfill Table With NAs
#'
#' @description
#' When a list of values is returned by a signal generator or a position
#'   modifier, and the list contains more elements than there are columns in
#'   the corresponding signal/position table, new columns are appended to the
#'   table and previous rows are backfilled with NAs in those columns.
#'
#' This situation occurs when a signal generator outputs values in addition to
#'   the trade signal, or when a position modifier outputs values in addition to
#'   the modified position.
#'
#' @param signal_table Signal table
#' @param new_row New row containing elements beyond the columns in the signal
#'   table
#'
#' @return A named list
#' @export
#'
#' @examples
backfill_table <- function(
    table,
    new_row
) {
  ## Create named list of only additional output
  new_cols <- new_row[
    setdiff(
      names(new_row), ## Exclude t
      colnames(table)
    )
  ]
  n_new_cols <- length(new_cols)
  n_rows <- nrow(table)
  ## backfill each element in the list with NAs
  backfilled_new_cols <- rep(
    list(
      rep(NA, n_rows)
    ),
    n_new_cols
  )
  names(backfilled_new_cols) <- names(new_cols)
  new_sig_tbl <- cbind(table, backfilled_new_cols)

  ###############
  ## Enter browser when backfilling position_table:
  # if(colnames(table)[3] == "instrument_risk") {
  #   browser()
  # }
  ###############

  rbind(new_sig_tbl, new_row)
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
      weights_by_instrument[i] <- 1 / num_signals[[i]]
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
  rep(1 / n, n)
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



#' Modify Position
#'
#' @description
#'
#'
#' @param variable_param_vals
#' @param position_modifier
#' @param position_size_ccy
#'
#' @return
#' @export
#'
#' @examples
modify_position <- function(
    position_modifier,
    mod_variable_param_vals,
    position_multipliers,
    mul_variable_param_vals,
    position_size_ccy,
    ...
  ) {

  ## Modify position if modifier list contains no NAs
  test_pos_mod <- function(position_modifier) {
    if(length(position_modifier) == 1) {
      !is.na(position_modifier[[1]])
    } else {
      !anyNA(position_modifier)
    }
  }

  ## Multiply position if multiplier list contains no NAs
  test_pos_mul <- function(position_multipliers) {
    # if(length(position_multipliers) == 1) {
    #   !is.na(position_multipliers[[1]])
    # } else {
      !anyNA(position_multipliers)
    # }
  }

  ## test_pos_mod() is TRUE if no elements in position_modifier is NA
  if(test_pos_mod(position_modifier)){
    if(names(mod_variable_param_vals[1]) != "t") {
      stop("The first variable parameter must be t.")
    }
    if(names(mod_variable_param_vals[2]) != "position_size_ccy") {
      stop("The second variable parameter must be position_size_ccy.")
    }

    ## Pass values from instrument data to variable params
    variable_params <- mod_variable_param_vals
    ## Assign the variable names from algo to appropriate values

    names(variable_params) <- names(position_modifier$variable_params)

    params <- c(
      variable_params, ## assign variable params
      position_modifier$fixed_params ## fixed params
    )

    position_modifier_output <- do.call(
      position_modifier$modifier_function,
      params
    )
  } else {
    position_modifier_output <- list(modifier_value = position_size_ccy)
  }

  if(test_pos_mul(position_multipliers)){
    position_multipliers_output <- multiply_position(
      mul_variable_param_vals,
      position_multipliers
    )
  } else {
    position_multipliers_output <- 1
  }

  ## Multiply modified position_size_ccy by multipliers
  modified_position_ccy <- position_modifier_output[[1]] * position_multipliers_output[[1]]

  modifiers_output <- c(
    list(modified_position_ccy = modified_position_ccy),
    position_modifier_output,
    position_multipliers_output
  )

  make_list_names_unique(modifiers_output)
}



#' Multiply Position By Multiplier Functions
#'
#' @description
#' Apply multipliers from parsed list of position multipliers to a single
#'   position.
#'
#' @param mult_variable_param_vals Multiplier variable param values
#' @param position_multipliers A list of multipliers affecting a single
#'   instrument position
#'
#' @return List of multiplier function outputs. The first element must be the
#'   multiplier value.
#' @export
#'
#' @details
#' Note: If different multiplier functions are applied to the same instrument,
#'   and these functions have elements in their outputs with identical names,
#'   the column names in `position_table` will be modified. E.g.
#'   if function `f1` outputs `list(combined_multiplier = 2, x = 10, y = 20)`
#'   and function `f2` outputs `list(combined_multiplier = 3, x = 10, z = 20)`,
#'   then this will result in the following columns in `position_table`:
#'   `combined_multiplier`, `x`, `y`, `x.1` and `z`.
#'
#' @examples
multiply_position <- function(
    mult_variable_param_vals,
    position_multipliers
  ) {

  mult_variable_params <- list()
  mult_params <- list()
  position_multipliers_output <- list()
  position_multiplier_additional_output <- list()

  ## Assign names as dummy values
  position_multiplier_values <- get_multiplier_names_from_multiplier_list(
    position_multipliers
  )
  names(position_multiplier_values) <- position_multiplier_values

  for(i in seq_along(position_multipliers)) {
    if(!anyNA(position_multipliers[[i]])) {
      # if(names(mod_variable_param_vals[1]) != "t") {
      #   stop("The first variable parameter must be t.")
      # }
      mult_variable_params[[i]] <- list()
      mult_params[[i]] <- list()

      ## Pass values from instrument data to variable params
      mult_variable_params[[i]] <- mult_variable_param_vals[[i]]

      ## Assign the variable names from algo to appropriate values
      names(mult_variable_params[[i]]) <- names(
        position_multipliers[[i]]$variable_params
      )

      mult_params[[i]] <- c(
        mult_variable_params[[i]], ## assign variable params
        position_multipliers[[i]]$fixed_params ## fixed params
      )

      position_multipliers_output[[i]] <- do.call(
        position_multipliers[[i]]$multiplier_function,
        mult_params[[i]]
      )

      position_multiplier_values[[i]] <- position_multipliers_output[[i]][[1]]
      position_multiplier_additional_output[[i]] <- position_multipliers_output[[i]][-1]

    } else {
      position_multiplier_values[[i]] <- 1
      position_multiplier_additional_output[[i]] <- list()
    }
  }

  c(
    ## Multiply all multiplier values for one instrument
    list(combined_multiplier = prod(unlist(position_multiplier_values))),
    position_multiplier_values,
    rlang::flatten(position_multiplier_additional_output)
  )
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
#' Get latest price from data frame in trade system list according to algo.
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
#' Get latest time from data frame in `algo` list.
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


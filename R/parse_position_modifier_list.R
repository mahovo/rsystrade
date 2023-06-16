#' Parse List Of Position Modifiers
#'
#' @description
#' Takes a named list of position modifiers and parses it to an appropriate
#'   format for the system. Names are the names of instruments.
#'
#' @param position_modifiers Named list.
#'
#' @details
#' Position modifier functions must comply with these specifications:
#'  * The first parameter must be `t`, current time index. `t` is provided by
#'      `update_position_table_row()`.
#'  * The second parameter must be `position_size_ccy`. `position_size_ccy` is
#'      provided by `update_position_table_row()`.
#'  * The first two parameters are variable parameters. This means that they are
#'      provided with data as the system is being updated. Parameters that are not
#'      fixed are variable. I.e. any parameters that are not provided with a value
#'      when running `make_system()`, is a variable parameter.
#'  * Any parameters that are provided with a value when running `make_system()`,
#'      is a fixed parameter.
#'  * The output of a modifier function must be a list, where the first element
#'      is `modified_position_size_ccy`. Any additional elements will appear in
#'      the `position_table` as columns. These will typically be intermediate
#'      values calculated by the modifier function.
#'
#' Additional valid params available from the system are:
#'
#'  * prices
#'  * instrument_risk
#'  * rescaled_combined_signal
#'  * clamped_combined_signal
#'  * required_leverage_factor
#'  * subsystem_position
#'  * notional_exposure
#'  * target_position_size_units
#'  * position_size_units
#'  * position_size_ccy
#'  * t_last_position_entry
#'  * latest_trade_direction
#'  * trade_on
#'  * direction
#'
#' The input format of the `position_modifiers` list provided to `make_system()`
#'   is (example):
#' ```R
#' position_modifiers <- list(
#'   list(
#'     instruments = list("inst1", "inst2", "inst3"),
#'     modifier = list(
#'       "f1",
#'       f1,
#'      y1 = 10
#'    )
#'  )
#' )
#' ```
#' This will be parsed into a list formated as
#' ```R
#' position_modifiers <- list(
#'   inst1 = list(
#'     modifier_name = "f1",
#'     modifier_function = f1,
#'     variable_params = list(
#'       x1 = "x1"
#'     ),
#'     fixed_params = list(
#'       y1 = 10
#'     )
#'   ),
#'   inst2 = list(
#'     modifier_name = "f1",
#'     modifier_function = f1,
#'     variable_params = list(
#'       x1 = "x1"
#'     ),
#'     fixed_params = list(
#'       y1 = 10
#'     )
#'   )
#' )
#' ```
#' First element in each input `position_modifiers[[i]]$modifier` list is the
#'   modifier name, second element is the modifier function definition itself.
#'   Remaining elements are the fixed params.
#'
#' Note: You may provide any name as modifier name. If the modifier
#'   function is assigned to a variable, you may put this variable as the
#'   function definition. So the function name doesn't have to match the
#'   modifier name. The modifier name is just a label for your convenience.
#'
#' Note: Variable params are not provided by user, i.e. do not appear
#'   in modifier list.
#'
#' If the list of data sets only contains `"inst1"` and `"inst2"`, then `"inst3"`
#'   will be ignored when parsing the `position_modifiers` list.
#'
#' @return Named list.
#' @export
#'
#' @examples
parse_position_modifiers <- function(position_modifiers) {
  parsed_posmod_list <- list()
  k <- 1
  for(i in seq_along(position_modifiers)) {
    modifier <- position_modifiers[[i]]$modifier
    modifier_name <- modifier[[1]]
    modifier_function <- modifier[[2]]
    ## Exclude modifier name and modifier function itself from list.
    fixed_params <- modifier[-c(1, 2)]
    fixed_params_names <- names(fixed_params)
    variable_params <- {
      x = names(formals(modifier_function))
      setdiff(x, fixed_params_names)
    }## All params that are not fixed
    names(variable_params) <- variable_params

    for(j in seq_along(position_modifiers[[i]]$instruments)) {
      parsed_posmod_list[[k]] <- list(
        modifier_name = modifier_name,
        modifier_function = modifier_function,
        variable_params = variable_params,
        fixed_params = fixed_params
      )
      names(parsed_posmod_list)[k] <- position_modifiers[[i]]$instruments[[j]]
      k <- k + 1
    }
  }
  inst_names_by_posmod <- get_inst_names_by_position_modifier(parsed_posmod_list)
  if(length(inst_names_by_posmod) != length(unique(inst_names_by_posmod))) {
    stop("Only one position modifier function may be provided for each instrument.")
  }

  parsed_posmod_list
}


#' Make List Of Position Modifiers From Position Modifiers List
#'
#' @description
#' Creates a named list. Values are all `NA` and the names are the names of
#'   instruments (or any vector of character strings provided as input).
#'
#' @param position_modifiers User provided named list of position modifier
#'   functions.
#' @param inst_names Vector of instrument names.
#'
#' @details
#' Initialize list of position modifiers. User may provide a named list where
#'   the names are instrument names and values are position modifier functions.
#'
#' `make_position_modifiers_list()` creates a named list. Each element corresponds
#'   to an instrument, in the order instruments occur in the inst_data list. The
#'   name of each element is the name of the instrument. User provided position
#'   modifier functions will be assigned to the appropriate element in the list.
#'
#' Any position modifier function for which the name doesn't match any
#'   instrument in the system, will be ignored.
#'
#' If no position modifier function is assigned to an instrument, the position
#'   of that instrument will not be modified.
#'
#' Position modifier functions must comply with these specifications:
#'  * The first parameter must be `t`, current time index. `t` is provided by
#'    `update_position_table_row()`.
#'  * The second parameter must be `position_size_ccy`. `position_size_ccy` is
#'    provided by `update_position_table_row()`.
#'  * The first two parameters are variable parameters. This means that they are
#'    provided with data as the system is being updated. Parameters that are not
#'    fixed are variable. I.e. any parameters that are not provided with a value
#'    when running `make_system()`, is a variable parameter.
#'  * Any parameters that are provided with a value when running `make_system()`,
#'    is a fixed parameter.
#'
#' The input format of the `position_modifiers` list provided to `make_system()`
#'   is (example):
#' ```R
#' position_modifiers <- list(
#'   list(
#'     instruments = list("inst1", "inst2", "inst3"),
#'     modifier = list(
#'       "f1",
#'       f1,
#'      y1 = 10
#'    )
#'  )
#' )
#' ```
#' This will be parsed into a list formated as
#' ```R
#' position_modifiers <- list(
#'   inst1 = list(
#'     modifier_name = "f1",
#'     modifier_function = f1,
#'     variable_params = list(
#'       x1 = "x1"
#'     ),
#'     fixed_params = list(
#'       y1 = 10
#'     )
#'   ),
#'   inst2 = list(
#'     modifier_name = "f1",
#'     modifier_function = f1,
#'     variable_params = list(
#'       x1 = "x1"
#'     ),
#'     fixed_params = list(
#'       y1 = 10
#'     )
#'   )
#' )
#' ```
#' First element in each input `position_modifiers[[i]]$modifier` list is the
#' modifier name, second element is the modifier function definition itself.
#' Remaining elements are the fixed params.
#' Note: You may provide any name as modifier name. If the modifier
#' function is assigned to a variable, you may put this variable as the
#' function definition. So the function name doesn't have to match the
#' modifier name. The modifier name is just a label for your convenience.
#' Note: Variable params are not provided by user, i.e. do not appear
#' in modifier list.
#'
#' If the list of data sets only contains `"inst1"` and `"inst2"`, then `"inst3"`
#'   will be ignored when parsing the `position_modifiers` list.
#'
#' @return Named list
#' @export
#'
#' @examples
make_position_modifiers_list <- function(
    position_modifiers = list(),
    inst_names
  ) {

  initialize_pos_mods <- function() {
    n <- length(inst_names)
    init_pos_mods <- as.list(rep(NA, n))
    names(init_pos_mods) <- inst_names
    init_pos_mods
  }
  make_pos_mods <- function() {
    init_pos_mods <- initialize_pos_mods()
    parsed_pos_mods <- parse_position_modifiers(position_modifiers)

    for(i in seq_along(parsed_pos_mods)) {
      pos_mod <- names(parsed_pos_mods[i])

      if(!is.null(init_pos_mods[[pos_mod]])) {
        init_pos_mods[pos_mod] <- parsed_pos_mods[pos_mod]
      }
    }

    ## Position modifier functions have now been assigned to the init_pos_mods
    ## list where provided.
    init_pos_mods
  }

  if(length(position_modifiers) == 0) {
    initialize_pos_mods()
  } else {
    # if(is.null(names(position_modifiers))) {
    #   stop("Provided list of position modifier functions must be named. Each name should correspond to an instrument name.")
    # }
    make_pos_mods()
  }
}


#' Get Instrument Names From List Of Position Modifiers
#'
#' @param parsed_posmod_list
#'
#' @return
#' @export
#'
#' @examples
get_inst_names_by_position_modifier <- function(parsed_posmod_list) {
  ## Collect all instrument names from parsed_posmod_list
  instrument_names <- list()
  for(i in seq_along(parsed_posmod_list)) {
    instrument_names[i] <- names(parsed_posmod_list[i])
  }

  ## Make list of only instrument names
  instrument_names
}


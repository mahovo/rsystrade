## NOTE ON CODE STRUCTURE
##
## Yes, position modifier lists and position multiplier lists are very similar
## and their associated functions have a lot of similarities which could
## probably be generalized. But it is not expected that many new functions - if
## any - of this type will be written in the future, so it a bit of copy/paste
## seems to be the simplest way to do it. Definitely not basis for switch to
## OOP. But if these functions need to be modified a lot, a more general
## structure could be contemplated in the process.
##
## Writing more general functions would be easy, when you realize that:
## While multipliers are a special case of modifiers, modifier *functions* are
## special cases of multiplier *functions*. That is because there can only be
## one modifier for each instrument, while the number of multipliers is
## unbounded. So for-loops in functions related to multipliers - such as parsers
## etc - (not "position multiplier functions"" per se!) could also be made to
## work for modifiers, where the loops would just be redundant.
##
## The reason NOT just to implement general functions is, that the modifier
## lists would need a redundant level of nesting, which might be annoying to the
## user (indexing a list that could only ever have one element). However, you
## could also make the case that it would be more user friendly if modifier
## lists and multiplier lists followed the same structure, even if modifier
## lists are limited to one modifier.


#' Expand Nested List Of Position Multipliers
#'
#' @description
#' Takes a named list of position multipliers and expands it to an appropriate
#'   format for the system. Names are the names of corresponding instruments.
#'
#' @param position_multipliers Named list.
#'
#' @details
#' Position multiplier functions must comply with these specifications:
#'  * Any parameter that is provided with a value when running `make_system()`,
#'      is a fixed parameter.
#'  * Parameters that are not fixed are variable params. I.e. any parameter
#'      that is not provided with a value when running `make_system()`, is a
#'      variable parameter.
#'  * The output of a multiplier function must be a list, where the first element
#'      is `multiplier_value`. Any additional elements will appear in
#'      the `position_table` as columns. These will typically be intermediate
#'      values calculated by the multiplier function.
#'
#' Additional valid params are:
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
#'  * position_size_ccy
#'
#' In other words, position multiplier functions take the same params as
#'    position modifiers, plus `position_size_ccy`, which is provided to
#'    `multiply_position()` by `modify_position()` when `modify_position()`
#'    calls `multiply_position()`.
#'
#' Note that `multiply_position()` should only ever be called by
#'   `modify_position()`. (This is not enforced.)
#'
#' IMPORTANT: While only one *position modifier* may be provided to each
#'   instrument, a list of *position multipliers* may be provided. So be very
#'   careful to observe the nesting when providing a list of position
#'   multipliers.
#'
#' If an instrument name appears more than once, all multipliers associated with
#'   that name will be merged into one list.
#'
#' Duplicate multiplier names in combination with the same instrument name are
#'   not allowed and will throw an error. Note that even if the multiplier names
#'   must be unique, rsystrade does not check that the corresponding multiplier
#'   functions are unique. So it is possible to assign the same function to
#'   different multiplier names. E.g. if you define multiplier `"mult1"` as a
#'   function that doubles the position size, and `"mult2"` as a
#'   function that also doubles the position size, the effect will be to
#'   multiply the position size by 4. If this type of behaviour is not your
#'   intention, don't use duplicate multiplier functions for the same instrument
#'   (subsystem).
#'
#' Note: Each position multiplier list in the list of multipliers follows the
#'   same structure as a position modifier: First element in each input
#'   `position_multipliers[[i]]$multipliers[[m]]` list is the multiplier name,
#'   second element is the multiplier function definition itself. Remaining
#'   elements are the fixed params.
#'
#' Note: You may provide any name as multiplier name. If the multiplier
#' function is assigned to a variable, you may put this variable as the
#' function definition. So the function name doesn't have to match the
#' multiplier name. The multiplier name is just a label for your convenience.
#'
#' Note: Variable params are not provided by user, i.e. do not appear
#' in multiplier list.
#'
#' If the list of data sets only contains `"inst1"` and `"inst2"`, then `"inst3"`
#'   will be ignored when parsing the `position_multipliers` list.
#'
#' The input format of the `position_multipliers` list provided to
#'   `make_system()` is (example):
#' ```R
#' position_multipliers <- list(
#'   list(
#'     instruments = list("inst1", "inst2"),
#'     multipliers = list(
#'      list(
#'        "f1",
#'         f1,
#'         y1 = 10
#'      ),
#'      list(
#'        "f2",
#'         f2,
#'         y2 = 20
#'      )
#'     )
#'   ),
#'   list(
#'     instruments = list("inst3", "inst4"),
#'     multipliers = list(
#'      list(
#'        "f3",
#'         f3,
#'         y3 = 30
#'      ),
#'      list(
#'        "f4",
#'         f4,
#'         y4 = 40
#'      )
#'     )
#'   )
#' )
#' ```
#' This will be expanded into a list formatted as
#' ```R
#' position_multipliers <- list(
#'   inst1 = list(
#'    list(
#'      multiplier_name = "f1",
#'      multiplier_function = f1,
#'      variable_params = list(
#'         x1 = "x1"
#'      ),
#'      fixed_params = list(
#'         y1 = 10
#'      )
#'    ),
#'    list(
#'      multiplier_name = "f2",
#'      multiplier_function = f2,
#'      variable_params = list(
#'         x2 = "x2"
#'      ),
#'      fixed_params = list(
#'         y2 = 20
#'      )
#'    )
#'   ),
#'   inst2 = list(
#'    list(
#'      multiplier_name = "f1",
#'      multiplier_function = f1,
#'      variable_params = list(
#'         x1 = "x1"
#'      ),
#'      fixed_params = list(
#'         y1 = 10
#'      )
#'    ),
#'    list(
#'      multiplier_name = "f2",
#'      multiplier_function = f2,
#'      variable_params = list(
#'         x2 = "x2"
#'      ),
#'      fixed_params = list(
#'         y2 = 20
#'      )
#'    )
#'   ),
#'   inst3 = list(
#'    list(
#'      multiplier_name = "f3",
#'      multiplier_function = f3,
#'      variable_params = list(
#'         x3 = "x3"
#'      ),
#'      fixed_params = list(
#'         y3 = 30
#'      )
#'    ),
#'    list(
#'      multiplier_name = "f4",
#'      multiplier_function = f4,
#'      variable_params = list(
#'         x4 = "x4"
#'      ),
#'      fixed_params = list(
#'         y4 = 40
#'      )
#'    )
#'   ),
#'   inst4 = list(
#'    list(
#'      multiplier_name = "f3",
#'      multiplier_function = f3,
#'      variable_params = list(
#'         x3 = "x3"
#'      ),
#'      fixed_params = list(
#'         y3 = 30
#'      )
#'    ),
#'    list(
#'      multiplier_name = "f4",
#'      multiplierr_function = f4,
#'      variable_params = list(
#'         x4 = "x4"
#'      ),
#'      fixed_params = list(
#'         y4 = 40
#'      )
#'    )
#'   )
#' )
#' ```
#'
#' @return Named list.
#' @export
#'
#' @examples
# expand_position_multipliers <- function(position_multipliers) {
#   expanded_posmul_list <- list()
#   k <- 1
#   for(i in seq_along(position_multipliers)) {
#
#     ## For each  user provided position multiplier list,
#     ## cycle through all the instruments
#     for(j in seq_along(position_multipliers[[i]]$instruments)) {
#       expanded_posmul_list[[k]] <- list()
#
#       ## For each instrument in a user provided position multiplier list,
#       ## cycle through all the multipliers
#       for(m in seq_along(position_multipliers[[i]]$multipliers)) {
#         multiplier <- position_multipliers[[i]]$multipliers[[m]]
#         multiplier_name <- multiplier[[1]]
#         multiplier_function <- multiplier[[2]]
#         ## Exclude multiplier name and multiplier function itself from list.
#         fixed_params <- multiplier[-c(1, 2)]
#         fixed_params_names <- names(fixed_params)
#         ## Variable params are all params that are not fixed
#         variable_params <- {
#           x = names(formals(multiplier_function))
#           setdiff(x, fixed_params_names)
#         }
#         names(variable_params) <- variable_params
#         expanded_posmul_list[[k]][[m]] <- list(
#           multiplier_name = multiplier_name,
#           multiplier_function = multiplier_function,
#           variable_params = variable_params,
#           fixed_params = fixed_params
#         )
#
#
#       }
#
#
#       ## The names of the elements in the expanded position multipliers list, are
#       ## instrument names
#       names(expanded_posmul_list)[k] <- position_multipliers[[i]]$instruments[[j]]
#       k <- k + 1
#
#
#     }
#
#
#
#   }
#
#   #inst_names_by_posmul <- get_inst_names_by_position_multiplier(expanded_posmul_list)
#
#   expanded_posmul_list
# }
expand_position_multipliers <- function(position_multipliers) {

  ## Get instrument names from user-provided nested (not parsed) list of position
  ## multipliers.
  ## These functions are not public because they are applied to non-expanded
  ## lists.
  get_instrument_names <- function(position_multipliers) {
    inst_names <- list()
    k <- 1
    for(i in seq_along(position_multipliers)) {
      for(j in seq_along(position_multipliers[[i]])) {
        inst_names[[k]] <- position_multipliers[[i]]$instruments[[j]]
        k <- k + 1
      }
    }
    inst_names
  }
  get_unique_instrument_names <- function(position_multipliers) {
    inst_names <- get_instrument_names(position_multipliers)
    unique(inst_names)
  }

  expanded_posmul_list <- list()

  unique_instrument_names <- get_unique_instrument_names(position_multipliers)



  ## For each  user provided position multiplier list,
  ## cycle through all the unique instruments
  for(k in seq_along(unique_instrument_names)) {
    expanded_posmul_list[[k]] <- list()

    unique_instrument_name <- unique_instrument_names[[k]]

    for(i in seq_along(position_multipliers)) {


      #expanded_posmul_list[[k]] <- list()

      for(j in seq_along(position_multipliers[[i]]$instruments)) {
        ## For each instrument in a user provided position multiplier list,
        ## cycle through all the multipliers
        instrument_name <- position_multipliers[[i]]$instruments[[j]]

        if(instrument_name == unique_instrument_name) {
          for(m in seq_along(position_multipliers[[i]]$multipliers)) {
            multiplier <- position_multipliers[[i]]$multipliers[[m]]
            multiplier_name <- multiplier[[1]]
            multiplier_function <- multiplier[[2]]
            ## Exclude multiplier name and multiplier function itself from list.
            fixed_params <- multiplier[-c(1, 2)]
            fixed_params_names <- names(fixed_params)
            ## Variable params are all params that are not fixed
            variable_params <- {
              x = names(formals(multiplier_function))
              setdiff(x, fixed_params_names)
            }

            names(variable_params) <- variable_params

            expanded_posmul_list[[k]] <- c(
                if(length(expanded_posmul_list[[k]]) > 0) {
                  expanded_posmul_list[[k]]
                } else {list()},
                list(
                  list(
                    multiplier_name = multiplier_name,
                    multiplier_function = multiplier_function,
                    variable_params = variable_params,
                    fixed_params = fixed_params
                  )
                )
              )
          }

        }
      }
    }

    ## Check that names of multipliers associated with a single instrument are
    ## unique
    all_mult_names <- get_multiplier_names_from_multiplier_list(
      expanded_posmul_list[[k]]
    )
    unique_mult_names <- get_unique_multiplier_names_from_multiplier_list(
      expanded_posmul_list[[k]]
    )
    if(length(all_mult_names) != length(unique_mult_names)) {
      stop("All multiplier names associated with a single instrument (subsystem)
           must be unique.")
    }
  }

  names(expanded_posmul_list) <- unique_instrument_names

  expanded_posmul_list
}



#' Parse Nested List Of Position Multipliers
#'
#' @description
#' Creates a named list. Values are all `NA` and the names are the names of
#'   instruments (or any vector of character strings provided as input).
#'
#' @param position_multipliers User provided nested named list of position
#'   multiplier functions.
#' @param inst_names Vector of instrument names.
#'
#' @details
#' Initialize list of position multipliers. User may provide a named list where
#'   the names are instrument names and values are position multiplier functions.
#'
#' `parse_position_multifiers_list()` creates a named list. Each element
#'   corresponds to an instrument, in the order instruments occur in the
#'   inst_data list. The name of each element is the name of the instrument.
#'   User provided position multiplier functions will be assigned to the
#'   appropriate element in the list.
#'
#' Any position multiplier function for which the name doesn't match any
#'   instrument in the system, will be ignored.
#'
#' If no position multiplier function is assigned to an instrument, the position
#'   of that instrument will not be multiplied.
#'
#' Position multiplier functions must comply with these specifications:
#'  * The first parameter must be `t`, current time index. `t` is provided by
#'    `modify_position()`.
#'  * The second parameter must be `position_size_ccy`. `position_size_ccy` is
#'    provided by `modify_position()`. `position_size_ccy` may be a modified
#'    position calculated by a position modifier function in `modify_position()`,
#'    not necessarily the raw  `position_size_ccy` input to `modify_position()`.
#'  * The first two parameters are variable parameters. This means that they are
#'    provided with data as the system is being updated. Parameters that are not
#'    fixed are variable. I.e. any parameters that are not provided with a value
#'    when running `make_system()`, is a variable parameter.
#'  * Any parameters that are provided with a value when running `make_system()`,
#'    is a fixed parameter.
#'
#' First element in each input `position_multipliers[[i]]$multiplier[[m]]` list
#'    is the multiplier name, second element is the multiplier function
#'    definition itself. Remaining elements are the fixed params.
#'
#' Note: You may provide any name as multiplier name. If the multiplier
#'   function is assigned to a variable, you may put this variable as the
#'   function definition. So the function name doesn't have to match the
#'   multiplier name. The multiplier name is just a label for your convenience.
#'
#' Note: Variable params are not provided by user, i.e. do not appear
#'   in multiplier list.
#'
#' If the list of data sets only contains `"inst1"` and `"inst2"`, then `"inst3"`
#'   will be ignored when parsing the `position_multipliers` list.
#'
#' The input format of the `position_multipliers` list provided to
#'   `make_system()` is (example):
#' ```R
#' position_multipliers <- list(
#'   list(
#'     instruments = list("inst1", "inst2"),
#'     multipliers = list(
#'      list(
#'        "f1",
#'         f1,
#'         y1 = 10
#'      ),
#'      list(
#'        "f2",
#'         f2,
#'         y2 = 20
#'      )
#'     )
#'   ),
#'   list(
#'     instruments = list("inst3", "inst4"),
#'     multipliers = list(
#'      list(
#'        "f3",
#'         f3,
#'         y3 = 30
#'      ),
#'      list(
#'        "f4",
#'         f4,
#'         y4 = 40
#'      )
#'     )
#'   )
#' )
#' ```
#'
#' This will be parsed into a list formatted as
#' ```R
#' position_multipliers <- list(
#'   inst1 = list(
#'    list(
#'      multiplier_name = "f1",
#'      multiplier_function = f1,
#'      variable_params = list(
#'         x1 = "x1"
#'      ),
#'      fixed_params = list(
#'         y1 = 10
#'      )
#'    ),
#'    list(
#'      multiplier_name = "f2",
#'      multiplier_function = f2,
#'      variable_params = list(
#'         x2 = "x2"
#'      ),
#'      fixed_params = list(
#'         y2 = 20
#'      )
#'    )
#'   ),
#'   inst2 = list(
#'    list(
#'      multiplier_name = "f1",
#'      multiplier_function = f1,
#'      variable_params = list(
#'         x1 = "x1"
#'      ),
#'      fixed_params = list(
#'         y1 = 10
#'      )
#'    ),
#'    list(
#'      multiplier_name = "f2",
#'      multiplier_function = f2,
#'      variable_params = list(
#'         x2 = "x2"
#'      ),
#'      fixed_params = list(
#'         y2 = 20
#'      )
#'    )
#'   ),
#'   inst3 = list(
#'    list(
#'      multiplier_name = "f3",
#'      multiplier_function = f3,
#'      variable_params = list(
#'         x3 = "x3"
#'      ),
#'      fixed_params = list(
#'         y3 = 30
#'      )
#'    ),
#'    list(
#'      multiplier_name = "f4",
#'      multiplier_function = f4,
#'      variable_params = list(
#'         x4 = "x4"
#'      ),
#'      fixed_params = list(
#'         y4 = 40
#'      )
#'    )
#'   ),
#'   inst4 = list(
#'    list(
#'      multiplier_name = "f3",
#'      multiplier_function = f3,
#'      variable_params = list(
#'         x3 = "x3"
#'      ),
#'      fixed_params = list(
#'         y3 = 30
#'      )
#'    ),
#'    list(
#'      multiplier_name = "f4",
#'      multiplierr_function = f4,
#'      variable_params = list(
#'         x4 = "x4"
#'      ),
#'      fixed_params = list(
#'         y4 = 40
#'      )
#'    )
#'   )
#' )
#' ```
#'
#' @return Named list
#' @export
#'
#' @examples
parse_position_multipliers_list <- function(
    position_multipliers = list(),
    inst_names
) {

  initialize_pos_muls <- function() {
    n <- length(inst_names)
    init_pos_muls <- as.list(rep(NA, n))
    names(init_pos_muls) <- inst_names
    init_pos_muls
  }
  parse_pos_muls <- function() {
    init_pos_muls <- initialize_pos_muls()
    expanded_pos_muls <- expand_position_multipliers(position_multipliers)

    for(i in seq_along(expanded_pos_muls)) {
      pos_mul <- names(expanded_pos_muls[i])

      if(!is.null(init_pos_muls[[pos_mul]])) {
        init_pos_muls[pos_mul] <- expanded_pos_muls[pos_mul]
      }
    }

    ## Position multiplier functions have now been assigned to the init_pos_mods
    ## list where provided.
    init_pos_muls
  }

  if(length(position_multipliers) == 0) {
    initialize_pos_muls()
  } else {
    # if(is.null(names(position_multipliers))) {
    #   stop("Provided list of position multiplier functions must be named. Each name should correspond to an instrument name.")
    # }
    parse_pos_muls()
  }
}

#' Get Instrument Names From Parsed List Of Position Multipliers
#'
#' @param parsed_posmul_list
#'
#' @return
#' @export
#'
#' @examples
get_inst_names_by_position_multiplier <- function(parsed_posmul_list) {
  ## Collect all instrument names from parsed_posmod_list
  instrument_names <- list()
  for(i in seq_along(parsed_posmul_list)) {
    instrument_names[i] <- names(parsed_posmul_list[i])
  }

  ## Make list of only instrument names
  instrument_names
}

#' Get Multiplier Names From Parsed List Of Position Multipliers
#'
#' @param parsed_posmul_list
#'
#' @return
#' @export
#'
#' @examples
get_multiplier_names_from_multiplier_list <- function(parsed_posmul_list) {
  ## Collect all pultiplier names from parsed_posmod_list
  # multiplier_names <- list()
  # for(i in seq_along(parsed_posmul_list)) {
  #   multiplier_names[i] <- names(parsed_posmul_list[i])
  # }

  multiplier_names <- lapply(
    parsed_posmul_list,
    function(x) {x$multiplier_name}
  )

  ## Make list of only instrument names
  multiplier_names
}


#' Get Unique Multiplier Names From Parsed List Of Position Multipliers
#'
#' @param multiplier_list
#'
#' @return
#' @export
#'
#' @examples
get_unique_multiplier_names_from_multiplier_list <- function(parsed_posmul_list) {
  multiplier_names <- get_multiplier_names_from_multiplier_list(parsed_posmul_list)
  unique(multiplier_names)
}

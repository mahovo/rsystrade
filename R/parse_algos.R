
#' Parse algos list
#'
#' @description
#'
#' Loads data and rules into the system by parsing a list of algos.
#'
#' An algo is composed of one instrument (data) and one rule variation.
#'
#' Data sets must be csv files in a single folder. The system configuration must
#'   include a path to the data folder. Data sets are referenced by their file
#'   names (excluding extension) as character strings.
#'
#' A rule variation is a function that produces a trade signal. A
#'   *rule variation* is produced by a *signal generator*, which may implement
#'   several rules to produce one signal. The typical use case for multiple
#'   rules in one *signal generator* would by one rule generating an actual trade
#'   signal, and one rule triggering a position to be closed, e.g. a stop
#'   loss rule. (See vignette: `vignette(package = "rsystrade")`)
#'
#' `parse_algos()` takes a nested list of algo specifications and expands it to
#'   a list of all possible permutations of instrument names and rules. It then
#'   finally formats the list so that each element is a list containing one
#'   element named `instrument` containing an instrument name, and one element
#'   simply named `rule` containing
#'   -  one rule variation name
#'   -  one signal generator function, given as a function name which may be
#'      a character string or a variable to which the function is assigned.
#'      Note that the function name should not be followed by parentheses.
#'   -  One list of variable parameters. The first of these must always be `t`,
#'      which is provided by the system as it runs. Additional variable params,
#'      such as `price` will get their values from the instrument data sets.
#'   -  One list of fixed parameters. These are parameters to the signal
#'      generator function which are assigned a value.
#'
#' Note: In the input sublists we have `rules` (plural, indicating one or
#'   more), in the output sublists we have `rule` (singular).
#'
#'
#' In the the `algos` list, instruments must be provided as a path to a csv
#'   file. (For now. Eventually the way the `algos` list is parsed will depends
#'   on the mode, which can be specified as `sim` for _simulation mode_ or
#'   `live` for _live mode_.) Rule variations must be provided as character
#'   string. The *signal generator* name must match the name of a function which
#'   is loaded into the environment in which the system list is stored. In other
#'   words: You must define the signal generators in the same environment as the
#'   one where you call `make_system()` (typically the global environment).
#'   *Signal generator* functions included in the rsystrade package are loaded
#'   into the package env, which will also work.
#'
#' `make_system()` loads the unique data sets specified in the parsed algos into
#'   a sublist named `inst_data`.
#'
#' The `algos` parameter is a nested list in one in the following forms.
#'   Pairs of one instrument and one signal generator:
#'   ```R
#'   algos <- list(
#'     list( ## algo 1: algos[[1]]
#'       instruments = list("instr1"),
#'       rules = list(rule1)
#'     )
#'   )
#'   ```
#'   Or we can apply the same signal generator to multiple instruments:
#'   ```R
#'   algos <- list(
#'     list( ## algo 1: algos[[1]]
#'       instruments = list("instr1", "instr2"),
#'       rules = list(rule1)
#'     )
#'   )
#'   ```
#'   Or multiple rules to a single instrument.:
#'   ```R
#'   algos <- list(
#'     list( ## algo 1: algos[[1]]
#'       instruments = list("instr1"),
#'       rules = list(rule1, rule2)
#'     )
#'   )
#'   ```
#' In these examples `rule1` represents a rule variation. `rule2` represents
#'   another rule variation. An example of a rule variation might look like
#'   this:
#'   ```R
#'   list(
#'     "mac_2_4",
#'     r_mac,
#'     ma_fast = NA,
#'     ma_slow = NA,
#'     n_fast = 2L,
#'     n_slow = 4L,
#'     gap = 0,
#'     strict = TRUE,
#'     binary = FALSE
#'   )
#'   ```
#' In this example `r_mac` is the name of a function which takes `t` as the
#'   first parameter and `price` as the second. These are the *variable*
#'   *parameters*, which get input from the system. The remaining *fixed*
#'   *parameters* are specified in the remaining elements in the *rule*
#'   *variation list*.
#'
#' `r_mac` is the name of a *rule function*. We may use *rule functions* directly,
#'   provided that the interface conforms to the standardized interface of a
#'   *signal generator*. Alternatively we may use a *signal generator*, the name of
#'   which should be prefixed by `s_`. We may combine several *rule functions*
#'   in one *signal generator*.
#'
#' Subsets are then expanded to all possible permutations:
#'   ```R
#'   input <- list(
#'     list( ## algo 1: algos[[1]]
#'       instrument = "inst1",
#'       rule = list(
#'         rule_variation = "rule_variation_1",
#'         signal_generator = signal_generator_1,
#'         variable_params = variable_params_1,
#'         fixed_params = fixed_params_1,
#'       )
#'     ),
#'     list( ## algo 2: algos[[2]]
#'       instrument = "inst2",
#'       rule = list(
#'         rule_variation = "rule_variation_1",
#'         signal_generator = signal_generator_1,
#'         variable_params = variable_params_1,
#'         fixed_params = fixed_params_1,
#'       )
#'     )
#'     list( ## algo 3: algos[[3]]
#'       instrument = "inst1",
#'       rule = list(
#'         rule_variation = "rule_variation_2",
#'         signal_generator = signal_generator_2,
#'         variable_params = variable_params_2,
#'         fixed_params = fixed_params_2,
#'       )
#'     ),
#'     list( ## algo 4: algos[[4]]
#'       instrument = "inst2",
#'       rule = list(
#'         rule_variation = "rule_variation_2",
#'         signal_generator = signal_generator_2,
#'         variable_params = variable_params_2,
#'         fixed_params = fixed_params_2,
#'       )
#'     )
#'   )
#'   ```
#'
#' @param algos A nested list (see specification above)
#' @param mode `sim` for simulation mode or `live` for live trading mode.
#'
#' @return A list where each element is a list containing:
#'   -  `instrument` An instrument name. The instrument name is also assigned
#'      to the corresponding data frame as a comment.
#'   -  `rule` A list containing one rule variation list (see specification
#'      above.)
#'
#' @examples
parse_algos <- function(algos, mode = "sim") {
  if(mode == "sim") {

    ## Expand algos nested list to all permutations of algos.
    expanded_algos <- expand_algos(algos)

    ## Return list of all algos
    expanded_algos
  } else if (mode == "live") {
    stop("\"Live mode is not implemented yet.\"")
  } else {
    stop(
      paste("\nProvide a valid mode when running system().",
            "\nValid modes are \"sim\" (default) or \"live\".")
    )
  }
}

#' Expand algos
#'
#' @description
#' Takes a nested list of algo specifications and expands it to a list of all
#'   possible permutations.
#'
#' An algo is composed of one instrument (data) and one rule variation.
#'
#' Data sets must be csv files in a single folder. The system configuration must
#'   include a path to the data folder. Data sets are referenced by their file
#'   names (excluding extension) as character strings.
#'
#' A rule variation is a function that produces a trade signal. A
#'   *rule variation* is produced by a *signal generator*, which may implement
#'   several rules to produce one signal. The typical use case for multiple
#'   rules in one *signal generator* would by one rule generating an actual trade
#'   signal, and one rule triggering a position to be closed, e.g. a stop
#'   loss rule. (See vignette: `vignette(package = "rsystrade")`)
#'
#' `parse_algos()` takes a nested list of algo specifications and expands it to
#'   a list of all possible permutations of instrument names and rules. It then
#'   finally formats the list so that each element is a list containing one
#'   element named `instrument` containing an instrument name, and one element
#'   simply named `rule` containing
#'   -  one rule variation name
#'   -  one signal generator function, given as a function name which may be
#'      a character string or a variable to which the function is assigned.
#'      Note that the function name should not be followed by parentheses.
#'   -  One list of variable parameters. The first of these must always be `t`,
#'      which is provided by the system as it runs. Additional variable params,
#'      such as `price` will get their values from the instrument data sets.
#'   -  One list of fixed parameters. These are parameters to the signal
#'      generator function which are assigned a value.
#'
#' Note: In the input sublists we have `rules` (plural, indicating one or
#'   more), in the output sublists we have `rule` (singular).
#'
#'
#' In the the `algos` list, instruments must be provided as a path to a csv
#'   file. (For now. Eventually the way the `algos` list is parsed will depends
#'   on the mode, which can be specified as `sim` for _simulation mode_ or
#'   `live` for _live mode_.) Rule variations must be provided as character
#'   string. The *signal generator* name must match the name of a function which
#'   is loaded into the environment in which the system list is stored. In other
#'   words: You must define the signal generators in the same environment as the
#'   one where you call `make_system()` (typically the global environment).
#'   *Signal generator* functions included in the rsystrade package are loaded
#'   into the package env, which will also work.
#'
#' `make_system()` loads the unique data sets specified in the parsed algos into
#'   a sublist named `inst_data`.
#'
#' The `algos` parameter is a nested list in one in the following forms.
#'   Pairs of one instrument and one signal generator:
#'   ```R
#'   algos <- list(
#'     list( ## algo 1: algos[[1]]
#'       instruments = list("instr1"),
#'       rules = list(rule1)
#'     )
#'   )
#'   ```
#'   Or we can apply the same signal generator to multiple instruments:
#'   ```R
#'   algos <- list(
#'     list( ## algo 1: algos[[1]]
#'       instruments = list("instr1", "instr2"),
#'       rules = list(rule1)
#'     )
#'   )
#'   ```
#'   Or multiple rules to a single instrument.:
#'   ```R
#'   algos <- list(
#'     list( ## algo 1: algos[[1]]
#'       instruments = list("instr1"),
#'       rules = list(rule1, rule2)
#'     )
#'   )
#'   ```
#' In these examples `rule1` represents a rule variation. `rule2` represents
#'   another rule variation. An example of a rule variation might look like
#'   this:
#'   ```R
#'   list(
#'     "mac_2_4",
#'     r_mac,
#'     ma_fast = NA,
#'     ma_slow = NA,
#'     n_fast = 2L,
#'     n_slow = 4L,
#'     gap = 0,
#'     strict = TRUE,
#'     binary = FALSE
#'   )
#'   ```
#' In this example `r_mac` is the name of a function which takes `t` as the
#'   first parameter and `price` as the second. These are the *variable*
#'   *parameters*, which get input from the system. The remaining *fixed*
#'   *parameters* are specified in the remaining elements in the *rule*
#'   *variation list*.
#'
#' `r_mac` is the name of a *rule function*. We may use *rule functions* directly,
#'   provided that the interface conforms to the standardized interface of a
#'   *signal generator*. Alternatively we may use a *signal generator*, the name of
#'   which should be prefixed by `s_`. We may combine several *rule functions*
#'   in one *signal generator*.
#'
#' Subsets are then expanded to all possible permutations:
#'   ```R
#'   input <- list(
#'     list( ## algo 1: algos[[1]]
#'       instrument = "inst1",
#'       rule = list(
#'         rule_variation = "rule_variation_1",
#'         signal_generator = signal_generator_1,
#'         variable_params = variable_params_1,
#'         fixed_params = fixed_params_1,
#'       )
#'     ),
#'     list( ## algo 2: algos[[2]]
#'       instrument = "inst2",
#'       rule = list(
#'         rule_variation = "rule_variation_1",
#'         signal_generator = signal_generator_1,
#'         variable_params = variable_params_1,
#'         fixed_params = fixed_params_1,
#'       )
#'     )
#'     list( ## algo 3: algos[[3]]
#'       instrument = "inst1",
#'       rule = list(
#'         rule_variation = "rule_variation_2",
#'         signal_generator = signal_generator_2,
#'         variable_params = variable_params_2,
#'         fixed_params = fixed_params_2,
#'       )
#'     ),
#'     list( ## algo 4: algos[[4]]
#'       instrument = "inst2",
#'       rule = list(
#'         rule_variation = "rule_variation_2",
#'         signal_generator = signal_generator_2,
#'         variable_params = variable_params_2,
#'         fixed_params = fixed_params_2,
#'       )
#'     )
#'   )
#'   ```
#'
#' @param algos A nested list (see specification above)
#'
#' @return A list where each element is a list containing:
#'   -  `instrument` An instrument name. The instrument name is also assigned
#'      to the corresponding data frame as a comment.
#'   -  `rule` A list containing one rule variation list (see specification
#'      above.)
#'
#' @examples
expand_algos <- function(algos) {
  #expanded_algos_as_dataframes <- list()
  expanded_algos <- list()
  expanded_algos_ID <- 1

  for(subset_ID in seq_along(algos)) {
    for(instrument in algos[[subset_ID]]$instruments) {
      for(rule in algos[[subset_ID]]$rules) {
        #if(length(rule) == 1) {

        ## Exclude rule name and function
        fixed_params_names <- names(rule[-c(1, 2)])
        fixed_params <- rule[-c(1, 2)]

        signal_generator <- rule[[2]]

        ## For now use variable param names as place holders, but data will be
        ## assigned when the system is running.
        variable_params <- {x = names(formals(signal_generator)); setdiff(x, fixed_params_names)} ## All params that are not fixed
        names(variable_params) <- variable_params

        expanded_algos[[expanded_algos_ID]] <- list(
          instrument = instrument,
          rule = list(
            rule_variation = rule[[1]],
            signal_generator = signal_generator,
            variable_params = variable_params,
            fixed_params = fixed_params
          )
        )

        expanded_algos_ID = expanded_algos_ID + 1
        #} else {
          #stop("Exactly one signal signal generator name as a character string must be supplied for each algo.")
        #}
      }
    }
  }
  expanded_algos
}







# §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
# fix§0034
# This works on parsed algos, as it gets the instrument names from an
# expanded algo.
# Maybe parse_algos() is redundant. For now it only switches between "sim" and
# "live" mode, when only makes a difference to load_inst_data().
# §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
#' Get Unique Instrument Paths From Expanded Algos List
#'
#' @description
#' Get the unique instrument paths from an expanded algos list.
#'
#' @param expanded_algos Expanded list of algos.
#' @param instrument_data_folder_path Path to instrument data folder.
#'
#' @return
#' @export
#'
#' @examples
get_unique_inst_paths_from_expanded_algos_list <- function(
    expanded_algos,
    instrument_data_folder_path
) {
  ## Collect all instrument paths from expanded_algos
  instrument_paths <- list()
  for(i in seq_along(expanded_algos)) {
    instrument_paths[i] <-
      paste0(instrument_data_folder_path, expanded_algos[[i]]$instrument, ".csv")
  }

  ## Make list of only unique paths
  unique_instrument_paths <- unique(instrument_paths)
  unique_instrument_paths
}

# §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
# fix§0035
# We are not storing paths in algos anymore. Only instrument names.
# §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
# get_unique_inst_paths_from_parsed_algos_list <- function(parsed_algos) {
#   ## Collect all instrument paths from expanded_algos
#   instrument_paths <- list()
#   for(i in seq_along(parsed_algos)) {
#     instrument_paths[i] <- parsed_algos[[i]]$path
#   }
#
#   ## Make list of only unique paths
#   unique_instrument_paths <- unique(instrument_paths)
#   unique_instrument_paths
# }

#' Get Instrument Names By Parsed Algo
#'
#' @description
#' Get the instrument name from each parsed algo and place the names in a list
#'   in the same order as they appear in the algos list.
#'
#' @param parsed_algos List of expanded algos.
#'
#' @return
#' @export
#'
#' @examples
get_inst_names_by_parsed_algo <- function(parsed_algos) {
  ## Collect all instrument names from expanded_algos
  instrument_names <- list()
  for(i in seq_along(parsed_algos)) {
    instrument_names[i] <- parsed_algos[[i]]$instrument
  }

  ## Make list of only instrument names
  instrument_names
}

#' Get Unique Instrument Names From Expanded Algos List
#'
#' @description
#' Get unique instrument names from list of expanded algos.
#'
#' @param parsed_algos List of expanded algos.
#'
#' @return List of character strings
#' @export
#'
#' @examples
get_unique_inst_names_from_parsed_algos_list <- function(parsed_algos) {
  ## Collect all instrument names from expanded_algos
  instrument_names <- get_inst_names_by_parsed_algo(parsed_algos)

  ## Make list of only unique names
  unique(instrument_names)
}

#' Get Number Of Unique Instruments From Parsed Algos List
#'
#' @description
#' Get the number of unique instruments from the parsed algos list.
#'
#' @param parsed_algos List of expanded algos.
#'
#' @return
#' @export
#'
#' @examples
get_num_inst_from_parsed_algos_list <- function(parsed_algos) {
  num_inst <-
    length(get_unique_inst_names_from_parsed_algos_list(parsed_algos))

  ## Check that num_instr is at least 1 and not NA.
  ## In fact, at least one instrument must be provided to the system per algo.
  if(num_inst >= 1 && !is.na(num_inst)) {num_inst} else {
    warning(
  "At least one instrument must be provided to the system.
  In fact, at least one instrument must be provided to the system per algo.")
  }

}


#' Get Number Of Rules Per Instrument From Parsed Algos
#'
#' Get a named list of numbers of rules for each instrument.
#' One named item for each instrument. The name of the item is the name of the
#' instrument.
#' Each item in the list is a number of rules for the corresponding instrument.
#'
#' @param parsed_algos List of expanded algos.
#'
#' @return Named list of numbers of rules for each instrument.
#' @export
#'
#' @examples
get_num_rules_per_inst_from_parsed_algos <- function(parsed_algos) {
  inst_names <- unlist(get_inst_names_by_parsed_algo(parsed_algos))
  unique_names <- unique(inst_names)

  ## Vector of numbers of rules for each instrument.
  ## One named item for each instrument. The name of the item is the name of
  ## the instrument.
  ## Each item in the list is a number of rules for the corresponding
  ## instrument.
  num_rules <- numeric(length(unique_names))
  names(num_rules) <- unique_names

  for(i in seq_along(unique_names)) {
    num_rules[i] <- sum(inst_names == unique_names[i])
  }
  num_rules
}

#' Get Rule Variation Names By Parsed Algo
#'
#' @description
#' Get list of rule variation names by parsed algo.
#'
#' @param parsed_algos
#'
#' @return List of rule variation names in the order they appear in the parsed
#'   algos list
#' @export
#'
#' @examples
get_rule_variation_names_by_parsed_algo <- function(parsed_algos) {
  ## Collect all rule variation names from expanded_algos
  rule_variation_names <- list()
  for(i in seq_along(parsed_algos)) {
    ## Changed this because each algo now only contains one rule variation name.
    # rule_variation_names[[i]] <- lapply(
    #   parsed_algos[[i]]$rule,
    #   function(x) {x}
    # )
    rule_variation_names[[i]] <- parsed_algos[[i]]$rule
  }
  rule_variation_names
}

#' Get Unique Rule Variation Names By Parsed Algo
#'
#' @description
#' Get list of unique rule variation names by parsed algo.
#'
#' @param parsed_algos List of expanded algos
#'
#' @return List
#' @export
#'
#' @examples
get_unique_rule_variation_names_by_parsed_algo <- function(parsed_algos) {
  rule_variation_names <- get_rule_variation_names_by_parsed_algo(parsed_algos)
  #unique(unlist(rule_variation_names))
  unique(rule_variation_names)
}

#' Get Rule Names By Parsed Algo
#'
#' @description
#' Gets the rule name as a character string from each parsed algo and places the
#'   names in a list in the same order as they appear in the algos list.
#'
#' NOTE:
#' Since each algo now only accepts exactly one rule variation name, this in
#'   effect does the same as `get_rule_variation_names_by_parsed_algo()`, just in
#'   a more convoluted way. The rule name is now simply identical to the rule
#'   function name (whereas earlier, a *rule* would be a list of an arbitrary
#'   number of rule variation names).
#'
#' Rule names are the names of elements in the rule list for each algo, i.e. the
#'   same as the rule variation names.
#'
#' @param parsed_algos List of expanded algos.
#'
#' @return List of character strings.
#' @export
#'
#' @examples
# get_rule_names_by_parsed_algo <- function(parsed_algos) {
#   ## Collect all instrument names from expanded_algos
#   rule_names <- list()
#   for(i in seq_along(parsed_algos)) {
#     rule_names[[i]] <- ""
#     ## Append each name string to the previous one and separate by comma
#     for(j in seq_along(parsed_algos[[i]]$rule)) {
#       rule_names[[i]] <- paste(rule_names[[i]], names(parsed_algos[[i]]$rule[[j]]), sep = ",")
#     }
#
#     ## Remove leading comma
#     rule_names[[i]] <- substring(rule_names[[i]], 2)
#     ## Add square brackets (no need)
#     #rule_names[i] <- paste0("[",rule_names[i], "]")
#   }
#
#   ## Make list of only rule names
#   rule_names
# }

get_rule_names_by_parsed_algo <- function(parsed_algos) {
  ## Collect all instrument names from expanded_algos
  rule_names <- lapply(
    parsed_algos,
    function(parsed_algo) {
      parsed_algo$rule[[1]]
    }
  )
}

#' Get Unique Rule Names From Expanded Algos List
#'
#' @description
#' Get the unique rule names from an expanded algos list. Each rule is a
#'   combination of one signal rule and optionally one stop loss rule.
#'
#' NOTE:
#' This in effect does the same as
#'   `get_unique_rule_variation_names_by_parsed_algo()`. This is because
#'   `get_rule_names_by_parsed_algo()` in effect does the same as
#'   `get_rule_variation_names_by_parsed_algo()`. See help file for
#'   `get_rule_names_by_parsed_algo()`.
#'
#' To get the names of all unique signal and stop loss rules, use
#'   `get_unique_rule_variation_names_by_parsed_algo()`.
#'
#' @param parsed_algos List of expanded algos.
#'
#' @return List of character strings.
#' @export
#'
#' @examples
get_unique_rule_names_from_parsed_algos_list <- function(parsed_algos) {
  ## Collect all instrument names from expanded_algos
  rule_names <- get_rule_names_by_parsed_algo(parsed_algos)

  ## Make list of only unique names
  unique(rule_names)
}

#' Get Number Of Unique Rules From Parsed Algos List
#'
#' @description
#' Get the number of unique rules from an expanded algos list.
#'
#' @param parsed_algos List of expanded algos.
#'
#' @return Number
#' @export
#'
#' @examples
get_num_rules_from_parsed_algos_list <- function(parsed_algos) {
  num_rules <-
    length(get_unique_rule_names_from_parsed_algos_list(parsed_algos))

  ## Check that num_instr is at least 1 and not NA.
  if(num_rules >= 1 && !is.na(num_rules)) {num_rules} else {
    stop("At least one rule must be provided to the system.")
  }
}


#' Get Signal Normalization Factors By Algos
#'
#' @description
#' Get signal normalization factors by algos.
#'
#' @param signal_normalization_factors Named list of unique normalization
#'   factors.
#'
#' @return Unnamed list of normalization factors matching the order of the list
#'   of parsed algos
#' @export
#'
#' @examples
get_signal_normalization_factors_by_algos <- function(
    signal_normalization_factors, parsed_algos) {

  sig_norm_fact_by_algos <- list()
  rule_names_by_parsed_algo <- get_rule_names_by_parsed_algo(parsed_algos)

  for(i in seq_along(parsed_algos)) {
    sig_norm_fact_by_algos[[i]] <- signal_normalization_factors[rule_names_by_parsed_algo[[i]]][[1]]
  }
  rlang::flatten(sig_norm_fact_by_algos)
}




#' Parse algos list
#'
#' @description
#'
#' Loads data and rules into the system by parsing a list of algos.
#'
#' An algo is composed of one instrument (data) and one rule function.
#'
#' Data sets must be csv files in a single folder. The system configuration must
#'   include a path to the data folder. Data sets are referenced by their file
#'   names (excluding extension) as character strings.
#'
#' A rule function is a function that produces a trade signal. A *rule function*
#'   may implement several rules to produce one signal. The typical use case for
#'   multiple rules in one rule function would by one rule generating an actual
#'   trade signal, and one rule triggering a position to be closed, e.g. a stop
#'   loss rule. (See vignette: `vignette(package = "rsystrade")`)
#'
#' `parse_algos()` takes a nested list of algo specifications and expands it to
#'   a list of all possible permutations of instrument names and rules. Finally
#'   formats the list so that each element is a list containing one element
#'   named `instrument` containing an instrument name, and one element named
#'   `rule` containing one *signal generating rule function* name.
#'
#' Note: In the input sublists we have `rules` (plural, indicating one or
#'   more), in the output sublists we have `rule` (singular).
#'
#' Each rule in the `rules` list for each user-specified algo should be a
#'   function name provided as a character string. E.g.
#'    ```
#'    trade_system$algos[[1]]$rules[[1]] <- "MAC_20_80"
#'    ```
#'
#' #REDUNDANT
#' #
#' #With `get_rule_names_by_parsed_algo()` we can parse the name of rule 1
#' #  (entry rule 1 and exit rule 1 combined) as `"[MA_20_80, stoploss]"`.
#'
#' If for instance a moving average crossover rule is defined in the function
#'   `mac(data, fast, slow)`, a (20, 80) variation should be provided to the
#'   system as `MA_20_80 <- function(x) {mac(data, 20, 80)}`. (The name
#'   `MA_20_80` is arbitrary.)
#'
#' In the the `algos` list, instruments must be provided as a path to a csv
#'   file. (For now. Eventually the way the `algos` list is parsed will depends
#'   on the mode, which can be specified as `sim` for _simulation mode_ or
#'   `live` for _live mode_.) Rules must be provided as character string
#'   function names. The *signal generating rule function* name must match the
#'   name of a function which is loaded into the environment in which the system
#'   list is stored. In other words: You must define the rule functions in the
#'   same environment as the one where you call `make_system()` (typically the
#'   global environment).
#'
#' `make_system()` loads the unique data sets specified in the parsed algos into
#'   a sublist named `inst_data`.
#'
#' The `algos` parameter is a nested list in one in the following forms.
#'   Pairs of one instrument and one rule function:
#'   ```R
#'   algos <- list(
#'     list( ## algo 1: algos[[1]]
#'       instruments = list("instr1"),
#'       rules = list("rule1")
#'     ),
#'     list( ## algo 2: algos[[2]]
#'       instruments = list("instr2"),
#'       rules = list("rule2")
#'     )
#'   )
#'   ```
#'   Or we can apply the same rule function to multiple instruments:
#'   ```R
#'   algos <- list(
#'     list( ## algo 1: algos[[1]]
#'       instruments = list("instr1", "instr2"),
#'       rules = list("rule1")
#'     )
#'   )
#'   ```
#'   Or multiple rules to a single instrument.:
#'   ```R
#'   algos <- list(
#'     list( ## algo 1: algos[[1]]
#'       instruments = list("instr1"),
#'       rules = list("rule1", "rule2")
#'     )
#'   )
#'   ```
#'   In these examples `"rule1"` is the name of a rule function. `"rule2"` is
#'     the name of another rule function. Rule functionnames are supplied as
#'     character strings.
#'
#' Subsets are then expanded to all possible permutations:
#'   ```R
#'   input <- list(
#'     list( ## algo 1: algos[[1]]
#'       instrument = "inst1",
#'       rule = "rule1"
#'     ),
#'     list( ## algo 2: algos[[2]]
#'       instrument = "inst2",
#'       rule = "rule1"
#'     )
#'     list( ## algo 3: algos[[3]]
#'       instrument = "inst1",
#'       rule = "rule2"
#'     ),
#'     list( ## algo 4: algos[[4]]
#'       instrument = "<inst_2_name>",
#'       rule = "rule2"
#'     )
#'   )
#'   ```
#'
#' @param algos A nested list (see specification above)
#'
#' @return A list where each element is a list containing:
#'   -  `instrument` An instrument name. The instrument name is also assigned
#'      to the corresponding data frame as a comment.
#'   -  `rule` A list containing one signal rule functionname as a character
#'      string.
#'
#' @param mode `sim` for simulation mode or `live` for live trading mode.
#'
#' @return
#'
#' @examples
parse_algos <- function(algos, mode = "sim") {
  if(mode == "sim") {

    ## Expand algos nested list to all permutations of algos.
    expanded_algos <- expand_algos(algos)

    ## Collect all instrument paths from expanded_algos
    # instrument_paths <- list()
    # for(i in seq_along(expanded_algos)) {
    #   instrument_paths[i] <- expanded_algos[[i]]$instrument
    # }

    ## Make list of only unique paths given instrument names from input algos.
    ## This is only needed if we are loading the data into each algo. But we
    ## don't do that.
    #unique_instrument_paths <- unique(instrument_paths)
    # unique_instrument_paths <-
    #   get_unique_inst_paths_from_expanded_algos_list(expanded_algos)

    ## Make list of only unique instrument data sets
    ## This is only needed if we are loading the data into each algo. But we
    ## don't do that.
    # unique_inst_data <- list()

    ## Read the data into the list.
    ## This is only needed if we are loading the data into each algo. But we
    ## don't do that.
    # for(i in seq_along(unique_instrument_paths)) {
    #   unique_inst_data[[i]] <- data.frame(
    #     read.csv(unique_instrument_paths[[i]]))
    # }


    ## Make a list of instrument data sets
    #for(i in seq_along(expanded_algos)) {
    #path_i = algos[[i]]$instruments[[1]]
    ## This is only needed if we are loading the data into each algo. But we
    ## don't do that.
    #expanded_algos[[i]]$path = expanded_algos[[i]]$instrument

    ## Store the data for each algo inside the algo list.
    ## Note: We don't do that because if different rules are applied to the
    ## same data set, the same data set will be loaded multiple times (wich
    ## would work fine, but is not efficient).
    # expanded_algos[[i]]$data <-
    #   unique_inst_data[[which(unlist(unique_instrument_paths) ==
    #                                       expanded_algos[[i]]$path)]]

    ## When input instrument list in algo contains a path instead of a name:
    ## Set instrument to instrument name instead of instrument data set path
    # expanded_algos[[i]]$instrument <-
    #   tools::file_path_sans_ext(basename(expanded_algos[[i]]$path))

    ## Assign name as comment to data frame.
    ## This is only needed if we are loading the data into each algo. But we
    ## don't do that.
    # comment(expanded_algos[[i]]$data) <- expanded_algos[[i]]$instrument

    ## Now we can get the data frame name:
    ## > comment(expanded_algos[[1]]$data)
    ## [1] "instrument1"
    #}

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
#' An algo is a pair consisting of an instrument and a rule function.
#'
#'   The `algos` parameter is a nested list in one of the following forms.
#'   Pairs of one instrument and one rule:
#'   ```R
#'   algos <- list(
#'     list( ## algo 1: algos[[1]]
#'       instruments = list("instr1"),
#'       rules = list("rule1")
#'     ),
#'     list( ## algo 2: algos[[2]]
#'       instruments = list("instr2"),
#'       rules = list("rule2")
#'     )
#'   )
#'   ```
#'   Or we can apply the same rule function to multiple instruments:
#'   ```R
#'   algos <- list(
#'     list( ## algo 1: algos[[1]]
#'       instruments = list("instr1", "instr2"),
#'       rules = list("rule1")
#'     )
#'   )
#'   ```
#'   Or multiple rules to a single instrument.:
#'   ```R
#'   algos <- list(
#'     list( ## algo 1: algos[[1]]
#'       instruments = list("instr1"),
#'       rules = list("rule1", "rule2")
#'     )
#'   )
#'   ```
#'
#' @param algos A nested list (see specification above)
#'
#' @return A list where each element is a list containing one element named
#'   `instrument` containing one instrument as a character string and one list
#'   named `rule` containing one signal rule function name as a character
#'   string.
#'
#' @examples
expand_algos <- function(algos) {
  #expanded_algos_as_dataframes <- list()
  expanded_algos <- list()
  expanded_algos_ID <- 1

  for(subset_ID in seq_along(algos)) {
    for(instrument in algos[[subset_ID]]$instruments) {
      for(rule in algos[[subset_ID]]$rules) {
        if(length(rule) == 1) {
          expanded_algos[[expanded_algos_ID]] <- list(
            instrument = instrument,
            rule = rule)
          expanded_algos_ID = expanded_algos_ID + 1
        } else {
          stop("Exactly one signal rule function name as a character string must
               be supplied for each algo.")
        }
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

#' Get Rule Function Names By Parsed Algo
#'
#' @description
#' Get list of rule function names by parsed algo.
#'
#' @param parsed_algos
#'
#' @return List of rule function names in the order they appear in the parsed
#'   algos list
#' @export
#'
#' @examples
get_rule_function_names_by_parsed_algo <- function(parsed_algos) {
  ## Collect all rule function names from expanded_algos
  rule_function_names <- list()
  for(i in seq_along(parsed_algos)) {
    ## Changed this because each algo now only contains one rule function name.
    # rule_function_names[[i]] <- lapply(
    #   parsed_algos[[i]]$rule,
    #   function(x) {x}
    # )
    rule_function_names[[i]] <- parsed_algos[[i]]$rule
  }
  rule_function_names
}

#' Get Unique Rule Function Names By Parsed Algo
#'
#' @description
#' Get list of unique rule function names by parsed algo. These functions are
#'   either signal rule functions or stop loss rule functions.
#'
#' @param parsed_algos List of expanded algos
#'
#' @return List
#' @export
#'
#' @examples
get_unique_rule_function_names_by_parsed_algo <- function(parsed_algos) {
  rule_function_names <- get_rule_function_names_by_parsed_algo(parsed_algos)
  #unique(unlist(rule_function_names))
  unique(rule_function_names)
}

#' Get Rule Names By Parsed Algo
#'
#' @description
#' Gets the rule name as a character string from each parsed algo and places the
#'   names in a list in the same order as they appear in the algos list.
#'
#' NOTE:
#' Since each algo now only accepts exactly one rule function name, this in
#'   effect does the same as `get_rule_function_names_by_parsed_algo()`, just in
#'   a more convoluted way. The rule name is now simply identical to the rule
#'   function name (whereas earlier, a *rule* would be a list of an arbitrary
#'   number of rule function names).
#'
#' Rule names are the names of elements in the rule list for each algo, i.e. the
#'   same as the rule function names.
#'
#' @param parsed_algos List of expanded algos.
#'
#' @return List of character strings.
#' @export
#'
#' @examples
get_rule_names_by_parsed_algo <- function(parsed_algos) {
  ## Collect all instrument names from expanded_algos
  rule_names <- list()
  for(i in seq_along(parsed_algos)) {
    rule_names[[i]] <- ""
    ## Append each name string to the previous one and separate by comma
    for(j in seq_along(parsed_algos[[i]]$rule)) {
      rule_names[[i]] <- paste(rule_names[[i]], parsed_algos[[i]]$rule[[j]], sep = ",")
    }
    ## Remove leading comma
    rule_names[[i]] <- substring(rule_names[[i]], 2)
    ## Add square brackets (no need)
    #rule_names[i] <- paste0("[",rule_names[i], "]")
  }

  ## Make list of only rule names
  rule_names
}

#' Get Unique Rule Names From Expanded Algos List
#'
#' @description
#' Get the unique rule names from an expanded algos list. Each rule is a
#'   combination of one signal rule and optionally one stop loss rule.
#'
#' NOTE:
#' This in effect does the same as
#'   `get_unique_rule_function_names_by_parsed_algo()`. This is because
#'   `get_rule_names_by_parsed_algo()` in effect does the same as
#'   `get_rule_function_names_by_parsed_algo()`. See help file for
#'   `get_rule_names_by_parsed_algo()`.
#'
#' To get the names of all unique signal and stop loss rules, use
#'   `get_unique_rule_function_names_by_parsed_algo()`.
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



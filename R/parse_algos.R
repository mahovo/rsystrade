
#' Parse algos list
#'
#' @description
#'
#' Loads data and rules into the system by parsing a list of algos.
#'
#' An algo is composed of one instrument (data) and one rule (functions).
#'
#' A "rule" is a combination of one entering rule and one exiting rule.
#'
#' `parse_algos()` takes a nested list of algo specifications and expands it to
#'   a list of all possible permutations, then loads instrument data into the
#'   elements in the list. Finally formats the list so that each element is a
#'   list containing one instrument name, one corresponding data element and one
#'   list named `rule` containing one entering rule function and one exiting rule
#'   function.
#'
#' Note: In the input sublists we have `rules` (plural, indicating one or
#'   more), in the output sublists we have `rule` (singular).
#'
#' In the the `algos` list, instruments must be provided as a path to a csv
#'   file. Rules must be provided as functions. (For now. Eventually the way
#'   the `algos` list is parsed will depend on the mode, which can be specified
#'   as `sim` for _simulation mode_ or `live` for _live mode_.)
#'
#' The `algos` parameter is a nested list in one in the following forms.
#'   Pairs of one instrument and one rule:
#'   ```R
#'   algos <- list(
#'     list( ## algo 1: algos[[1]]
#'       instruments = list("instr1"),
#'       rules = list(rule1)
#'     ),
#'     list( ## algo 2: algos[[2]]
#'       instruments = list("instr2"),
#'       rules = list(rule2)
#'     )
#'   )
#'   ```
#'   Or we can apply the same rule to multiple instruments:
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
#'   In these examples `rule1` is the name of a list containing one entering rule
#'   and one exiting rule. `rule2` is the name of another list containing one
#'   entering rule function and one exiting rule function.
#'
#' Subsets are then expanded to all possible permutations:
#'   ```R
#'   input <- list(
#'     list( ## algo 1: algos[[1]]
#'       instrument = "<inst_1_data_frame>",
#'       data = <data_frame>,
#'       rule = rule1
#'     ),
#'     list( ## algo 2: algos[[2]]
#'       instrument = "<inst_name_2>",
#'       data = <inst_2_data_frame>,
#'       rule = rule1
#'     )
#'     list( ## algo 3: algos[[3]]
#'       instrument = "<inst_1_data_frame>",
#'       data = <data_frame>,
#'       rule = rule2
#'     ),
#'     list( ## algo 4: algos[[4]]
#'       instrument = "<inst_2_data_frame>",
#'       data = <data_frame>,
#'       rule = rule2
#'     )
#'   )
#'   ```
#'
#' @param algos A nested list (see specification above)
#'
#' @return A list where each element is a list containing:
#'   -  `instrument` An instrument name. The instrument name is also assigned
#'      to the data frame as a comment.
#'   -  `data` Corresponding data frame.
#'   -  `rule` A list containing one entering rule function and one exiting rule
#'      function.
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
    # unique_instrument_datasets <- list()

    ## Read the data into the list.
    ## This is only needed if we are loading the data into each algo. But we
    ## don't do that.
    # for(i in seq_along(unique_instrument_paths)) {
    #   unique_instrument_datasets[[i]] <- data.frame(
    #     read.csv(unique_instrument_paths[[i]]))
    # }


    ## Make a list of instrument datasets
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
    #   unique_instrument_datasets[[which(unlist(unique_instrument_paths) ==
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
#'   An algo is a pair consisting of an instrument and a rule.
#'
#'   A "rule" is a combination of one entering rule and one exiting rule.
#'
#'   The `algos` parameter is a nested list in one in the following forms.
#'   Pairs of one instrument and one rule:
#'   ```R
#'   algos <- list(
#'     list( ## algo 1: algos[[1]]
#'       instruments = list("instr1"),
#'       rules = list(rule1)
#'     ),
#'     list( ## algo 2: algos[[2]]
#'       instruments = list("instr2"),
#'       rules = list(rule2)
#'     )
#'   )
#'   ```
#'   Or we can apply the same rule to multiple instruments:
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
#'
#' @param algos A nested list (see specification above)
#'
#' @return A list where each element is a list containing one element named
#'   `instrument` and one list named `rule` containing one entering and one
#'   exiting rule.
#'
#' @examples
expand_algos <- function(algos) {
  #expanded_algos_as_dataframes <- list()
  expanded_algos <- list()
  expanded_algos_ID <- 1

  for(subset_ID in seq_along(algos)) {
    for(instrument in algos[[subset_ID]]$instruments) {
      for(rule in algos[[subset_ID]]$rules) {
        expanded_algos[[expanded_algos_ID]] <- list(
          instrument = instrument,
          rule = rule)
        expanded_algos_ID = expanded_algos_ID + 1
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
# "live" mode, when only makes a difference to load_instrument_data_sets().
# §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
#' Get Unique Instrument Paths From Expanded Algos List
#'
#' @param expanded_algos
#' @param instrument_data_folder_path
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
#' Get Unique Instrument Paths From Parsed Algos List
#'
#' @param parsed_algos
#'
#' @return
#' @export
#'
#' @examples
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

#' Get Unique Instrument Names From Expanded Algos List
#'
#' @param parsed_algos
#'
#' @return
#' @export
#'
#' @examples
get_unique_inst_names_from_parsed_algos_list <- function(parsed_algos) {
  ## Collect all instrument names from expanded_algos
  instrument_names <- list()
  for(i in seq_along(parsed_algos)) {
    instrument_names[i] <- parsed_algos[[i]]$instrument
  }

  ## Make list of only unique names
  unique_instrument_names <- unique(instrument_names)
  unique_instrument_names
}

#' Get Instrument Names By Parsed Algo
#'
#' @description
#' The instrument name from each parsed algo and place the names in a list in
#'   the same order as they appear in the algos list.
#'
#' @param parsed_algos
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

#' Get Number Of Unique Instruments From Parsed Algos List
#'
#' @param parsed_algos
#'
#' @return
#' @export
#'
#' @examples
get_num_inst_from_parsed_algos_list <- function(parsed_algos) {
  num_instr <-
    length(get_unique_inst_names_from_parsed_algos_list(parsed_algos))

  ## Check that num_instr is at least 1 and not NA.
  if(num_instr >= 1 && !is.na(num_instr)) {num_instr} else {
    stop("At least one instrument must be provided to the system.")
  }

}

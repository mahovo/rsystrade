## Accounting ====

#' Get All Subsystem Position Sizes In Price Units From Position Tables
#'
#' @description
#' Get all subsystem position sizes at time \eqn{t} in price units from position tables.
#'
#' @param position_tables Position tables
#'
#' @return Vector of position sizes in price units
#' @export
#'
#' @examples
get_all_position_sizes_ccy <- function(position_tables, t) {
  unlist(
    lapply(
     position_tables,
     function(x) x$position_size_ccy[t]
   )
  )
}

#' Get Price Returns From All Instruments
#'
#' @description
#' Get price returns from all instruments at time \eqn{t}.
#'
#' @param position_tables List of all position tables.
#' @param t Time index ("now")
#'
#' @return Vector of price returns
#' @export
#'
#' @examples
get_all_returns <-function(position_tables, t) {
  unlist(
    lapply(
      position_tables,
      function(x) x$instrument_return[t]
    )
  )
}

#' Get All Subsystem Profits/Losses From Position Tables
#'
#' @description
#' Get all subsystem profits/losses in price units from position tables at time
#'   \eqn{t}.
#'
#' @param position_tables List of all position tables.
#'
#' @return Vector of profits/losses in price units
#' @export
#'
#' @examples
get_all_subsystem_pandls <- function(position_tables, t) {
  unlist(
    lapply(
      position_tables,
      function(x) x$subsystem_pandl[t]
    )
  )
}

#' Get All Position Changes
#'
#' @description
#' Get All position changes in price units.
#'
#' @param position_tables List of position tables.
#' @param t Time index ("now").
#'
#' @return Vector of amounts in price units
#' @export
#'
#' @examples
get_all_position_changes_ccy <- function(position_tables, t) {
  unlist(
    lapply(
      position_tables,
      function(x) x$position_change_ccy[t]
    )
  )
}











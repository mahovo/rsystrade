#' Parse List Of Portfolio Multipliers
#'
#' @description
#' Parse a list of portfolio multiplier specification lists.
#'
#' Portfolio multiplier specification lists are provided by the user as
#'   parameter `portfolio_multipliers` to `make_system()`.
#'
#' @param portfolio_multipliers Input list of portfolio multipliers.
#'
#' @return Output list of portfolio multipliers
#' @export
#'
#' @details
#' Each input portfolio multiplier specification list specifies a multiplier
#'   name, a multiplier function and any number of fixed parameters. The parser
#'   automatically determines which parameters of each multiplier function are
#'   fixed parameters provided by the user, and which are variable parameters
#'   provided by the system.
#'
#' Input format:
#' ```R
#' portfolio_multipliers = list(
#'   list(
#'     <multiplier-name-1>,
#'     <multiplier-function-1>,
#'     <fixed-params-1_1>,
#'     <fixed-params-1_2>
#'   ),
#'   list(
#'     <multiplier-name-2>,
#'     <multiplier-function-2>,
#'     <fixed-params-2_1>,
#'     <fixed-params-2_2>
#'   )
#' )
#' ```
#'
#' Output format:
#' ```R
#' portfolio_multipliers = list(
#'   list(
#'     modifier_name = <multiplier-name-1>,
#'     modifier_function = <multiplier-function-1>,
#'     variable_params = list(
#'       <variable-param-1_1>,
#'       <variable-param-1_2>
#'     )
#'     fixed_params = list(
#'       <fixed-params-1_1>,
#'       <fixed-params-1_2>
#'      )
#'   ),
#'   list(
#'     modifier_name = <multiplier-name-2>,
#'     modifier_function = <multiplier-function-2>,
#'     variable_params = list(
#'       <variable-param-2_1>,
#'       <variable-param-2_2>
#'     )
#'     fixed_params = list(
#'       <fixed-params-2_1>,
#'       <fixed-params-2_2>
#'      )
#'   )
#'   )
#' )
#' ```
#'
#' Unlike *position multipliers* and *position modifiers*, variable params for
#'   *portfolio multipliers* don't take input directly from `inst_data`.
#'   Instrument data can be accessed via the system variables, which are
#'   available to portfolio multiplier functions.
#'
#' System variables which are available to portfolio multiplier functions are:
#'
#'   * `trade_system` (everything in the system up until t - 1)
#'   * `t`
#'   * `signal_weights_all_algos`
#'   * `sig_norm_fact_by_algos`
#'   * `signal_tables`
#'   * `subsystem_pandl_vectors`
#'   * `instrument_list`
#'   * `subsystem_ret_cor_win_len`
#'   * `subsystem_ret_cor_mat`
#'   * `instrument_weights`
#'   * `combined_signals_and_sdm`
#'   * `raw_combined_signals`
#'   * `signal_div_mul_vect`
#'   * `inst_div_mul`
#'   * `instrument_risk_target`
#'   * `position_tables`
#'
#' @examples
parse_portfolio_multipliers_list <- function(
    portfolio_multipliers
  ) {
  parsed_portmul_list <- list()

  for(i in seq_along(portfolio_multipliers)) {
    multiplier <- portfolio_multipliers[[i]]
    multiplier_name <- multiplier[[1]]
    multiplier_function <- multiplier[[2]]

    ## Exclude multiplier name and multiplier function itself from list.
    fixed_params <- multiplier[-c(1, 2)]
    fixed_params_names <- names(fixed_params)

    ## Variable params are all params that are not fixed (i.e. change over time)
    variable_params <- {
      x = names(formals(multiplier_function))
      setdiff(x, fixed_params_names)
    }

    names(variable_params) <- variable_params

    ## Append multiplier to multiplier list
    parsed_portmul_list <- c(
      if(length(parsed_portmul_list) > 0) {
        parsed_portmul_list
      } else {list()},
      list(list(
        multiplier_name = multiplier_name,
        multiplier_function = multiplier_function,
        variable_params = variable_params,
        fixed_params = fixed_params
      ))
    )

  }
  parsed_portmul_list
}

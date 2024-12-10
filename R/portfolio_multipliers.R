
## [AFTS, p. 570ff + 578]
#' Limit Portfolio Risk
#'
#' @description
#' Portfolio risk limiting multiplier.
#'
#' @param t Time index.
#' @param position_tables List of position tables.
#' @param max_risk Maximum risk level.
#' @param capital Capital.
#' @param cor_method Method of correlation calculation as character string.
#'   * `"Pearson"`
#'   * `"ewa"` Exponentially Weighted Correlation. @seealso [f_ewa_cor_mat()]
#'     * Additional parameters: `lambda` (number), `lookback` (integer)
#'
#' @return List of multiplier output. The first element must be a multiplier
#'   value in \eqn{[0, 1]}. Additional elements are optional.
#' @export
#'
#' @examples
o_limit_pf_risk <- function(
    t,
    position_tables,
    max_risk = NA,
    capital,
    cov_method#,
    #...
) {

  portfolio_returns <- lapply(
    position_tables,
    function(x) {
      if(is.null(x$price)) {
        stop("limit_portfolio_risk() position multiplier needs position tables
        with price column as input.")
      }

      f_percentage_returns(
        2:t,
        x$price
      )
    }
  )

  portfolio_returns_df <- data.frame(portfolio_returns)
  colnames(portfolio_returns_df) <- names(position_tables)

  # w <- lapply(
  #   position_tables,
  #   function(x) {
  #     if(is.null(x$price)) {
  #       stop("limit_portfolio_risk() position multiplier needs position tables
  #       with price column as input.")
  #     }
  #
  #     x$final_position_size_units * x$price[t] / capital
  #   }
  # )

  w <- get_position_weights(
    t,
    position_tables,
    capital
  )

  portfolio_returns_df <- data.frame(portfolio_returns)

  Sigma <- f_cov_mat(
    data = portfolio_returns_df,
    method = cov_method#,
    #...
  )

  portfolio_risk <- drop(sqrt(w %*% Sigma %*%  w))

  multiplier_value <- min(1, max_risk / portfolio_risk)

  list(
    multiplier_value = multiplier_value,
    portfolio_risk = portfolio_risk
  )
}


## [AFTS, p. 573ff + 578]
#' Limit Shock Portfolio Risk
#'
#' @param t Time index.
#' @param position_tables Position tables.
#' @param max_risk Maximum risk. Risk limit.
#' @param sd_percentile Shock percentile. Worst percentile of standard deviations.
#' @param capital Total system capital.
#' @param sd_window_length Standard deviation window length.
#' @param sd_method Standard deviation calculation method.
#' @param cor_method Correlation calculation method.
#' @param ...
#'
#' @return List of multiplier output. The first element is the multiplier
#'   value in \eqn{[0, 1]}. The second element is `shock_pf_risk`.
#' @export
#'
#' @examples
o_limit_pf_shock_risk <- function(
    t,
    position_tables,
    max_risk = NA,
    sd_percentile,
    capital,
    sd_window_length,
    sd_method = "unbiased",
    cor_method = "Pearson"#,
    #...
  ) {

  if(is.na(max_risk)) {
    stop("max_risk needs a decimal fraction value. See o_limit_pf_shock_risk()
         help.")
  }

  portfolio_returns <- lapply(
    position_tables,
    function(x) {
      if(is.null(x$price)) {
        stop("limit_portfolio_risk() position multiplier needs position tables
        with price column as input.")
      }

      f_percentage_returns(
        2:t,
        x$price
      )
    }
  )

  portfolio_returns_df <- data.frame(portfolio_returns)

  # w <- as.matrix(data.frame(lapply(
  #   position_tables,
  #   function(x) {
  #     if(is.null(x$price)) {
  #       stop("limit_portfolio_risk() position multiplier needs position tables
  #       with price column as input.")
  #     }
  #
  #     x$final_position_size_units * x$price[t] / capital
  #   }
  # )))

  w <- get_position_weights(
    t,
    position_tables,
    capital
  )

  # For each instrument in portfolio: Measure the annualised standard deviation
  # of each instrument from percentage returns, on a rolling basis e.g.
  # exponential weighted sd of weekly prices.

  sigma_shock_mat <- lapply(
    portfolio_returns_df,
    function(x) {
      rolling_window(
        x = x,
        first_t = 2,
        last_t = NA,
        window_length = sd_window_length,
        func = f_sd,
        method = "unbiased",
        mode = 1
      )
    }
  )

  # For each instrument in portfolio: Find the 99th percentile of each series of
  # standard deviations.

  shock_pf_risk <- lapply(
    sigma_shock_mat,
    function(z) {
      stats::quantile(z, sd_percentile, na.rm = TRUE)
    }
  )

  shock_pf_risk_mat <- diag(shock_pf_risk)

  cor_mat <- as.matrix(data.frame(
    f_cor_mat(
      data = portfolio_returns_df,
      method = cor_method#,
      #...
    )
  ))

  Sigma_shock <- shock_pf_risk_mat %*% cor_mat %*% shock_pf_risk_mat

  shock_pf_risk <- drop(sqrt(w %*% Sigma_shock %*% w))

  multiplier_value <- min(1, max_risk / shock_pf_risk)

  list(
    multiplier_value = multiplier_value,
    shock_pf_risk = shock_pf_risk
  )
}



## [AFTS, p. 574ff + 578]
#' Limit Shock Portfolio Correlation Risk
#'
#' @description
#'
#' Calculate a limit multiplier for shock portfolio correlation. The assumption
#'   here is that if absolute correlation gets close to 1, the portfolio risk
#'   will approximately be be the sum of weighted instrument risks. Further, as
#'   the absolute correlation decreases, so will the sum of weighted instrument
#'   risks. The smaller the absolute correlation, the bigger the difference
#'   between the portfolio risk and the sum of weighted instrument risks.
#'
#' @param t Time index.
#' @param position_tables Position tables list.
#' @param max_risk Maximum risk. Risk limit.
#' @param capital Total system capital.
#'
#' @details
#' The idea is to limit the sum of weighted standard risks to a maximum value.
#'
#' A way to determine the `max_risk` could be to calculate the (worst) 99th
#'   percentile of *correlation shock portfolio risk*.
#'
#' @return List of multiplier output. The first element is the multiplier
#'   value in \eqn{[0, 1]}. The second element is `cor_shock_pf_risk`.
#' @export
#'
#' @examples
o_limit_cor_risk <- function(
    t,
    position_tables,
    max_risk = NA,
    capital
  ) {

  w <- get_position_weights(
    t,
    position_tables,
    capital
  )

  instr_risks <- unlist(lapply(
    position_tables,
    function(x) {
      x$instrument_risk[t]
    }
  ))

  ## w could in theory be negative if price is negative, so we take abs.
  #weighted_instr_variances <- abs(w) %*% instr_risks^2
  weighted_instr_risks <- w %*% instr_risks

  # cor_shock_pf_risk <- sqrt(
  #   sum(
  #     weighted_instr_variances
  #   )
  # )

  cor_shock_pf_risk <- sum(weighted_instr_risks)

  multiplier_value <- min(1, max_risk / cor_shock_pf_risk)

  list(
    multiplier_value = multiplier_value,
    cor_shock_pf_risk = cor_shock_pf_risk
  )
}




## [AFTS, p. 576ff + 578]
#' Limit Portfolio Leverage
#'
#' @param t Time index.
#' @param position_tables Position tables list.
#' @param max_leverage Maximum leverage. Upper leverage limit.
#' @param capital Total system capital.
#'
#' @details
#' The idea is to limit the portfolio leverage to a maximum value.
#'
#' Portfolio leverage is calculated as the total value of all positions in the
#'   currency of the account divided by the system capital.
#'
#' A way to determine the `max_leverage` could be to calculate the (worst) 99th
#'   percentile of *portfolio leverage* over time.
#'
#' @return List of multiplier output. The first element is the multiplier
#'   value in \eqn{[0, 1]}. The second element is `pf_leverage`.
#' @export
#'
#' @examples
o_limit_leverage_risk <- function(
    t,
    position_tables,
    max_leverage = NA,
    capital
  ) {

  w <- get_position_weights(
    t,
    position_tables,
    capital
  )

  pf_leverage <- sum(w)

  multiplier_value <- min(1, max_leverage / pf_leverage)

  list(
    multiplier_value = multiplier_value,
    pf_leverage = pf_leverage
  )
}


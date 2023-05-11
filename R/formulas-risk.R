## Notes
## 1)
## Decimal fractions are used instead of percentages throughout.
## Example: 87 percent is expressed as 0.87.
##
## 2)
## "LT" refers to Robert Carver: Leveraged Trading
## "ST" refers to Robert Carver: Systematic Trading
## "F" for "formula".

## LT F4 [p. 63]
#' Calculate Required Leverage Factor (RLF)
#'
#' @description
#' Calculates the required leverage factor based on a risk target and the
#'   volatility of an instrument in the same units.
#'
#' The risk measures must be compatible. E.g. if instrument risk target is an
#'   annual instrument volatility target in decimal percentages, and instrument
#'   risk is the volatility annualized volatility of daily returns in decimal
#'   percentages.
#'
#' Note, the annual instrument volatility target can be calculated from a
#'   system volatility target by dividing this by the Instrument Diversification
#'   Diversifier.
#'
#' @param instrument_risk_target Instrument volatility target
#' @param instrument_risk Instrument volatility
#'
#' @return A single number
#' @export
#'
#' @examples
f_required_leverage_factor <- function(
    instrument_risk_target,
    instrument_risk
    ) {
  instrument_risk_target/instrument_risk
}


#' Notional exposure
#'
#' @description
#' Calculate the notional exposure.
#'
#' @param combined_signal Combined signal. Should be normalized to a an expected
#'   absolute value of 1 and capped between -2 and 2.
#' @param capital Trading capital in account currency.
#' @param required_leverage_factor Required leverage factor. Indstrument risk
#'   target divided by instrument risk (in same units).
#' @param instrument_weight Instrument weight.
#'
#' @returns Number
#' @export
#'
#' @example
#'
f_notional_exposure <- function(
    combined_signal, ## normalized and clamped
    capital,
    required_leverage_factor,
    instrument_weight) {
  combined_signal * capital * required_leverage_factor * instrument_weight
}







#' Signals Correlation Matrix
#'
#' @description
#' Calculates the correlation matrix of signal vectors. Typically calculates the
#'   correlation matrix for a subsystem of all the signal vectors affecting a
#'   single instrument in the system.
#'
#' @param signals Dataframe where each column is a signal vector
#'
#' @return Correlation matrix
#' @export
#'
#' @examples
f_signal_cor_mat <- function(
    signals
) {
  stats::cor(signals)
}


#' Subsystem Returns Correlation Matrix
#'
#' @description
#' Calculates the correlation matrix of vectors of subsystem (daily) percentage
#'   returns. The subsystem returns are the returns produced by the subsystem in
#'   backtesting. Notice, these are not the returns that we get directly from
#'   the price series of the each instrument, but rather the returns we get from
#'   applying the rules of the subsystem to the instrument of the subsystem. A
#'   subsystem consists of one instrument and one or more rules applied to this
#'   instrument.
#'
#' @param subsystem_returns Dataframe where each columns is a vector of
#'   subsystem (daily) percentage returns
#' @param method Method
#' * `"Pearson"`, Pearsons Correlation Coefficient
#' * `ewa`, Exponential Weighted Average Correlation
#'   \deqn{E[X_t | X_{t-1}] = \frac{1}{\sum_{i=0}^{t-2}A^i}\sum_{j=1}^{t-1}A^{j-1} X_{t-j}}
#'
#' @return Correlation matrix
#' @export
#'
#' @examples
f_subsystem_ret_cor_mat <- function(
    subsystem_returns,
    method = "Pearson"
    ) {
  switch(
    method,
    "Pearson" = stats::cor(subsystem_returns),
    "ewa" = f_ewa_cor(subsystem_returns)
  )
}


#' Exponential Wrighted Correlation
#'
#' @param x Vector
#' @param y Vector
#' @param lookback Integer. Lookback window length. If no `lookback` is
#'   provided, the entire \eqn{x} and \eqn{y} vectors will be used.
#'
#' @return Number
#' @export
#'
#' @examples
f_ewa_cor <- function(x, y, lookback) {
  L <- lookback
  x_window <- utils::tail(x, L)
  y_window <- utils::tail(y, L)
  mu_x <- f_ewa(x)
  mu_y <- f_ewa(y)
  num <- f_ewa((x - mu_x) * (y - mu_y))
  denom <- sqrt(f_ewa((x - mu_x)^2) * f_ewa((y - mu_y)^2))
  num / denom
}

#' Exponential Wrighted Correlation Matrix
#'
#' Exponentially Weighted Moving-Average estimate of the correlation matrix.
#' See Tsay: Analysis Of Financial Time Series (3rd Ed., 10.1, p. 507)
#'
#' @param data Dataframe.
#' @param lookback Integer. Lookback window length.
#'
#' @return Matrix
#' @export
#'
#' @examples
ew_cor_matrix <- function(data, lookback = NA) {
  if(is.na(lookback)) {
    L <- nrow(data)
    data_window <- data
  } else {
    L <- lookback
    data_window <- utils::tail(data, L)
  }
  m <- ncol(data)
  cor_mat <- matrix(0, ncol=m, nrow=m)
  for (i in 1:m) {
    for (j in 1:m) {
      cor_mat[i, j] <- f_ewa_cor(data_window[ , i], data_window[ , j], L)
    }
  }
  cor_mat
}

#' Exponential Wrighted Covariance
#'
#' @param x Vector
#' @param y Vector
#' @param lookback Integer. Lookback window length. If no `lookback` is
#'   provided, the entire \eqn{x} and \eqn{y} vectors will be used.
#'
#' @return Number
#' @export
#'
#' @examples
f_ewa_cov <- function(x, y, lookback) {
  L <- lookback
  x_window <- utils::tail(x, L)
  y_window <- utils::tail(y, L)
  mu_x <- f_ewa(x)
  mu_y <- f_ewa(y)
  f_ewa((x - mu_x) * (y - mu_y))
}

#' Exponential Wrighted Covariance Matrix
#'
#' Exponentially Weighted Moving-Average estimate of the covariance matrix.
#' See Tsay: Analysis Of Financial Time Series (3rd Ed., 10.1, p. 507)
#'
#' @param data Dataframe.
#' @param lookback Integer. Lookback window length.
#'
#' @return Matrix
#' @export
#'
#' @examples
ew_cov_matrix <- function(data, lookback = NA) {
  if(is.na(lookback)) {
    L <- nrow(data)
    data_window <- data
  } else {
    L <- lookback
    data_window <- utils::tail(data, L)
  }
  m <- ncol(data)
  cor_mat <- matrix(0, ncol=m, nrow=m)
  for (i in 1:m) {
    for (j in 1:m) {
      cov_mat[i, j] <- f_ewa_cov(data_window[ , i], data_window[ , j], L)
    }
  }
  cov_mat
}

## ST F6 [p. 298]
#' Instrument Risk In Units Of Percentage Of Price
#'
#' @description
#' Price Volatility as Exponentially Weighted Moving Average (EWMA)
#'
#' * \eqn{E_{t}}: EWMA til tid \eqn{t}
#' * \eqn{P_{t}}: Price at time \eqn{t}
#' * \eqn{a}: Smoothing Parameter
#' * \eqn{L}: Window length
#' \deqn{a = \frac{2}{1 + L}}
#' \deqn{E_{t} = a \cdot P_{t} + E_{t - 1}(1 - a)}
#'
#' At f_price_volatility_ewma\eqn{t_0} use `last_ewma` = `current_price`.
#'
#' @param current_price Current price.
#' @param last_ewma Last exponential weighted moving average.
#' @param window_length Window length.
#'
#' @return Instrument risk in units of percentage of price
#' @export
#'
#' @examples
f_price_volatility_ewma <- function(
    current_price, ## Price at time t
    last_ewma, ## EWMA at time t - 1
    window_length = 25
    ) {
  a = 2 / (1 + window_length)
  a * current_price + (1 - a) * last_ewma
}


## LT, p. 220
## IDM: LT, F25.1
#' Instrument Risk Target
#'
#' @param system_risk_target Annualized volatility target in percentages
#' @param IDM Instrument Diversification Multiplier
#'    \deqn{\text{IDM} = \frac{1}{\sqrt{W^{T}HW}}}
#'    where \eqn{H} is the correlation matrix for returns, and \eqn{W} is the vector
#'    of instrument weights summing to one.
#'
#' @return Decimal fraction (percentage divided by 100)
#' @export
#'
#' @examples
f_instrument_risk_target <- function(system_risk_target, IDM) {
  system_risk_target * IDM
}

#' Instrument Diversification Multiplier (IDM)
#'
#' @description
#' \deqn{\text{IDM} = \frac{1}{\sqrt{W^{T}HW}}}
#'    Where $H$ is the correlation matrix for returns in percentage terms, and
#'    $W$ is the vector of instrument weights summing to one.
#'
#' @param inst_ret_cor_mat Instrument returns correlations matrix
#' @param instrument_weights Vector of weights
#' @param min_cor Minimum value for each element in `inst_ret_cor_mat`
#'   matrix
#' @param max_idm Maximum output value of IDM
#'
#' @return Scalar
#' @export
#'
#' @examples
f_inst_div_mult <- function(
    inst_ret_cor_mat,
    instrument_weights,
    min_cor = 0,
    max_idm = 2.5) {
  H <- inst_ret_cor_mat
  w <- instrument_weights

  clamped_H <- clamp_matrix_lower(H, min_cor)
  clamp_signal_upper(
    1/sqrt(crossprod(t(w %*% clamped_H),  w)),
    max_signal = max_idm
  )
}


#' Signal Diversification Multiplier (SDM)
#'
#' @description
#' \deqn{\text{SDM} = \frac{1}{\sqrt{W^{T}HW}}}
#'    Where \eqn{H} is the correlation matrix for signals, and \eqn{W} is the vector
#'    of instrument weights summing to one. We typically calculate the SDM for
#'    each subsystem. E.g. for each instrument we calculate the SDM using all
#'    the signals for that instrument instrument as input. The purpose is to
#'    compensate for the reduction in volatility resulting from combining
#'    multiple signals into one when we want to reach a particular risk target.
#'
#' Recommended upper limit: 2.5.
#'
#' Negative correlations should be floored at 0 before calculation of SDM.
#'
#' @param signal_correlations Correlations matrix for signals
#' @param signal_weights Vector of signal weights
#' @param min_cor Minimum value for each element in `signal_correlations`
#'   matrix
#' @param max_sdm Maximum output value of SDM
#'
#' @return Scalar
#' @export
#'
#' @examples
f_sig_div_mult <- function(
    signal_correlations,
    signal_weights,
    min_cor = 0,
    max_sdm = 2.5) {
  H <- signal_correlations
  w <- signal_weights

  clamped_H <- clamp_matrix_lower(H, min_cor)
  clamp_signal_upper(
    1/sqrt(crossprod(t(w %*% clamped_H),  w)),
    max_signal = max_sdm
  )
}


## LT: RAF, Risk Adjusted Forecast
#' Instrument risk in units of annual standard deviation of price returns
#'
#' @description
#'  *Instrument risk* in units of *annual standard deviation of price returns*
#'    to match units of the rule.
#'
#' @param raw_signal Single number
#' @param current_price Price in currency as a single number
#' @param inst_risk Risk in units of annual standard deviation of price returns
#'   as a single number
#' @return
#' @export
#'
#' @examples
f_risk_adjusted_signal <- function(raw_signal, current_price, inst_risk) {
  raw_signal / (current_price * inst_risk)
}


#' Normalize Signal
#'
#' @description
#' Normalize a value of a single signal based on the a signal normalization
#'   factor. The signal normalization factor for the system can be calculated
#'   with update_signal_normalization_factors().
#'
#' @param raw_signal_value Single raw signal value.
#' @param signal_normalization_factor Normalization target.
#'
#' @return Normalized signal as a number.
#' @export
#'
#' @examples
f_normalize_signal <- function(raw_signal_value, signal_normalization_factor) {
  raw_signal_value * signal_normalization_factor
}

#' Update Signal Normalization Factors
#'
#' @description
#' Each raw signal is scaled by a normalization factor. This normalization
#'   factor is the _required leverage target_.
#'
#' `update_signal_normalization_factors()` calculates the normalization factors
#'   and updates the `normalization_factor` in each algo list.
#'
#' `normalization_factors()` calculates the normalization_factors for all
#'   signals based on the algos list.
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
    as.list(rep(args$equal_norm_factor, n))
  }

  pool_traded <- function(
    signal_tables,
    parsed_algos
  ) {
    ## Get rule name for each algo in the order they appear in the parsed algos
    ## list
    rule_names_by_algo <- get_rule_names_by_parsed_algo(parsed_algos)

    ## Get all unique rule names in a list
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
      ll <- lapply(
        signal_tables[unlist(IDs_grouped_by_rule_names[rule])],
        function(x) {x$raw_signal}
      )
      df <- data.frame(ll)
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
      data_sets, ## instrument data sets
      args = list(min_periods_median_pool_all = 250)
    ) {

    ## Get number of rows from first data set.
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

    ## For each unique rule name, get the first ID of that rule in the
    ## rule_names_by_algo list
    rule_functions <- lapply(all_unique_rule_names,
                             function(x) {
                               #ID = which(x == rule_names_by_algo)[1]
                               #This should be equivalent:
                               ID = match(x, rule_names_by_algo)
                               ## Get functions at each ID in algos list
                               ## Exit rule should be 0 or 1. The combined
                               ## rule is the product of entry and exit rule.
                               ## (Exit rule is optional, i.e constant 1 if
                               ## omitted.)
                               c(
                                 parsed_algos[[ID]]$rule[1],
                                 parsed_algos[[ID]]$rule[2]
                               )
                             }
    )

    ## Make list of algos for simulation
    sim_algos <- list()
    k <- 1
    for(i in seq_along(rule_functions)) {
      for(j in seq_along(data_sets)) {
        sim_algos[[k]] <- list(
          "data" = data_sets[[j]],
          "rule" = list(
            rule_functions[[i]][[1]], ## Entry rule
            rule_functions[[i]][[2]]  ## Exit rule
          )
        )
        k <- k + 1
      }
    }

    ## For each unique rule, apply that rule to all instruments.
    ## Each column in raw_signals_df is a signal vector.
    ## One data frame for each rule.
    ## In each data frame, one column for each instrument.
    raw_signals_df <- data.frame()
    for(i in seq_along(sim_algos)) {
      for(t in (min_periods + 1):num_rows) {

# TODO
# Should handle situation with no exit rule.

# §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
# fix§0039
# No good here:
# trade_system$signal_tables
# trade_system$position_tables
#
#
  # raw_signals_df[i, t] <- apply_rule(
  #   sim_algos[[i]],
  #   trade_system$signal_tables[[i]],
  #   trade_system$position_tables[[i]],
  #   t
  # )

  raw_signals_df[i, t] <- generate_signal(
    prices, ## For now only price data is allowed as input for rules
    sim_algos[[i]],
    rule_function
    #trade_system$signal,
  )
# §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

      }
    }

    ## Calculate cross section medians across all instruments for each
    ## rule.
    ## Columns are vectors of cross section medians across all instruments for
    ## each rule.
    ## Each column in cs_medians corresponds to a rule.
    ## Rows are observations (oldest to newest, so higher row numbers mean newer).
    cs_medians <- data.frame()
    for(i in seq_along(data_sets)) {
      for(j in 1:(num_rows - min_periods)) {

        cs_medians[j, i] <- stats::median(unlist(raw_signals_df[j, ])) ## Row median
      }
    }

    ## Take mean absolute value of each vector of medians
    mav_for_each_rule <- lapply(
      cs_medians,
      function(x) {unlist(mean(abs(x)))}
    )

    target / unlist(mav_for_each_rule)
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


get_normalization_factors <- NA

#' Calculate Normalization Factor From Signal Itself
#'
#' @description
#' Calculate normalization factor based on the mean absolute value of past
#'   values of the signal itself.
#'
#' See update_normalization_factors() for better options.
#'
#' Each raw signal is scaled by a normalization factor. This normalization
#'   factor is the _required leverage target_.
#'   f_indiv_normalization_factor() calculates the normalization factor based
#'   on a vector of (previous) raw signal values.
#'   The normalization factor is
#'
#'   \deqn{| \text{target} / E(\text{abs(signal)}) |}
#'
#'   As a default the expected absolute value of the scaled values will be 1.
#'
#' @param raw_signal A vector of signal values on which the normalization is
#'   based.
#' @param target The target expected value of the signal scaled by the
#'   normalization factor.
#'
#' @return
#' @export
#'
#' @examples
f_indiv_normalization_factor <- function(
    raw_signal_vector,
    target = 1
  ) {
  target/mean(abs(raw_signal_vector))
}

## LT, F14.a
#' Instrument risk, annualized
#'
#' @description
#' Annualised standard deviation of returns in percentage terms.
#' \eqn{P_t}, price in currency at time \eqn{t}.
#' \eqn{n}, number of price observations.
#'
#' \deqn{\overline{R} = \dfrac{1}{n} \sum_{t=2}^n \left(\dfrac{P_t}{P_{t-1}} - 1\right)}
#'
#' \deqn{\text{Instrument Risk} = \sqrt{\dfrac{1}{n-1}\sum_{t=2}^n \left(\left(\dfrac{P_t}{P_{t-1}} - 1\right) - \overline{R}\right)^2}}
#'
#' The square root of the number of business days per year is hard coded:
#'
#'   ```sqrt(252) = 15.87451```
#'
#'   252 is the number of business days in a year.
#'   (We could also round off to 16, and we would probably be fine.)
#'
#' @param prices A vector of prices in currency. Oldest first. Top to bottom:
#'   Older to newer. The last observation is time t.
#' @param window_length Window length. Includes all if `window_length` is NA.
#' @param t Time index.
#'
#' @returns Standard deviation
#' @export
#'
#' @example
#'
f_inst_risk <- function(prices, t, window_length = 25) {
  if(is.na(window_length)) {window_length = t} ## Include all if window_length is NA
  if(t > window_length) {
    sd_ <- stats::sd(f_returns_from_prices(prices[(t - window_length + 1):t])) * 15.87451 ## 252 is the number of business days in a year. sqrt(252) = 15.87451
  } else if (t > 1) {
    sd_ <- sd(f_returns_from_prices(prices[1:t])) * 15.87451
    warning("Window length for instrument risk calculation is bigger than length of price vector.\n")
  } else {
    sd_ <- NA
    warning("inst_risk=NA. Careful! This might break something.\n")
  }
  sd_
}

#' Minimum Exposure
#'
#' @param min_exposure Notional exposure of the minimum trade
#' @param inst_risk Instrument risk as decimal fraction
#' @param instrument_risk_target Target risk as decimal fraction
#'
#' @returns
#' @export
#'
#' @example
#'
f_min_exposure <- function(min_exposure, inst_risk, instrument_risk_target) {
  (min_exposure * inst_risk) / instrument_risk_target
}

## LT F21
#' Minimum capital
#'
#' @description
#' Calculate minimum capital required to trade the system.
#'
#' @param min_exposure Minimum exposure.
#' @param inst_risk Instrument risk.
#' @param instrument_risk_target Instrument risk target.
#'
#' @returns
#' @export
#'
#' @example
#'
f_min_capital <- function(min_exposure, inst_risk, instrument_risk_target) {
  (min_exposure * inst_risk) / instrument_risk_target
}

## LT F22
#' Price Unit Volatility (instrument risk in price units)
#'
#' @param prices A vector of prices in currency. Oldest first. Top to bottom:
#'   Older to newer. The last observation is time t.
#' @param inst_risk instrument risk.
#'
#' @returns
#' @export
#'
#' @example
#'
f_price_unit_vol <- function(price, inst_risk) {
  inst_risk * price
}

## LT F24
#' High Water Mark
#'
#' @description
#' Highest price since entry of a current open position.
#'
#' @param prices A vector of prices in currency. Oldest first. Top to bottom:
#'   Older to newer. The last observation is time t.
#' @param t Time index.
#' @param t_trade_entry Index of current open trade entry time.
#'
#' @returns A single price value
#' @export
#'
#' @example
#'
f_high_water_mark <- function (prices, t, t_trade_entry) {
  max(prices[t_trade_entry:t])
}

## LT F24
#' Low water mark
#'
#' @description
#' Lowest price since entry of a current open position.
#'
#' @param prices A vector of prices in currency. Oldest first. Top to bottom:
#'   Older to newer. The last observation is time t.
#' @param t Time index.
#' @param t_trade_entry Time index for the time when the trade is opened.
#'
#' @returns
#' @export
#'
#' @example
#'
f_low_water_mark <- function (prices, t, t_trade_entry) {
  min(prices[t_trade_entry:t])
}

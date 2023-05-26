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
  instrument_risk_target / instrument_risk
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
    method = "Pearson",
    min_cor
    ) {
  cor_mat <- switch(
    method,
    "Pearson" = f_signal_cor_mat(subsystem_returns), #stats::cor(subsystem_returns),
    "ewa" = f_ewa_cor(subsystem_returns)
  )

  ## Check if any correlation is NA
  if(sum(is.na(cor_mat)) > 0) {
    warning("NAs in correlation matrix have been replaced by min_cor value. NAs in a correlation matrix are common when previous returns are identical, resulting in standard deviations of zero.
Replacing NAs in correlation matrix by min_cor value is supposed to fix this problem.")

    ## Replace missing values (divide-by-zero NA's) with minimum correlation
    cor_mat[is.na(cor_mat)] <- min_cor
  }

  cor_mat
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
f_ewa_cor <- function(x, y, lookback, min_cor_mat) {
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

  if(!is.integer(window_length)) {stop("window_length must be an integer (e.g. 25L).")}
  if(!(window_length > 0L)) {stop("window_length must be positive.")}

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
    1 / sqrt(crossprod(t(w %*% clamped_H),  w)),
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
    1 / sqrt(crossprod(t(w %*% clamped_H),  w)),
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
  target / mean(abs(raw_signal_vector))
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
    ## t - window_length is the time index before the first return we want to calculate
    sd_ <- stats::sd(f_returns_from_prices(prices[(t - window_length):t])) * 15.87451 ## 252 is the number of business days in a year. sqrt(252) = 15.87451
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
#' Highest price since entry of a current (time t) open position.
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

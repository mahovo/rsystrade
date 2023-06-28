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

## ST, p. 298
#' Calculate Exponentially Weighted Average
#'
#' Calculate EWA of vector \eqn{x} at time \eqn{t} based on a lookback window.
#'   If the length of the lookback window is \eqn{\lambda}, the range of the lookback
#'   window is \eqn{[t-\lambda, t-1]}.
#'
#' @param x Vector. Top to bottom: Oldest to newest.
#' @param lambda Smoothing parameter. If no lambda is provided, set lambda to  1 - (2 / (1 + L)).
#' @param lookback Lookback window length as positive integer. If no `lookback`
#'   is provided, the entire \eqn{x} vector will be used.
#'
#' @return Single exponentially weighted average value
#' @export
#'
#' @details
#' \deqn{E[X_t | X_{t-1}] = \frac{1}{\sum_{i=0}^{t-2}\lambda^i}\sum_{j=1}^{t-1}\lambda^{j-1} X_{t-j}}
#'
#' @examples
#'
#' @references Tsay: Analysis Of Financial Time Series
f_ewa <- function(x, lambda = NA, lookback = NA) {

  if(is.na(lookback)) {
    L <- length(x)
    x_window <- x
  } else {
    L <- lookback
    x_window <- utils::tail(x, L)
  }
  if(!is.integer(L)) {stop("lookback must be an integer (e.g. 25L).")}
  if(!(L >= 0L)) {stop("lookback must be zero or positive.")}

  if(is.na(lambda)) {
    lambda <- 1 - (2 / (1 + L))
  }

  ## Reversing the order of weights instead of reversing the order of observations.
  w <- lambda^((L - 1):(0))
  drop((w %*% x_window) / sum(w))
}

#' Exponential Weighted Standard Deviation
#'
#' @param x Input vector.
#' @param lambda Smoothing parameter.
#' @param lookback Length of lookback window.
#'
#' @return Single value
#' @export
#'
#' @examples
f_ewa_sd <- function(
    x,
    lambda = NA,
    lookback = NA
  ) {
  if(is.na(lookback)) {
    x_window <- x
  } else {
    x_window <- utils::tail(x, lookback)
  }

  mu_x <- f_ewa(x_window, lambda, lookback)

  sqrt(
    f_ewa(
      (x_window - as.vector(mu_x))^2,
      lambda,
      lookback
    )
  )
}

#' Exponential Weighted Covariance
#'
#' @param x Vector.
#' @param y Vector.
#' @param lookback Integer. Lookback window length. If no `lookback` is
#'   provided, the entire \eqn{x} and \eqn{y} vectors will be used.
#'
#' @return Number
#' @export
#'
#' @examples
f_ewa_cov <- function(
    x,
    y,
    lambda = NA,
    lookback = NA
  ) {
  if(is.na(lookback)) {
    x_window <- x
    y_window <- y
  } else {
    x_window <- utils::tail(x, lookback)
    y_window <- utils::tail(y, lookback)
  }
  mu_x <- f_ewa(x_window, lambda, lookback)
  mu_y <- f_ewa(y_window, lambda, lookback)
  f_ewa(
    (x_window - mu_x) * (y_window - mu_y),
    lambda,
    lookback
  )
}

#' Exponential Weighted Covariance Matrix
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
ew_cov_matrix <- function(data, lambda = NA, lookback = NA) {
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
      cov_mat[i, j] <- f_ewa_cov(
        data_window[ , i], data_window[ , j],
        lambda = lambda,
        lookback = L
      )
    }
  }
  cov_mat
}

#' Signals Correlation Matrix
#'
#' @description
#' Calculates the correlation matrix of signal vectors. Typically calculates the
#'   correlation matrix for a subsystem of all the signal vectors affecting a
#'   single instrument in the system.
#'
#' @param signals Dataframe where each column is a signal vector.
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
#'   subsystem (daily) percentage returns.
#' @param method Method.
#' * `"Pearson"`, Pearsons Correlation Coefficient
#' * `ewa`, Exponential Weighted Average
#'   \deqn{E[X_t | X_{t-1}] = \frac{1}{\sum_{i=0}^{t-2}\lambda^i}\sum_{j=1}^{t-1}\lambda^{j-1} X_{t-j}}
#'
#' @return Correlation matrix
#' @export
#'
#' @examples
#'
#' @references Tsay: Analysis Of Financial Time Series
#'
#' @seealso [f_ewa()]
f_subsystem_ret_cor_mat <- function(
    subsystem_returns,
    method = "Pearson",
    min_cor,
    ...
    ) {
  cor_mat <- switch(
    method,
    "Pearson" = f_signal_cor_mat(subsystem_returns), #stats::cor(subsystem_returns),
    "ewa" = f_ewa_cor(subsystem_returns, lambda, lookback)
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


#' Exponential Weighted Correlation
#'
#' @param x Vector.
#' @param y Vector.
#' @param lookback Integer. Lookback window length. If no `lookback` is
#'   provided, the entire \eqn{x} and \eqn{y} vectors will be used.
#'
#' @details
#'   \deqn{s^2(X) = \frac{1}{n - 1}\sum_i^n (X_i - \mu_X)^2}
#'   \deqn{\text{COV}(X, Y) = \frac{1}{n - 1} \sum_i^n ((X_i - \mu_X)(Y_i - \mu_Y))}
#'   \deqn{\text{COR}(X, Y) = \frac{1}{n - 1} \frac{\sum_i^n ((X_i - \mu_X)(Y_i - \mu_Y))}{s(X) s(Y)}}
#'
#' @return Number
#' @export
#'
#' @examples
f_ewa_cor <- function(x, y, lambda = NA, lookback = NA) {
  if(is.na(lookback)) {
    x_window <- x
    y_window <- y
  } else {
    x_window <- utils::tail(x, lookback)
    y_window <- utils::tail(y, lookback)
  }
  #mu_x <- f_ewa(x_window, lambda, lookback)
  #mu_y <- f_ewa(y_window, lambda, lookback)

  # num <- f_ewa(
  #   (x_window - mu_x) * (y_window - mu_y),
  #   lambda,
  #   lookback
  # )
  num <- f_ewa_cov(
    x_window,
    y_window,
    lambda,
    lookback
  )

  # denom <- sqrt(
  #   f_ewa(
  #     (x_window - mu_x)^2,
  #     lambda,
  #     lookback
  #   ) * f_ewa(
  #         (y_window - mu_y)^2,
  #         lambda,
  #         lookback
  #       ),
  #   lambda,
  #   lookback
  # )
  denom <- f_ewa_sd(x_window, lambda, lookback) * f_ewa_sd(y_window, lambda, lookback)

  num / denom
}

#' Exponential Weighted Correlation Matrix
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
ew_cor_matrix <- function(data, lambda = NA, lookback = NA) {
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
      cor_mat[i, j] <- f_ewa_cor(
        data_window[ , i], data_window[ , j],
        lambda = lambda,
        lookback = L
      )
    }
  }
  cor_mat
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
#' At \eqn{t_0} use `last_ewma` = `current_price`.
#'
#' @param current_price Current price.
#' @param last_ewma Last exponential weighted moving average.
#' @param window_length Window length.
#'
#' @return Instrument risk in units of percentage of price


# f_price_volatility_ewma <- function(
#     current_price, ## Price at time t
#     last_ewma, ## EWMA at time t - 1
#     window_length = 25
#     ) {
#
#   if(!is.integer(window_length)) {stop("window_length must be an integer (e.g. 25L).")}
#   if(!(window_length > 0L)) {stop("window_length must be positive.")}
#
#   a = 2 / (1 + window_length)
#   a * current_price + (1 - a) * last_ewma
# }


## LT, p. 220
## IDM: LT, F25.1
#' Instrument Risk Target
#'
#' @description
#' Calculate instrument risk target.
#'
#' @param system_risk_target Annualized volatility target in percentages.
#' @param IDM Instrument Diversification Multiplier.
#'    \deqn{\text{IDM} = \frac{1}{\sqrt{W^{T}HW}}}
#'    where \eqn{H} is the correlation matrix for returns, and \eqn{W} is the vector
#'    of instrument weights summing to one.
#'
#' @return Decimal fraction (percentage divided by 100)
#' @export
#'
#' @examples
#'
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
#' @param inst_ret_cor_mat Instrument returns correlations matrix.
#' @param instrument_weights Vector of weights.
#' @param min_cor Minimum value for each element in `inst_ret_cor_mat`
#'   matrix.
#' @param max_idm Maximum output value of IDM.
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
#' @param signal_correlations Correlations matrix for signals.
#' @param signal_weights Vector of signal weights.
#' @param min_cor Minimum value for each element in `signal_correlations`
#'   matrix.
#' @param max_sdm Maximum output value of SDM.
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
#' @param raw_signal Single number.
#' @param current_price Price in currency as a single number.
#' @param inst_risk Risk in units of annual standard deviation of price returns
#'   as a single number.
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
#' Instrument Risk
#'
#' @description
#' Standard deviation of returns in percentage terms. Annualized by default.
#'
#' @param t The time index of the observation to estimate.
#' @param prices A vector of prices in currency. Oldest first. Top to bottom:
#'   Older to newer. The last observation is time `t-1`.
#' @param window_length Window length of the returns. Includes all if
#'   `window_length` is NA.
#' @param annualized TRUE if annualized output is desired.
#' @param periods Number of periods per year. E.g. 252 for daily input.
#' @param method Standard deviation calculation method (see details below).
#'   * `return_rate_sd`
#'   * `return_rate_ewsd`
#'
#' @details
#' \eqn{p_t}, price in currency at time \eqn{t}.
#'
#' \eqn{r_t}, Returns at time \eqn{t}.
#'
#' \eqn{n}, number of price observations. \eqn{t \leq n+1}.
#'
#' Notice, if today is time \eqn{T}, and you want to calculate risk from past
#'   data at time 1 to \eqn{T - 1}, set the \eqn{t} parameter to \eqn{T}. The
#'   last return observation in the lookback window is \eqn{r_{t-1}},
#'   which is calculated from prices \eqn{p_{t-2}} and \eqn{p_{t-1}}.
#'   The input price vector must be of length at least `window_length` + 1.
#'
#' Methods
#' * `return_rate_sd`
#' \deqn{r_t = \dfrac{p_t}{p_{t-1}} - 1}
#' \deqn{\overline{r_t} = \dfrac{1}{n} \sum_{t=2}^n r_t}
#' \deqn{\sigma = \sqrt{\dfrac{1}{n-1}\sum_{t=2}^n \left(r_t - \overline{r}\right)^2}}
#'
#' * `return_rate_ewsd`
#' \deqn{r_t = \dfrac{p_t}{p_{t-1}} - 1}
#' \deqn{\mu_t \equiv E[r_t | r_{t-1}] = \frac{1}{\sum_{i=0}^{t-2} \lambda^i}\sum_{j=1}^{t-1} \lambda^{j-1} r_{t-j}}
#' \deqn{\sigma_t^{\text{ewa}} =  \sqrt{\frac{1}{\sum_{i=0}^{t-2} \lambda^i}\sum_{j=1}^{t-1} \lambda^{j-1} (\mathbf{r}_{t-j} - \mu_{t-j})(\mathbf{r}_{t-j} - \mu_{t-j})^{'}}}
#' \deqn{0 < \lambda < 1}
#'
#' Additional parameters:
#'
#' * `lambda` Smoothing parameter. If no lambda is provided, set lambda to  1 - (2 / (1 + L)).
#'
#' @returns Standard deviation
#' @export
#'
#' @example
#'
f_inst_risk <- function(
    t,
    prices,
    window_length = NA,
    annualized = TRUE,
    periods = 252,
    method = 1,
    ...
  ) {

  scaling_factor <- if(annualized) {
      sqrt(periods)
    } else {
      1
    }

  if(is.na(window_length)) {
    window_length = t - 2 ## Minus two because we want to estimate r_t based on
                          ## r_1 to r_{t-1}, and returns vector is one shorter
                          ## than price vector.
  }

  return_rate_sd <- function(time_range) {
    stats::sd(f_returns_from_prices(prices[time_range]))
  }
  return_rate_ewsd <- function(time_range, lambda) {
    f_ewa_sd(
      f_returns_from_prices(prices[time_range]),
      lambda,
      lookback = as.integer(window_length)
    )
  }

  volatility <- switch(
    method,
    "1" = return_rate_sd,
    "2" = return_rate_ewsd
  )

  if(t > window_length + 1) {
    ## (t - window_length - 1) is the time index before the first return we want to calculate
    sd_ <- volatility((t - window_length - 1):(t - 1), ...) * scaling_factor
  } else if (t > 1) {
    sd_ <- volatility(1:(t - 1), ...) * scaling_factor
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
#' @param min_exposure Minimum exposure
#' @param inst_risk Instrument risk
#' @param instrument_risk_target Instrument risk target
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
#' Price Unit Volatility
#'
#' @description
#' Instrument risk in price units.
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

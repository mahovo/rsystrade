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
   risk_target/instrument_risk
}


## LT F14 [p. 93]
#' Notional exposure
#'
#' @description
#' Calculate the notional exposure.
#'
#' @param signal Input signal. Should be normalized to a an expected absolute
#'   value of 1 and capped between -2 and 2.
#' @param capital Trading capital in account currency.
#' @param required_leverage_factor Required leverage factor. Indstrument risk
#'   target divided by instrument risk (in same units).
#' @param weight
#'
#' @returns
#' @export
#'
#' @example
#'
f_notional_exposure <- function(
    signal, ## normalized and clamped
    capital,
    required_leverage_factor,
    weight) {
  signal * capital * required_leverage_factor * weight
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
  cor(signals)
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
#'
#' @return Correlation matrix
#' @export
#'
#' @examples
f_subsystem_ret_cor_mat <- function(
    subsystem_returns
    ) {
  cor(subsystem_returns)
}

## ST F6 [p. 298]
#' Instrument risk in units of percentage of price
#'
#' @description
#' Price Volatility as Exponentially Weighted Moving Average (EWMA)
#'
#' $E_{t}$: EWMA til tid $t$
#'
#' $P_{t}:$ Price at time t
#' $a$, Smoothing Parameter
#' $L$: Window length
#' $$a = \frac{2}{1 + L}$$
#' $$E_{t} = a \cdot P_{t} + E_{t - 1}(1 - a)$$
#'
#' At $t_0$ use `last_ewma` = `current_price`.
#'
#' @param current_price
#' @param last_ewma
#' @param smoothing_parameter
#' @param window_length
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
#'    $$\text{IDM} = \frac{1}{\sqrt{W^{T}HW}}$$
#'    Where $H$ is the correlation matrix for returns, and $W$ is the vector
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
#' $$\text{IDM} = \frac{1}{\sqrt{W^{T}HW}}$$
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
  H <- instrument_correlations
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
#' $$\text{SDM} = \frac{1}{\sqrt{W^{T}HW}}$$
#'    Where $H$ is the correlation matrix for signals, and $W$ is the vector
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
#' @param weights Vector of signal weights
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
#' @param signal
#' @param normalization_factor
#'
#' @return Normalized signal as a number.
#' @export
#'
#' @examples
f_normalize_signal <- function(signal, normalization_factor = 1) {
  signal * normalization_factor
}

#' Calculate Normalization Factor
#'
#' @description
#' The raw signal is scaled by a normalization factor.
#'   f_normalization_factor() calculates the normalization factor based
#'   on a vector of (previous) raw signals.
#'   The normalization factor is
#'
#'   $$| \text{target} / E(\text{signal}) | $$
#'
#'   As a default the expected absolute value of the scaled values will be 1.
#'
#' @param raw_signal A vector of signals on which the normalization is based.
#' @param target The target expected value of the signal scaled by the
#'   normalization factor.
#'
#' @return
#' @export
#'
#' @examples
f_normalization_factor <- function(raw_signal, target = 1) {
  target/mean(abs(raw_signal))
}

## LT, F14.a
#' Instrument risk, annualized
#'
#' Annualised standard deviation of returns in percentage terms.
#' $P_t$, price in currency at time $t$.
#' $n$, number of price observations.
#'
#' $$ \overline{R} = \dfrac{1}{n} \sum_{t=2}^n \left(\dfrac{P_t}{P_{t-1}} - 1\right)$$
#'
#' $$\text{Instrument Risk} = \sqrt{\dfrac{1}{n-1}\sum_{t=2}^n \left(\left(\dfrac{P_t}{P_{t-1}} - 1\right) - \overline{R}\right)^2}$$
#'
#' The square root of the number of business days per year is hard coded:
#'   sqrt(252) = 15.87451
#'   252 is the number of business days in a year.
#'   (We could also cound off to 16, and we would probably be fine.)
#'
#' @param prices A time series of prices in currency. Newest first.
#'   Top to bottom: Newer to older.
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
    sd_ <- sd(returns_from_prices(prices[(t - window_length + 1):t])) * 15.87451 ## 252 is the number of business days in a year. sqrt(252) = 15.87451
  } else if (t > 1) {
    sd_ <- sd(returns_from_prices(prices[1:t])) * 15.87451
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
#' @param min_exposure
#' @param inst_risk
#' @param instrument_risk_target
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
#' @param inst_risk Instrument risk.
#' @param price A vector of prices in currency. Newest first. Top to bottom:
#'   Newer to older.
#'
#' @returns
#' @export
#'
#' @example
#'
f_price_unit_vol <- function(inst_risk, price) {
  inst_risk * price
}

## LT F24
#' High water mark
#'
#' @param prices
#' @param t
#' @param t_trade_open
#'
#' @returns
#' @export
#'
#' @example
#'
f_high_water_mark <- function (prices, t, t_trade_open) {
  max(prices[t_trade_open:t])
}

## LT F24
#' Low water mark
#'
#' @param prices A vector of prices in currency. Newest first. Top to bottom:
#'   Newer to older.
#' @param t Time index.
#' @param t_trade_open Time index for the time when the trade is opened.
#'
#' @returns
#' @export
#'
#' @example
#'
f_low_water_mark <- function (prices, t, t_trade_open) {
  min(prices[t_trade_open:t])
}

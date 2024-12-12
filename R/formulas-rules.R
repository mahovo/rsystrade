## Notes
## 1)
## Decimal fractions are used instead of percentages throughout.
## Example: 87 percent is expressed as 0.87.
##
## 2)
## "LT" refers to Robert Carver: Leveraged Trading
## "ST" refers to Robert Carver: Systematic Trading
## "F" for "formula".



## Alpha ====


## LT F12
#' Moving Average Step
#'
#' @description
#'
#' !!! Use `rolling_window_step()` instead with `func = f_average`. !!!
#'
#' Calculates average of $n$ items prior to time $t$ in data vector.
#' $N$ (length of data vector) smaller than or equal to $n$ is accepted.
#' This will not be the desired moving average, but will also not fail.
#'
#' `f_moving_average_step()` doesn't actually calculate a "moving" average. It
#' only calculates one step of the moving average, i.e. the mean of the past $n$
#' prices.
#'
#' If the length of the vector is shorter than the window length, the
#' window length will be set to the length of the price vector, and a warning
#' will be given. The function will not fail or abort in this case.
#'
#' @param t Time index.
#' @param data A vector. Oldest first. Top to bottom:
#'   Older to newer.
#' @param last_data_id Time index of the last observation in the window.
#' @param window_length Window length.
#'
#' @details
#' Note that in a typical use case, we want to calculate the average for the
#'   most recent observations. So if we are at time `t`, we want to calculate
#'   the average for the interval \eqn{[t - \text{window length}, t - 1]}.
#'
#' If `t` is given a value and `last_data_id` is `NA`, the last observation in
#'   the window will be `t - 1`. Alternatively one could set `last_data_id` and
#'   let `t` be `NA`. If `t` and `last_data_id` are both `NA`, `last_data_id`
#'   will be set to the length of the data vector. If `t` and `last_data_id` are
#'   both given a value, `t` will be ignored.
#'
#' @returns A single number. One step of a moving average.
#' @export
#'
#' @example
# f_moving_average_step <- function(
#     t = NA,
#     data,
#     last_data_id = NA,
#     window_length,
#     method = "simple",
#     ...
#   ) {
#   #N <- length(data)
#   #stopifnot(N >= window_length)
#
#   last <- if(is.na(last_data_id)) {
#     if(is.na(t)) {
#       length(data)
#     } else {t - 1}
#   } else {last_data_id}
#
#   if(last < (window_length + 1)) { ## Handle t less than or equal to n
#     ma_t <- f_average(
#       data = data[1:last],
#       method = method,
#       ...
#     )
#     warning("Length of vector is shorter than or equal to desired MA window length.
#                   Moving average calculated for shorter window.\n")
#   } else {
#     ma_t <- f_average(
#       data = data[(last - window_length + 1):last],
#       method = method,
#       ...
#     )
#   }
#   ma_t
#
#   # if(t > window_length) {
#   #   #ma <- sum(data[(t - n + 1):N]) / n
#   #   ma <- f_average(
#   #     data = data[(t - window_length + 1):t],
#   #     method = method,
#   #     ...
#   #   )
#   # } else { ## Handle t less than or equal to window_length
#   #   ma <- f_average(
#   #     data = data[1:t],
#   #     method = method,
#   #     ...
#   #   )
#   #   warning("Data vector is shorter than MA window. Window length modified.")
#   # }
#   # ma
# }


# §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
# fix§0019
# See also zoo::rollmean
# Maybe use if depending on `zoo` anyway.
# §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
## LT F12
#' Calculate Moving Average Vector
#'
#' @description
#'
#' !!! Use `rolling_window()` instead with `func = f_average`. !!!
#'
#' Calculates moving average for each element in a vector.
#' `N` (length of price vector) smaller than or equal to `n` is accepted.
#' This will not be the desired moving average, but will also not fail.
#'
#' @param data A vector, e.g. a column in a data frame. Newest first. Top to
#'   bottom: Newer to older.
#' @param first_data_id Time index of the first observation in the window.
#' @param last_data_id Time index of the last observation in the window.
#' @param window_length Moving Average window length.
#' @param method
#' @param ...
#'
#' @returns A vector of moving averages
#' @export
#'
#' @example
#'
# f_moving_average <- function(
#     data,
#     first_data_id = 1,
#     last_data_id = NA,
#     window_length,
#     method = "simple",
#     ...
#   ) {
#
#   data_length <- length(data)
#
#   first <- if(is.na(first_data_id)) {
#     1
#   } else {first_data_id}
#   last <- if(is.na(last_data_id)) {
#     data_length
#   } else {last_data_id}
#
#   if(!is.integer(window_length)) {stop("window_length must be an integer (e.g. 16L).")}
#   if(!(window_length > 0L)) {stop("window_length must be positive.")}
#
#   # rolling_window <- function(t) {
#   #   f_average(
#   #     data = data[(t - window_length + 1):t],
#   #     method = method,
#   #     ...
#   #   )
#   # }
#   #
#   # ## Expanding window is a stupid idea!
#   # expanding_window <- function(t) {
#   #   f_average(
#   #     data = data[first:t],
#   #     method = method,
#   #     ...
#   #   )
#   # }
#
#   #stopifnot(N >= n)
#   ma <- rep(NA, data_length)
#
#   for(t in (first + 1):(last + 1)) {
#     ma[t] <- f_moving_average_step(
#       t = t,
#       data = data,
#       last_data_id = NA,
#       window_length = window_length,
#       method = method,
#       ...
#     )
#   }
#   ma
#
#   # for(t in first:(first + window_length - 1)) { ## Handle t less than or equal to n
#   #   ma[t] <- f_average(
#   #     data = data[first:t],
#   #     method = method,
#   #     ...
#   #   )
#   # }
#   # if(last >= window_length) {
#   #   for(t in (first + window_length):last) {
#   #     # ma[t] <- switch(
#   #     #   window_type,
#   #     #   "rolling" = rolling_window(t),
#   #     #   "expanding" = expanding_window(t)
#   #     # )
#   #     ma[t] <- f_average(
#   #       data = data[(t - window_length + 1):t],
#   #       method = method,
#   #       ...
#   #     )
#   #   }
#   # } else {warning("Length of vector is shorter than or equal to desired MA window length.
#   #                 Moving average calculated for shorter window.\n")}
#   # ma
# }

#' Moving Standard Deviation Step
#'
#' @description
#'
#' !!! Use `moving_step()` instead with `func = f_sd`. !!!
#'
#' Calculates standard deviation of $n$ items prior to time $t$ in data vector.
#' $N$ (length of data vector) smaller than or equal to $n$ is accepted.
#' This will not be the desired moving average, but will also not fail.
#'
#' `f_moving_sd_step()` doesn't actually calculate a "moving" standard
#'   deviation. It only calculates one step of the moving average, i.e. the mean
#'   of the past $n$ prices.
#'
#' If the length of the vector is shorter than the window length, the
#'   window length will be set to the length of the price vector, and a warning
#'   will be given. The function will not fail or abort in this case.
#'
#' @param t Time index.
#' @param data A vector. Oldest first. Top to bottom:
#'   Older to newer. The last observation is time `t`.
#' @param last_data_id Time index of the last observation in the window.
#' @param window_length Window length.
#'
#' @details
#' Note that in a typical use case, we want to calculate the average for the
#'   most recent observations. So if we are at time `t`, we want to calculate
#'   the average for the interval \eqn{[t - \text{window length}, t - 1]}.
#'
#' If `t` is given a value and `last_data_id` is `NA`, the last observation in
#'   the window will be `t - 1`. Alternatively one could set `last_data_id` and
#'   let `t` be `NA`. If `t` and `last_data_id` are both `NA`, `last_data_id`
#'   will be set to the length of the data vector. If `t` and `last_data_id` are
#'   both given a value, `t` will be ignored.
#'
#' @returns A single number. One step of a moving average.
#' @export
#'
#' @example
# f_moving_sd_step <- function(
#     data,
#     first_data_id = 1,
#     last_data_id = NA,
#     window_length,
#     method = "unbiased",
#     ...
#   ) {
#   #N <- length(data)
#   #stopifnot(N >= window_length)
#
#   last <- if(is.na(last_data_id)) {
#     if(is.na(t)) {
#       length(data)
#     } else {t - 1}
#   } else {last_data_id}
#
#   if(last < (window_length + 1)) { ## Handle t less than or equal to n
#     std_dev_t <- f_sd(
#       data = data[1:last],
#       method = method,
#       ...
#     )
#     warning("Length of vector is shorter than or equal to desired window length.
#                   Standard calculated for shorter window.\n")
#   } else {
#     std_dev_t <- f_sd(
#       data = data[(last - window_length + 1):last],
#       method = method,
#       ...
#     )
#   }
#   std_dev_t
# }

#' Calculate Moving Standard Deviation Vector
#'
#' @description
#'
#' !!! Use `moving()` instead with `func = f_sd`. !!!
#'
#' Calculates moving standard deviation for each element in a vector.
#' `N` (length of price vector) smaller than or equal to `n` is accepted.
#' This will not be the desired moving average, but will also not fail.
#'
#' @param data A vector, e.g. a column in a data frame. Newest first. Top to
#'   bottom: Newer to older.
#' @param first_data_id Time index of the first observation in the window.
#' @param last_data_id Time index of the last observation in the window.
#' @param window_length Moving standard deviation window length.
#' @param method
#' @param ...
#'
#' @returns A vector of moving averages
#' @export
#'
#' @example
# f_moving_sd <- function(
#     data,
#     first_data_id = 1,
#     last_data_id = NA,
#     window_length,
#     method = "unbiased",
#     ...
#   ) {
#
#   data_length <- length(data)
#
#   first <- if(is.na(first_data_id)) {
#     1
#   } else {first_data_id}
#   last <- if(is.na(last_data_id)) {
#     data_length
#   } else {last_data_id}
#
#   if(!is.integer(window_length)) {stop("window_length must be an integer (e.g. 16L).")}
#   if(!(window_length > 0L)) {stop("window_length must be positive.")}
#
#   #stopifnot(N >= n)
#   std_dev <- rep(NA, data_length)
#
#   for(t in (first + 1):(last + 1)) {
#     std_dev[t] <- f_moving_sd_step(
#       t = t,
#       data = data,
#       last_data_id = NA,
#       window_length = window_length,
#       method = method,
#       ...
#     )
#   }
#   std_dev
# }


## Stop Loss ====

## LT F24
#' Stop loss level
#'
#' @param hwm High Water Mark.
#' @param lwm Low Water Mark.
#' @param stop_loss_gap Stop loss gap.
#' @param direction Is current trade long or short? 1 for long, -1 for short.
#' @param rnd If TRUE, add small random amount to stop loss level. Negative if
#'   short.
#'
#' @returns
#' @export
#'
#' @example
#'
f_stop_loss_level <- function(
    hwm,
    lwm,
    stop_loss_gap,
    direction = 0,
    rnd = FALSE) {

  RND_MIN <- 0.01
  RND_MAX <- 0.03

  rnd_val <- stats::runif(1, RND_MIN, RND_MAX)

  if(direction == 1){ ## If long
    hwm - stop_loss_gap + rnd_val * rnd
  } else {lwm + stop_loss_gap - rnd_val * rnd}
}

## LT F23
#' Calculate Stop Loss Gap
#'
#' @description
#' Calculate stop loss gap. Stop loss only takes effect if the difference
#'   between the current price and the stop loss level is bigger than the stop
#'   loss gap.
#'
#' @param price_unit_vol Volatility of returns in price units.
#' @param stop_loss_fraction Stop loss fraction.
#'
#' @returns
#' @export
#'
#' @example
#'
f_stop_loss_gap <- function(price_unit_vol, stop_loss_fraction) {
  price_unit_vol * stop_loss_fraction
}

## F15, LT, p. 113
#' Decimal fraction of capital at risk per trade for system with one instrument
#'   only.
#'
#' @description
#' When using stop loss to exit trades using while trading one instrument only.
#'
#' @param risk_target Annualized system risk target in percentage terms given
#'   as a decimal fraction
#' @param stop_loss_fraction Stop loss fraction
#'
#' @returns
#' @export
#'
#' @example
#'
f_stop_loss_risky_capital_pct <- function(risk_target, stop_loss_fraction) {
  risk_target * stop_loss_fraction
}







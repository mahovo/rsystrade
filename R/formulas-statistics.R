#' Calculate Average
#'
#' @description
#' Calculate average using a specified method.
#'
#' @param x Vector.
#' @param method Calculation method.
#'   * `"simple"` Simple mean.
#'     * @seealso [mean()]
#'   * `"ewa"` Exponentially Weighted Average.
#'     * Additional parameters: `lambda`(number), `lookback` (integer).
#'     * @seealso [f_ewa()]
#' @param ...
#'
#' @return Vector of moving standard deviations.
#' @export
#'
#' @examples
#' f_average(1:10, method = "ewa", lambda = 0.8, lookback = 4L)
f_average <- function(
    x,
    method = "simple",
    ...
) {
  switch(
    method,
    "simple" = mean(x),
    "ewa" = f_ewa(x, ...)
  )
}

## ST, p. 298
#' Calculate Exponentially Weighted Average
#'
#' @description
#' Calculate EWA of vector \eqn{x}.
#'
#' @param x Vector. Top to bottom: Oldest to newest.
#' @param lambda Smoothing parameter. If no lambda is provided, set lambda to
#'   \eqn{1 - (2 / (1 + L))}.
#' @param lookback Lookback window length as positive integer. This is the tail
#'   of the vector. If no `lookback` is provided, the entire \eqn{x} vector will
#'   be used.
#'
#' @return Single exponentially weighted average value
#' @export
#'
#' @details
#' \deqn{E[X_t | X_{t-1}] = \frac{1}{\sum_{i=0}^{t-2}\lambda^i}\sum_{j=1}^{t-1}\lambda^{j-1} X_{t-j}}
#'
#' Note: According to the formula, if the length of the lookback window is \eqn{L},
#'   the range of the lookback window is \eqn{[t-L, t-1]}.
#'   In practice we may want to calculate an average of prices in a window
#'   from day \eqn{t - L + 1} to day \eqn{t} as soon as (the same day as) we have the
#'   price for day \eqn{t}, rather than the next day as the formula would imply.
#'   In this case the last  observation in the window would have the same time
#'   index as the calculated average. `f_ewa()` does not involve a time index,
#'   but these considerations come in to play when  applying `f_ewa()` with
#'   `rolling_window()` or `rolling_window_step()`.
#'
#' @examples
#' f_ewa(1:10, lambda = 0.8, lookback = 3L)
#'
#' @references Tsay: Analysis Of Financial Time Series (3rd Ed., 10.1, p. 507)
f_ewa <- function(x, lambda = NA, lookback = NA) {
  exists_and_not_null("x")

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

#' Calculate Standard Deviation
#'
#' @param x Vector.
#' @param method Calculation method.
#'   * `"unbiased"` Basic unbiased standard deviation.
#'     * @seealso [stats::sd()]
#'   * `"ewa"` Based on Exponentially Weighted Average.
#'     * Additional parameters: `lambda`(number), `lookback` (integer).
#'     * @seealso [f_ewa_sd]
#' @param ...
#'
#' @return Vector of moving standard deviations.
#' @export
#'
#' @examples
#' f_sd(1:10, method = "ewa", lambda = 0.8, lookback = 3L)
f_sd <- function(
    x,
    method = "unbiased",
    ...
) {
  switch(
    method,
    "unbiased" = stats::sd(x),
    "ewa" = f_ewa_sd(x, ...)
  )
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
#' @details
#' @seealso [f_ewa()]
#'
#' @examples
#' f_sd(1:10, lambda = 0.8, lookback = 3L)
f_ewa_sd <- function(
    x,
    lambda = NA,
    lookback = NA
) {
  exists_and_not_null("x")

  if(is.na(lookback)) {
    x_window <- x
  } else {
    x_window <- utils::tail(x, lookback)
  }

  N <- length(x)

  if(is.na(lookback)) {
    lookback <- N
  } else {
    if(N < lookback) {lookback <- N}
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
#' @param lambda Smoothing parameter.
#' @param lookback Integer. Lookback window length. If no `lookback` is
#'   provided, the entire \eqn{x} and \eqn{y} vectors will be used.
#'
#' @return Number
#' @export
#'
#' @details
#' @seealso [f_ewa()]
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
#' @description
#' Exponentially Weighted Moving-Average estimate of the covariance matrix.
#'
#' @param data Dataframe.
#' @param lambda Smoothing parameter.
#' @param lookback Integer. Lookback window length.
#'
#' @details
#' @seealso [f_ewa()]
#'
#' @return Matrix
#' @export
#'
#' @examples
#'
#' @references Tsay: Analysis Of Financial Time Series (3rd Ed., 10.1, p. 507)
f_ewa_cov_mat <- function(
    data,
    lambda = NA,
    lookback = NA
  ) {
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

#' Correlation Matrix
#'
#' @description
#' Calculate correlation of columns in a data frame.
#'
#' @param data Data frame.
#' @param method Method of correlation calculation as character string.
#'   * `"Pearson"`
#'   * `"ewa"` Exponentially Weighted Correlation. @seealso [f_ewa_cor_mat()]
#'     * Additional parameters: `lambda` (number), `lookback` (integer)
#'
#' @param ...
#'
#' @details
#' For `method = "ewa"`: If `L == NA`, `L` will be set to the number of rows of
#'   the data. If `lambda == NA`, `lambda` will be set to
#'   `1 - (2 / (1 + L))`.
#'
#' @return Correlation matrix
#' @export
#'
#' @examples
f_cor_mat <- function(
    data,
    method = "Pearson",
    ...
) {
  switch(
    method,
    "Pearson" = stats::cor(data),
    "ewa" = f_ewa_cor_mat(data, ...)
  )
}

#' Covariance Matrix
#'
#' @description
#' Calculate covariance of columns in a data frame.
#'
#' @param data Data frame.
#' @param method Method of covariance calculation as character string.
#'   * `"Pearson"`
#'   * `"ewa"` Exponentially Weighted Correlation. @seealso [f_ewa_cov_mat()]
#'     * Additional parameters: `lambda` (number), `lookback` (integer)
#'
#' @param ...
#'
#' @details
#' For `method = "ewa"`: If `L == NA`, `L` will be set to the number of rows of
#'   the data. If `lambda == NA`, `lambda` will be set to
#'   `1 - (2 / (1 + L))`.
#'
#' @return Correlation matrix
#' @export
#'
#' @examples
f_cov_mat <- function(
    data,
    method = "Pearson",
    ...
) {

  switch(
    method,
    "Pearson" = stats::cov(data),
    "ewa" = f_ewa_cov_mat(data, ...)
  )
}


#' Exponential Weighted Correlation
#'
#' @param x Vector.
#' @param y Vector.
#' @param lambda Smoothing parameter.
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
#'
#' @references Tsay: Analysis Of Financial Time Series (3rd Ed., 10.1, p. 507)
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
#'
#' @param data Dataframe.
#' @param lambda Smoothing parameter. If `NA`
#' @param lookback Integer. Lookback window length.
#'
#' @return Matrix
#' @export
#'
#' @examples
#'
#' @references Tsay: Analysis Of Financial Time Series (3rd Ed., 10.1, p. 507)
f_ewa_cor_mat <- function(data, lambda = NA, lookback = NA) {
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


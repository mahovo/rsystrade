
#' Apply Function To A Rolling Window
#'
#' @description
#' Applies a function to a rolling window of a vector as the time index `t`
#'   moves through the vector. For instance, if the input `func` is `f_average`,
#'   then `rolling_window()` will calculate a moving average.
#'
#' If the window length is shorter than the length of the vector up until `t`
#'   for a step, the window length will be set to the length of the vector, and
#'   a warning will be given. The function will not fail or abort in this case.
#'
#' @param x A vector, e.g. a column in a data frame. Newest first. Top to
#'   bottom: Newer to older.
#' @param first_t Time index of the first observation in the window.
#' @param last_t Time index of the last observation in the window.
#' @param window_length Moving Average window length.
#' @param func Function to be applied to window.
#' @param mode Rolling mode
#'   * `1` Time index range of window is \eqn{[t -} window_length\eqn{, t - 1]}.
#'   * `2` Time index range of window is \eqn{[t -} window_length \eqn{+ 1, t]}.
#' @param ...
#'
#' @details
#' The first element of the output vector is always `NA`, since there will be no
#'   elements in the window at time \eqn{t = 1}.
#'
#' @returns A vector of length `length(x)`  if `mode==1` and `length(x) + 1` if
#'   `mode==2`.
#' @export
#'
#' @example
#' rolling_window(
#'   x = rnorm(20),
#'   first_data_id = 6,
#'   last_data_id = NA,
#'   window_length = 5L,
#'   func = f_average,
#'   method = "ewa"
#' )
rolling_window <- function(
    x,
    first_t = 2,
    last_t = NA,
    window_length,
    func,
    mode = 1,
    ...
) {

  out_length <- if(mode == 2) {length(x) + 1} else {length(x)}

  first <- if(is.na(first_t)) {
    2
  } else {first_t}
  last <- if(is.na(last_t)) {
    out_length
  } else {last_t}

  if(!is.integer(window_length)) {stop("window_length must be an integer (e.g. 16L).")}
  if(!(window_length > 0L)) {stop("window_length must be positive.")}

  #stopifnot(N >= n)
  out <- rep(NA, out_length)

  for(t in (first):(last)) {
    out[t] <- rolling_window_step(
      t = if(mode == 2) {t} else {NA},
      x = x,
      last_win_id = if(mode == 1) {t} else {NA},
      window_length = window_length,
      func = func,
      ...
    )
  }
  out
}

#' Apply Function To A Rolling Window Step
#'
#' @description
#'
#' Applies a function to a time window of a vector. For instance, if the input
#'   `func` is `f_average`, then `rolling_window()` will calculate a single
#'   step of a moving average, i.e. the mean of the data in the window.
#'
#' If the window length is shorter than the length of the vector, the window
#'   length will be set to the length of the vector, and a warning
#'   will be given. The function will not fail or abort in this case.
#'
#' @param t Time index.
#' @param x A vector. Oldest first. Top to bottom: Older to newer.
#' @param last_win_id Time index of the last observation in the window.
#' @param window_length Window length.
#'
#' @details
#' If we are at time `t`, and we want to calculate the average for the interval
#'   \eqn{[t - \text{window length}, t - 1]}, we can set `t` to the time index
#'   immediately subsequent to the last observation in the window and let
#'   `last_x_id` be `NA`.
#'
#' A typical use case, however, is to calculate a moving average of the window
#'   \eqn{[t - L + 1, t]}, where \eqn{L} is the length of the window. In this
#'   case we could let `t` be `NA` and set `last_x_id` to the current time index.
#'
#' If `t` is given a value and `last_x_id` is `NA`, the last observation in
#'   the window will be `t - 1`. Alternatively one could set `last_x_id` and
#'   let `t` be `NA`. If `t` and `last_x_id` are both `NA`, `last_x_id`
#'   will be set to the length of the data vector. If `t` and `last_x_id` are
#'   both given a value, `t` will be ignored.
#'
#' @returns A single number (or whatever is the output of `func`)
#' @export
#'
#' @example
rolling_window_step <- function(
    t = NA,
    x,
    last_win_id = NA,
    window_length,
    func,
    ...
) {
  last <- if(is.na(last_win_id)) {
    if(is.na(t)) {
      length(x)
    } else {t - 1}
  } else {last_win_id}

  if(last < (window_length)) {
    out_t <- func(
      x = x[1:last],
      ...
    )
    warning("Length of vector is shorter than or equal to desired MA window length.
                  Moving average calculated for shorter window.\n")
  } else {
    out_t <- func(
      x = x[(last - window_length + 1):last],
      ...
    )
  }

  out_t
}


#' Check That Object Exists And Is Not Null
#'
#' @description
#' Validate that object exists and is not null or empty. Returns `FALSE` if `x`
#'   does not exist, is `NULL` or is empty. Otherwise returns `TRUE`.
#'
#' Main use case is to check that a vector, list or data frame is not empty.
#'   While an empty vector will also be `NULL`, an empty list is not `NULL`.
#'   So this validator function checks both, given that the input object exists.
#'
#' @param x Object name as character string.
#' @param warn_or_stop Option to throw warning (`"warn"`) or stop (`"stop"`).
#'
#' @return Boolean
#' @export
#'
#' @details
#' Validation should return `FALSE` for data frames with all empty columns as
#'   well. The validation function is using the length of the object to
#'   determine if it is empty. Eventhough the length of a data frame is the
#'   number of columns (not rows!), empty columns do not count in the number of
#'   columns of a data frame.
#'
#' Note, in most situations this validation should be redundant, as R already
#'   will let us know if an object is missing or `NULL`. E.g. checking that an
#'   input object exists at the top of a function definition should not be
#'   necessary, because R won't execute a function if an input object doesn't
#'   exist (it could still be `NULL`, though). But we might like to validate an
#'   object before we pass it to a function. The validation function allows us
#'   to check before R starts complaining on it's own.
#'
#' Also note that the function throwing any errors will be
#'   `exists_and_not_null()`, which may not be helpful.
#'
#' @examples
exists_and_not_null <- function(
    x,
    warn_or_stop = "warn"
  ) {
  calling_env <- parent.frame()
  a <- exists(x, where = calling_env)
  if(!a) {
    switch(
      warn_or_stop,
      "warn" = warning(x, " does not exist."),
      "stop" = stop(x, " does not exist.")
    )
  }
  if(a){
    b <- !is.null(
      eval(
        parse(text = x),
        envir = calling_env
      )
    )
    if(!b) {
      switch(
        warn_or_stop,
        "warn" = warning(x, " is null."),
        "stop" = stop(x, " is null.")
      )
    }
    c <- length(
      eval(
        parse(text = x),
        envir = calling_env
      )
    ) > 0
    if(!c) {
      switch(
        warn_or_stop,
        "warn" = warning(x, " is empty."),
        "stop" = stop(x, " is empty.")
      )
    }
  } else {b <- FALSE}
  a && b
}

#' Clamp Signal At Upper And Lower Limits
#'
#' @description
#' Clamp a signal between a min and a max value.
#'
#' If input value is NA, output will be NA too.
#'
#' @param signal Single number or numeric vector
#' @param min_signal Minimum signal value
#' @param max_signal Maximum signal value
#'
#' @return
#' @export
#'
#' @examples
clamp_signal <- function(signal, min_signal = -Inf, max_signal = Inf) {
  #if(length(signal) != 1) {stop("clamp_signal() only takes a single number
  #                              as input.")}
  vapply(
    signal,
    function(x) {max(min_signal, min(x, max_signal))},
    numeric(1)
  )
}




soft_clip_lower_exp <- function(
    signal,
    min_signal, ## min_signal > 0
    softness = 5, ## 0 to 10, 0 is hard, 5 is medium, 10 is soft.
                 ## 2 is medium hard, 8 is medium soft.
    k = 4,
    c_max = 10,
    delta = 1e-8,
    lambda = 0,
    mode = 1,
    calibrate = FALSE
  ) {

  e <- exp(1)

  if(k <= e) {
    warning("soft_clip_lower_exp() k parameter is not greater than exp(1).
  k has been changed to the default (k = 4).")
  }

  if(min_signal <= 0) {
    warning("soft_clip_lower_exp() min_signal parameter is not strictly positive.
  soft_clip_lower_exp() needs a positive min_signal less than 1.
  min_signal has been changed to delta.")
    min_signal <- delta
  } else if(min_signal >= 1) {
    warning("soft_clip_lower_exp() min_signal >= 1.
  soft_clip_lower_exp() needs a positive min_signal less than 1.
  min_signal has been changed to 1-delta.")
    min_signal <- 1 - delta
  }

  x <- signal
  b <- min_signal

  if(softness > 10) {
    stop("soft_clip_lower_exp() softness parameter is greater than 10.
  soft_clip_lower_exp() needs a softness value in [0, 10].")
  } else if(softness < 0) {
    stop("soft_clip_lower_exp() is negative.
  soft_clip_lower_exp() needs a softness value in [0, 10].")
  }

  tau <- function(z) {
    force(b)
    force(e)
    force(k)
    z * b * log(b) * (e - k) - 1
  }
  min_valid_c <- 1e-12 ## c > 0
  max_valid_c <- try(stat::uniroot(tau, interval = c(min_valid_c, c_max))$root, silent = TRUE)
  if(!is.numeric(max_valid_c)) {
    stop("soft_clip_lower_exp() could not find a valid c value between 0 and c_max.
  Try increasing c_max.")
  }
  z <-  min_valid_c + (max_valid_c - min_valid_c) * (10 - softness) / 10

  x_extreme <- -log(k) / (z * log(b))
  if(signal < 1e-12) {
    warning("soft_clip_lower_exp() input signal not strictly positive.
  signal has been changed to 1e-12.")
    signal = 1e-12 ## x > 0
  } else if(signal > x_extreme) {
    warning(paste0("soft_clip_lower_exp() input signal is greater than x_extreme=",x_extreme,".
  Try increasing k or b.
  signal has been changed to -log(k) / (c * log(b))."))
    signal <- x_extreme
  }

  a <- function() {
    if(mode == 1) {
      e * b
    } else if(mode == 2) {
      -1 / (z * log(b))
    }
  }

  f <- function() {
    if(mode == 1) {
      d = 0
      z = -1 / (e * b * log(b))
    } else if(mode == 2) {
      d = (e * z * b * log(b) + 1)
    }
    denom <- exp(log(b) * (z * x - 1))
    1/denom + d * x
  }

  h <- function() {
    if(x >= a()) {
      x
    } else {
        f()
    }
  }
  lambda * h() + (1 - lambda) * max(x, b)
}



soft_clip_lower_quad <- function(
    signal,
    min_signal, ## min_signal > 0
    softness = 5,
    delta = 1e-8
  ){

  if(signal < 1e-12) {
    warning("soft_clip_lower_quad() input signal not strictly positive.
  signal has been changed to 1e-12.")
    signal = 1e-12 ## x > 0
  }

  if(min_signal <= 0) {
    warning("soft_clip_lower_quad() min_signal is not strictly positive.
  soft_clip_lower_quad() needs a positive min_signal.
  min_signal has been changed to delta.")
    min_signal <- delta
  }

  x <- signal
  a <- 1/softness
  b <- min_signal
  z <- 1 / (4 * a)

  if(softness < z) {
    warning("soft_clip_lower_quad() softness parameter was above the valid range.
    softness has been changed to the maximum valid value of 4 * b.")
    a = 1 / (4 * b)
  }

  f <- function() {
    a * (x - b + z)^2 + b
  }

  h <- function() {
    if(x <= b - z) {
      b
    } else {
      if(x <= b + z) {
        f()
      } else {
        x
      }
    }
  }

  h()
}



soft_clip_upper_exp <- function(
    signal,
    min_signal, ## min_signal > 0
    softness = 5, ## 0 to 10, 0 is hard, 5 is medium, 10 is soft.
    ## 2 is medium hard, 8 is medium soft.
    delta = 1e-8,
    lambda = 0,
    calibrate = FALSE
) {

  if(min_signal <= 0) {
    warning("soft_clip_lower_exp() min_signal parameter is not strictly positive.
  soft_clip_lower_exp() needs a positive min_signal.
  min_signal has been changed to delta.")
    min_signal <- delta
  }

  x <- signal
  b <- min_signal

  if(softness > 10) {
    stop("soft_clip_lower_exp() softness parameter is greater than 10.
  soft_clip_lower_exp() needs a softness value in [0, 10].")
  } else if(softness < 0) {
    stop("soft_clip_lower_exp() is negative.
  soft_clip_lower_exp() needs a softness value in [0, 10].")
  }

  min_valid_c <- 1 / b ## c > 0
  c_max <- 100 / b
  z <- min_valid_c + (c_max - min_valid_c) * (10 - softness) / 10
  z <- log_to_lin(z,  min_valid_c,  c_max,  min_valid_c,  c_max)

  if(signal < 1e-12) {
    warning("soft_clip_lower_exp() input signal not strictly positive.
  signal has been changed to 1e-12.")
    signal = 1e-12 ## x > 0
  }

  a <- function() {
    (b * z - 1) / z
  }

  f <- function() {
    d = (b * z - 1) / z
    b - (
      b - d
    ) / exp(
      z * (
        x - d
      )
    )
  }

  h <- function() {
    if(x <= a()) {
      x
    } else {
      f()
    }
  }

  lambda * h() + (1 - lambda) * max(x, b)
}




soft_clip_upper_quad <- function(
    signal,
    min_signal, ## min_signal > 0
    softness = 5,
    delta = 1e-8
){

  if(signal < 1e-12) {
    warning("soft_clip_lower_quad() input signal not strictly positive.
  signal has been changed to 1e-12.")
    signal = 1e-12 ## x > 0
  }

  if(min_signal <= 0) {
    warning("soft_clip_lower_quad() min_signal is not strictly positive.
  soft_clip_lower_quad() needs a positive min_signal.
  min_signal has been changed to delta.")
    min_signal <- delta
  }

  x <- signal
  a <- 1/softness
  b <- min_signal
  z <- 1 / (4 * a)

  if(softness < z) {
    warning("soft_clip_lower_quad() softness parameter was above the valid range.
    softness has been changed to the maximum valid value of 4 * b.")
    a = 1 / (4 * b)
  }

  f <- function() {
    -a * (x - b - z)^2 + b
  }

  h <- function() {
    if(x <= b - z) {
      x
    } else {
      if(x <= b + z) {
        f()
      } else {
        b
      }
    }
  }

  h()
}



#' Clamp Single Signal Value At Lower Limit
#'
#' @description
#' Limit any signal value below `min_signal` to `min_signal`.
#'
#' @param signal Single signal value
#' @param min_signal Minimum signal value
#'
#' @return Number
#' @export
#'
#' @examples
clamp_signal_lower <- function(signal, min_signal = -Inf) {
  max(min_signal, signal)
}

#' Clamp Single Signal Value At Upper Limit
#'
#' @description
#' Limit any signal value above `max_signal` to `max_signal`.
#'
#' @param signal Single signal value
#' @param max_signal Maximum signal value
#'
#' @return
#' @export
#'
#' @examples
clamp_signal_upper <- function(signal, max_signal = Inf) {
  min(signal, max_signal)
}


#' Clamp Matrix Elements At Upper And Lower Limit
#'
#' @description
#' For each element in the matrix limit any signal value below `min_signal` to
#' `min_signal` and any value above `max_signal` to `max_signal`.
#'
#' @param input_matrix Input matric
#' @param min_signal Minimum signal value
#' @param max_signal Maximum signal value
#'
#' @return
#' @export
#'
#' @examples
clamp_matrix <- function(input_matrix, min_signal = -Inf, max_signal = Inf) {
  apply(input_matrix,
        c(1,2),
        clamp_signal,
        min_signal = min_signal,
        max_signal = max_signal
  )
}


#' Clamp Matrix Elements At Lower Limit
#'
#' @description
#' For each element in the matrix limit any signal value below `min_signal` to
#' `min_signal`.
#'
#' @param input_matrix Input matrix
#' @param min_signal Minimum signal value
#'
#' @return
#' @export
#'
#' @examples
clamp_matrix_lower <- function(input_matrix, min_signal) {
  apply(input_matrix,
        c(1,2),
        clamp_signal_lower,
        min_signal = min_signal
  )
}



#' Clamp Matrix Elements At Upper Limit
#'
#' @description
#' For each element in the matrix limit any signal value above `max_signal` to
#' `max_signal`.
#'
#' @param input_matrix Input matrix
#' @param max_signal Maximum signal
#'
#' @return
#' @export
#'
#' @examples
clamp_matrix_upper <- function(input_matrix, max_signal) {
  apply(input_matrix,
        c(1,2),
        clamp_signal_upper,
        max_signal = max_signal
  )
}

#' Convert Binary Class Label
#'
#' Converts binary label from "binary" format to "sign" format.
#'
#' * "binary" format: \eqn{\{0, 1\}}
#' * "sign" format: \eqn{\{-1, 1\}}
#' * "auto format": Automatically converts from "binary" to "sign" or vice
#'   versa. This is a faster than the other options, but doesn't check
#'   validity of the input. WARNING: Nonsensical input will produce nonsensical
#'   output. E.g. input 2 will produce output -3. "to_format" is ignored, if
#'   "from_format" is "auto".
#'
#' If output format is same as input format, it will not fail, just return the
#'   input unchanged.
#'
#' @param input 0 or 1 for "binary" input. -1 or 1 for "sign" input.
#' @param from_format Format to convert from.
#' @param to_format Format to convert to.
#'
#' @return Number
#' @export
#'
#' @examples
#' convert_binary_class_label(0) # -1
#' convert_binary_class_label(1) # 1
#' convert_binary_class_label(-1, "sign", "binary") # 0
#' convert_binary_class_label(0, "sign", "binary") # 0 not valid input
convert_binary_class_label <- function(
    input,
    from_format = "binary",
    to_format = "sign") {
  switch(from_format,
         "auto" = if(input == 1) {1} else {input * (-1) - 1},
         "binary" = if(
           input %in% c(0, 1)
         ) {switch(to_format,
                   "sign" = (input * 2) - 1,
                   "binary" = input
         )
         } else {warning("Input must be 0 or 1.")},
         "sign" = if(
           input %in% c(-1, 1)
         ) {switch(to_format,
                   "binary" = (input + 1) / 2,
                   "sign" = input
         )
         } else {warning("Input must be -1 or 1.")}
  )
}

#' Make List Names Unique
#'
#' Appends `.#` to duplicate list names, where `#` represents a consequtive
#'   number.
#'
#' @param list Named list
#'
#' @return Named list with unique names
#' @export
#'
#' @examples
make_list_names_unique <- function(list) {
  names(list) <- make.names(names(list), unique=TRUE)
  list
}


lin_to_log <- function(
    y,
    y_min,
    y_max,
    x_min,
    x_max,
    base = 10
) {
  a <- x_max - x_min
  b <- log(y, base) - log(y_min, base)
  c <- log(y_max, base) - log(y_min, base)
  a * b/ c + x_min
}


log_to_lin <- function(
    x,
    x_min,
    x_max,
    y_min,
    y_max,
    base = 10
  ) {
  a <- x - x_min
  b <- x_max - x_min
  c <- log(y_max, base) - log(y_min, base)
  base^(a * c / b + log(y_min, base))
}


#' Fix NAs in correlation matrix
#'
#' @param cor_mat A correlation matrix
#' @param value The value to replace NA by
#'
#' @return
#' @export
#'
#' @examples
fix_cor_mat_NAs <- function(cor_mat, value) {
  ## Check if any correlation is NA
  if(sum(is.na(cor_mat)) > 0) {
    warning("NAs in correlation matrix have been replaced by min_cor value. NAs in a correlation matrix are common when previous returns are identical, resulting in standard deviations of zero.
Replacing NAs in correlation matrix by min_cor value is supposed to fix this problem.")

    ## Replace missing values (divide-by-zero NA's) with minimum correlation
    cor_mat[is.na(cor_mat)] <- value
  }

  cor_mat
}

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

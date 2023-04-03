## The code in this document is adapted from on code by Jennifer Bryan
## (CC BY-SA 4.0)
## https://github.com/jennybc/row-oriented-workflows/blob/master/iterate-over-rows.md


#' Benchmark
#'
#' @description
#' Run benchmark test on a single function.
#'
#' Execute `expr` `n` times.
#'
#' Explicit garbage collection, then execute `expr` `n` times w/o explicit gc,
#'   finally return timings.
#'
#' A progress bar shows progress for each individual run, when running time is
#'   more than 2 secs and expected remaining time is more than 2 secs.
#'
#' @param n Number of test runs per function.
#' @param expr Function call as character string. E.g. `"my_function(df)"`.
#' @param envir Environment in wich to evaluate the function. Typically
#'   `benchmark()` is called from `run_benchmark()` so the tested function will
#'   be evaluated in the environment of `run_benchmark()`.
#'
#' @return List of benchmark times.
#' @export
#'
#' @examples
benchmark <- function(n = 1, expr, envir = parent.frame()) {
  print(paste0("Running ", n, " benchmark tests on ", expr, "..."))
  expr <- parse(text = expr)
  gc()
  benchmarks <- purrr::map(
    seq_len(n),
    ~ system.time(eval(expr, envir), gcFirst = FALSE),
    .progress = as.character(expr) ## Progress bar
  )
  #invisible(print(benchmarks))
  print(paste0("Done testing ", expr, "."))
  benchmarks
}

#' Run benchmark
#'
#' @description
#' Run `benchmark()` test on one or more functions taking a data frame as input.
#'
#' By providing different numbers of rows or columns, it is possible to test
#'   the functions sensitivity to each dimension.
#'   It is also possible to provide a single data frame as input for the
#'   function, in which case the provided dimensions will be ignored. However,
#'   this is not the typical use case.
#'
#' @param nrow Number of rows in data frame used as input for the function to
#'   be tested.
#' @param ncol Number of columns in data frame used as input for the function to
#'   be tested.
#' @param times Number of times to repeat the test.
#' @param functions List of functions to be tested. Each function call is
#'   provided in the list as a character string. (See example)
#' @param df Optional data frame. Default is `df = NA`. If `df = NA`, a
#'   data frame with the given dimensions will be created where all elements are
#'   `element`. If a data frame is provided, the provided dimensions will
#'   ignored, and the dimensions of the data frame will be used.
#' @param elements
#'   Specify how the elements in the data frame should be generated. Elements
#'   will be generated as one long vector and then wrapped as a matrix, which
#'   is finally turned into a data frame. So the data frame is not defined
#'   row wise or column wise.
#'   + `"same"`: All elements in the df will be the element given by `element`.
#'   + `"unif"`: Elements in the df are uniformly distributed.
#'   + `"norm"`: Elements in the df are drawn from a standard normal
#'   distribution.
#'   + `"seq"`: A sequence of integers `1,2,3,...`.
#'   + `"letters"`: A sequence of letters `"a", "b", "c", ...`. Wraps back to
#'   "a" after "z".
#' @param element Specify the element when `elements = "same"`. `"element"` may
#'   be an integer, double, character, or any element that a data frame will
#'   accept.
#'
#' @return A tibble of benchmark times.
#' @export
#'
#' @examples
#' \dontrun{
#' f1 <- function() {rnorm(20)}
#' f2 <- function() {runif(20)}
#' f_list <- list(f1 = "f1", f2 = "f2")
#' df_test <- map_df(
#'   10^(1:2),
#'   run_benchmark,
#'   ncol = 3,
#'   functions = f_list, times = 1
#' ) %>% order_graphs()
#' plot_bm(df_test, "nrow", mean_or_median = median)
#' }
run_benchmark <- function(
    nrow = 10,
    ncol = 3,
    functions = list(),
    times = 5,
    df = NA,
    elements = "same",
    element = 2) {
  if(is.na(df)) {
    df <- switch(elements,
      "same" = data.frame(matrix(rep(element, nrow * ncol), ncol = ncol)),
      "unif" = data.frame(matrix(stats::runif(nrow * ncol), ncol = ncol)),
      "norm" = data.frame(matrix(stats::rnorm(nrow * ncol), ncol = ncol)),
      "seq" = data.frame(matrix(as.integer(seq_len(nrow * ncol)), ncol = ncol)),
      "letters" = data.frame(matrix(rep_len(letters, length.out = nrow * ncol), ncol = ncol))
    )
  } else {
    nrow <- nrow(df)
    ncol <- ncol(df)
  }
  res <- purrr::map(functions, ~ benchmark(times, .x))
  res <- purrr::map(res, ~ map_dbl(.x, "elapsed"))
  tibble::tibble(
    nrow = nrow,
    ncol = ncol,
    method = rep(names(res), lengths(res)),
    time = rlang::flatten_dbl(res)
  )
}

## Force plot legends to present methods in order of time
order_graphs <- function(df) {
  dplyr::mutate(df, method = forcats::fct_reorder(method, .x = dplyr::desc(time)))
}

plot_bm <- function(df, nrow_or_ncol = "nrow", mean_or_median = mean) {
  log10_breaks <- scales::trans_breaks("log10", function(x) 10^x)
  log10_mbreaks <- function(x) {
    limits <- c(floor(log10(x[1])), ceiling(log10(x[2])))
    breaks <- 10^seq(limits[1], limits[2])

    unlist(lapply(breaks, function(x) x * seq(0.1, 0.9, by = 0.1)))
  }

  # Instead of doing this, we use "@importFrom purr" in `R/pkg-package.R`.
  #`%>%` <- purrr::`%>%` ## Import the pipe operator

  log10_labels <- scales::trans_format("log10", scales::math_format(10^.x))

  ggplot2::ggplot(
    df %>% dplyr::filter(time > 0),
    aes_string(x = nrow_or_ncol, y = "time", colour = "method")
  ) +
    geom_point() +
    stat_summary(aes(group = method), fun = mean_or_median, geom = "line") +
    scale_y_log10(
      breaks = log10_breaks, labels = log10_labels, minor_breaks = log10_mbreaks
    ) +
    scale_x_log10(
      breaks = log10_breaks, labels = log10_labels, minor_breaks = log10_mbreaks
    ) +
    labs(
      x = paste0("Number of ", if (nrow_or_ncol == "nrow") "rows" else "columns"),
      y = "Time (s)"
    ) +
    theme_bw() +
    theme(aspect.ratio = 1, legend.justification = "top")
}




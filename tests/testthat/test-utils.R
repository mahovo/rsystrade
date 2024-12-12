
test_that("rolling_window() works", {
  my_test_window_simple <- rolling_window(
    x = 1:10,
    first_t = 6,
    last_t = NA,
    window_length = 5L,
    func = f_average,
    method = "simple"
  )
  my_test_window_ewa <- round(
    rolling_window(
      x = 1:10,
      first_t = 6,
      last_t = NA,
      window_length = 5L,
      func = f_average,
      method = "ewa"
    )
  )
  my_test_window <- c(my_test_window_simple, my_test_window_ewa)
  my_expected_window_simple <- c(rep(NA, 5), 4:8)
  my_expected_window_ewa <- c(rep(NA, 5), 5:9)
  my_expected_window <- c(my_expected_window_simple, my_expected_window_ewa)

  expect_equal(
    my_test_window,
    my_expected_window
  )
})

test_that("clamp_signal() works", {
  expect_equal(
    clamp_signal(
      (-11):11,
      min_signal = -10,
      max_signal = 10
    ),
    c(-10, (-10:10), 10)
  )
})

test_that("clamp_signal_lower() works", {
  expect_equal(
    clamp_signal_lower(
      -11,
      min_signal = -10
    ),
    -10
  )
})

test_that("clamp_signal_upper() works", {
  expect_equal(
    clamp_signal_upper(
      11,
      max_signal = 10
    ),
    10
  )
})

test_that("clamp_matrix() works", {
  expect_equal(
    clamp_matrix(
      matrix(c((-12):12), byrow = 5),
      min_signal = -10,
      max_signal = 10
    ),
    matrix(c(-10, -10, (-10):10, 10, 10), byrow = 5)
  )
})

test_that("clamp_matrix_lower() works", {
  expect_equal(
    clamp_matrix_lower(
      matrix(c((-12):12), byrow = 5),
      min_signal = -10
    ),
    matrix(c(-10, -10, (-10):12), byrow = 5)
  )
})

test_that("clamp_matrix_upper() works", {
  expect_equal(
    clamp_matrix_upper(
      matrix(c((-12):12), byrow = 5),
      max_signal = 10
    ),
    matrix(c((-12):10, 10, 10), byrow = 5)
  )
})

test_that("convert_binary_class_label() works", {
  expect_equal(
    c(
      convert_binary_class_label(0),
      convert_binary_class_label(1),
      convert_binary_class_label(-1, "sign", "binary"),
      convert_binary_class_label(1, "sign", "binary")
      ),
   c(-1, 1, 0, 1)
  )
})

test_that("convert_binary_class_label() gives warning", {
  expect_warning(
      convert_binary_class_label(0, "sign", "binary")
  )
})



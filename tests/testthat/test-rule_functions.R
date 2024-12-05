test_that("r_mac() works", {
  price = 100 + 10 * sin((1:100)/10)
  my_test_mac <- lapply(
    r_mac(
      t = 90,
      price = price,
      ma_fast = NA,
      ma_slow = NA,
      n_fast = 20L,
      n_slow = 80L,
      ma_method = "simple",
      gap = 0,
      strict = TRUE,
      binary = FALSE
    ),
    function(x) {round(x[[1]], 4)}
  )
  my_expected_mac <- list(
    signal = 6.4711,
    ma_fast = 108.257,
    ma_slow = 101.7859
  )

  expect_equal(
    my_test_mac,
    my_expected_mac
  )
})

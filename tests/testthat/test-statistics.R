test_that("f_average() works", {
  x <- 1:10

  y_simple <- f_average(
    x,
    method = "simple"
  )
  y_ewa <- f_average(
    x,
    method = "ewa",
    lambda = 0.8,
    lookback = 4L
  )

  my_test_averages <- c(y_simple, y_ewa)
  my_expected_averages <- c(
    5.5,
    (10 + 9 * 0.8^1 + 8 * 0.8^2 + 7 * 0.8^3) / (1 + 0.8^1 + 0.8^2 + 0.8^3)
  )

  expect_equal(
    my_test_averages,
    my_expected_averages
  )
})

test_that("f_sd() works", {
  x <- 1:4

  y_simple <- f_sd(
    x,
    method = "unbiased"
  )
  y_ewa <- f_sd(
    x,
    method = "ewa",
    lambda = 0.8,
    lookback = 3L
  )

  my_test_sds <- c(
    y_simple,
    y_ewa
  )
  my_expected_sd_simple <- sqrt(
    ((1 - 2.5)^2 + (2 - 2.5)^2 + (3 - 2.5)^2 + (4 - 2.5)^2 ) / 3
  )

  my_ewa <-  (4 + 3 * 0.8 + 2 * 0.8^2) / (1 + 0.8 + 0.8^2)
  my_expected_sd_ewa <- sqrt(
    ((4 -  my_ewa)^2 + (3 -  my_ewa)^2 * 0.8 + (2 -  my_ewa)^2 * 0.8^2) / (1 + 0.8 + 0.8^2)
  )
  my_expected_sds <- c(
    my_expected_sd_simple,
    my_expected_sd_ewa
  )


  expect_equal(
    my_test_sds,
    my_expected_sds
  )
})



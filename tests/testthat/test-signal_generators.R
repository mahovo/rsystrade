test_that("s_mac_25_100_stoploss() works", {

  my_test_system <- make_test_system()

  my_test_s_mac_25_100_stoploss <- ms_mac_25_100_stoploss(
    prices = my_test_system$instruments[[1]]$prices,
    signal_table = my_test_system$signal_tables[[1]],
    position_table = my_test_system$position_tables[[1]],
    t = 20,
    config = my_test_system$config
  )
  my_test_s_mac_25_100_stoploss

  expect_equal(
    my_test_s_mac_25_100_stoploss,
    my_test_s_mac_25_100_stoploss
  )
})

test_that("p_stop_loss() works", {
  min_periods = 25
  #
  # algos <- make_test_algos(
  #   list(
  #     "mac_30_60",
  #     r_mac,
  #     ma_fast = NA,
  #     ma_slow = NA,
  #     n_fast = 30L,
  #     n_slow = 60L,
  #     ma_method = "simple",
  #     gap = 0,
  #     strict = TRUE,
  #     binary = FALSE,
  #     mode = 1
  #   ),
  #   list(
  #     "mac_20_80",
  #     r_mac,
  #     ma_fast = NA,
  #     ma_slow = NA,
  #     n_fast = 20L,
  #     n_slow = 80L,
  #     ma_method = "simple",
  #     gap = 0,
  #     strict = TRUE,
  #     binary = FALSE,
  #     mode = 1
  #   )
  # )

  # my_test_system <- make_system(
  #   algos = algos,
  #   init_capital = 1000000,
  #   system_risk_target = 0.12,
  #   risk_window_length = 5,
  #   position_modifiers = list(),
  #   min_periods = min_periods,
  #   mode = "sim",
  #   instrument_data_folder_path = testthat::test_path("fixtures/")
  # )

  my_stoploss_test_system <- readRDS(test_path("fixtures", "my_expected_system.rds"))

  pos_mods <- list(
    list(
      instruments = list("testdata5", "testdata6"),
      modifier = list(
        "p_stop_loss",
        p_stop_loss,
        stop_loss_fraction = 0.2,
        rnd = FALSE
      )
    )
  )
  my_stoploss_test_system$position_modifiers <- expand_position_modifiers(pos_mods)

  suppressWarnings(
    my_stoploss_test_system <- run_system(
      my_stoploss_test_system,
      min_periods = min_periods,
      mode = "sim",
      instrument_data_folder_path = testthat::test_path("fixtures/")
    )
  )

  my_test_stop_loss <- my_stoploss_test_system$position_tables

  ## Uncomment to generate expected data:

  # saveRDS(
  #   my_test_stop_loss,
  #   file=test_path("fixtures/", "my_expected_stop_loss.RData")
  # )

  my_expected_stop_loss <- readRDS(test_path("fixtures", "my_expected_stop_loss.RData"))

  ## Test ----
  expect_equal(
    my_test_stop_loss,
    my_expected_stop_loss
  )
})

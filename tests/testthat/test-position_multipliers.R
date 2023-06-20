test_that("m_block_same_direction_entry() works", {

  min_periods = 10

  algos <- make_test_algos(
    list(
      "mac_2_4",
      r_mac,
      ma_fast = NA,
      ma_slow = NA,
      n_fast = 2L,
      n_slow = 4L,
      gap = 0,
      strict = TRUE,
      binary = FALSE
    ),
    list(
      "mac_3_9",
      r_mac,
      ma_fast = NA,
      ma_slow = NA,
      n_fast = 3L,
      n_slow = 9L,
      gap = 0,
      strict = TRUE,
      binary = FALSE
    )
  )

  my_test_system <- make_system(
    algos = algos,
    init_capital = 1000000,
    system_risk_target = 0.12,
    risk_window_length = 5,
    position_modifiers = list(),
    min_periods = min_periods,
    mode = "sim",
    instrument_data_folder_path = testthat::test_path("fixtures/")
  )

  pos_mods <- list(
    list(
      instruments = list("testdata3", "testdata4"),
      modifier = list(
        "p_stop_loss",
        p_stop_loss,
        stop_loss_fraction = 0.5,
        rnd = FALSE
      )
    )
  )
  my_test_system$position_modifiers <- expand_position_modifiers(pos_mods)

  pos_muls <- list(
    list(
      instruments = list("testdata3", "testdata4"),
      multipliers = list(
        list(
          "m_block_same_direction_entry",
          m_block_same_direction_entry,
          mode = 2
        )
      )
    )
  )
  my_test_system$position_multipliers <- expand_position_multipliers(pos_muls)

  suppressWarnings(
    my_test_system <- run_system(
      my_test_system,
      min_periods = min_periods,
      mode = "sim",
      instrument_data_folder_path = testthat::test_path("fixtures/")
    )
  )

  my_test_pos_mul <- my_test_system$position_tables

  saveRDS(
    my_test_pos_mul,
    file=test_path("fixtures/", "my_expected_pos_mul.RData")
  )

  my_expected_pos_mul <- readRDS(test_path("fixtures", "my_expected_pos_mul.RData"))

  ## Test ----
  expect_equal(
    my_test_pos_mul,
    my_expected_pos_mul
  )
})

test_that("generate_signal() works", {

  ## Setup ----
  fff <- function(
    price,
    some_other_param_1,
    some_other_param_2
  ) {price + some_other_param_1 + some_other_param_2}


  algos <- make_test_algos(
    list(
       "fff1",
      fff,
      some_other_param_1 = 2,
      some_other_param_2 = 3
    ),
    list(
      "fff2",
      fff,
      some_other_param_1 = 4,
      some_other_param_2 = 5
    )
  )

  parsed_algos <- parse_algos(algos)

  # my_test_signal <- generate_signal(
  #   variable_param_vals = 10,
  #   signal_table = NA,
  #   position_table = NA,
  #   algo = parsed_algos[[1]], ## Single parsed algo from the algos list
  #   t = NA,
  #   config = NA
  #   #signal_table, ## signal table for the instrument
  # )

  inst_data <- load_instrument_data_sets(
    parsed_algos = parsed_algos,
    instrument_data_folder_path = testthat::test_path("fixtures/")
  )

  variable_param_vals <- get_variable_param_vals(
    t = 10,
    inst_data,
    algo = parsed_algos[[1]]
  )

  my_test_signal <- generate_signal(
    variable_param_vals,
    algo = parsed_algos[[1]] ## Single parsed algo from the algos list
  )


## Test ----
  expect_equal(
    my_test_signal,
    15
  )
})



test_that("update_signal_normalization_factors() works", {

  suppressWarnings(
    my_test_system <- make_test_system()
  )

  my_test_signal_normalization_factors_1 <- update_signal_normalization_factors(
    my_test_system$algos,
    my_test_system$signal_tables,
    my_test_system$inst_data,
    target = 1,
    method = "equal",
    args = list(equal_norm_factor = 1)
  )

  my_test_signal_normalization_factors_2 <- update_signal_normalization_factors(
    my_test_system$algos,
    my_test_system$signal_tables,
    my_test_system$inst_data,
    target = 1,
    method = "pool_traded"
  )

  my_test_signal_normalization_factors_3 <- update_signal_normalization_factors(
    my_test_system$algos,
    my_test_system$signal_tables,
    my_test_system$inst_data,
    target = 1,
    method = "median_pool_all",
    args = list(min_periods_median_pool_all = 15)
  )

  my_test_signal_normalization_factors <- list(
    my_test_signal_normalization_factors_1,
    my_test_signal_normalization_factors_2,
    my_test_signal_normalization_factors_3
  )

  # my_expected_signal_normalization_factors <- list(
  #   my_test_signal_normalization_factors_1,
  #   my_test_signal_normalization_factors_2,
  #   my_test_signal_normalization_factors_3
  # )
  #
  # saveRDS(
  #   my_expected_signal_normalization_factors,
  #   file=test_path("fixtures/", "my_expected_signal_normalization_factors.RData")
  # )


  my_expected_signal_normalization_factors <- readRDS(test_path("fixtures", "my_expected_signal_normalization_factors.RData"))

    ## Test ----
  expect_equal(
    my_test_signal_normalization_factors,
    my_expected_signal_normalization_factors
  )
})



test_that("modify_position() works", {

  min_periods = 7

  pos_mod_test_function_1 <- function(
    t,
    position_size_ccy,
    pos_scalar,
    prices
  ) {
    list(
      modified_position_size_ccy = position_size_ccy * pos_scalar,
      pos_scalar = pos_scalar,
      price_diff_sign = sign(prices[t] - prices[t - 1])
    )
  }

  ## Use clamp_signal() to modify position, eventhough we distinguish between
  ## signals and positions. This will fail when we introduce type validation.
  pos_mod_test_function_2 <- function(
    t,
    position_size_ccy,
    lower_limit,
    upper_limit,
    prices
  ) {
    list(
      modified_position_size_ccy = clamp_signal(
        position_size_ccy,
        min_signal = lower_limit,
        max_signal = upper_limit
      ),
      lower_limit = lower_limit,
      upper_limit = upper_limit,
      price_diff_sign = sign(prices[t] - prices[t - 1])
    )
  }

  pos_mul_test_function_1 <- function(
    t,
    position_size_ccy,
    threshold, ## fixed param
    prices ## variable param from data
  ) {
    lag_2 <- {prices[t] - prices[t - 2] > threshold}
    price_diff_sign <- sign(prices[t] - prices[t - 1])

    list(
      multiplier = lag_2 * price_diff_sign,
      lag_2 = lag_2,
      price_diff_sign = price_diff_sign
    )
  }

  pos_mul_test_function_2 <- function(
    t,
    position_size_ccy,
    threshold, ## fixed param
    prices ## variable param from data
  ) {
    lag_3 <- {prices[t] - prices[t - 3] > threshold}
    price_diff_sign <- sign(prices[t] - prices[t - 1])
    list(
      multiplier = lag_3 * price_diff_sign,
      lag_3 = lag_3,
      price_diff_sign = price_diff_sign
    )
  }

  signal_generator_1 <- list(
      "mac_2_4",
      r_mac,
      ma_fast = NA,
      ma_slow = NA,
      n_fast = 2L,
      n_slow = 4L,
      gap = 0,
      strict = TRUE,
      binary = FALSE
    )
  signal_generator_2 <- list(
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

  algos <- list(
    list( ## We might name this "subset1"
      instruments = list("testdata3", "testdata4"),
      rules = list(
        rule1 = signal_generator_1,
        rule2 = signal_generator_2
      )
    ),
    list( ## We might name this "subset1"
      instruments = list("testdata5", "testdata6"),
      rules = list(
        rule1 = signal_generator_1,
        rule2 = signal_generator_2
      )
    )
  )

  pos_mods <- list(
    list(
      instruments = list("testdata3", "testdata4"),
      modifier = list(
        "pos_mod_test_function_1",
        pos_mod_test_function_1,
        pos_scalar = 1/1000000
      )
    ),
    list(
      instruments = list("testdata5", "testdata6"),
      modifier = list(
        "pos_mod_test_function_2",
        pos_mod_test_function_2,
        lower_limit = -100000,
        upper_limit = 100000
      )
    )
  )

  pos_muls <- list(
    list(
      instruments = list("testdata3", "testdata4"),
      multipliers = list(
        list(
          "pos_mul_test_function_1",
          pos_mul_test_function_1,
          threshold = 0.02
        ),
        list(
          "pos_mul_test_function_2",
          pos_mul_test_function_2,
          threshold = -0.02
        )
      )
    ),
    list(
      instruments = list("testdata5", "testdata6"),
      multipliers = list(
        list(
          "pos_mul_test_function_1",
          pos_mul_test_function_1,
          threshold = 0.03
        ),
        list(
          "pos_mul_test_function_2",
          pos_mul_test_function_2,
          threshold = -0.03
        )
      )
    )
  )

  my_test_system <- make_system(
    algos = algos,
    init_capital = 1000000,
    system_risk_target = 0.12,
    risk_window_length = 5,
    #position_modifiers = pos_mods,
    position_multipliers = pos_muls,
    min_periods = min_periods,
    mode = "sim",
    instrument_data_folder_path = testthat::test_path("fixtures/")
  )



  ## Put the parsed modifiers in the system (this is a hack for testing
  ## purposes.
  #my_test_system$position_modifiers <- parse_position_modifiers(pos_mods)

  ## Put the parsed multipliers in the system (this is a hack for testing
  ## purposes.
  #my_test_system$position_multipliers <- parse_position_multipliers(pos_muls)

  suppressWarnings(
    my_test_system <- run_system(
      my_test_system,
      min_periods = min_periods,
      mode = "sim",
      instrument_data_folder_path = testthat::test_path("fixtures/")
    )
  )

  my_test_modified_positions <- my_test_system$position_tables

  # saveRDS(
  #   my_test_modified_positions,
  #   file=test_path("fixtures/", "my_expected_modified_positions.RData")
  # )


  my_expected_modified_positions <- readRDS(testthat::test_path("fixtures", "my_expected_modified_positions.RData"))

  ## Test ----
  expect_equal(
    my_test_modified_positions,
    my_expected_modified_positions
  )
})

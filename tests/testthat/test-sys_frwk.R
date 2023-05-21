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

my_test_system <- make_test_system()

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

  min_periods = 10

  pos_mod_test_function <- function(
    t,
    position_size_ccy,
    multiplier
  ) {
    list(
      modified_position_size_ccy = position_size_ccy * multiplier,
      multiplier = multiplier
    )
  }

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
      instruments = list("inst1", "inst2", "inst3"),
      modifier = list(
        "pos_mod_test_function",
        pos_mod_test_function,
        multiplier = 1/1000
      )
    )
  )
  my_test_system$position_modifiers <- parse_position_modifiers(pos_mods)

  my_test_system <- run_system(
    my_test_system,
    min_periods = min_periods,
    mode = "sim",
    instrument_data_folder_path = testthat::test_path("fixtures/")
  )

  my_test_modified_positions <- my_test_system$position_tables

  # saveRDS(
  #   my_test_modified_positions,
  #   file=test_path("fixtures/", "my_expected_modified_positions.RData")
  # )


  my_expected_modified_positions <- readRDS(test_path("fixtures", "my_expected_modified_positions.RData"))

  ## Test ----
  expect_equal(
    my_test_modified_positions,
    my_expected_modified_positions
  )
})

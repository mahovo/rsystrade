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

# test_that("portfolio multipliers work", {
#   min_periods = 10
#
#   algos <- make_test_algos(
#     list(
#       "mac_2_4",
#       r_mac,
#       ma_fast = NA,
#       ma_slow = NA,
#       n_fast = 2L,
#       n_slow = 4L,
#       ma_method = "simple",
#       gap = 0,
#       strict = TRUE,
#       binary = FALSE,
#       mode = 1
#     ),
#     list(
#       "mac_3_9",
#       r_mac,
#       ma_fast = NA,
#       ma_slow = NA,
#       n_fast = 3L,
#       n_slow = 9L,
#       ma_method = "simple",
#       gap = 0,
#       strict = TRUE,
#       binary = FALSE,
#       mode = 1
#     )
#   )
#
#   my_test_system <- make_system(
#     algos = algos,
#     init_capital = 1000000,
#     system_risk_target = 0.12,
#     risk_window_length = 5,
#     position_modifiers = list(), ## Add manually to system for test (see below)
#     position_multipliers = list(), ## Add manually to system for test (see below)
#     portfolio_multipliers = list(), ## Add manually to system for test (see below)
#     min_periods = min_periods,
#     min_signal = -2,
#     max_signal = 2,
#     instrument_data_folder_path = testthat::test_path("fixtures/")
#   )
#
#   pos_mods <- list(
#     list(
#       instruments = list("testdata3", "testdata4"),
#       modifier = list(
#         "p_stop_loss",
#         p_stop_loss,
#         stop_loss_fraction = 0.5,
#         rnd = FALSE
#       )
#     )
#   )
#   my_test_system$position_modifiers <- expand_position_modifiers(pos_mods)
#
#   pos_muls <- list(
#     list(
#       instruments = list("testdata3", "testdata4"),
#       multipliers = list(
#         list(
#           "m_block_same_direction_entry",
#           m_block_same_direction_entry,
#           mode = 2
#         )
#       )
#     )
#   )
#   my_test_system$position_multipliers <- expand_position_multipliers(pos_muls)
#
#   portfolio_muls <- list(
#     list(
#       "o_limit_pf_risk",
#       o_limit_pf_risk,
#       max_risk = NA,
#       cov_method = "ewa"
#     ),
#     list(
#       " o_limit_leverage_risk",
#       o_limit_leverage_risk,
#       max_leverage = NA
#     )
#   )
#   my_test_system$portfolio_multipliers <-
#     parse_portfolio_multipliers_list(portfolio_muls)
#
#   suppressWarnings(
#     my_test_system <- run_system(
#       my_test_system,
#       min_periods = min_periods,
#       mode = "sim",
#       instrument_data_folder_path = testthat::test_path("fixtures/")
#     )
#   )
#
#   my_test_stop_loss <- my_test_system$position_tables
#
#   ## Uncomment to generate expected data:
#
#   # saveRDS(
#   #   my_test_stop_loss,
#   #   file=test_path("fixtures/", "my_expected_stop_loss.RData")
#   # )
#
#   my_expected_stop_loss <- readRDS(test_path("fixtures", "my_expected_stop_loss.RData"))
#
#   ## Test ----
#   expect_equal(
#     my_test_stop_loss,
#     my_expected_stop_loss
#   )
# })

test_that("combined portfolio multipliers work", {

  return_one <- function(t, fixed_param_1, variable_param_1) {
    multiplier_value <- fixed_param_1 + variable_param_1
    list(
      multiplier_value = multiplier_value,
      additional_output = "add_out"
    )
  }
  return_less_than_one <- function(t, fixed_param_2, variable_param_2) {
    multiplier_value <- fixed_param_2 + variable_param_2
    list(
      multiplier_value = multiplier_value,
      additional_output = "add_out"
    )
  }

  portfolio_multipliers <- list(
    list(
      multiplier_name = "return_one",
      multiplier_function = return_one,
      fixed_params = list(fixed_param_1 = 0.6),
      variable_params = list(t = 11, variable_param_1 = "variable_param_1")
    ),
    list(
      multiplier_name = "return_less_than_one",
      multiplier_function = return_less_than_one,
      fixed_params = list(fixed_param_2 = 0.3),
      variable_params = list(t = 11, variable_param_2 = "variable_param_2")
    )
  )

  system_vars <- list(
    variable_param_1 = 0.4,
    variable_param_2 = 0.2
  )

  my_test_combined_portfolio_multipliers <- combine_portfolio_multipliers(
    portfolio_multipliers = portfolio_multipliers,
    system_vars = system_vars,
    combi_method = "multiply"
  )

  my_expected_combined_portfolio_multipliers <- list(
    comb_port_mul_vals = 0.5,
    port_mul_vals.return_one = 1,
    port_mul_vals.return_less_than_one = 0.5,
    comb_port_mul_out1 = list(additional_output = "add_out"),
    comb_port_mul_out2 = list(additional_output = "add_out")
  )

  expect_equal(
    my_test_combined_portfolio_multipliers,
    my_expected_combined_portfolio_multipliers
  )
})

test_that("o_limit_pf_risk() works", {
  position_tables <- list(
    data.frame(
      price = c(100, 105, 103, 106, 104, 101, 99, 100, 102, 97),
      final_position_size_units = c(2, 1, 1, 2, 1, 0, -1, -1, -1, -1)
    ),
    data.frame(
      price = c(103, 101, 100, 97, 105, 102, 103, 92, 93, 98),
      final_position_size_units = c(1, 1, 2, 2, 2, 3, 3, 0, 1, 1)
    )
  )
  names(position_tables) <- c("a", "b")

  my_test_limit_pf_risk <- o_limit_pf_risk(
    t = 10,
    position_tables = position_tables,
    max_risk = 0.5,
    capital = 1000,
    cov_method = "Pearson"
  )
  my_test_limit_pf_risk <- lapply(my_test_limit_pf_risk, function(x) round(x, 5))

  my_expected_limit_pf_risk <- list(
    multiplier_value = 1,
    portfolio_risk = 0.00723
  )

  expect_equal(
    my_test_limit_pf_risk,
    my_expected_limit_pf_risk
  )
})

test_that("o_limit_pf_shock_risk() works", {
  position_tables <- list(
    data.frame(
      price = c(100, 105, 103, 106, 104, 101, 99, 100, 102, 97),
      final_position_size_units = c(2, 1, 1, 2, 1, 0, -1, -1, -1, -1)
    ),
    data.frame(
      price = c(103, 101, 100, 97, 105, 102, 103, 92, 93, 98),
      final_position_size_units = c(1, 1, 2, 2, 2, 3, 3, 0, 1, 1)
    )
  )
  names(position_tables) <- c("a", "b")

  suppressWarnings(
    my_test_limit_pf_shock_risk <- o_limit_pf_shock_risk(
      t = 10,
      position_tables = position_tables,
      max_risk = 0.5,
      sd_percentile = 0.99,
      capital =  1000,
      sd_window_length = 5L,
      sd_method = "unbiased",
      cor_method = "Pearson"
    )
  )
  my_test_limit_pf_shock_risk <- lapply(my_test_limit_pf_shock_risk, function(x) round(x, 5))

  my_expected_limit_pf_shock_risk <- list(
    multiplier_value = 1,
    shock_pf_risk = 0.00977
  )

  expect_equal(
    my_test_limit_pf_shock_risk,
    my_expected_limit_pf_shock_risk
  )
})

test_that("o_limit_cor_risk() works", {
  position_tables <- list(
    data.frame(
      price = c(100, 105, 103, 106, 104, 101, 99, 100, 102, 97),
      final_position_size_units = c(2, 1, 1, 2, 1, 0, -1, -1, -1, -1),
      instrument_risk = c(0.04, 0.05, 0.03, 0.06, 0.04, 0.01, 0.02, 0.03, 102, 0.04)
    ),
    data.frame(
      price = c(103, 101, 100, 97, 105, 102, 103, 92, 93, 98),
      final_position_size_units = c(1, 1, 2, 2, 2, 3, 3, 0, 1, 1),
      instrument_risk = c(0.03, 0.01, 0.02, 0.03, 0.05, 0.02, 0.03, 0.02, 0.03, 0.04)
    )
  )
  names(position_tables) <- c("a", "b")

  suppressWarnings(
    my_test_limit_cor_risk <- o_limit_cor_risk(
      t = 10,
      position_tables = position_tables,
      max_risk = 0.5,
      capital =  1000
    )
  )
  my_test_limit_cor_risk <- lapply(my_test_limit_cor_risk, function(x) round(x, 5))

  my_expected_limit_cor_risk <- list(
    multiplier_value = 1,
    cor_shock_pf_risk = 0.00004
  )

  expect_equal(
    my_test_limit_cor_risk,
    my_expected_limit_cor_risk
  )
})



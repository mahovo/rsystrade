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
      final_buffered_pos_ccy = c(200, 105, 103, 212, 104, 0, -99, -100, -102, -97)
    ),
    data.frame(
      price = c(103, 101, 100, 97, 105, 102, 103, 92, 93, 98),
      final_buffered_pos_ccy = c(103, 101, 200, 194, 210, 306, 309, 0, 93, 98)
    )
  )
  names(position_tables) <- c("a", "b")

  my_test_limit_pf_risk <- o_limit_pf_risk(
    t = 10,
    position_tables = position_tables,
    max_risk = 0.5,
    capital = c(1003, 1100, 1097, 1111, 1204, 1205, 897, 992, 992),
    cov_method = "Pearson"
  )
  my_test_limit_pf_risk <- lapply(my_test_limit_pf_risk, function(x) round(x, 5))

  my_expected_limit_pf_risk <- list(
    multiplier_value = 1,
    portfolio_risk = 0.00729
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
      final_buffered_pos_ccy = c(200, 105, 103, 212, 104, 0, -99, -100, -102, -97)
    ),
    data.frame(
      price = c(103, 101, 100, 97, 105, 102, 103, 92, 93, 98),
      final_buffered_pos_ccy = c(103, 101, 200, 194, 210, 306, 309, 0, 93, 98)
    )
  )
  names(position_tables) <- c("a", "b")

  suppressWarnings(
    my_test_limit_pf_shock_risk <- o_limit_pf_shock_risk(
      t = 10,
      position_tables = position_tables,
      max_risk = 0.5,
      sd_percentile = 0.99,
      c(1003, 1100, 1097, 1111, 1204, 1205, 897, 992, 992),
      sd_window_length = 5L,
      sd_method = "unbiased",
      cor_method = "Pearson"
    )
  )
  my_test_limit_pf_shock_risk <- lapply(my_test_limit_pf_shock_risk, function(x) round(x, 5))

  my_expected_limit_pf_shock_risk <- list(
    multiplier_value = 1,
    shock_pf_risk = 0.00985
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
      final_buffered_pos_ccy = c(200, 105, 103, 212, 104, 0, -99, -100, -102, -97),
      instrument_risk = c(0.04, 0.05, 0.03, 0.06, 0.04, 0.01, 0.02, 0.03, 102, 0.04)
    ),
    data.frame(
      price = c(103, 101, 100, 97, 105, 102, 103, 92, 93, 98),
      final_buffered_pos_ccy = c(103, 101, 200, 194, 210, 306, 309, 0, 93, 98),
      instrument_risk = c(0.03, 0.01, 0.02, 0.03, 0.05, 0.02, 0.03, 0.02, 0.03, 0.04)
    )
  )
  names(position_tables) <- c("a", "b")

  suppressWarnings(
    my_test_limit_cor_risk <- o_limit_cor_risk(
      t = 10,
      position_tables = position_tables,
      max_risk = 0.5,
      capital = c(1003, 1100, 1097, 1111, 1204, 1205, 897, 992, 992)
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



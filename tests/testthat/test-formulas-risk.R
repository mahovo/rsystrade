test_that("method = \"equal\" in update_signal_normalization_factors() works", {

  ## Make testsystem ----

  set.seed(1)
  #my_test_system <- make_test_system()
  #saveRDS(my_test_system, file = test_path("fixtures/", "my_test_system.rds"), compress = FALSE)
  my_test_system <- readRDS(test_path("fixtures", "my_expected_system.rds"))

  ## Generate output for test ----
  my_test_norm_fact_equal <- update_signal_normalization_factors(
    parsed_algos = my_test_system$algos,
    signal_tables = my_test_system$signal_tables,
    instrument_data_sets = my_test_system$inst_data,
    target = 1,
    method = "equal",
    args = list(equal_norm_factor = 0.12345)
  )

  ## Compare ----
  expect_equal(
    my_test_norm_fact_equal,
    list(mac_2_4 = 0.12345, mac_3_9 = 0.12345)
  )

})

test_that("method = \"pool_traded\" in update_signal_normalization_factors() works", {

  ## Make testsystem ----
  set.seed(1)
  #my_test_system <- make_test_system()
  #saveRDS(my_test_system, file = test_path("fixtures/", "my_test_system.rds"), compress = FALSE)
  my_test_system <- readRDS(test_path("fixtures", "my_expected_system.rds"))

  ## Generate output for test ----
  my_test_norm_fact_pool_traded <- update_signal_normalization_factors(
    parsed_algos = my_test_system$algos,
    signal_tables = my_test_system$signal_tables,
    instrument_data_sets = my_test_system$inst_data,
    target = 1,
    method = "pool_traded"
  )
  # saveRDS(my_test_norm_fact_pool_traded, file = test_path("fixtures/", "my_expected_norm_fact_pool_traded.rds"), compress = FALSE)
  my_expected_norm_fact_pool_traded <- readRDS(test_path("fixtures", "my_expected_norm_fact_pool_traded.rds"))

  ## Compare ----
  expect_equal(
    my_test_norm_fact_pool_traded,
    my_expected_norm_fact_pool_traded
  )
})

# test_that("method = \"median_pool_all\" inupdate_signal_normalization_factors() works", {
#
#   ## Make testsystem ----
#   set.seed(1)
#   #my_test_system <- make_test_system()
#   #saveRDS(my_test_system, file = test_path("fixtures/", "my_test_system.rds"), compress = FALSE)
#   my_test_system <- readRDS(test_path("fixtures", "my_test_system.rds"))
#
#   ## Generate output for test ----
#   my_test_norm_fact_median_pool_all <- update_signal_normalization_factors(
#     parsed_algos = my_test_system$algos,
#     signal_tables = my_test_system$signal_tables,
#     instrument_data_sets = my_test_system$inst_data,
#     target = 1,
#     method = "median_pool_all",
#     args = list(min_periods_median_pool_all = 25)
#   )
#   #saveRDS(my_test_norm_fact_median_pool_all, file = test_path("fixtures/", "my_expected_norm_fact_median_pool_all.rds"), compress = FALSE)
#   my_expected_norm_fact_median_pool_all <- readRDS(test_path("fixtures", "my_expected_norm_fact_median_pool_all.rds"))
#
#   ## Compare ----
#   expect_equal(
#     my_test_norm_fact_median_pool_all,
#     my_expected_norm_fact_median_pool_all
#   )
# })

test_that("f_inst_risk() works", {

  returns <- rep(c(0, 2), 10)
  prices <- f_prices_from_returns(returns, 1)

  ## If mu is simple mean (not exponential):
  ## Each squared deviation from the mean is 1, so we can just calculate the sum
  ## of the weights, which by design is supposed to be 1.
  ## Of course, with exponential mu, setting lambda to 1 is equivalent to
  ## simple mean


  my_test_norm_inst_risk <- list()
  my_test_norm_inst_risk[[1]] <- f_inst_risk(
    t = 22,
    prices = prices,
    window_length = NA,
    annualized = FALSE,
    periods = 20,
    method = 1
  ) * sqrt(19/20)
  my_test_norm_inst_risk[[2]] <- f_inst_risk(
    t = 22,
    prices = prices,
    window_length = NA,
    annualized = FALSE,
    periods = 20,
    method = 2,
    lambda = 1
  )
  my_test_norm_inst_risk[[3]] <- signif(
    f_inst_risk(
      t = 22,
      prices = prices,
      window_length = NA,
      annualized = FALSE,
      method = 2,
      lambda = NA
    ),
    13
  )
  my_test_norm_inst_risk[[4]] <- signif(
    f_inst_risk(
      t = 22,
      prices = prices,
      window_length = 10,
      annualized = FALSE,
      method = 2,
      lambda = NA
    ),
    13
  )
  my_test_norm_inst_risk[[5]] <- f_inst_risk(
    t = 22,
    prices = prices,
    window_length = 10,
    annualized = FALSE,
    method = 2,
    lambda = 1
  )

  my_expected_inst_risk <- list(
    1,
    1,
    0.9987492177719,
    0.9949874371066,
    1
  )


  ## Compare ----
  expect_equal(
    my_test_norm_inst_risk,
    my_expected_inst_risk
  )
})


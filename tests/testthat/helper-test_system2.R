make_test_algos2 <- function(
    signal_generator_1, ## list
    signal_generator_2 ## list
) {
  list(
    list( ## We might name this "subset1"
      instruments = list("testdata7"),
      rules = list(
        rule1 = signal_generator_1,
        rule2 = signal_generator_2
      )
    ),
    list( ## We might name this "subset2"
      instruments = list("testdata8"),
      rules = list(
        rule1 = signal_generator_1,
        rule2 = signal_generator_2
      )
    )
  )
}

make_test_system2 <- function() {
  #n = 100L
  min_periods <- 25L

  ## *** Generate instrument data ***

  #times <- timeDate::as.timeDate(seq(from = as.Date("2000-01-01"), by = "day", length.out = n))
  # times <- 1:n
  #
  # prices1 <- round(100 + cumsum(rnorm(n, 0, 0.5)), 2)
  # prices2 <- round(100 + cumsum(rnorm(n, 0, 0.5)), 2)
  #
  # plot(prices1, ylim = c(min(c(prices1, prices2)), max(c(prices1, prices2))), pch = 16, cex = 0.3, col = "red")
  # points(prices2, pch = 16, cex = 0.3, col = "blue")
  # #
  # # cor(prices1, prices2)
  # # sd(prices1)
  # # sd(prices2)
  #
  # df1 <- data.frame(
  #   time = times,
  #   price <- prices1
  #   # price = round(
  #   #   c(100, 100 * cumprod(1 + rnorm(n - 1, 0, 0.1/16))),
  #   #   2
  #   # )
  # )
  # names(df1) <- c("time", "price")
  # names(df1)
  #
  # df2 <- data.frame(
  #   time = times,
  #   price <- prices2
  #   # price = round(
  #   #   c(100, 100 * cumprod(1 + rnorm(n -1, 0, 0.1/16))),
  #   #   2
  #   # )
  # )
  # names(df2) <- c("time", "price")
  # names(df2)
  # write.csv(df1, testthat::test_path("fixtures/", "testdata7.csv"), row.names=FALSE)
  # write.csv(df2, testthat::test_path("fixtures/", "testdata8.csv"), row.names=FALSE)

  algos <- make_test_algos2(
    list(
      "mac_30_60",
      r_mac,
      ma_fast = NA,
      ma_slow = NA,
      n_fast = 30L,
      n_slow = 60L,
      ma_method = "simple",
      gap = 0,
      strict = TRUE,
      binary = FALSE,
      mode = 1
    ),
    list(
      "mac_20_80",
      r_mac,
      ma_fast = NA,
      ma_slow = NA,
      n_fast = 20L,
      n_slow = 80L,
      ma_method = "simple",
      gap = 0,
      strict = TRUE,
      binary = FALSE,
      mode = 1
    )
  )

  my_test_system2 <- make_system(
    algos = algos,
    init_capital = 1000000,
    system_risk_target = 0.12,
    risk_window_length = 20,
    min_periods = min_periods,
    mode = "sim",
    instrument_data_folder_path = testthat::test_path("fixtures/"),
    position_multipliers <- list(
      list(
        instruments = list("testdata7", "testdata8"),
        multipliers = list(
          list(
            "m_block_same_direction_entry",
            m_block_same_direction_entry,
            mode = 2
          )
        )
      )
    ),
    position_modifiers = list(
      list(
        instruments = list("testdata7", "testdata8"),
        modifier = list(
          "p_stop_loss",
          p_stop_loss,
          stop_loss_fraction = 0.5,
          rnd = FALSE
        )
      )
    ),
    portfolio_multipliers = list(
      list(
        modifier_name = "o_limit_pf_risk",
        modifier_function = o_limit_pf_risk,
        max_risk = 0.1,
        cov_method = "Pearson"
      ),
      list(
        modifier_name = "o_limit_pf_shock_risk",
        modifier_function = o_limit_pf_shock_risk,
        max_risk = 0.1,
        sd_percentile = 0.99,
        sd_window_length = 5L,
        sd_method = "unbiased",
        cor_method = "Pearson"
      )
    )
  )

  my_test_system2 <- run_system(
    my_test_system2,
    min_periods = min_periods,
    mode = "sim",
    instrument_data_folder_path = testthat::test_path("fixtures/")
  )

  my_test_system2
}

# o_limit_pf_risk <- function(
#     t,
#     position_tables,
#     max_risk = NA,
#     capital,
#     cov_method,
#     ...
# )

## Write expected system to disk

# my_test_system <- make_test_system()
# saveRDS(my_test_system, file = test_path("fixtures/", "my_expected_system.rds"), compress = FALSE)



## Change path to avoid mismatch between actual and expected.
## This is needed when running test with testthat (cmd+shift+E), not when running the test code in test_system.R.

# my_expected_system <- readRDS(test_path("fixtures", "my_expected_system.rds"))
# my_expected_system$config$instrument_data_folder_path <- "fixtures/"
# saveRDS(my_expected_system, file = test_path("fixtures/", "my_expected_system.rds"), compress = FALSE)


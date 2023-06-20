
make_test_algos <- function(
    signal_generator_1, ## list
    signal_generator_2 ## list
  ) {
  list(
      list( ## We might name this "subset1"
        instruments = list("testdata3"),
        rules = list(
            rule1 = signal_generator_1,
            rule2 = signal_generator_2
          )
      ),
      list( ## We might name this "subset2"
        instruments = list("testdata4"),
        rules = list(
          rule1 = signal_generator_1,
          rule2 = signal_generator_2
        )
      )
  )
}

## This test data is designed to remind us that low volatility will blow us up :-)
make_test_system <- function() {
  #n = 20
  min_periods <- 10L

  ## *** Generate instrument data ***

  # #times <- timeDate::as.timeDate(seq(from = as.Date("2000-01-01"), by = "day", length.out = n))
  # times <- 1:20
  #
  # prices1 <- 100 + c(0, 1, 2, 3, 4, 5, 4, 3, 4, 5, 6, 7, 8, 9, 10, 9, 8, 7, 6, 5)/100
  # prices2 <- 100 + c(0, -2, -4, -6, -4, -2, 0, 2, 4, 6, 8, 10, 8, 6, 4, 2, 0, -2, 0, 2)/100
  #
  # # plot(prices1, ylim = c(99.94, 100.1), pch = 16, cex = 0.3, col = "red")
  # # points(prices2, pch = 16, cex = 0.3, col = "blue")
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
  # write.csv(df1, testthat::test_path("fixtures/", "testdata3.csv"), row.names=FALSE)
  # write.csv(df2, testthat::test_path("fixtures/", "testdata4.csv"), row.names=FALSE)

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

  # parsed_algos <- parse_algos(algos)
  #
  # inst_data <- load_instrument_data_sets(
  #   parsed_algos = parsed_algos,
  #   #instrument_data_folder_path = "~/git/rsystrade/misc/temp/data/"
  #   instrument_data_folder_path = testthat::test_path("fixtures")
  # )
  # names(inst_data) <- unlist(get_unique_inst_names_from_parsed_algos_list(parsed_algos))
  #
  # rule_functions <- load_rule_functions(parsed_algos)
  # names(rule_functions) <- get_unique_rule_function_names_by_parsed_algo(parsed_algos)
  #
  # num_signals <- get_num_rules_from_parsed_algos_list(parsed_algos)
  # signal_tables <- list()
  # for(i in 1:num_signals) {
  #   ## One table for each algo (i.e each instrument + rule combination)
  #   signal_tables[[i]] <- data.frame(
  #     time = inst_data[[parsed_algos[[i]]$instrument]]$time[1:min_periods],
  #     price = inst_data[[parsed_algos[[i]]$instrument]]$price[1:min_periods],
  #     raw_signal = rep(NA, min_periods),
  #     normalized_signal = rep(NA, min_periods),
  #     clamped_signal = rep(NA, min_periods),
  #     signal_weight = rep(NA, min_periods)
  #   )
  # }


  my_test_system <- make_system(
    algos = algos,
    init_capital = 1000000,
    system_risk_target = 0.12,
    risk_window_length = 5,
    min_periods = min_periods,
    mode = "sim",
    instrument_data_folder_path = testthat::test_path("fixtures/")
  )

  my_test_system <- run_system(
    my_test_system,
    min_periods = min_periods,
    mode = "sim",
    instrument_data_folder_path = testthat::test_path("fixtures/")
  )

  my_test_system
}



## Write expected system to disk

# my_test_system <- make_test_system()
# saveRDS(my_test_system, file = test_path("fixtures/", "my_expected_system.rds"), compress = FALSE)



## Change path to avoid mismatch between actual and expected:

# my_expected_system <- readRDS(test_path("fixtures", "my_expected_system.rds"))
# my_expected_system$config$instrument_data_folder_path <- "fixtures/"
# saveRDS(my_expected_system, file = test_path("fixtures/", "my_expected_system.rds"), compress = FALSE)








## *** NOTE *** *** *** *** *** *** *** *** *** *** *** *** *** *** ***
## This part is not used!
## *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** ***
## *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** ***
##
## * IMPORTANT *
## Rule functions need to be loaded in the test env.
## Do shift+cmd+L once with these lines uncommented.
## This will fix the env of the rule functions in the my_expected_system
## written to disk.
# mac_2_4 <- function(
#     prices,
#     signal_table,
#     position_table,
#     t
# ) {
#   mac_rule(
#     prices,
#     t = t,
#     ma_fast = NA,
#     ma_slow = NA,
#     n_fast = 2L,
#     n_slow = 4L,
#     gap = 0,
#     strict = TRUE,
#     binary = FALSE
#   )
# }
# mac_3_6 <- function(
#     prices,
#     signal_table,
#     position_table,
#     t
# ) {
#   mac_rule(
#     prices,
#     t = t,
#     ma_fast = NA,
#     ma_slow = NA,
#     n_fast = 3L,
#     n_slow = 6L,
#     gap = 0,
#     strict = TRUE,
#     binary = FALSE
#   )
# }
# my_expected_system_2 <- readRDS("~/git/rsystrade/tests/testthat/fixtures/my_test_system.rds")
# my_expected_system_2$rule_functions$mac_2_4 <- rlang::set_env(mac_2_4, rlang::current_env())
# my_expected_system_2$rule_functions$mac_3_6 <- rlang::set_env(mac_3_6, rlang::current_env())
# saveRDS(my_expected_system_2, file = "~/git/rsystrade/tests/testthat/fixtures/my_test_system.rds")
## *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** ***
## *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** ***

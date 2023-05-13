make_test_system <- function() {
  n <- 50
  min_periods <- 25L

  set.seed(1)

  ## *** Generate instrument data ***

  # times <- timeDate::as.timeDate(seq(from = as.Date("2000-01-01"), by = "day", length.out = n))
  # df1 <- data.frame(
  #   time = times,
  #   price = round(
  #     c(100, 100 * cumprod(1 + rnorm(n - 1, 0, 0.1/16))),
  #     2
  #   )
  # )
  # names(df1) <- c("time", "price")
  # names(df1)
  # df2 <- data.frame(
  #   time = times,
  #   price = round(
  #     c(100, 100 * cumprod(1 + rnorm(n -1, 0, 0.1/16))),
  #     2
  #   )
  # )
  # names(df2) <- c("time", "price")
  # names(df2)
  # write.csv(df1, testthat::test_path("fixtures/", "testdata1.csv"))
  # write.csv(df2, testthat::test_path("fixtures/", "testdata2.csv"))

  algos <- list(
    list( ## We might name this "subset1"
      instruments = list("testdata1"),
      rules = list(
        list("mac_10_30"),
        list("mac_12_36")
      )
    ),
    list( ## We might name this "subset2"
      instruments = list("testdata2"),
      rules = list(
        c("mac_10_30"),
        c("mac_12_36")
      )
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
    risk_window_length = 25,
    stop_loss_fraction = 0.5,
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

## *** NOTE ***
## Rule functions need to be loaded into the package env to work with tests.
## This is done here when doing shift+cmd+L.
## This is only true for testing. In normal use, rule functions need to be in
## the global env.
## The test system written to disk has been modified, so that the rule
## functions are in the package env (not the global env).
## This is done by commenting out the code at the bottom of this document.
## This only needs to be done again, if the test system on disk is overwritten.
mac_10_30 <- function(prices) {
  mac_rule(
    prices,
    n_fast = 10L,
    n_slow = 30L
  )
}
mac_12_36 <- function(prices) {
  mac_rule(
    prices,
    n_fast = 12L,
    n_slow = 36L
  )
}

mac_10_30 <- rlang::set_env(mac_10_30, rlang::global_env())
mac_12_36 <- rlang::set_env(mac_12_36, rlang::global_env())


# my_test_system <- make_test_system()
# saveRDS(my_test_system, file = test_path("fixtures/", "my_test_system.rds"), compress = FALSE)

## Change path to avoid mismatch between actual and expected:
# my_test_system <- readRDS(test_path("fixtures", "my_test_system.rds"))
# my_test_system$config$instrument_data_folder_path <- "fixtures/"
# saveRDS(my_test_system, file = test_path("fixtures/", "my_test_system.rds"), compress = FALSE)


## **** IMPORTANT ****

## Rule functions need to be loaded in the test env.
## Do shift+cmd+L once with these lines uncommented.
## This will fix the env of the rule functions in the my_expected_system
## written to disk.
# mac_10_30 <- function(prices) {
#   mac_rule(
#     prices,
#     n_fast = 10L,
#     n_slow = 30L
#   )
# }
# mac_12_36 <- function(prices) {
#   mac_rule(
#     prices,
#     n_fast = 12L,
#     n_slow = 36L
#   )
# }
# my_expected_system <- readRDS("~/git/rsystrade/tests/testthat/fixtures/my_test_system.rds")
# my_expected_system$rule_functions$mac_10_30 <- rlang::set_env(mac_10_30, rlang::current_env())
# my_expected_system$rule_functions$mac_12_36 <- rlang::set_env(mac_12_36, rlang::current_env())
# saveRDS(my_expected_system, file = "~/git/rsystrade/tests/testthat/fixtures/my_test_system.rds")




################################################################################
# test system 2
################################################################################


make_test_system_2 <- function() {
  min_periods <- 7L

  ## *** Generate instrument data ***

  #times <- timeDate::as.timeDate(seq(from = as.Date("2000-01-01"), by = "day", length.out = n))
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
  # write.csv(df1, testthat::test_path("fixtures/", "testdata3.csv"))
  # write.csv(df2, testthat::test_path("fixtures/", "testdata4.csv"))

  algos <- list(
    list( ## We might name this "subset1"
      instruments = list("testdata3"),
      rules = list(
        list("mac_2_4"),
        list("mac_3_6")
      )
    ),
    list( ## We might name this "subset2"
      instruments = list("testdata4"),
      rules = list(
        c("mac_2_4"),
        c("mac_3_6")
      )
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
    stop_loss_fraction = 0.5,
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

## *** NOTE ***
## Rule functions need to be loaded into the package env to work with tests.
## This is done here when doing shift+cmd+L.
## This is only true for testing. In normal use, rule functions need to be in
## the global env.
## The test system written to disk has been modified, so that the rule
## functions are in the package env (not the global env).
## This is done by commenting out the code at the bottom of this document.
## This only needs to be done again, if the test system on disk is overwritten.
mac_2_4 <- function(
    prices,
    signal_table,
    position_table,
    t
  ) {
  mac_rule(
    prices,
    t = t,
    ma_fast = NA,
    ma_slow = NA,
    n_fast = 2L,
    n_slow = 4L,
    gap = 0,
    strict = TRUE,
    binary = FALSE
  )
}
mac_3_6 <- function(
    prices,
    signal_table,
    position_table,
    t
) {
  mac_rule(
    prices,
    t = t,
    ma_fast = NA,
    ma_slow = NA,
    n_fast = 3L,
    n_slow = 6L,
    gap = 0,
    strict = TRUE,
    binary = FALSE
  )
}


mac_2_4 <- rlang::set_env(mac_2_4, rlang::global_env())
mac_3_6 <- rlang::set_env(mac_3_6, rlang::global_env())


## Write expected system to disk
# my_test_system_2 <- make_test_system_2()
# saveRDS(my_test_system_2, file = test_path("fixtures/", "my_test_system_2.rds"), compress = FALSE)

## Change path to avoid mismatch between actual and expected:
# my_test_system_2 <- readRDS(test_path("fixtures", "my_test_system_2.rds"))
# my_test_system_2$config$instrument_data_folder_path <- "fixtures/"
# saveRDS(my_test_system_2, file = test_path("fixtures/", "my_test_system_2.rds"), compress = FALSE)


## **** IMPORTANT ****

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
# my_expected_system_2 <- readRDS("~/git/rsystrade/tests/testthat/fixtures/my_test_system_2.rds")
# my_expected_system_2$rule_functions$mac_2_4 <- rlang::set_env(mac_2_4, rlang::current_env())
# my_expected_system_2$rule_functions$mac_3_6 <- rlang::set_env(mac_3_6, rlang::current_env())
# saveRDS(my_expected_system_2, file = "~/git/rsystrade/tests/testthat/fixtures/my_test_system_2.rds")


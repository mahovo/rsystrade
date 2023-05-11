test_that("expand_algos() works", {

  algos <- list(
    list( ## We might name this "subset1"
      instruments = list("testdata1"),
      rules = list("rule1", "rule2")
    ),
    list( ## We might name this "subset2"
      instruments = list("testdata2"),
      rules = list("rule1", "rule2")
    ),
    list( ## We might name this "subset3"
      instruments = list("testdata3", "testdata4"),
      rules = list("rule1", "rule2")
    )
  )

  my_test_expanded_algos <- expand_algos(algos)

  ## Write expected expanded algos to file
  # saveRDS(my_test_expanded_algos, file = test_path(
  #   "fixtures/",
  #   "my_expected_expanded_algos.rds"),
  #   compress = FALSE
  # )

  my_expected_expanded_algos <- readRDS(
    test_path("fixtures",
              "my_expected_expanded_algos.rds"
    )
  )

  expect_equal(
    my_test_expanded_algos,
    my_expected_expanded_algos)
})

test_that("get_unique_inst_paths_from_expanded_algos_list() works", {

  algos <- list(
    list( ## We might name this "subset1"
      instruments = list("testdata1"),
      rules = list("rule1", "rule2")
    ),
    list( ## We might name this "subset2"
      instruments = list("testdata2"),
      rules = list("rule1", "rule2")
    ),
    list( ## We might name this "subset3"
      instruments = list("testdata3", "testdata4"),
      rules = list("rule1", "rule2")
    )
  )

  my_test_expanded_algos <- expand_algos(algos)

  my_test_unique_inst_paths <- get_unique_inst_paths_from_expanded_algos_list(
    expanded_algos = my_test_expanded_algos,
    instrument_data_folder_path = testthat::test_path("fixtures/")
  )

  ## Write expected output to file
  # my_expected_unique_inst_paths <- my_test_unique_inst_paths
  # my_expected_unique_inst_paths <- list(
  #   "fixtures/testdata1.csv",
  #   "fixtures/testdata2.csv",
  #   "fixtures/testdata3.csv",
  #   "fixtures/testdata4.csv"
  # )
  # saveRDS(
  #   my_expected_unique_inst_paths,
  #   file = test_path(
  #     "fixtures",
  #     "my_expected_unique_inst_paths.rds"
  #   ),
  #   compress = FALSE
  # )

  my_expected_unique_inst_paths <- readRDS(
    test_path("fixtures",
              "my_expected_unique_inst_paths.rds"
    )
  )

  expect_equal(
    my_test_unique_inst_paths,
    my_expected_unique_inst_paths)
})

test_that("get_inst_names_by_parsed_algo() works", {

  algos <- list(
    list( ## We might name this "subset1"
      instruments = list("testdata1"),
      rules = list("rule1", "rule2")
    ),
    list( ## We might name this "subset2"
      instruments = list("testdata2"),
      rules = list("rule1", "rule2")
    ),
    list( ## We might name this "subset3"
      instruments = list("testdata3", "testdata4"),
      rules = list("rule1", "rule2")
    )
  )

  my_test_expanded_algos <- expand_algos(algos)

  my_test_inst_names <- get_inst_names_by_parsed_algo(
    parsed_algos = my_test_expanded_algos
  )

  ## Write expected output to file
  # my_expected_inst_names <- my_test_inst_names
  # saveRDS(
  #   my_expected_inst_names,
  #   file = test_path(
  #     "fixtures",
  #     "my_expected_inst_names.rds"
  #   ),
  #   compress = FALSE
  # )

  my_expected_inst_names <- readRDS(
    test_path("fixtures",
              "my_expected_inst_names.rds"
    )
  )

  expect_equal(
    my_test_inst_names,
    my_expected_inst_names)
})

test_that("get_unique_inst_names_from_parsed_algos_list() works", {

  algos <- list(
    list( ## We might name this "subset1"
      instruments = list("testdata1"),
      rules = list("rule1", "rule2")
    ),
    list( ## We might name this "subset2"
      instruments = list("testdata2"),
      rules = list("rule1", "rule2")
    ),
    list( ## We might name this "subset3"
      instruments = list("testdata3", "testdata4"),
      rules = list("rule1", "rule2")
    )
  )

  my_test_expanded_algos <- expand_algos(algos)

  my_test_unique_inst_names <- get_unique_inst_names_from_parsed_algos_list(
    parsed_algos = my_test_expanded_algos
  )

  ## Write expected output to file
  # my_expected_unique_inst_names <- my_test_unique_inst_names
  # saveRDS(
  #   my_expected_unique_inst_names,
  #   file = test_path(
  #     "fixtures",
  #     "my_expected_unique_inst_names.rds"
  #   ),
  #   compress = FALSE
  # )

  my_expected_unique_inst_names <- readRDS(
    test_path("fixtures",
              "my_expected_unique_inst_names.rds"
    )
  )

  expect_equal(
    my_test_unique_inst_names,
    my_expected_unique_inst_names)
})

test_that("get_num_inst_from_parsed_algos_list() works", {

  algos <- list(
    list( ## We might name this "subset1"
      instruments = list("testdata1"),
      rules = list("rule1", "rule2")
    ),
    list( ## We might name this "subset2"
      instruments = list("testdata2"),
      rules = list("rule1", "rule2")
    ),
    list( ## We might name this "subset3"
      instruments = list("testdata3", "testdata4"),
      rules = list("rule1", "rule2")
    )
  )

  my_test_expanded_algos <- expand_algos(algos)

  my_test_num_inst <- get_num_inst_from_parsed_algos_list(
    parsed_algos = my_test_expanded_algos
  )

  expect_equal(
    my_test_num_inst,
    4)
})

## *** NOTE ***
## expect_warning() does not seem to work.

# test_that("get_num_inst_from_parsed_algos_list() throws error", {
#
#   algos <- list(
#
#   )
#
#   my_test_expanded_algos <- expand_algos(algos)
#
#   # get_inst_names_by_parsed_algo(
#   #   parsed_algos = my_test_expanded_algos
#   # )
#
#   my_test_num_inst <- get_num_inst_from_parsed_algos_list(
#     parsed_algos = my_test_expanded_algos
#   )
#
#   expect_warning(my_test_num_inst)
# })

test_that("get_num_rules_per_inst_from_parsed_algos() works", {

  algos <- list(
    list( ## We might name this "subset1"
      instruments = list("testdata1"),
      rules = list("rule1", "rule2")
    ),
    list( ## We might name this "subset2"
      instruments = list("testdata2"),
      rules = list("rule1", "rule2")
    ),
    list( ## We might name this "subset3"
      instruments = list("testdata3", "testdata4"),
      rules = list("rule1", "rule2")
    )
  )

  my_test_expanded_algos <- expand_algos(algos)

  my_test_num_rules <- get_num_rules_per_inst_from_parsed_algos(
    parsed_algos = my_test_expanded_algos
  )

  my_expected_num_rules <- c(2, 2, 2, 2)
  names(my_expected_num_rules) <- c(
    "testdata1",
    "testdata2",
    "testdata3",
    "testdata4"
  )

  expect_equal(
    my_test_num_rules,
    my_expected_num_rules
  )
})

test_that("get_rule_function_names_by_parsed_algo() works", {

  algos <- list(
    list( ## We might name this "subset1"
      instruments = list("testdata1"),
      rules = list("rule1", "rule2")
    ),
    list( ## We might name this "subset2"
      instruments = list("testdata2"),
      rules = list("rule1", "rule2")
    ),
    list( ## We might name this "subset3"
      instruments = list("testdata3", "testdata4"),
      rules = list("rule1", "rule2")
    )
  )

  my_test_expanded_algos <- expand_algos(algos)

  my_test_rule_function_names <- get_rule_function_names_by_parsed_algo(
    parsed_algos = my_test_expanded_algos
  )

  my_expected_rule_function_names <- list(
    "rule1",
    "rule2",
    "rule1",
    "rule2",
    "rule1",
    "rule2",
    "rule1",
    "rule2"
  )

  expect_equal(
    my_test_rule_function_names,
    my_expected_rule_function_names
  )
})

test_that("get_unique_rule_function_names_by_parsed_algo() works", {

  algos <- list(
    list( ## We might name this "subset1"
      instruments = list("testdata1"),
      rules = list("rule1", "rule2")
    ),
    list( ## We might name this "subset2"
      instruments = list("testdata2"),
      rules = list("rule1", "rule2")
    ),
    list( ## We might name this "subset3"
      instruments = list("testdata3", "testdata4"),
      rules = list("rule1", "rule2")
    )
  )

  my_test_expanded_algos <- expand_algos(algos)

  my_test_unique_rule_function_names <- get_unique_rule_function_names_by_parsed_algo(
    parsed_algos = my_test_expanded_algos
  )

  my_expected_unique_rule_function_names <- list(
    "rule1",
    "rule2"
  )

  expect_equal(
    my_test_unique_rule_function_names,
    my_expected_unique_rule_function_names
  )
})

test_that("get_rule_names_by_parsed_algo() works", {

  algos <- list(
    list( ## We might name this "subset1"
      instruments = list("testdata1"),
      rules = list("rule1", "rule2")
    ),
    list( ## We might name this "subset2"
      instruments = list("testdata2"),
      rules = list("rule1", "rule2")
    ),
    list( ## We might name this "subset3"
      instruments = list("testdata3", "testdata4"),
      rules = list("rule1", "rule2")
    )
  )

  my_test_expanded_algos <- expand_algos(algos)

  my_test_rule_function_names <- get_rule_names_by_parsed_algo(
    parsed_algos = my_test_expanded_algos
  )

  my_expected_rule_function_names <- list(
    "rule1",
    "rule2",
    "rule1",
    "rule2",
    "rule1",
    "rule2",
    "rule1",
    "rule2"
  )

  expect_equal(
    my_test_rule_function_names,
    my_expected_rule_function_names
  )
})

test_that("get_unique_rule_names_from_parsed_algos_list() works", {

  algos <- list(
    list( ## We might name this "subset1"
      instruments = list("testdata1"),
      rules = list("rule1", "rule2")
    ),
    list( ## We might name this "subset2"
      instruments = list("testdata2"),
      rules = list("rule1", "rule2")
    ),
    list( ## We might name this "subset3"
      instruments = list("testdata3", "testdata4"),
      rules = list("rule1", "rule2")
    )
  )

  my_test_expanded_algos <- expand_algos(algos)

  my_test_unique_rule_function_names <- get_unique_rule_names_from_parsed_algos_list(
    parsed_algos = my_test_expanded_algos
  )

  my_expected_unique_rule_function_names <- list(
    "rule1",
    "rule2"
  )

  expect_equal(
    my_test_unique_rule_function_names,
    my_expected_unique_rule_function_names
  )
})

test_that("get_num_rules_from_parsed_algos_list() works", {

  algos <- list(
    list( ## We might name this "subset1"
      instruments = list("testdata1"),
      rules = list("rule1", "rule2")
    ),
    list( ## We might name this "subset2"
      instruments = list("testdata2"),
      rules = list("rule1", "rule2")
    ),
    list( ## We might name this "subset3"
      instruments = list("testdata3", "testdata4"),
      rules = list("rule1", "rule2")
    )
  )

  my_test_expanded_algos <- expand_algos(algos)

  my_test_num_rule_function_names <- get_num_rules_from_parsed_algos_list(
    parsed_algos = my_test_expanded_algos
  )

  my_expected_num_rule_function_names <- 2

  expect_equal(
    my_test_num_rule_function_names,
    my_expected_num_rule_function_names
  )
})

test_that("get_signal_normalization_factors_by_algos() works", {

  algos <- list(
    list( ## We might name this "subset1"
      instruments = list("testdata1"),
      rules = list("rule1", "rule2")
    ),
    list( ## We might name this "subset2"
      instruments = list("testdata2"),
      rules = list("rule1", "rule2")
    ),
    list( ## We might name this "subset3"
      instruments = list("testdata3", "testdata4"),
      rules = list("rule1", "rule2")
    )
  )

  my_test_expanded_algos <- expand_algos(algos)

  my_test_signal_normalization_factors <- get_signal_normalization_factors_by_algos(
    signal_normalization_factors = list(rule1 = 1, rule2 = 1),
    parsed_algos = my_test_expanded_algos
  )

  my_expected_signal_normalization_factors <- list(
    1, 1, 1, 1, 1, 1, 1, 1
  )

  expect_equal(
    my_test_signal_normalization_factors,
    my_expected_signal_normalization_factors
  )
})

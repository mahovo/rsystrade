test_that("generate_signal() works", {

  ## Setup ----
  fff <- function(
    price,
    some_other_param_1,
    some_other_param_2
  ) {price + some_other_param_1 + some_other_param_2}


  algos <- make_test_algos(
    signal_generator_1 = list(
      fff1 = fff,
      some_other_param_1 = 2,
      some_other_param_2 = 3
    ),
    signal_generator_2 = list(
      list(
      fff2 = fff,
      some_other_param_1 = 4,
      some_other_param_2 = 5
      )
    )
  )

  parsed_algos <- parse_algos(algos)

  my_test_signal <- generate_signal(
    variable_param_vals = 10,
    signal_table = NA,
    position_table = NA,
    algo = parsed_algos[[1]], ## Single parsed algo from the algos list
    t = NA,
    config = NA
    #signal_table, ## signal table for the instrument
  )


## Test ----
  expect_equal(
    my_test_signal,
    15
  )
})

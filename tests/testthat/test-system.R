
## *** IMPORTANT ***
## This only tests that the system runs without failing.
## It doesn't test if the numbers are correct!!!
## In fact the numbers look wrong...

## *** IMPORTANT ***
## This tests a system with no position modifiers.

## *** NOTE ***
## Rule functions need to be loaded into the package env.
## This is done in helper-test_system.R
## The test system written to disk has been modified, so that the rule
## functions are in the package env (not the global env).
## This is done by commenting out the code at the bottom of helper-test_system.R
## This only needs to be done again, if the test system on disk is overwritten.

# test_that("system works", {
#
#   my_test_system <- make_test_system()
#   my_expected_system <- readRDS(test_path("fixtures", "my_test_system.rds"))
#
#   expect_equal(
#     my_test_system,
#     my_expected_system
#   )
# })


test_that("system works", {

  my_test_system <- make_test_system()
  my_expected_system <- readRDS(test_path("fixtures", "my_expected_system.rds"))

  expect_equal(
    my_test_system,
    my_expected_system
  )
})




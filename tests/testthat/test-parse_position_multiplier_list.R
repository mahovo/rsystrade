# test_that("parse_position_multipliers_list works (old)", {
#   f1 <- function(x1, y1) {x1^2 + y1}
#   f2 <- function(x2, y2) {x2^3 + y2}
#   inst_names <- c("inst1", "inst2")
#
#   ## Input list
#
#   ## parse_position_modifiers_list() is supposed to ignore "inst3", because we only
#   ## have two instruments inour inst_names list
#   position_modifiers <- list(
#     list(
#       instruments = list("inst1", "inst2", "inst3"),
#       modifier = list(
#         "f1",
#         f1,
#         y1 = 10
#       )
#     )
#   )
#
#   position_modifiers_error <- list(
#     list(
#       instruments = list("inst1", "inst2"),
#       modifier_function = "f1",
#       y1 = 10
#     ),
#     list(
#       instruments = list("inst1", "inst2"),
#       modifier_function = "f2",
#       y2 = 100
#     )
#   )
#
#   ## Output list format
#   # position_modifiers <- list(
#   #   inst1 = list(
#   #     modifier_name = "f1",
#   #     modifier_function = f1,
#   #     rnd = FALSE ## variable param
#   #   ),
#   #   inst2 = list(
#   #     modifier_name = "f2",
#   #     modifier_function = f2,
#   #     rnd = FALSE
#   #   )
#   # )
#
#
#   my_test_pos_mod_list <- parse_position_modifiers_list(
#     position_modifiers = position_modifiers,
#     inst_names = inst_names
#   )
#
#   my_expected_pos_mod_list <- list(
#     inst1 = list(
#       modifier_name = "f1",
#       modifier_function = function(x1, y1) {x1^2 + y1},
#       variable_params = c(x1 = "x1"),
#       fixed_params = list(y1 = 10)
#     ),
#     inst2 = list(
#       modifier_name = "f1",
#       modifier_function = function(x1, y1) {x1^2 + y1},
#       variable_params = c(x1 = "x1"),
#       fixed_params = list(y1 = 10)
#     )
#   )
#
#   expect_equal(
#     my_test_pos_mod_list,
#     my_expected_pos_mod_list
#   )
# })



test_that("parse_position_multipliers_list works", {

  x1 <- function(x) {x}
  x2 <- function(x) {x}

  ## parse_position_modifiers_list() is supposed to ignore "inst3", because we only
  ## have two instruments inour inst_names list
  inst_names <- c("inst1", "inst2", "inst3", "inst4")

  pos_muls <- list(
    list(
      instruments = list("inst1", "inst2"),
      multipliers = list(
        list("mult1", x1),
        list("mult2", x2)
      )
    ),
    list(
      instruments = list("inst3", "inst4"),
      multipliers = list(
        list("mult1", x1),
        list("mult2", x2)
      )
    ),
    list(
      instruments = list("inst1", "inst3", "inst5"),
      multipliers = list(
        list("mult3", x1),
        list("mult4", x2)
      )
    )
  )

  my_test_parsed_pos_muls <- parse_position_multipliers_list(
    pos_muls,
    inst_names
  )

  ## Load test list into current env
  my_test_parsed_pos_muls <- list2env(
    my_test_parsed_pos_muls,
    rlang::current_env()
  )

  ## Write expected list to disk
  # saveRDS(
  #   my_test_parsed_pos_muls,
  #   file = test_path("fixtures/", "my_expected_parsed_pos_muls.rds"),
  #   compress = FALSE
  # )

  my_expected_parsed_pos_muls <- readRDS(test_path("fixtures", "my_expected_parsed_pos_muls.rds"))

  ## Load expected list into current env
  my_expected_parsed_pos_muls <- list2env(
    my_expected_parsed_pos_muls,
    rlang::current_env()
  )

  expect_equal(
    my_test_parsed_pos_muls,
    my_expected_parsed_pos_muls
  )
})

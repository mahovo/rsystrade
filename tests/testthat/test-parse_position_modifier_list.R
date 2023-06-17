test_that("parse_position_modifiers_list works", {
  f1 <- function(x1, y1) {x1^2 + y1}
  f2 <- function(x2, y2) {x2^3 + y2}
  inst_names <- c("inst1", "inst2")

  ## Input list

  ## parse_position_modifiers_list() is supposed to ignore "inst3", because we only
  ## have two instruments inour inst_names list
  position_modifiers <- list(
    list(
      instruments = list("inst1", "inst2", "inst3"),
      modifier = list(
        "f1",
        f1,
        y1 = 10
      )
    )
  )

  position_modifiers_error <- list(
    list(
      instruments = list("inst1", "inst2"),
      modifier_function = "f1",
      y1 = 10
    ),
    list(
      instruments = list("inst1", "inst2"),
      modifier_function = "f2",
      y2 = 100
    )
  )

  ## Output list format
  # position_modifiers <- list(
  #   inst1 = list(
  #     modifier_name = "f1",
  #     modifier_function = f1,
  #     rnd = FALSE ## variable param
  #   ),
  #   inst2 = list(
  #     modifier_name = "f2",
  #     modifier_function = f2,
  #     rnd = FALSE
  #   )
  # )


  my_test_pos_mod_list <- parse_position_modifiers_list(
    position_modifiers = position_modifiers,
    inst_names = inst_names
  )

  my_expected_pos_mod_list <- list(
    inst1 = list(
      modifier_name = "f1",
      modifier_function = function(x1, y1) {x1^2 + y1},
      variable_params = c(x1 = "x1"),
      fixed_params = list(y1 = 10)
    ),
    inst2 = list(
      modifier_name = "f1",
      modifier_function = function(x1, y1) {x1^2 + y1},
      variable_params = c(x1 = "x1"),
      fixed_params = list(y1 = 10)
    )
  )

  expect_equal(
    my_test_pos_mod_list,
    my_expected_pos_mod_list
  )
})

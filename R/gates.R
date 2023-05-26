## A gate function is a function to be multiplied by a (modified) position size.
## Gates can not be applied directly in a system, but can be implemented in a
## position modifier function.
## E.g. if we want to implement a stop loss position modifier, and we want to
## control when a new position may be entered after the stop loss was invoked,
## we can do this with a gate. Then the position modifier function should first
## calculated the modified position based on the stop loss rule, and then
## determine if the conditions for opening a new position are met. If the
## conditions are met, the gate should return 1, otherwise 0. If a stop loss
## was invoked on a long position at t - 1, we may get a new long signal at t.
## If the gate determines, that a new long position at t is not allowed, the
## position will be multiplied by 0, returning a modified position of 0.
##
## An alternative way of doing this could be to implement a gate as part of a
## signal generating function. How ever, if multiple rules are applied to the
## same instrument, there would be redundancy due to the gate calculations being
## performed for each rule.


g_reentry <- function(
    position
  ) {

}

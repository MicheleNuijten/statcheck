context("Messages to warn for one-tailed tests or different alpha levels")

# test if the following cases lead to messages ------------------------------

# # one-tailed tests
# test_that("statcheck prints message to warn for one-tailed tests", {
#   # this p-value would have been correct if it was a one-tailed test:
#   txt1 <- "t(28) = 2.20, p = .018" 
#   
#   expect_message(statcheck(txt1), "Check for one tailed tests.")
# })

# alpha levels
test_that("statcheck prints message to warn for different alpha levels", {
  # this would have been a decision error if alpha == .01
  txt1 <- "t(28) = 2.20, p < .01"
  
  # this would have been a decision error if alpha == .10
  txt2 <- "t(28) = 1.20, p < .10"
  
  expect_message(statcheck(txt1), "Check the significance level.")
  expect_message(statcheck(txt2), "Check the significance level.")
})

# # one-tailed tests AND alpha levels
# test_that("statcheck prints message to warn for one-tailed tests and different alpha levels", {
#   # this would have been a decision error if alpha == .01, and
#   # this p-value would have been correct if it was a one-tailed test
#   txt1 <- "t(28) = 2.50, p < .01"
#   
#   expect_message(statcheck(txt1), "Check the significance level.")
#   expect_message(statcheck(txt1), "Check for one tailed tests.")
# })


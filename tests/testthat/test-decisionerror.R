context("check if gross inconsistencies/decision errors are correctly classified")

# test if the following cases are correctly identified as decision errors -----

# check classification of regular decision errors in all types of tests
test_that("simple decision errors are classified as such", {
  txt1  <- "t(28) = 2.20, p = .06"
  txt2  <- "t(28) = 1.20, p = .03"
  
  txt3  <- "F(2, 28) = 22.20, p = .06"
  txt4  <- "F(2, 28) = 2.20, p = .03"
  
  txt5  <- "r(28) = .22, p = .03"
  txt6  <- "r(28) = .52, p = .06"
  
  txt7  <- "chi2(28) = 2.20, p = .03"
  txt8  <- "chi2(28) = 52.20, p = .06"
  
  txt9  <- " z = 1.20, p = .03"
  txt10 <- " z = 2.20, p = .06"
  
  txt11 <- "Q(28) = 2.20, p = .03"
  txt12 <- "Q(28) = 52.20, p = .06"
  
  expect_true(statcheck(txt1,  messages = FALSE)$DecisionError)
  expect_true(statcheck(txt2,  messages = FALSE)$DecisionError)
  expect_true(statcheck(txt3,  messages = FALSE)$DecisionError)
  expect_true(statcheck(txt4,  messages = FALSE)$DecisionError)
  expect_true(statcheck(txt5,  messages = FALSE)$DecisionError)
  expect_true(statcheck(txt6,  messages = FALSE)$DecisionError)
  expect_true(statcheck(txt7,  messages = FALSE)$DecisionError)
  expect_true(statcheck(txt8,  messages = FALSE)$DecisionError)
  expect_true(statcheck(txt9,  messages = FALSE)$DecisionError)
  expect_true(statcheck(txt10, messages = FALSE)$DecisionError)
  expect_true(statcheck(txt11, messages = FALSE)$DecisionError)
  expect_true(statcheck(txt12, messages = FALSE)$DecisionError)
})

# test if different arguments concerning decision errors work -----------------

# alpha: detect decision errors for different alpha levels
test_that("decision errors are correctly classified when alpha levels are changed", {
  # all cases below are errors
  txt1 <- "t(28) = 2.20, p = .06"
  txt2 <- "t(28) = 1.20, p = .03"
  
  txt3 <- "t(28) = 2.20, p = .11"
  txt4 <- "t(28) = 1.20, p = .09"
  
  txt5 <- "t(28) = 5.20, p = .02"
  txt6 <- "t(28) = 1.20, p = .005"
  
  expect_true(statcheck(txt1, messages = FALSE, alpha = .05)$DecisionError)
  expect_true(statcheck(txt2, messages = FALSE, alpha = .05)$DecisionError)
  
  expect_true(statcheck(txt3, messages = FALSE, alpha = .10)$DecisionError)
  expect_true(statcheck(txt4, messages = FALSE, alpha = .10)$DecisionError)
  
  expect_true(statcheck(txt5, messages = FALSE, alpha = .01)$DecisionError)
  expect_true(statcheck(txt6, messages = FALSE, alpha = .01)$DecisionError)
})

# pEqualAlphaSig: should p = .05 be considered significant or not
test_that("decision errors are correctly classified when argument p = alpha is changed", {
  
  txt1 <- "t(28) = 2.20, p = .05"
  txt2 <- "t(28) = 2.20, p = .10"
  
  expect_false(statcheck(txt1, messages = FALSE)$DecisionError)
  expect_true(statcheck(txt1, messages = FALSE, pEqualAlphaSig = FALSE)$DecisionError)
  
  # check pEqualAlphaSig for different alpha
  expect_false(statcheck(txt2, messages = FALSE, alpha = .10)$DecisionError)
  expect_true(statcheck(txt2, messages = FALSE, pEqualAlphaSig = FALSE, 
                        alpha = .10)$DecisionError)

})
















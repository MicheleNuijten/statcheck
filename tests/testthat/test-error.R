context("check if inconsistencies/errors are correctly classified")

# test if the following cases are correctly identified as errors --------------

# check classification of regular errors in all types of tests
test_that("simple errors are classified as such", {
  txt1 <- "t(28) = 2.20, p = .03"
  txt2 <- "F(2, 28) = 2.20, p = .15"
  txt3 <- "r(28) = .22, p = .26"
  txt4 <- "chi2(28) = 22.20, p = .79"
  txt5 <- " z = 2.20, p = .04"
  txt6 <- "Q(28) = 22.20, p = .79"

  expect_true(statcheck(txt1, messages = FALSE)$Error)
  expect_true(statcheck(txt2, messages = FALSE)$Error)
  expect_true(statcheck(txt3, messages = FALSE)$Error)
  expect_true(statcheck(txt4, messages = FALSE)$Error)
  expect_true(statcheck(txt5, messages = FALSE)$Error)
  expect_true(statcheck(txt6, messages = FALSE)$Error)
})

# also classify decision errors as errors
test_that("decision errors are also classified as errors",{
  txt1 <- "t(28) = 1.20, p = .03"
  txt2 <- "t(28) = 2.20, p = .30"

  expect_true(statcheck(txt1, messages = FALSE)$Error)
  expect_true(statcheck(txt2, messages = FALSE)$Error)
})

# test if the following cases are correctly identified as correct -------------

# correct rounding
test_that("correctly rounded p-values are not considered errors", {
  txt1 <- "t(28) = 2, p = .02"
  txt2 <- "t(28) = 2, p = .14"
  txt3 <- "t(28) = 2.2, p = .03"
  txt4 <- "t(28) = 2.2, p = .04"
  txt5 <- "t(28) = 2.20, p = .036"
  txt6 <- "t(28) = 2.20, p = .037"

  expect_false(statcheck(txt1, messages = FALSE)$Error)
  expect_false(statcheck(txt2, messages = FALSE)$Error)
  expect_false(statcheck(txt3, messages = FALSE)$Error)
  expect_false(statcheck(txt4, messages = FALSE)$Error)
  expect_false(statcheck(txt5, messages = FALSE)$Error)
  expect_false(statcheck(txt6, messages = FALSE)$Error)
})

# test if different arguments concerning errors work --------------------------

# OneTailedTests: assume all tests are one-tailed
test_that("OneTailedTests considers everything as one-tailed", {
  txt1 <- "t(28) = 2.20, p = .02"
  txt2 <- "t(28) = 2.20, p = .04"
  txt3 <- "this test is one-tailed: t(28) = 2.20, p = .02, but this one is not: t(28) = 2.20, p = .04"

  expect_false(statcheck(txt1, messages = FALSE,  OneTailedTests = TRUE)$Error)
  expect_true(statcheck(txt2, messages = FALSE, OneTailedTests = TRUE)$Error)
  expect_equal(statcheck(txt3, messages = FALSE, OneTailedTests = TRUE)$Error, c(FALSE, TRUE))
})

# OneTailedTxt: automated detection of one-tailed test in text
test_that("automated one-tailed test detection works", {
  txt1 <- "t(28) = 2.20, p = .018"
  txt2 <- "t(28) = 2.20, p = .01, one-tailed"
  txt3 <- "t(28) = 2.20, p = .018, one-tailed"
  txt4 <- "t(28) = 2.20, p = .018, one-sided"
  txt5 <- "t(28) = 2.20, p = .018, directional"

  # don't correct for one-tailed testing here
  expect_true(statcheck(txt1, messages = FALSE)$Error)
  expect_true(statcheck(txt1, messages = FALSE, OneTailedTxt = TRUE)$Error)
  expect_true(statcheck(txt2, messages = FALSE, OneTailedTxt = TRUE)$Error)
  expect_true(statcheck(txt3, messages = FALSE)$Error)

  # correct for one-tailed testing here
  expect_false(statcheck(txt3, messages = FALSE, OneTailedTxt = TRUE)$Error)
  expect_false(statcheck(txt4, messages = FALSE, OneTailedTxt = TRUE)$Error)
  expect_false(statcheck(txt5, messages = FALSE, OneTailedTxt = TRUE)$Error)
})

# pZeroError: check if p = .000 is counted as an inconsistency or not
test_that("you can adapt whether p = .000 is counted as inconsistent or not", {
  txt1 <- "t(28) = 22.20, p = .000"
  txt2 <- "t(28) = 22.20, p < .000" # this is always an Error

  expect_true(statcheck(txt1, messages = FALSE)$Error)
  expect_false(statcheck(txt1, messages = FALSE, pZeroError = FALSE)$Error)

  expect_true(statcheck(txt2, messages = FALSE)$Error)
  expect_true(statcheck(txt2, messages = FALSE, pZeroError = FALSE)$Error)
})


context("Options for statcheck()")

test_that("Check a vector of strings", {
  txt1 <- "t(100) = 1, p < 0.001"
  txt2 <- "F(2,45) = 2.81, p = .45"
  txt3 <- "No effect found."

  result <- statcheck(c(txt1, txt2, txt3))

  expect_equal(nrow(result), 2)
  expect_equal(as.character(result$Statistic), c("t", "F"))
})


test_that("Select specific statistics", {
  txt <- "Result 1 is t(100) = 1, p < 0.001, and
  result 2 is F(2,45) = 2.81, p = .45"

  result <- statcheck(txt, stat = c("t", "cor"))

  expect_equal(nrow(result), 1)
  expect_equal(as.character(result$Statistic), "t")
})


test_that("One-tailed tests divide p-values by 2", {
  txt <- "Result 1 is t(100) = 1, p = 0.3, and
  result 2 is F(2,45) = 2.81, p = .45"

  one_tail <- statcheck(txt, OneTailedTests = TRUE)
  two_tail <- statcheck(txt, OneTailedTests = FALSE)

  expect_equal(two_tail$Computed / 2, one_tail$Computed, tolerance = .00001)
})


test_that("Alpha adjusts signifiance level", {
  # Reported: Sig. at .05 but not sig. at .01.
  # Computed: Sig. at .05 and .01.
  txt <- "F(2,45) = 5.81, p = .03"

  one_in_hundred <- statcheck(txt, alpha = .01)
  one_in_twenty <- statcheck(txt, alpha = .05)
  one_in_ten <- statcheck(txt, alpha = .10)

  expect_true(one_in_hundred$Error)
  expect_true(one_in_twenty$Error)
  expect_true(one_in_ten$Error)

  # An error is a decision error based on alpha
  expect_false(one_in_twenty$DecisionError)
  expect_false(one_in_ten$DecisionError)

  # computed < alpha < reported
  expect_true(one_in_hundred$DecisionError)

  # reported < alpha < computed
  txt2 <- "F(2,45) = 4, p = .01"
  result2 <- statcheck(txt2, alpha = .02)
  expect_true(result2$DecisionError)
})


test_that("Handling p = alpha tests", {
  # Computed < alpha = reported
  txt <- "F(2,45) = 4.10, p = .05"
  result1 <- statcheck(txt, pEqualAlphaSig = TRUE, alpha = .05)
  result2 <- statcheck(txt, pEqualAlphaSig = FALSE, alpha = .05)

  expect_true(result1$Error)
  expect_equal(result1$Error, result2$Error)

  expect_false(result1$DecisionError)
  expect_false(result1$DecisionError == result2$DecisionError)
})


test_that("P values cannot be zero", {
  txt <- "F(2,45) = 4004.10, p = .000"
  result1 <- statcheck(txt, pZeroError = TRUE)
  result2 <- statcheck(txt, pZeroError = FALSE)

  expect_true(result1$Error)
  expect_false(result2$Error)
  expect_false(result2$DecisionError)
  expect_false(result2$DecisionError)
})


test_that("Look for one-tailed tests", {
  txt_no_clue <- "... t(48) = 1.82, p < .05"
  result1 <- statcheck(txt_no_clue, OneTailedTxt = FALSE)
  result2 <- statcheck(txt_no_clue, OneTailedTxt = TRUE)

  expect_false(result1$OneTailedInTxt)
  expect_false(result2$OneTailedInTxt)

  expect_true(result1$Error)
  expect_true(result1$DecisionError)

  expect_true(result2$Error)
  expect_true(result2$DecisionError)

  # Text with a clue can change decision error, if enabled
  txt_clue <- "... t(48) = 1.82, p < .05, all tests were one-tailed."
  result1 <- statcheck(txt_clue, OneTailedTxt = FALSE)
  result2 <- statcheck(txt_clue, OneTailedTxt = TRUE)

  expect_true(result1$OneTailedInTxt)
  expect_true(result2$OneTailedInTxt)

  expect_true(result1$Error)
  expect_true(result1$DecisionError)

  expect_false(result2$Error)
  expect_false(result2$DecisionError)
})



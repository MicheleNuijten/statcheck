context("Statistical notation parsing regressions")

test_that("scientific p-values retain the precision of their exponent", {
  scientific <- c("p = 3.4e-3", "p = 3.40E-3", "p = 0.0034e1", "p = 0.0034e0")
  decimal <- c("p = .0034", "p = .00340", "p = .034", "p = .0034")

  result <- statcheck(scientific, AllPValues = TRUE, messages = FALSE)

  expect_equal(result, statcheck(decimal, AllPValues = TRUE, messages = FALSE))
  expect_equal(result[[VAR_P_DEC]], c(4, 5, 3, 4))
})

test_that("scientific p-values receive the same inconsistency flags as decimals", {
  scientific <- statcheck(" z = 2.93, p = 3.4e-3", messages = FALSE)
  decimal <- statcheck(" z = 2.93, p = .0034", messages = FALSE)

  expect_false(scientific[[VAR_ERROR]])
  expect_equal(scientific[[VAR_REPORTED_P]], decimal[[VAR_REPORTED_P]])
  expect_equal(scientific[[VAR_COMPUTED_P]], decimal[[VAR_COMPUTED_P]])
  expect_equal(scientific[[VAR_ERROR]], decimal[[VAR_ERROR]])
  expect_equal(scientific[[VAR_DEC_ERROR]], decimal[[VAR_DEC_ERROR]])
})

test_that("Z-tests are recognized both at the start of text and after whitespace", {
  texts <- c("z = 2.93, p = .0034", "Z = 2.93, p = .0034",
             " z = 2.93, p = .0034", " Z = 2.93, p = .0034")

  result <- statcheck(texts, messages = FALSE)

  expect_equal(nrow(result), 4)
  expect_equal(result[[VAR_TYPE]], rep("Z", 4))
  expect_equal(result[[VAR_COMPUTED_P]], rep(2 * pnorm(-2.93), 4))
  expect_equal(result[[VAR_ERROR]], rep(FALSE, 4))
  expect_true(all(is.na(result[[VAR_DF1]])))
  expect_true(all(is.na(result[[VAR_DF2]])))
})

test_that("Z-tests are not extracted from lowercase word endings", {
  expect_message(
    statcheck("quiz = 2.93, p = .0034", messages = FALSE),
    "did not find any results"
  )
})

test_that("uppercase Q suffixes retain their within or between classification", {
  texts <- paste0(c("QW", "QWithin", "Q-Within", "QB", "QBetween", "Q-Between"),
                  "(2) = 2.20, p = .33")

  result <- statcheck(texts, messages = FALSE)

  expect_equal(nrow(result), 6)
  expect_equal(result[[VAR_TYPE]], c(rep("Qw", 3), rep("Qb", 3)))
  expect_equal(result[[VAR_DF1]], rep(2, 6))
  expect_equal(result[[VAR_COMPUTED_P]], rep(pchisq(2.2, 2, lower.tail = FALSE), 6))
})

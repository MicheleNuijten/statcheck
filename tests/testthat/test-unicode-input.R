context("Extract statistics with mathematical Unicode characters")

test_that("mathematical italic test symbols and p-values match ASCII input", {
  unicode <- c(
    "\U0001D461(28) = 2.20, \U0001D45D = .04",
    "\U0001D439(2, 28) = 2.20, \U0001D45D = .13"
  )
  ascii <- c("t(28) = 2.20, p = .04", "F(2, 28) = 2.20, p = .13")

  result <- statcheck(unicode, messages = FALSE)

  expect_equal(result, statcheck(ascii, messages = FALSE))
  expect_equal(result[[VAR_TYPE]], c("t", "F"))
})

test_that("mathematical bold symbols match ASCII input", {
  unicode <- c(
    "\U0001D42D(28) = 2.20, \U0001D429 < .05",
    "\U0001D405(2, 28) = 2.20, \U0001D429 > .05"
  )
  ascii <- c("t(28) = 2.20, p < .05", "F(2, 28) = 2.20, p > .05")

  result <- statcheck(unicode, messages = FALSE)

  expect_equal(result, statcheck(ascii, messages = FALSE))
  expect_equal(result[[VAR_P_COMPARISON]], c("<", ">"))
})

test_that("Unicode minus signs preserve negative test statistics", {
  unicode <- c(
    "t(28) = \u22122.20, p = .04",
    "\U0001D461(28) = \u22122.20, \U0001D45D = .04"
  )
  ascii <- rep("t(28) = -2.20, p = .04", 2)

  result <- statcheck(unicode, messages = FALSE)

  expect_equal(result, statcheck(ascii, messages = FALSE))
  expect_equal(result[[VAR_TEST_VALUE]], c(-2.2, -2.2))
})

test_that("normalization preserves source names and missing texts", {
  unicode <- c(
    italic_t = "\U0001D461(28) = 2.20, \U0001D45D = .04",
    missing = NA_character_,
    bold_F = "\U0001D405(2, 28) = 2.20, \U0001D429 = .13"
  )
  ascii <- c(
    italic_t = "t(28) = 2.20, p = .04",
    missing = NA_character_,
    bold_F = "F(2, 28) = 2.20, p = .13"
  )

  result <- statcheck(unicode, messages = FALSE)

  expect_equal(result, statcheck(ascii, messages = FALSE))
  expect_equal(result[[VAR_SOURCE]], c("italic_t", "bold_F"))
})

test_that("normalization supports standalone p-values and Q subtypes", {
  unicode <- c(
    "\U0001D45D = .05; \U0001D429 < .01",
    "\U0001D444-within(2) = 2.20, \U0001D45D > .05",
    "\U0001D444-between(2) = 2.20, \U0001D45D = .33"
  )
  ascii <- c(
    "p = .05; p < .01",
    "Q-within(2) = 2.20, p > .05",
    "Q-between(2) = 2.20, p = .33"
  )

  expect_equal(
    statcheck(unicode, AllPValues = TRUE, messages = FALSE),
    statcheck(ascii, AllPValues = TRUE, messages = FALSE)
  )
  result <- statcheck(unicode, messages = FALSE)
  expect_equal(result, statcheck(ascii, messages = FALSE))
  expect_equal(result[[VAR_TYPE]], c("Qw", "Qb"))
})

test_that("footnote and subscript digits do not extend reported p-values", {
  unicode <- c(
    "\U0001D461(28) = 2.20, \U0001D45D = .03\u00B9",
    "\U0001D461(28) = 2.20, \U0001D45D = .03\u2081"
  )

  result <- statcheck(unicode, messages = FALSE)
  pvalues <- statcheck(unicode, AllPValues = TRUE, messages = FALSE)

  expect_equal(result[[VAR_REPORTED_P]], c(.03, .03))
  expect_equal(pvalues[[VAR_REPORTED_P]], c(.03, .03))
})

test_that("script letters and mathematical digits match ASCII input", {
  unicode <- c(
    "\u2131(2, 28) = 2.20, \U0001D45D = .13",
    "\U0001D439(\U0001D7D9, 28) = 2.20, \U0001D45D = .15"
  )
  ascii <- c("F(2, 28) = 2.20, p = .13", "F(1, 28) = 2.20, p = .15")

  expect_equal(
    statcheck(unicode, messages = FALSE),
    statcheck(ascii, messages = FALSE)
  )
})

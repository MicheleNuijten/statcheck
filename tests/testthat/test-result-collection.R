context("Collect and summarize results across texts")

test_that("batch extraction preserves source order and APA factors", {
  texts <- c(
    beta = "t(28) = 2.20, p = .04; F(2, 28) = 2.20, p = .13",
    empty = "No statistics",
    alpha = "r(28) = .22, p = .26",
    beta = " z = 1.20, p = .23; p < .10",
    missing = NA_character_
  )

  result <- statcheck(texts, messages = FALSE)

  expect_equal(result[[VAR_SOURCE]], c("beta", "beta", "alpha", "beta"))
  expect_equal(result[[VAR_TYPE]], c("t", "F", "r", "Z"))
  expect_equal(result[[VAR_REPORTED_P]], c(.04, .13, .26, .23))
  expect_equal(result[[VAR_APAFACTOR]], c(.75, .75, 1, .75))
  expect_equal(rownames(result), as.character(1:4))

  selected <- statcheck(texts, stat = c("t", "Z"), messages = FALSE)
  expect_equal(selected[[VAR_SOURCE]], c("beta", "beta"))
  expect_equal(selected[[VAR_TYPE]], c("t", "Z"))
  expect_equal(selected[[VAR_APAFACTOR]], c(.5, .5))
  expect_equal(rownames(selected), c("1", "2"))
})

test_that("p-value-only extraction retains all sources and comparison symbols", {
  texts <- c(
    beta = "t(28) = 2.20, p = .04; F(2, 28) = 2.20, p = .13",
    empty = "No statistics",
    alpha = "p > .26",
    beta = " z = 1.20, p = .23; p < .10",
    missing = NA_character_
  )
  expected <- data.frame(
    source = c("beta", "beta", "alpha", "beta", "beta"),
    p_comp = c("=", "=", ">", "=", "<"),
    reported_p = c(.04, .13, .26, .23, .10),
    p_decimals = rep(2, 5),
    stringsAsFactors = FALSE
  )

  result <- statcheck(texts, AllPValues = TRUE, messages = FALSE)

  expect_equal(result, expected)
  expect_equal(
    statcheck(texts, stat = "F", AllPValues = TRUE, messages = FALSE),
    expected
  )
})

test_that("empty and missing sources retain their existing return behavior", {
  empty <- c(empty = "No statistics", missing = NA_character_)

  expect_null(expect_message(
    statcheck(empty, messages = FALSE), "did not find any results"
  ))
  expect_null(expect_message(
    statcheck(empty, AllPValues = TRUE, messages = FALSE), "did not find any p-values"
  ))

  result <- statcheck(c(empty, p_only = "p = .03"),
                      AllPValues = TRUE, messages = FALSE)
  expect_equal(result[[VAR_SOURCE]], "p_only")
  expect_equal(result[[VAR_REPORTED_P]], .03)
})

test_that("summaries retain source ordering and ignore missing flags", {
  object <- data.frame(
    source = c("beta", "alpha", "beta", "alpha", "gamma", NA_character_),
    error = c(TRUE, FALSE, NA, TRUE, NA, TRUE),
    decision_error = c(FALSE, NA, TRUE, TRUE, NA, TRUE),
    stringsAsFactors = FALSE
  )
  class(object) <- c("statcheck", "data.frame")

  result <- summary(object)

  expect_equal(result[[VAR_SOURCE]], c("alpha", "beta", "gamma", NA, "Total"))
  expect_equal(result[[VAR_NR_PVALUES]], c(2, 2, 1, 1, 6))
  expect_equal(result[[VAR_NR_ERRORS]], c(1, 1, 0, 1, 3))
  expect_equal(result[[VAR_NR_DEC_ERRORS]], c(1, 1, 0, 1, 3))
  expect_s3_class(result, "statcheck")

  object[[VAR_SOURCE]] <- factor(object[[VAR_SOURCE]],
                                 levels = c("gamma", "beta", "unused", "alpha"))
  factor_result <- summary(object)
  expect_equal(factor_result[[VAR_SOURCE]], c("gamma", "beta", "alpha", NA, "Total"))
  expect_equal(factor_result[[VAR_NR_PVALUES]], c(1, 2, 2, 1, 6))
  expect_equal(factor_result[[VAR_NR_ERRORS]], c(0, 1, 1, 1, 3))
  expect_equal(factor_result[[VAR_NR_DEC_ERRORS]], c(0, 1, 1, 1, 3))
})

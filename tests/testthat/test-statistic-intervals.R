context("Statistic intervals respect zero, signs, and the correlation domain")

test_that("rounded perfect correlations do not abort a scan", {
  texts <- c(
    "r(28) = 1.00, p < .001",
    "r(28) = -1.00, p < .001",
    "r(28) = 1.00, p = .10",
    "r(28) = -1.00, p = .10"
  )

  for(one_tailed in c(FALSE, TRUE)){
    result <- statcheck(texts, OneTailedTests = one_tailed, messages = FALSE)
    expect_equal(nrow(result), 4)
    expect_equal(result[[VAR_COMPUTED_P]], rep(0, 4))
    expect_equal(result[[VAR_ERROR]], c(FALSE, FALSE, TRUE, TRUE))
    expect_equal(result[[VAR_DEC_ERROR]], c(FALSE, FALSE, TRUE, TRUE))
  }
})

test_that("rounding intervals crossing zero include their maximum p-value", {
  tests <- c("t(28)", " z", "r(28)")

  for(one_tailed in c(FALSE, TRUE)){
    reported_p <- if(one_tailed) ".5000" else "1.0000"
    texts <- paste0(tests, " = 0.0, p = ", reported_p)
    result <- statcheck(texts, OneTailedTests = one_tailed, messages = FALSE)

    expect_equal(result[[VAR_COMPUTED_P]], rep(as.numeric(reported_p), 3))
    expect_equal(result[[VAR_ERROR]], rep(FALSE, 3))
    expect_equal(result[[VAR_DEC_ERROR]], rep(FALSE, 3))
  }

  # Including zero must not make the whole p-value range acceptable.
  result <- statcheck(paste0(tests, " = 0.0, p = .10"), messages = FALSE)
  expect_equal(result[[VAR_ERROR]], rep(TRUE, 3))
  expect_equal(result[[VAR_DEC_ERROR]], rep(FALSE, 3))

  result <- statcheck(paste0(tests, " = 0.0, p = .5000, one-tailed"),
                      OneTailedTxt = TRUE, messages = FALSE)
  expect_equal(result[[VAR_COMPUTED_P]], rep(.5, 3))
  expect_equal(result[[VAR_ERROR]], rep(FALSE, 3))
})

test_that("negative upper bounds imply larger statistic magnitudes", {
  tests <- c("t(28) < -2.20", " z < -2.20", "r(28) < -.50")

  for(one_tailed in c(FALSE, TRUE)){
    for(equal_alpha_sig in c(FALSE, TRUE)){
      inconsistent <- statcheck(paste0(tests, ", p > .50"),
                                OneTailedTests = one_tailed,
                                pEqualAlphaSig = equal_alpha_sig,
                                messages = FALSE)
      expect_equal(inconsistent[[VAR_ERROR]], rep(TRUE, 3))
      expect_equal(inconsistent[[VAR_DEC_ERROR]], rep(TRUE, 3))
    }

    consistent <- statcheck(paste0(tests, ", p < .01"),
                            OneTailedTests = one_tailed, messages = FALSE)
    expect_equal(consistent[[VAR_ERROR]], rep(FALSE, 3))
    expect_equal(consistent[[VAR_DEC_ERROR]], rep(FALSE, 3))
  }
})

test_that("negative lower bounds permit both small and large magnitudes", {
  tests <- c("t(28) > -2.20", " z > -2.20", "r(28) > -.50")

  for(reported_p in c(".90", ".001")){
    result <- statcheck(paste0(tests, ", p = ", reported_p), messages = FALSE)
    expect_equal(result[[VAR_ERROR]], rep(FALSE, 3))
    expect_equal(result[[VAR_DEC_ERROR]], rep(FALSE, 3))
  }

  for(reported_p in c(".50", ".001")){
    result <- statcheck(paste0(tests, ", p = ", reported_p),
                        OneTailedTests = TRUE, messages = FALSE)
    expect_equal(result[[VAR_ERROR]], rep(FALSE, 3))
    expect_equal(result[[VAR_DEC_ERROR]], rep(FALSE, 3))
  }

  # The smaller-tail p-value cannot exceed .5, but this bound still cannot
  # determine whether the underlying result is significant.
  result <- statcheck(paste0(tests, ", p = .90"),
                      OneTailedTests = TRUE, messages = FALSE)
  expect_equal(result[[VAR_ERROR]], rep(TRUE, 3))
  expect_equal(result[[VAR_DEC_ERROR]], rep(FALSE, 3))

  result <- statcheck(paste0(tests, ", p < .90"),
                      OneTailedTests = TRUE, messages = FALSE)
  expect_equal(result[[VAR_ERROR]], rep(FALSE, 3))
})

test_that("negative bounds near zero do not imply nonsignificance", {
  texts <- c("t(28) > -0.01, p = .001", " z > -0.01, p = .90",
             "r(28) > -.01, p = .001")
  result <- statcheck(texts, messages = FALSE)
  expect_equal(result[[VAR_ERROR]], rep(FALSE, 3))
  expect_equal(result[[VAR_DEC_ERROR]], rep(FALSE, 3))

  # Impossible p-value inequalities must remain errors even when every
  # statistic magnitude is possible and zero reporting is otherwise allowed.
  result <- statcheck("t(28) > -2.20, p < .000", pZeroError = FALSE,
                      messages = FALSE)
  expect_true(result[[VAR_ERROR]])
  expect_false(result[[VAR_DEC_ERROR]])
})

test_that("signed inequality bounds at zero permit any magnitude", {
  tests <- c("t(28)", " z", "r(28)")

  for(comparison in c("<", ">")){
    for(one_tailed in c(FALSE, TRUE)){
      for(reported_p in c(".001", if(one_tailed) ".50" else ".90")){
        result <- statcheck(paste0(tests, " ", comparison, " 0.0, p = ",
                                   reported_p),
                            OneTailedTests = one_tailed, messages = FALSE)
        expect_equal(result[[VAR_ERROR]], rep(FALSE, 3))
        expect_equal(result[[VAR_DEC_ERROR]], rep(FALSE, 3))
      }
    }
  }
})

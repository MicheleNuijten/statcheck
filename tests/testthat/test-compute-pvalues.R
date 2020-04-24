context("Correctly recalculate p-values")

test_that("p-values for t-tests are correctly calculated", {
  txt <- "t(28) = 2.20, p = .03"
  computed <- pt(-1 * abs(2.20), 28) * 2
  
  result <- statcheck(txt, messages = FALSE)
  
  expect_equal(result[[VAR_COMPUTED_P]], computed)
})


test_that("p-values for F-tests are correctly calculated", {
  txt <- "F(2, 28) = 2.20, p = .15"  
  computed <- pf(2.20, 2, 28, lower.tail = FALSE)
  
  result <- statcheck(txt, messages = FALSE)
  
  expect_equal(result[[VAR_COMPUTED_P]], computed)
})

test_that("p-values for correlations are correctly calculated", {
  txt <- "r(28) = .22, p = .26"
  
  pComputed <-
    pmin(pt(-1 * abs(r2t(.22, 28)), 28) * 2, 1)
  pComputed[is.nan(pComputed)] <- NA
  computed <-  pComputed
  
  result <- statcheck(txt, messages = FALSE)
  
  expect_equal(result[[VAR_COMPUTED_P]], computed)
})


test_that("p-values for z-tests are correctly calculated", {
  txt <- " z = 2.20, p = .04"
  computed <- pnorm(abs(2.20), lower.tail = FALSE) * 2
  
  result <- statcheck(txt, messages = FALSE)
  
  expect_equal(result[[VAR_COMPUTED_P]], computed)
})


test_that("p-values for chi2-tests are correctly calculated", {
  txt <- "chi2(28) = 22.20, p = .79"
  computed <- pchisq(22.20, 28, lower.tail = FALSE)
  
  result <- statcheck(txt, messages = FALSE)
  
  expect_equal(result[[VAR_COMPUTED_P]], computed)
})


test_that("p-values for Q-tests are correctly calculated", {
  txt <- "Q(28) = 22.20, p = .79"
  computed <- pchisq(22.20, 28, lower.tail = FALSE)
  
  result <- statcheck(txt, messages = FALSE)
  
  expect_equal(result[[VAR_COMPUTED_P]], computed)
})

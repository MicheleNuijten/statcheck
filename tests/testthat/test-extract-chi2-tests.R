context("Extract chi2-tests from string")

# test if the following chi2-tests are correctly retrieved ----------------------

# standard chi2-test
test_that("chi2-tests are correctly parsed", {
  txt1 <- "chi2(28) = 2.20, p = .03"
  
  result <- statcheck(txt1, messages = FALSE)
  
  expect_equal(nrow(result), 1)
  expect_equal(as.character(result[[VAR_TYPE]]), "Chi2")
  expect_equal(result[[VAR_DF1]], 28)
  expect_true(is.na(result[[VAR_DF2]]))
  expect_equal(as.character(result[[VAR_TEST_COMPARISON]]), "=")
  expect_equal(result[[VAR_TEST_VALUE]], 2.2)
  expect_equal(as.character(result[[VAR_P_COMPARISON]]), "=")
  expect_equal(result[[VAR_REPORTED_P]], 0.03)
  expect_equal(as.character(result[[VAR_RAW]]), "2(28) = 2.20, p = .03")
})

# standard chi2-tests in text
test_that("chi2-tests are retrieved from sentences", {
  txt1 <- "The effect was very significant, chi2(28) = 2.20, p = .03."
  txt2 <- "Both effects were very significant, chi2(28) = 2.20, p = .03, chi2(28) = 1.23, p = .04."
  
  result <- statcheck(c(txt1, txt2), messages = FALSE)
  
  expect_equal(nrow(result), 3)
  expect_equal(as.character(result[[VAR_SOURCE]]), c("1", "2", "2"))
})

# variations in extracted "chi"
test_that("variations in spelling the Greek letter chi are picked up", {
  txt1 <- "X2(28) = 2.20, p = .03"
  txt2 <- "x2(28) = 2.20, p = .03"
  txt3 <- "chi_2(28) = 2.20, p = .03"
  
  result <- statcheck(c(txt1, txt2, txt3), messages = FALSE)
  
  expect_equal(nrow(result), 3)
})

# variation in degrees of freedom
test_that("different variations in df of chi2 are parsed correctly", {
  txt1 <- "chi2(28, N = 129) = 2.2, p = .03"
  txt2 <- "chi2(1, N = 11,455) = 16.78, p <.001"
  
  result <- statcheck(c(txt1, txt2), messages = FALSE)
  
  expect_equal(nrow(result), 2)
  
})

# variation in spacing
test_that("chi2-tests with different spacing are retrieved from text", {
  txt1 <- " chi2 ( 28 ) = 2.20 , p = .03"
  txt2 <- "chi2(28)=2.20,p=.03"
  
  result <- statcheck(c(txt1, txt2), messages = FALSE)
  
  expect_equal(nrow(result), 2)
})

# chi-square between parentheses
test_that("chi2 test between parentheses are correctly parsed", {
  
  # here the X was not converted. Example from Jesse, A., & Mitterer, H. (2011). 
  # Pointing gestures do not influence the perception of lexical stress. In 
  # Twelfth Annual Conference of the International Speech Communication 
  # Association.
  txt <- " (2(1)=0.03, p=.86" 
  
  result <- statcheck(txt, messages = FALSE)
  
  expect_equal(nrow(result), 1)
  
})

# test if the following 'incorrect' chi2-tests are not retrieved --------------

# do not extract things that look like chi2 but aren't
test_that("results that only look like chi2 are not retrieved", {
  txt1 <- "t  (28) = 2.2, p < .05"
  txt2 <- "U(65) = 499.0, p = 0.55" # non-parametric Mann-Whitney U-test
  
  expect_output(statcheck(c(txt1, txt2), messages = FALSE), "did not find any results")
  
})



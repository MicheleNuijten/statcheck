context("Extract chi2-tests from string")

# test if the following chi2-tests are correctly retrieved ----------------------

# standard chi2-test
test_that("chi2-tests are correctly parsed", {
  txt1 <- "chi2(28) = 2.20, p = .03"
  
  result <- statcheck(txt1, messages = FALSE)
  
  expect_equal(nrow(result), 1)
  expect_equal(as.character(result$Statistic), "Chi2")
  expect_equal(result$df1, 28)
  expect_equal(result$df2, NA)
  expect_equal(as.character(result$Test.Comparison), "=")
  expect_equal(result$Value, 2.2)
  expect_equal(as.character(result$Reported.Comparison), "=")
  expect_equal(result$Reported.P.Value, 0.03)
  expect_equal(as.character(result$Raw), "i2(28) = 2.20, p = .03")
})

# standard chi2-tests in text
test_that("chi2-tests are retrieved from sentences", {
  txt1 <- "The effect was very significant, chi2(28) = 2.20, p = .03."
  txt2 <- "Both effects were very significant, chi2(28) = 2.20, p = .03, chi2(28) = 1.23, p = .04."
  
  result <- statcheck(c(txt1, txt2), messages = FALSE)
  
  expect_equal(nrow(result), 3)
  expect_equal(as.character(result$Source), c("1", "2", "2"))
})

# variations in extracted "chi"
test_that("variations in spelling the Greek letter chi are picked up", {
  txt1 <- "X2(28) = 2.20, p = .03"
  txt2 <- "x2(28) = 2.20, p = .03"
  txt3 <- "chi_2(28) = 2.20, p = .03"
  
  result <- statcheck(c(txt1, txt2, txt3), messages = FALSE)
  
  expect_equal(nrow(result), 3)
})

# variation in spacing
test_that("chi2-tests with different spacing are retrieved from text", {
  txt1 <- " chi2 ( 28 ) = 2.20 , p = .03"
  txt2 <- "chi2(28)=2.20,p=.03"
  
  result <- statcheck(c(txt1, txt2), messages = FALSE)
  
  expect_equal(nrow(result), 2)
})

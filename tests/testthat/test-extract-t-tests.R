context("Extract t-tests from string")

# test if the following t-tests are correctly retrieved ----------------------

# standard t-test
test_that("t-tests are correctly parsed", {
  txt1 <- "t(28) = 2.20, p = .03"
  
  result <- statcheck(txt1, messages = FALSE)
  
  expect_equal(nrow(result), 1)
  expect_equal(as.character(result$Statistic), "t")
  expect_equal(result$df1, NA)
  expect_equal(result$df2, 28)
  expect_equal(as.character(result$Test.Comparison), "=")
  expect_equal(result$Value, 2.2)
  expect_equal(as.character(result$Reported.Comparison), "=")
  expect_equal(result$Reported.P.Value, 0.03)
  expect_equal(as.character(result$Raw), "t(28) = 2.20, p = .03")
})

# standard t-tests in text
test_that("t-tests are retrieved from sentences", {
  txt1 <- "The effect was very significant, t(28) = 2.20, p = .03."
  txt2 <- "Both effects were very significant, t(28) = 2.20, p = .03, t(28) = 1.23, p = .04."
  
  result <- statcheck(c(txt1, txt2), messages = FALSE)
  
  expect_equal(nrow(result), 3)
  expect_equal(as.character(result$Source), c("1", "2", "2"))
})

# variation in spacing
test_that("t-tests with different spacing are retrieved from text", {
  txt1 <- " t ( 28 ) = 2.20 , p = .03"
  txt2 <- "t(28)=2.20,p=.03"
  
  result <- statcheck(c(txt1, txt2), messages = FALSE)
  
  expect_equal(nrow(result), 2)
})

# variations test statistic
test_that("variations in the t-statistic are retrieved from text", {
  txt1 <- "t(28) = -2.20, p = .03"
  txt2 <- "t(28) = 2,000.20, p = .03"
  txt3 <- "t(28) < 2.20, p = .03"
  txt4 <- "t(28) > 2.20, p = .03"
  
  result <- statcheck(c(txt1, txt2, txt3, txt4), messages = FALSE)
  
  expect_equal(nrow(result), 4)
})

# variations p-value
test_that("variations in the p-value are retrieved from text", {
  txt1 <- "t(28) = 2.20, p = 0.03"
  txt2 <- "t(28) = 2.20, p < .03"
  txt3 <- "t(28) = 2.20, p > .03"
  txt4 <- "t(28) = 2.20, ns"
  txt5 <- "t(28) = 2.20, p = .5e-3"
  
  result <- statcheck(c(txt1, txt2, txt3, txt4, txt5), messages = FALSE)
  
  expect_equal(nrow(result), 5)
})

# corrected degrees of freedom
test_that("corrected degrees of freedom in t-tests are retrieved from text", {
  txt1 <- "t(28.1) = 2.20, p = .03"
  
  result <- statcheck(txt1, messages = FALSE)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$df2, 28.1)
})

# test if the following incorrect t-tests are not retrieved ------------------

# punctuation
test_that("incorrect punctuation in t-tests are not retrieved from text", {
  txt1 <- "t(28) = 2.20; p = .03"
  txt2 <- "t[28] = 2.20, p = .03"
  
  expect_output(statcheck(c(txt1, txt2), messages = FALSE), "did not find any results")
})

# not a p-value
test_that("tests with 'p-values' larger than 1 are not retrieved from text", {
  txt1 <- "t(28) = 2.20, p = 1.03"
  
  expect_output(statcheck(txt1, messages = FALSE), "did not find any results")
})


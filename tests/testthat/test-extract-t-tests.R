context("Extract t-tests from string")

# test if the following t-tests are correctly retrieved ----------------------

# standard t-test
test_that("t-tests are correctly parsed", {
  txt1 <- "t(28) = 2.20, p = .03"
  
  result <- statcheck(txt1, messages = FALSE)
  
  expect_equal(nrow(result), 1)
  expect_equal(as.character(result[[VAR_TYPE]]), "t")
  expect_true(is.na(result[[VAR_DF1]]))
  expect_equal(result[[VAR_DF2]], 28)
  expect_equal(as.character(result[[VAR_TEST_COMPARISON]]), "=")
  expect_equal(result[[VAR_TEST_VALUE]], 2.2)
  expect_equal(as.character(result[[VAR_P_COMPARISON]]), "=")
  expect_equal(result[[VAR_REPORTED_P]], 0.03)
  expect_equal(as.character(result[[VAR_RAW]]), "t(28) = 2.20, p = .03")
})

# standard t-tests in text
test_that("t-tests are retrieved from sentences", {
  txt1 <- "The effect was very significant, t(28) = 2.20, p = .03."
  txt2 <- "Both effects were very significant, t(28) = 2.20, p = .03, t(28) = 1.23, p = .04."
  
  result <- statcheck(c(txt1, txt2), messages = FALSE)
  
  expect_equal(nrow(result), 3)
  expect_equal(as.character(result[[VAR_SOURCE]]), c("1", "2", "2"))
})

# variation in spacing
test_that("t-tests with different spacing are retrieved from text", {
  txt1 <- " t ( 28 ) = 2.20 , p = .03"
  txt2 <- "t(28)=2.20,p=.03"
  txt3 <- "t(43) = - 2.57, p < .05"
  
  result <- statcheck(c(txt1, txt2, txt3), messages = FALSE)
  
  expect_equal(nrow(result), 3)
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
  expect_equal(result[[VAR_DF2]], 28.1)
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

# wrong df
test_that("t-tests with 2 dfs are not retrieved from text", {
  txt1 <- "t(2,28) = 2.20, p = .03"
  
  expect_output(statcheck(txt1, messages = FALSE), "did not find any results")
})


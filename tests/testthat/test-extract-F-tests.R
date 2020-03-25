context("Extract F-tests from string")

# test if the following F-tests are correctly retrieved ----------------------

# standard F-test
test_that("F-tests are correctly parsed", {
  txt1 <- "F(2, 28) = 2.20, p = .03"
  
  result <- statcheck(txt1, messages = FALSE)
  
  expect_equal(nrow(result), 1)
  expect_equal(as.character(result$Statistic), "F")
  expect_equal(result$df1, 2)
  expect_equal(result$df2, 28)
  expect_equal(as.character(result$Test.Comparison), "=")
  expect_equal(result$Value, 2.2)
  expect_equal(as.character(result$Reported.Comparison), "=")
  expect_equal(result$Reported.P.Value, 0.03)
  expect_equal(as.character(result$Raw), "F(2, 28) = 2.20, p = .03")
})

# standard F-tests in text
test_that("F-tests are retrieved from sentences", {
  txt1 <- "The effect was very significant, F(2, 28) = 2.20, p = .03."
  txt2 <- "Both effects were very significant, F(2, 28) = 2.20, p = .03, F(2, 28) = 1.23, p = .04."
  
  result <- statcheck(c(txt1, txt2), messages = FALSE)
  
  expect_equal(nrow(result), 3)
  expect_equal(as.character(result$Source), c("1", "2", "2"))
})

# variation in spacing
test_that("F-tests with different spacing are retrieved from text", {
  txt1 <- " F ( 2 , 28 ) = 2.20 , p = .03"
  txt2 <- "F(2,28)=2.20,p=.03"
  
  result <- statcheck(c(txt1, txt2), messages = FALSE)
  
  expect_equal(nrow(result), 2)
})

# variations in the degrees of freedom
test_that("corrected degrees of freedom in F-tests are retrieved from text", {
  txt1 <- "F(2.1, 28.1) = 2.20, p = .03"
  
  result <- statcheck(txt1, messages = FALSE)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$df2, 28.1)
})

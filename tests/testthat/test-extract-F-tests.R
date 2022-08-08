context("Extract F-tests from string")

# test if the following F-tests are correctly retrieved ----------------------

# standard F-test
test_that("F-tests are correctly parsed", {
  txt1 <- "F(2, 28) = 2.20, p = .03"
  
  result <- statcheck(txt1, messages = FALSE)
  
  expect_equal(nrow(result), 1)
  expect_equal(as.character(result[[VAR_TYPE]]), "F")
  expect_equal(result[[VAR_DF1]], 2)
  expect_equal(result[[VAR_DF2]], 28)
  expect_equal(as.character(result[[VAR_TEST_COMPARISON]]), "=")
  expect_equal(result[[VAR_TEST_VALUE]], 2.2)
  expect_equal(as.character(result[[VAR_P_COMPARISON]]), "=")
  expect_equal(result[[VAR_REPORTED_P]], 0.03)
  expect_equal(as.character(result[[VAR_RAW]]), "F(2, 28) = 2.20, p = .03")
})

# standard F-tests in text
test_that("F-tests are retrieved from sentences", {
  txt1 <- "The effect was very significant, F(2, 28) = 2.20, p = .03."
  txt2 <- "Both effects were very significant, F(2, 28) = 2.20, p = .03, F(2, 28) = 1.23, p = .04."
  
  result <- statcheck(c(txt1, txt2), messages = FALSE)
  
  expect_equal(nrow(result), 3)
  expect_equal(as.character(result[[VAR_SOURCE]]), c("1", "2", "2"))
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
  expect_equal(result[[VAR_DF1]], c(2.1))
  expect_equal(result[[VAR_DF2]], c(28.1))
})

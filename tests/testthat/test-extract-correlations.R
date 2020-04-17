context("Extract correlations from string")

# test if the following correlations are correctly retrieved ----------------------

# standard correlation
test_that("correlations are correctly parsed", {
  txt1 <- "r(28) = .20, p = .03"
  
  result <- statcheck(txt1, messages = FALSE)
  
  expect_equal(nrow(result), 1)
  expect_equal(as.character(result$Statistic), "r")
  expect_true(is.na(result$df1))
  expect_equal(result$df2, 28)
  expect_equal(as.character(result$Test.Comparison), "=")
  expect_equal(result$Value, .2)
  expect_equal(as.character(result$Reported.Comparison), "=")
  expect_equal(result$Reported.P.Value, 0.03)
  expect_equal(as.character(result$Raw), "r(28) = .20, p = .03")
})

# standard correlations in text
test_that("correlations are retrieved from sentences", {
  txt1 <- "The effect was very significant, r(28) = .20, p = .03."
  txt2 <- "Both effects were very significant, r(28) = .20, p = .03, r(28) = .23, p = .04."
  
  result <- statcheck(c(txt1, txt2), messages = FALSE)
  
  expect_equal(nrow(result), 3)
  expect_equal(as.character(result$Source), c("1", "2", "2"))
})

# variation in spacing
test_that("correlations with different spacing are retrieved from text", {
  txt1 <- " r ( 28 ) = .20 , p = .03"
  txt2 <- "r(28)=.20,p=.03"
  
  result <- statcheck(c(txt1, txt2), messages = FALSE)
  
  expect_equal(nrow(result), 2)
})

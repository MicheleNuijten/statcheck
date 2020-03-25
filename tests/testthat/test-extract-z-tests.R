context("Extract z-tests from string")

# test if the following z-tests are correctly retrieved ----------------------

# standard z-tests
test_that("z-tests are correctly parsed", {
  txt1 <- " z = 2.20, p = .03"
  
  result <- statcheck(txt1, messages = FALSE)
  
  expect_equal(nrow(result), 1)
  expect_equal(as.character(result$Statistic), "Z")
  expect_equal(result$df1, NA)
  expect_equal(result$df2, NA)
  expect_equal(as.character(result$Test.Comparison), "=")
  expect_equal(result$Value, 2.2)
  expect_equal(as.character(result$Reported.Comparison), "=")
  expect_equal(result$Reported.P.Value, 0.03)
  expect_equal(as.character(result$Raw), "Z = 2.20, p = .03")
})

# standard z-tests in text
test_that("z-tests are retrieved from sentences", {
  txt1 <- "The effect was very significant, z = 2.20, p = .03."
  txt2 <- "Both effects were very significant, z = 2.20, p = .03, z = 1.23, p = .04."
  
  result <- statcheck(c(txt1, txt2), messages = FALSE)
  
  expect_equal(nrow(result), 3)
  expect_equal(as.character(result$Source), c("1", "2", "2"))
})

# variation in spacing
test_that("z-tests with different spacing are retrieved from text", {
  txt1 <- " z = 2.20 , p = .03"
  txt2 <- " z=2.20,p=.03"
  
  result <- statcheck(c(txt1, txt2), messages = FALSE)
  
  expect_equal(nrow(result), 2)
})

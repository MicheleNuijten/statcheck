context("Extract Q-tests from string")

# test if the following Q-tests are correctly retrieved ----------------------

# standard Q-tests
test_that("Q-tests are correctly parsed", {
  txt1 <- "Q(2) = 2.20, p = .03"
  
  result <- statcheck(txt1, messages = FALSE)
  
  expect_equal(nrow(result), 1)
  expect_equal(as.character(result$Statistic), "Q")
  expect_equal(result$df1, 2)
  expect_true(is.na(result$df2))
  expect_equal(as.character(result$Test.Comparison), "=")
  expect_equal(result$Value, 2.2)
  expect_equal(as.character(result$Reported.Comparison), "=")
  expect_equal(result$Reported.P.Value, 0.03)
  expect_equal(as.character(result$Raw), "Q(2) = 2.20, p = .03")
})

# standard Q-tests in text
test_that("Q-tests are retrieved from sentences", {
  txt1 <- "The effect was very significant, Q(2) = 2.20, p = .03."
  txt2 <- "Both effects were very significant, Q(2) = 2.20, p = .03, Q(2) = 1.23, p = .04."
  
  result <- statcheck(c(txt1, txt2), messages = FALSE)
  
  expect_equal(nrow(result), 3)
  expect_equal(as.character(result$Source), c("1", "2", "2"))
})

# variation in spacing
test_that("Q-tests with different spacing are retrieved from text", {
  txt1 <- " Q ( 2 ) = 2.20 , p = .03"
  txt2 <- "Q(2)=2.20,p=.03"
  
  result <- statcheck(c(txt1, txt2), messages = FALSE)
  
  expect_equal(nrow(result), 2)
})

# different types of Q-tests
test_that("different types of Q-tests are correctly parsed", {
  txt1 <- "Qw(2) = 2.20, p = .03"
  txt2 <- "Qwithin(2) = 2.20, p = .03"
  txt3 <- "Q-within(2) = 2.20, p = .03"
  
  txt4 <- "Qb(2) = 2.20, p = .03"
  txt5 <- "Qbetween(2) = 2.20, p = .03"
  txt6 <- "Q-between(2) = 2.20, p = .03"
  
  result <- statcheck(c(txt1, txt2, txt3, txt4, txt5, txt6), messages = FALSE)
  
  expect_equal(nrow(result), 6)
  expect_equal(as.vector(result$Statistic), c(rep("Qw", 3), rep("Qb", 3)))
})

# test if the following 'incorrect' Q-tests are not retrieved ----------------
test_that("stats that only look like Q-tests are not retrieved", {
  # txt1 <- "q(2) = 2.2, p = .03" # lower case q is a different test
  txt2 <- "Qs(2) = 2.2, p = .03"
  txt3 <- "Qb(2, N = 187) = 2.20, p = .03"
  
  expect_output(statcheck(c(txt2, txt3), messages = FALSE), "did not find any results")
  
})



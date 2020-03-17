context("Calculate the APA factor")

test_that("correct APA factor is calculated", {
  
  txt1 <- "This text has 50% of its stats in APA style: t(28) = 2.20, p < .05, some other p = .035."
  txt2 <- "This text has 100% of its stats in APA style: t(28) = 2.20, p < .05."
  
  result1 <- statcheck(txt1, messages = FALSE)
  result2 <- statcheck(txt2, messages = FALSE)
  result12 <- statcheck(c(txt1, txt2), messages = FALSE)
  
  expect_equal(result1$APAfactor, .5)
  expect_equal(result2$APAfactor, 1)
  expect_equal(result12$APAfactor, c(.5, 1))
    
})

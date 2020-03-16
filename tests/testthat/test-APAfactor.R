context("Calculate the APA factor")

test_that("correct APA factor is calculated", {
  
  txt <- "This text has 50% of its stats in APA style: t(28) = 2.20, p < .05, some other p = .035"
  txt2 <- "This text also has 50% of its stats in APA style: t(28) = 2.20, p < .05, some other p = .035, t(29) = 2.20, p < .05, some other p = .045"
  
  result <- statcheck(txt, messages = FALSE)
  
  expect_equal(result$APAfactor, .5)
    
})

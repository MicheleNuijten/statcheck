context("Calculate the APA factor")

test_that("correct APA factor is calculated", {
  
  txt1 <- "This text has 50% of its stats in APA style: t(28) = 2.20, p < .05, some other p = .035."
  txt2 <- "This text has 100% of its stats in APA style: t(28) = 2.20, p < .05."
  
  result1 <- statcheck(txt1, messages = FALSE)
  result2 <- statcheck(txt2, messages = FALSE)
  result12 <- statcheck(c(txt1, txt2), messages = FALSE)
  
  expect_equal(result1[[VAR_APAFACTOR]], .5)
  expect_equal(result2[[VAR_APAFACTOR]], 1)
  expect_equal(result12[[VAR_APAFACTOR]], c(.5, 1))
    
})

test_that("APA factor is calculated without problems if 1 source has no NHST", {
  
  txt1 <- "This text has 50% of its stats in APA style: t(28) = 2.20, p < .05, some other p = .035."
  txt2 <- "This text has 0% of its stats in APA style: p < .05."
  
  result <- statcheck(c(txt1, txt2), messages = FALSE)
  
  expect_equal(result[[VAR_APAFACTOR]], .5)
  expect_equal(nrow(result), 1)
  
})


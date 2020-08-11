context("correctly parse NHST with deviations from APA style")

test_that("F with MSE is extracted and parsed",{
  txt1 <- "F(2,25) = 11.37, MSE = 236, p < .01"
  txt2 <- "F(2,25) = 11.37, MSE = 236.43, p < .01"
  
  result <- statcheck(c(txt1, txt2), apa_style = FALSE, messages = FALSE)
  
  expect_equal(nrow(result), 2)
  expect_equal(result[[VAR_DF1]], rep(2, 2))
  expect_equal(result[[VAR_DF2]], rep(25, 2))
  expect_equal(result[[VAR_TEST_VALUE]], rep(11.37, 2))
  
  # don't extract these results when apa_style is TRUE
  expect_output(statcheck(c(txt1, txt2), messages = FALSE), 
                "did not find any results")
  
})

test_that("df between square, curly, or no brackets are extracted and parsed",{
  txt1 <- "t[28] = 2.2, p = .03"
  txt2 <- "F[2, 28] = 2.2, p = .03"
  txt3 <- "t{28} = 2.2, p = .03"
  txt4 <- "F{2, 28} = 2.2, p = .03"
  txt5 <- "F2,28 = 2.2, p = .03" # df in subscript
  txt6 <- "t28 = 2.2, p = .03"
  
  result <- statcheck(c(txt1, txt2, txt3, txt4, txt5, txt6), apa_style = FALSE, 
                      messages = FALSE)
  
  expect_equal(nrow(result), 6)
  expect_equal(result[[VAR_DF1]], c(NA, 2, NA, 2, 2, NA))
  expect_equal(result[[VAR_DF2]], rep(28, 6))
  
  # don't extract these results when apa_style is TRUE
  expect_output(statcheck(c(txt1, txt2, txt3, txt4, txt5, txt6), 
                          messages = FALSE), "did not find any results")
})

test_that("DF in degrees of freedom is extracted and parsed", {
  txt1 <- "chi2(DF = 2) = 45.1, p < 0.001"
  txt2 <- "F(DF = 3, 1115) = 7.7, p < 0.001"
  
  result <- statcheck(c(txt1, txt2), apa_style = FALSE, 
                      messages = FALSE)
  
  expect_equal(nrow(result), 2)
  expect_equal(result[[VAR_DF1]], c(2, 3))
  expect_equal(result[[VAR_DF2]], c(NA, 1115))
  
  # don't extract these results when apa_style is TRUE
  expect_output(statcheck(c(txt1, txt2), messages = FALSE), 
                "did not find any results")
  
})

test_that("semi-colons instead of commas are extracted and parsed", {
   txt1 <- "t(28) = 2.2; p = .03"
  
   result <- statcheck(txt1, apa_style = FALSE, messages = FALSE)
   
   expect_equal(nrow(result), 1)
   
   # don't extract these results when apa_style is TRUE
   expect_output(statcheck(txt1, messages = FALSE), 
                 "did not find any results")
   
})

test_that("DF in degrees of freedom AND semi-colons are extracted and parsed", {
  txt1 <- "chi2(DF = 2) = 45.1; p < 0.001"
  txt2 <- "F(DF = 3, 1115) = 7.7; p < 0.001"
  
  result <- statcheck(c(txt1, txt2), apa_style = FALSE, messages = FALSE)
  
  expect_equal(nrow(result), 2)
  expect_equal(result[[VAR_DF1]], c(2, 3))
  expect_equal(result[[VAR_DF2]], c(NA, 1115))
  
  # don't extract these results when apa_style is TRUE
  expect_output(statcheck(c(txt1, txt2), messages = FALSE), 
                "did not find any results")
  
})

test_that("correlations with N instead of df are extracted and parsed", {
  txt1 <- "r(N=95)=.41, p < .001"
  
  result <- statcheck(txt1, apa_style = FALSE, messages = FALSE)
  
  expect_equal(nrow(result), 1)
  
  # don't extract these results when apa_style is TRUE
  expect_output(statcheck(txt1, messages = FALSE), 
                "did not find any results")
  
})

test_that("incorrect spacing is still extracted and parsed", {
  txt1 <-  "t(191) = 8.22, p < . 001"
  txt2 <- "t(191) = 9.54, p <. 001"
  txt3 <- "t  (191  ) = 9.   42,p<.01"
  txt4 <- "z  =1. 94, p  <. 05"
  
  result <- statcheck(c(txt1, txt2, txt3, txt4), 
                      apa_style = FALSE, messages = FALSE)
  
  expect_equal(nrow(result), 4)
  
  # don't extract these results when apa_style is TRUE
  expect_output(statcheck(c(txt1, txt2), messages = FALSE), 
                "did not find any results")
  
})

test_that("incorrect punctuation in test is extracted and parsed", {
  txt1 <- "t(102)=.1.84,p=.068"
  txt2 <- "t(102)=..1.84,p=.068"
  
  result <- statcheck(c(txt1, txt2), 
                      apa_style = FALSE, messages = FALSE)
  
  expect_equal(nrow(result), 2)
  
})


test_that("tests with 'subscripts' extracted and parsed", {
  txt1 <-  "F1(1, 73) = 5.41, MSE = 454009, p = .023"
  txt2 <- "F2(1, 62) = 15.760, MSE = 212146, p < .001"
  txt3 <- "t2(39) = 41.2, p > .01"
  
  
  
})




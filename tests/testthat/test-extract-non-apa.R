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


test_that("df between square and curly brackets are extracted and parsed",{
  txt1 <- "t[28] = 2.2, p = .03"
  txt2 <- "F[2, 28] = 2.2, p = .03"
  txt3 <- "t{28} = 2.2, p = .03"
  txt4 <- "F{2, 28} = 2.2, p = .03"
  
  result <- statcheck(c(txt1, txt2, txt3, txt4), apa_style = FALSE, 
                      messages = FALSE)
  
  expect_equal(nrow(result), 4)
  expect_equal(result[[VAR_DF1]], c(NA, 2, NA, 2))
  expect_equal(result[[VAR_DF2]], rep(28, 4))
  
  # don't extract these results when apa_style is TRUE
  expect_output(statcheck(c(txt1, txt2, txt3, txt4), messages = FALSE), 
                "did not find any results")
})


test_that("semi-colons instead of commas are extracted and parsed",{
   txt1 <- "t(28) = 2.2; p = .03"
   
   
})


test_that("incorrect spacing is still extracted and parsed", {
  txt1 <-  "t(191) = 8.22, p < . 001"
  txt2 <- "t(191) = 9.54, p <. 001"
  
})



test_that("F tests with 'subscripts' extracted and parsed", {
  txt1 <-  "F1(1, 73) = 5.41, MSE = 454009, p = .023"
  txt2 <- "F2(1, 62) = 15.760, MSE = 212146, p < .001"
  
})

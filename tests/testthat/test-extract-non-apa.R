context("correctly parse NHST with deviations from APA style")

test_that("F with MSE is extracted and parsed",{
  txt1 <- "F(2,25) = 11.37, MSE = 236, p < .01"
  txt2 <- "F(2,25) = 11.37, MSE = 236.43, p < .01"
  
  result <- statcheck(c(txt1, txt2), apa_style = FALSE, messages = FALSE)
  
  expect_equal(nrow(result), 2)
  expect_equal(result[[VAR_DF1]], rep(2, 2))
  expect_equal(result[[VAR_DF2]], rep(25, 2))
  expect_equal(result[[VAR_TEST_VALUE]], rep(11.37, 2))
  
  # don't extract these results when apa_style is FALSE
  expect_output(statcheck(c(txt1, txt2), messages = FALSE), 
                "did not find any results")
  
})


test_that("df between square brackets are extracted and parsed",{
  txt1 <- "t[28] = 2.2, p = .03"
  txt2 <- "F[2, 28] = 2.2, p = .03"
  
  
})


test_that("semi-colons instead of commas are extracted and parsed",{
   txt1 <- "t(28) = 2.2; p = .03"
   
   
})
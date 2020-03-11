context("Check t-tests in string")

# test if the following t-tests are correctly retrieved ----------------------

# standard t-tests
test_that("standard t-tests are retrieved from text", {
  txt1 <- "t(28) = 2.20, p = .03"
  txt2 <- "The effect was very significant, t(28) = 2.20, p = .03."
  txt3 <- "Both effects were very significant, t(28) = 2.20, p = .03, t(28) = 1.23, p = .04."
  
  ddpcr::quiet(result <- statcheck(c(txt1, txt2, txt3)))
  
  expect_equal(nrow(result), 4)
})

# variation in spacing
test_that("t-tests with different spacing are retrieved from text", {
  txt1 <- " t ( 28 ) = 2.20 , p = .03"
  txt2 <- "t(28)=2.20,p=.03"
  
  ddpcr::quiet(result <- statcheck(c(txt1, txt2)))
  
  expect_equal(nrow(result), 2)
})

# variations test statistic
test_that("variations in the t-statistic are retrieved from text", {
  txt1 <- "t(28) = -2.20, p = .03"
  txt2 <- "t(28) = 2,000.20, p = .03"
  txt3 <- "t(28) < 2.20, p = .03"
  txt4 <- "t(28) > 2.20, p = .03"
  
  ddpcr::quiet(result <- statcheck(c(txt1, txt2, txt3, txt4)))
  
  expect_equal(nrow(result), 4)
})

# variations p-value
test_that("variations in the p-value are retrieved from text", {
  txt1 <- "t(28) = 2.20, p = 0.03"
  txt2 <- "t(28) = 2.20, p < .03"
  txt3 <- "t(28) = 2.20, p > .03"
  txt4 <- "t(28) = 2.20, ns"
  txt5 <- "t(28) = 2.20, p = .5e-3"
  
  ddpcr::quiet(result <- statcheck(c(txt1, txt2, txt3, txt4, txt5)))
  
  expect_equal(nrow(result), 5)
})

# corrected degrees of freedom
test_that("corrected degrees of freedom in t-tests are retrieved from text", {
  txt1 <- "t(28.1) = 2.20, p = .03"
  
  ddpcr::quiet(result <- statcheck(txt1))
  
  expect_equal(nrow(result), 1)
})

# test if the following incorrect t-tests are not retrieved ------------------

# punctuation
test_that("incorrect punctuation in t-tests are not retrieved from text", {
  txt1 <- "t(28) = 2.20; p = .03"
  txt2 <- "t[28] = 2.20, p = .03"
  
  ddpcr::quiet(expect_output(statcheck(c(txt1, txt2)), "did not find any results"))
})

# not a t-test
test_that("the word 'test' is not recognized as a t-test in text", {
  txt1 <- "test(28) = 2.20, p = .03"  
  
  ddpcr::quiet(expect_output(statcheck(txt1), "did not find any results"))
})

# not a p-value
test_that("tests with 'p-values' larger than 1 are not retrieved from text", {
  txt1 <- "t(28) = 2.20, p = 1.03"
  
  ddpcr::quiet(expect_output(statcheck(txt1), "did not find any results"))
})


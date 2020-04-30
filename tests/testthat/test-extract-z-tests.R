context("Extract z-tests from string")

# test if the following z-tests are correctly retrieved ----------------------

# standard z-tests
test_that("z-tests are correctly parsed", {
  txt1 <- " z = 2.20, p = .03"
  
  result <- statcheck(txt1, messages = FALSE)
  
  expect_equal(nrow(result), 1)
  expect_equal(as.character(result[[VAR_TYPE]]), "Z")
  expect_true(is.na(result[[VAR_DF1]]))
  expect_true(is.na(result[[VAR_DF2]]))
  expect_equal(as.character(result[[VAR_TEST_COMPARISON]]), "=")
  expect_equal(result[[VAR_TEST_VALUE]], 2.2)
  expect_equal(as.character(result[[VAR_P_COMPARISON]]), "=")
  expect_equal(result[[VAR_REPORTED_P]], 0.03)
  expect_equal(as.character(result[[VAR_RAW]]), "z = 2.20, p = .03")
})

# standard z-tests in text
test_that("z-tests are retrieved from sentences", {
  txt1 <- "The effect was very significant, z = 2.20, p = .03."
  txt2 <- "Both effects were very significant, z = 2.20, p = .03, z = 1.23, p = .04."
  
  result <- statcheck(c(txt1, txt2), messages = FALSE)
  
  expect_equal(nrow(result), 3)
  expect_equal(as.character(result[[VAR_SOURCE]]), c("1", "2", "2"))
})

# variation in spacing
test_that("z-tests with different spacing are retrieved from text", {
  txt1 <- " z = 2.20 , p = .03"
  txt2 <- " z=2.20,p=.03"
  
  result <- statcheck(c(txt1, txt2), messages = FALSE)
  
  expect_equal(nrow(result), 2)
})

# variation in capitalization
test_that("upper case z-tests are retrieved from text", {
  txt <- " Z = 2.20 , p = .03"
  
  result <- statcheck(txt, messages = FALSE)
  
  expect_equal(nrow(result), 1)
})

# test if the following incorrect z-tests are not retrieved --------------------

# z test cannot have df
test_that("a z followed by degrees of freedom is not matched", {
  txt <- " z(28) = 2.20, p = .03"
  
  expect_output(statcheck(txt, messages = FALSE), "did not find any results")
  
})

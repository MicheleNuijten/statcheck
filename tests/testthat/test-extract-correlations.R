context("Extract correlations from string")

# test if the following correlations are correctly retrieved ----------------------

# standard correlation
test_that("correlations are correctly parsed", {
  txt1 <- "r(28) = .20, p = .03"
  
  result <- statcheck(txt1, messages = FALSE)
  
  expect_equal(nrow(result), 1)
  expect_equal(as.character(result[[VAR_TYPE]]), "r")
  expect_true(is.na(result[[VAR_DF1]]))
  expect_equal(result[[VAR_DF2]], 28)
  expect_equal(as.character(result[[VAR_TEST_COMPARISON]]), "=")
  expect_equal(result[[VAR_TEST_VALUE]], .2)
  expect_equal(as.character(result[[VAR_P_COMPARISON]]), "=")
  expect_equal(result[[VAR_REPORTED_P]], 0.03)
  expect_equal(as.character(result[[VAR_RAW]]), "r(28) = .20, p = .03")
})

# standard correlations in text
test_that("correlations are retrieved from sentences", {
  txt1 <- "The effect was very significant, r(28) = .20, p = .03."
  txt2 <- "Both effects were very significant, r(28) = .20, p = .03, r(28) = .23, p = .04."
  
  result <- statcheck(c(txt1, txt2), messages = FALSE)
  
  expect_equal(nrow(result), 3)
  expect_equal(as.character(result[[VAR_SOURCE]]), c("1", "2", "2"))
})

# variation in spacing
test_that("correlations with different spacing are retrieved from text", {
  txt1 <- " r ( 28 ) = .20 , p = .03"
  txt2 <- "r(28)=.20,p=.03"
  
  result <- statcheck(c(txt1, txt2), messages = FALSE)
  
  expect_equal(nrow(result), 2)
})

# test if the following incorrect correlations are not retrieved -------------

# correlations > 1 are not retrieved
test_that("'correlations' > 1 are not retrieved", {
  txt <- "r(16) = 26.05, p = .10"
  
  expect_output(statcheck(txt, messages = FALSE), "did not find any results")
})

# subscripts ending on r are not recognized as correlations
test_that("subscripts ending on r are not recognized as correlations", {
  txt <- "QError(16) = .26, p = .10"
  
  expect_output(statcheck(txt, messages = FALSE), "did not find any results")
})

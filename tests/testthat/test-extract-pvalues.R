context("Extract p-values from string")

# test if the following p-values are correctly retrieved ----------------------

# standard p-values
test_that("p-values are correctly parsed", {
  txt1 <- "p = .05"
  txt2 <- "p < .05"
  txt3 <- "p > .05"
  
  result <- statcheck(c(txt1, txt2, txt3), messages = FALSE,
                        AllPValues = TRUE)
  
  expect_equal(nrow(result), 3)
  expect_equal(as.character(result[[VAR_P_COMPARISON]]), c("=", "<", ">"))
  expect_equal(result[[VAR_REPORTED_P]], rep(0.05, 3))
})

# non-significant results
test_that("results reported as ns are correctly parsed", {
  txt1 <- "the result was not significant, ns"
  
  result <- statcheck(txt1, messages = FALSE,
                      AllPValues = TRUE)
  
  expect_equal(nrow(result), 1)
  expect_equal(as.character(result[[VAR_P_COMPARISON]]), "ns")
  expect_true(is.na(result[[VAR_REPORTED_P]]))
})

# test if the following non p-values are not retrieved ------------------

# page number
test_that("page numbers are not extracted", {
  txt1 <- "see p. 01"
  
  expect_output(statcheck(txt1, messages = FALSE,
                          AllPValues = TRUE), "did not find any p-values")
})
  
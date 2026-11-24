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

test_that("scientific p-values preserve the value and reported precision", {
  text <- c("p = 5.81 \u00d7 10\u221248", "p < 5.81e-48",
            "p > 1 \u00d7 10^-4", "p = .05")
  result <- statcheck(text, messages = FALSE, AllPValues = TRUE)
  expected <- c(5.81e-48, 5.81e-48, 1e-4, .05)

  expect_equal(result[[VAR_REPORTED_P]] / expected, rep(1, 4))
  expect_equal(result[[VAR_P_DEC]], c(50, 50, 4, 2))
  expect_equal(as.character(result[[VAR_P_COMPARISON]]), c("=", "<", ">", "="))
})

test_that("scientific p-values are checked in complete chi-square results", {
  text <- paste0("\u03c72(5) = 231.24, p = ", c("5.78", "5.79", "5.81", "5.82"),
                 " \u00d7 10\u221248")
  result <- statcheck(text, messages = FALSE)
  expected <- c(5.78e-48, 5.79e-48, 5.81e-48, 5.82e-48)

  expect_equal(result[[VAR_REPORTED_P]] / expected, rep(1, 4))
  expect_equal(as.character(result[[VAR_RAW]]), text)
  expect_equal(result[[VAR_ERROR]], c(TRUE, FALSE, FALSE, TRUE))
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


# minus sign as a dash, following a p-value
test_that("minus signs used as a dash are correctly parsed", {
  txt <- "p = .09-and we are sad"
  
  expect_no_warning(statcheck(txt, messages = FALSE,
                   AllPValues = TRUE))
  
})

# test if the following non p-values are not retrieved ------------------

# page number
test_that("page numbers are not extracted", {
  txt1 <- "see p. 01"
  
  expect_message(statcheck(txt1, messages = FALSE,
                          AllPValues = TRUE), "did not find any p-values")
})
  
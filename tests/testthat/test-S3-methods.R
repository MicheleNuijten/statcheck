context("Summarize statcheck output")

# Note: so far I haven't found a straightforward way to test plot.statcheck()
# and identify.statcheck()

# test summary.statcheck() -----------------------------------------------------
test_that("summary.statcheck() works for a single source", {
  
  txt1 <- "t(28) = 2.20, p = .06"
  txt2 <- "F(2, 28) = 2.20, p = .15"
  txt3 <- "r(28) = .22, p = .26"
  txt4 <- "chi2(28) = 22.20, p = .77"
  txt5 <- " z = 2.20, p = .04"
  txt6 <- "Q(28) = 22.20, p = .03"
  
  txt <- paste(txt1, txt2, txt3, txt4, txt5, txt6)
  result <- statcheck(txt, messages = FALSE)
  summary <- summary(result)
  
  expect_equal(nrow(summary), 2)
  expect_equal(as.vector(summary[[VAR_SOURCE]]), c("1", "Total"))
  expect_equal(summary[[VAR_NR_PVALUES]], c(6, 6))
  expect_equal(summary[[VAR_NR_ERRORS]], c(5, 5))
  expect_equal(summary[[VAR_NR_DEC_ERRORS]], c(2, 2))
  
})

test_that("summary.statcheck() works for multiple sources", {
  
  txt1 <- "t(28) = 2.20, p = .06"
  txt2 <- "F(2, 28) = 2.20, p = .15"
  txt3 <- "r(28) = .22, p = .26"
  txt4 <- "chi2(28) = 22.20, p = .77"
  txt5 <- " z = 2.20, p = .04"
  txt6 <- "Q(28) = 22.20, p = .03"
  
  result <- statcheck(c(txt1, txt2, txt3, txt4, txt5, txt6), messages = FALSE)
  summary <- summary(result)
  
  expect_equal(nrow(summary), 7)
  expect_equal(as.vector(summary[[VAR_SOURCE]]), c(as.character(1:6), "Total"))
  expect_equal(summary[[VAR_NR_PVALUES]], c(rep(1, 6), 6))
  expect_equal(summary[[VAR_NR_ERRORS]], c(1,1,1,0,1,1,5))
  expect_equal(summary[[VAR_NR_DEC_ERRORS]], c(1,0,0,0,0,1,2))
  
})

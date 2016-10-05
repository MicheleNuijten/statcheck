context("Catching errors")

test_that("Detecting a p-value error from a t-test", {
  txt <- "The effect was very significant (t(48) = 1.02, p < .05)"
  result <- statcheck(txt)

  testthat::expect_equal(as.character(result$Statistic), "t")
  testthat::expect_true(result$Error)
  testthat::expect_true(result$DecisionError)
})


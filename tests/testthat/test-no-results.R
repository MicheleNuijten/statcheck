context("Text without statistical results")

test_that("Text without APA results yields nothing", {
  txt <- "All the results were significant, all ps < .05"

  expect_output(statcheck(txt), "did not find any results")
  expect_null(statcheck(txt))
})

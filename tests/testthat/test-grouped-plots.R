test_that("grouped plots render the p-values in the correct source panels", {
  result <- statcheck(
    c(article_a = "t(28) = 2.20, p = .06",
      article_b = "t(28) = 1.00, p = .20"), messages = FALSE)

  built <- ggplot2::ggplot_build(plot(result, group = VAR_SOURCE))
  points <- built$data[[1]]

  expect_equal(points$x, result[[VAR_REPORTED_P]])
  expect_equal(points$y, result[[VAR_COMPUTED_P]])
  expect_equal(as.integer(points$PANEL), c(1L, 2L))
  expect_equal(built$layout$layout[[VAR_SOURCE]], c("article_a", "article_b"))
})

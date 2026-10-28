test_that("UTF-8 HTML text is preserved independently of the native locale", {
  old_locale <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", old_locale), add = TRUE)
  Sys.setlocale("LC_CTYPE", "C")

  html_file <- tempfile(fileext = ".html")
  on.exit(unlink(html_file), add = TRUE)
  text <- "Caf\u00e9: t(28) = 2.20, p = .04"
  writeBin(charToRaw(enc2utf8(paste0("<p>", text, "</p>"))), html_file)

  expect_identical(getHTML(html_file)[[1]], text)
  expected <- statcheck(
    setNames("t(28) = 2.20, p = .04", basename(html_file)), messages = FALSE)
  expect_equal(checkHTML(html_file, messages = FALSE), expected)
})

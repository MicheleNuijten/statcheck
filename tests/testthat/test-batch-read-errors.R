test_that("an unreadable article does not stop an HTML or PDF directory scan", {
  skip_if_not_installed("testthat", "3.2.0")
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  for (format in c("htm", "pdf")) {
    scan <- if (format == "htm") checkHTMLdir else checkPDFdir
    read_name <- if (format == "htm") "getHTML" else "getPDF"
    read <- get(read_name, envir = asNamespace("statcheck"))

    directory <- file.path(root, format)
    dir.create(directory)

    good <- paste0(c("Paper1.", "Paper3."), format)
    fixtures <- system.file("test_materials", "test_dir", good, package = "statcheck")
    expect_true(all(file.copy(fixtures, directory)))
    expected <- suppressMessages(scan(directory, messages = FALSE))
    expect_gt(nrow(expected), 0)

    middle <- file.path(directory, paste0("Paper2.", format))
    expect_true(file.create(middle))
    read_with_missing_file <- function(path) {
      if (identical(path, middle)) {
        path <- file.path(directory, "missing")
      }
      read(path)
    }
    reader_stub <- setNames(list(read_with_missing_file), read_name)
    expect_message(
      actual <- testthat::with_mocked_bindings(
        suppressWarnings(scan(directory, messages = FALSE)),
        !!!reader_stub,
        .package = "statcheck"
      ),
      paste0("Skipping.*Paper2\\.", format)
    )
    expect_identical(actual, expected)
  }
})

test_that("statcheckReport generates an HTML file", {
  
  # Create a temporary directory
  tmp_dir <- tempdir()
  
  # Prepare dummy statcheck output
  res <- statcheck("t(23) = 3.1, p = .19")
  
  # Run the report function
  outputFile <- "test_report.html"
  
  expect_no_error(
    statcheckReport(
      statcheckOutput = res,
      outputFile = outputFile,
      outputDir = tmp_dir)
  )
  
  # Check that the file exists
  out_path <- file.path(tmp_dir, outputFile)
  expect_true(file.exists(out_path))
  
  # Check content
  content <- readLines(out_path, warn = FALSE)
  
  # Check that the HTML contains a <table> element
  expect_true(any(grepl("<table", content)))
  
  # Check that the table contains at least one row with the expected data
  expect_true(any(grepl("t\\(23\\) = 3.1", content)))
  
})

test_that("report fails with empty statcheckOutput", {
  expect_error(
    statcheckReport(data.frame(), outputFile = "test", outputDir = tempdir()),
    "No statcheck results to report"
  )
})

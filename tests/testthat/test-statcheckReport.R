test_that("statcheck_report generates an HTML file", {
  
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
  expect_true(any(grepl("Consistent", content)))
})
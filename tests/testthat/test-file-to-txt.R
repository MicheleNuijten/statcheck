context("Check if statistics from an article are correctly retrieved and parsed")

# tests concerning parsing stats from pdf files ------------------------------

# individual pdf files
test_that("statistics from a pdf are correctly retrieved and parsed", {
  
  pdf_file <- system.file("test_materials/nuijten.pdf",
                          package = "statcheck")
  
  result <- checkPDF(pdf_file, messages = FALSE)
  result_1tailed <- checkPDF(pdf_file, messages = FALSE, OneTailedTxt = TRUE)
  
  # extract 4 tests from paper
  expect_equal(nrow(result), 8)
  expect_equal(as.character(result[[VAR_TYPE]]), c("t", "Chi2", "t", "F",
                                                   "F", "F", "Chi2", "t"))
  expect_equal(result[[VAR_TEST_VALUE]], c(-4.93, 6.9, 2, 1.203,
                                           12.03, 23.95, 6.53, 2))
  
  # check errors
  expect_equal(result[[VAR_ERROR]], c(FALSE, FALSE, FALSE, TRUE,
                                      FALSE, FALSE, FALSE, TRUE))
  expect_equal(result[[VAR_DEC_ERROR]], c(FALSE, FALSE, FALSE, TRUE,
                                          FALSE, FALSE, FALSE, TRUE))
  
  # check errors with one-tailed test detection
  expect_equal(result_1tailed[[VAR_ERROR]], c(FALSE, FALSE, FALSE, TRUE,
                                              FALSE, FALSE, FALSE, FALSE))
  expect_equal(result_1tailed[[VAR_DEC_ERROR]], c(FALSE, FALSE, FALSE, TRUE,
                                                  FALSE, FALSE, FALSE, FALSE))
})

# pdfs in folder
test_that("statistics from all pdfs in a folder are correctly retrieved and parsed", {
  
  pdf_folder <- system.file("test_materials", package = "statcheck")
  
  result <- checkPDFdir(pdf_folder, messages = FALSE, subdir = FALSE)
  
  # extract 53 tests from 4 papers
  expect_equal(nrow(result), 53)
  expect_equal(length(unique(result[[VAR_SOURCE]])), 4)
  
})

# tests concerning parsing stats from html files ------------------------------

# individual html files
test_that("statistics from a html are correctly retrieved and parsed", {
  
  html_file <- system.file("test_materials/nuijten.html",
                          package = "statcheck")
  
  result <- checkHTML(html_file, messages = FALSE)
  result_1tailed <- checkHTML(html_file, messages = FALSE, OneTailedTxt = TRUE)
  
  # extract 6 tests from paper
  expect_equal(nrow(result), 6)
  expect_equal(as.character(result[[VAR_TYPE]]), c("t", "Chi2", "t", "F", "F", "t"))
  expect_equal(result[[VAR_TEST_VALUE]], c(-4.93, 6.9, 2, 1.203, 12.03, 2))
  
  # check errors
  expect_equal(result[[VAR_ERROR]], c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE))
  expect_equal(result[[VAR_DEC_ERROR]], c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE))

  # check errors with one-tailed test detection
  expect_equal(result_1tailed[[VAR_ERROR]], c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(result_1tailed[[VAR_DEC_ERROR]], c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))
  
})

# htmls in folder
test_that("statistics from all htmls in a folder are correctly retrieved and parsed", {
  
  html_dir <- system.file("test_materials", package = "statcheck")
  
  result <- checkHTMLdir(html_dir, messages = FALSE, subdir = FALSE)
  result_1tailed <- checkHTMLdir(html_dir, messages = FALSE, subdir = FALSE, OneTailedTxt = TRUE)
  
  # extract 6 tests from paper
  expect_equal(nrow(result), 6)
  expect_equal(as.character(result[[VAR_TYPE]]), c("t", "Chi2", "t", "F", "F", "t"))
  expect_equal(result[[VAR_TEST_VALUE]], c(-4.93, 6.9, 2, 1.203, 12.03, 2))
  
  # check errors
  expect_equal(result[[VAR_ERROR]], c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE))
  expect_equal(result[[VAR_DEC_ERROR]], c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE))
  
  # check errors with one-tailed test detection
  expect_equal(result_1tailed[[VAR_ERROR]], c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(result_1tailed[[VAR_DEC_ERROR]], c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))
  
})

# tests concerning parsing stats from all files ------------------------------

# pdfs and htmls in folder
test_that("statistics from all pdfs and htmls in a folder are correctly retrieved and parsed", {
  
  dir <- system.file("test_materials", package = "statcheck")
  
  result <- checkdir(dir, subdir = FALSE, messages = FALSE)
 
  # extract 59 tests (6 from html and 53 from pdf)
  expect_equal(nrow(result), 59)
})


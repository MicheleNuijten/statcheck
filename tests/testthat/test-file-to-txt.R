context("Check if statistics from an article are correctly retrieved and parsed")

# tests concerning parsing stats from pdf files ------------------------------

# individual pdf files
# pdftools
test_that("pdftools correctly retrieves and parses statistics from a pdf", {
  
  # load the pdf file of Nuijten et al. 2016
  # https://doi.org/10.3758/s13428-015-0664-2
  pdf_file <- system.file("test_materials/nuijten.pdf",
                          package = "statcheck")
  
  # skip test if file is not available
  if(pdf_file == "") skip("Test file not available.")
  
  # load the reference file with manually extracted statistics
  # load_manual is a helper function written for this package
  manual <- load_manual(path_manual = "test_materials/manually_extracted_stats.csv", 
                        file_id = "nuijten_pdf", apa = TRUE, typesetting_issues = FALSE)
  
  result <- checkPDF(pdf_file, messages = FALSE)
  result_1tailed <- checkPDF(pdf_file, messages = FALSE,
                             OneTailedTxt = TRUE)
  
  # extract 7 tests from paper
  expect_equal(nrow(result), 
               nrow(manual))
  expect_equal(as.character(result[[VAR_TYPE]]), 
               manual$test_type)
  expect_equal(as.character(result[[VAR_TEST_VALUE]]), 
               manual$test_value)
  
  # check errors
  expect_equal(as.numeric(result[[VAR_ERROR]]), 
               manual$error)
  expect_equal(as.numeric(result[[VAR_DEC_ERROR]]), 
               manual$decision_error)
  
  # check errors with one-tailed test detection
  expect_equal(as.numeric(result_1tailed[[VAR_ERROR]]), 
               manual$error_1tail)
  expect_equal(as.numeric(result_1tailed[[VAR_DEC_ERROR]]), 
               manual$decision_error_1tail)
  
})

# pdfs in folder
test_that("stats from all pdfs in a folder are correctly retrieved & parsed", {
  
  # this folder contains 4 "fake" pdf papers and 3 "fake" html/htm papers
  # one of the pdf papers doesn't contain any stats
  pdf_dir <- system.file("test_materials/test_dir", package = "statcheck")
  
  # skip test if files are not available
  if(!any(grepl(".pdf", list.files(pdf_dir)))) skip("Test files not available.")
  
  # load the reference file with manually extracted statistics
  manual <- 
    load_manual(path_manual = "test_materials/test_dir/manually_extracted_stats_testpapers.csv",
                apa = TRUE, file_type = "pdf")
  
  result <- checkPDFdir(pdf_dir, messages = FALSE, subdir = FALSE)
  
  # extract 11 tests from 3 papers
  expect_equal(nrow(result), 
               nrow(manual))
  expect_equal(length(unique(result[[VAR_SOURCE]])), 
               length(unique(manual$paper_id)))
  
})


# tests concerning parsing stats from html files ------------------------------

# individual html files
test_that("statistics from a html are correctly retrieved and parsed", {
  
  html_file <- system.file("test_materials/nuijten.html",
                           package = "statcheck")
  
  # skip test if file is not available
  if(html_file == "") skip("Test file not available.")
  
  # load the reference file with manually extracted statistics
  manual <- load_manual(path_manual = "test_materials/manually_extracted_stats.csv", 
                        file_id = "nuijten_html", apa = TRUE, typesetting_issues = FALSE)
  
  result <- checkHTML(html_file, messages = FALSE)
  result_1tailed <- checkHTML(html_file, messages = FALSE, OneTailedTxt = TRUE)
  
  # extract 6 tests from paper
  expect_equal(nrow(result), 
               nrow(manual))
  expect_equal(as.character(result[[VAR_TYPE]]), 
               manual$test_type)
  expect_equal(as.character(result[[VAR_TEST_VALUE]]), 
               manual$test_value)
  
  # check errors
  expect_equal(as.numeric(result[[VAR_ERROR]]), 
               manual$error)
  expect_equal(as.numeric(result[[VAR_DEC_ERROR]]), 
               manual$decision_error)
  
  # check errors with one-tailed test detection
  expect_equal(as.numeric(result_1tailed[[VAR_ERROR]]), 
               manual$error_1tail)
  expect_equal(as.numeric(result_1tailed[[VAR_DEC_ERROR]]), 
               manual$decision_error_1tail)
  
})

# htmls in folder
test_that("stats from all htmls in a folder are correctly retrieved & parsed", {
  
  # this folder contains 4 "fake" pdf papers and 3 "fake" html/htm papers
  # one of the pdf papers doesn't contain any stats
  html_dir <- system.file("test_materials/test_dir", package = "statcheck")
  
  # skip test if files are not available
  if(!any(grepl(".htm*", list.files(html_dir)))) skip("Test files not available.")
  
  # load the reference file with manually extracted statistics
  manual <- 
    load_manual(path_manual = "test_materials/test_dir/manually_extracted_stats_testpapers.csv",
                apa = TRUE, file_type = "html")
  
  result <- checkHTMLdir(html_dir, messages = FALSE, subdir = FALSE)
  
  # extract 11 tests from 3 papers
  expect_equal(nrow(result), 
               nrow(manual))
  expect_equal(length(unique(result[[VAR_SOURCE]])), 
               length(unique(manual$paper_id)))
  
})

# tests concerning parsing stats from all files ------------------------------

# pdfs and htmls in folder
test_that("stats from all pdfs and htmls in a folder are correctly retrieved 
          & parsed", {
            
            dir <- system.file("test_materials/test_dir", package = "statcheck")
            
            # skip test if files are not available
            if(!any(grepl(".htm*|.pdf", list.files(dir)))) skip("Test files not available.")
            
            # load the reference file with manually extracted statistics
            manual <- 
              load_manual(path_manual = "test_materials/test_dir/manually_extracted_stats_testpapers.csv",
                          apa = TRUE, file_type = "all")
            
            result <- checkdir(dir, subdir = FALSE, messages = FALSE)
            
            # extract 2*11 tests from 2*3 papers
            # extract 11 tests from 3 papers
            expect_equal(nrow(result), 
                         nrow(manual))
            expect_equal(length(unique(result[[VAR_SOURCE]])), 
                         length(unique(manual$paper_id)))
          })


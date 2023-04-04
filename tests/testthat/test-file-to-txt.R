context("Check if statistics from an article are correctly retrieved & parsed")

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
  
  result <- checkPDF(pdf_file, method = "pdftools", messages = FALSE)
  result_1tailed <- checkPDF(pdf_file, method = "pdftools", messages = FALSE,
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

test_that("pdftools is called if method is not specified", {
  
  # load the pdf file of Nuijten et al. 2016
  # https://doi.org/10.3758/s13428-015-0664-2
  pdf_file <- system.file("test_materials/nuijten.pdf",
                          package = "statcheck")
  
  # skip test if file is not available
  if(pdf_file == "") skip("Test file not available.")
  
  result <- checkPDF(pdf_file, messages = FALSE)
  expect_false(nrow(result) == 0)
  
})

# xpdf

test_that("xpdf correctly retrieves and parses statistics from a pdf", {
  
  # load the pdf file of Nuijten et al. 2016
  # https://doi.org/10.3758/s13428-015-0664-2
  pdf_file <- system.file("test_materials/nuijten.pdf",
                          package = "statcheck")
  
  # skip test if file is not available
  if(pdf_file == "") skip("Test file not available.")
  
  # load the reference file with manually extracted statistics
  manual <- load_manual(path_manual = "test_materials/manually_extracted_stats.csv", 
                        file_id = "nuijten_pdf", apa = TRUE, typesetting_issues = FALSE)
  
  result <- checkPDF(pdf_file, method = "xpdf", messages = FALSE)
  result_1tailed <- checkPDF(pdf_file, method = "xpdf", messages = FALSE, 
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

# tests concerning pdf encoding ----------------------------------------------

# For all tests below, we compare statcheck output to manually extracted stats
# from papers with known encoding peculiarities. 
# See codebook_manually_extracted_stats.xlsx for details on the manual reference
# file.

# stats from mausbach et al. 2012, health psych (apa journal)
# DOI: 10.1037/a0027783
test_that("all stats from apa pdf are extracted with pdftools", {
  
  # SKIP IF ARTICLE IS NOT AVAILABLE
  # THE ARTICLE CANNOT BE UPLOADED TO GITHUB/CRAN BECAUSE OF COPYRIGHT 
  # RESTRICTIONS
  pdf_file <- system.file("test_materials/mausbach.pdf", package = "statcheck") 
  
  if(pdf_file == ""){
    skip("pdf article not available for testing, because of copyright 
         restrictions")
  }
  
  # reference file with manually extracted statistics
  manual <- load_manual(path_manual = "test_materials/manually_extracted_stats.csv", 
                        file_id = "mausbach_pdf", apa = TRUE, 
                        pdf_conversion_issues = TRUE, typesetting_issues = FALSE)
  
  # in this pdf, =, <, and - are wrongly encoded
  # the method pdftools function in statcheck should be able to deal with this
  result <- checkPDF(pdf_file, method = "pdftools", messages = FALSE)
  
  # compare results statcheck with manually extracted stats
  expect_equal(nrow(manual), nrow(result))
  expect_equal(manual$test_value, as.character(result$test_value))
})


# stats from costa et al. 2018, jrnl environmental psych (elsevier journal)
# DOI: 10.1016/j.jenvp.2017.12.004
test_that("all stats from elsevier pdf are extracted with pdftools", {
  
  # SKIP IF ARTICLE IS NOT AVAILABLE
  # THE ARTICLE CANNOT BE UPLOADED TO GITHUB/CRAN BECAUSE OF COPYRIGHT 
  # RESTRICTIONS
  pdf_file <- system.file("test_materials/costa.pdf", package = "statcheck") 
  
  if(pdf_file == ""){
    skip("pdf article not available for testing, because of copyright 
         restrictions")
  }
  
  # reference file with manually extracted statistics
  manual <- load_manual(path_manual = "test_materials/manually_extracted_stats.csv", 
                        file_id = "costa_pdf", apa = TRUE, 
                        pdf_conversion_issues = TRUE, typesetting_issues = FALSE)
  
  result <- checkPDF(pdf_file, method = "pdftools", messages = FALSE)
  
  # compare results statcheck with manually extracted stats
  expect_equal(nrow(manual), nrow(result))
  expect_equal(manual$test_value, as.character(result$test_value))
  
})


# stats from todd et al. 2011, jrnl exp soc psych (elsevier journal)
# DOI: 10.1016/j.jesp.2010.08.00
test_that("b and N in a NHST result are read as < and >", {
  
  # SKIP IF ARTICLE IS NOT AVAILABLE
  # THE ARTICLE CANNOT BE UPLOADED TO GITHUB/CRAN BECAUSE OF COPYRIGHT 
  # RESTRICTIONS
  pdf_file <- system.file("test_materials/todd.pdf", package = "statcheck") 
  
  if(pdf_file == ""){
    skip("pdf article not available for testing, because of copyright 
         restrictions")
  }
  
  # reference file with manually extracted statistics
  manual <- load_manual(path_manual = "test_materials/manually_extracted_stats.csv", 
                        file_id = "todd_pdf", apa = TRUE, 
                        pdf_conversion_issues = TRUE, typesetting_issues = FALSE)

  result <- checkPDF(pdf_file, method = "pdftools", messages = FALSE)
  
  # compare results statcheck with manually extracted stats
  expect_equal(nrow(manual), nrow(result))
  expect_equal(manual$test_value, as.character(result$test_value))
  
})


# stats from zhang et al. 2010, jrnl of consumer research (oxford university press)
# DOI: 10.1086/655417
test_that("p and ! in a NHST result are read as = and <", {
  
  # SKIP IF ARTICLE IS NOT AVAILABLE
  # THE ARTICLE CANNOT BE UPLOADED TO GITHUB/CRAN BECAUSE OF COPYRIGHT 
  # RESTRICTIONS
  pdf_file <- system.file("test_materials/zhang.pdf", package = "statcheck") 
  
  if(pdf_file == ""){
    skip("pdf article not available for testing, because of copyright 
         restrictions")
  }
  
  # reference file with manually extracted statistics
  manual <- load_manual(path_manual = "test_materials/manually_extracted_stats.csv", 
                        file_id = "zhang_pdf", apa = TRUE, 
                        pdf_conversion_issues = TRUE, typesetting_issues = FALSE)
  
  result <- checkPDF(pdf_file, method = "pdftools", messages = FALSE)
  
  # compare results statcheck with manually extracted stats
  expect_equal(nrow(manual), nrow(result))
  expect_equal(manual$test_value, as.character(result$test_value))
  
})

# stats from sandoval et al. (2017), child development
# DOI: 10.1111/cdev.12723
test_that("cases with unusual spacing don't cause errors", {
  
  # SKIP IF ARTICLE IS NOT AVAILABLE
  # THE ARTICLE CANNOT BE UPLOADED TO GITHUB/CRAN BECAUSE OF COPYRIGHT 
  # RESTRICTIONS
  pdf_file <- system.file("test_materials/sandoval.pdf", package = "statcheck") 
  
  if(pdf_file == ""){
    skip("pdf article not available for testing, because of copyright 
         restrictions")
  }
  
  # reference file with manually extracted statistics
  manual <- load_manual(path_manual = "test_materials/manually_extracted_stats.csv", 
                        file_id = "sandoval_pdf", apa = TRUE, 
                        pdf_conversion_issues = TRUE, typesetting_issues = FALSE)
  
  xpdf <- checkPDF(pdf_file, method = "xpdf", messages = FALSE)
  pdftools <- checkPDF(pdf_file, method = "pdftools", messages = FALSE)
  
  expect_equal(nrow(xpdf), nrow(manual))
  expect_equal(nrow(pdftools), nrow(manual))
  
})


# stats from Aloe et al. (2009), Educational Researcher
# DOI: 10.3102/0013189X09353939
test_that("subscripts are not read as test statistics", {
  
  # SKIP IF ARTICLE IS NOT AVAILABLE
  # THE ARTICLE CANNOT BE UPLOADED TO GITHUB/CRAN BECAUSE OF COPYRIGHT 
  # RESTRICTIONS
  pdf_file <- system.file("test_materials/aloe.pdf", package = "statcheck") 
  
  if(pdf_file == ""){
    skip("pdf article not available for testing, because of copyright 
         restrictions")
  }
  
  # reference file with manually extracted statistics
  manual <- load_manual(path_manual = "test_materials/manually_extracted_stats.csv", 
                        file_id = "aloe_pdf", apa = TRUE, 
                        pdf_conversion_issues = TRUE, typesetting_issues = FALSE)
  
  result <- checkPDF(pdf_file, method = "pdftools", messages = FALSE)
  
  # compare results statcheck with manually extracted stats
  expect_equal(nrow(manual), nrow(result))
  expect_equal(manual$test_value, as.character(result$test_value))
  
})


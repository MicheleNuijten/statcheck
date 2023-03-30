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
  
  result <- checkPDF(pdf_file, method = "pdftools", messages = FALSE)
  result_1tailed <- checkPDF(pdf_file, method = "pdftools", messages = FALSE,
                             OneTailedTxt = TRUE)
  
  # extract 8 tests from paper
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

test_that("pdftools is called if method is not specified", {
  
  # load the pdf file of Nuijten et al. 2016
  # https://doi.org/10.3758/s13428-015-0664-2
  pdf_file <- system.file("test_materials/nuijten.pdf",
                          package = "statcheck")
  
  # skip test if file is not available
  if(pdf_file == "") skip("Test file not available.")
  
  result <- checkPDF(pdf_file, messages = FALSE)
  expect_equal(nrow(result), 8)
  
})

# xpdf

test_that("xpdf correctly retrieves and parses statistics from a pdf", {
  
  # load the pdf file of Nuijten et al. 2016
  # https://doi.org/10.3758/s13428-015-0664-2
  pdf_file <- system.file("test_materials/nuijten.pdf",
                          package = "statcheck")
  
  # skip test if file is not available
  if(pdf_file == "") skip("Test file not available.")
  
  result <- checkPDF(pdf_file, method = "xpdf", messages = FALSE)
  result_1tailed <- checkPDF(pdf_file, method = "xpdf", messages = FALSE, 
                             OneTailedTxt = TRUE)
  
  # extract 8 tests from paper
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
test_that("stats from all pdfs in a folder are correctly retrieved & parsed", {
  
  # this folder contains 4 "fake" pdf papers and 3 "fake" html/htm papers
  # one of the pdf papers doesn't contain any stats
  pdf_folder <- system.file("test_materials/test_dir", package = "statcheck")
  
  # skip test if files are not available
  if(!any(grepl(".pdf", list.files(pdf_folder)))) skip("Test files not available.")
  
  result <- checkPDFdir(pdf_folder, messages = FALSE, subdir = FALSE)
  
  # extract 11 tests from 3 papers
  expect_equal(nrow(result), 11)
  expect_equal(length(unique(result[[VAR_SOURCE]])), 3)
  
})

# tests concerning parsing stats from html files ------------------------------

# individual html files
test_that("statistics from a html are correctly retrieved and parsed", {
  
  html_file <- system.file("test_materials/nuijten.html",
                          package = "statcheck")
  
  # skip test if file is not available
  if(html_file == "") skip("Test file not available.")

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
test_that("stats from all htmls in a folder are correctly retrieved & parsed", {
  
  html_dir <- system.file("test_materials/test_dir", package = "statcheck")
  
  # skip test if files are not available
  if(!any(grepl(".htm*", list.files(html_dir)))) skip("Test files not available.")
  
  result <- checkHTMLdir(html_dir, messages = FALSE, subdir = FALSE)
  
  # extract 11 tests from 3 papers
  expect_equal(nrow(result), 11)
  expect_equal(length(unique(result[[VAR_SOURCE]])), 3)
  
})

# tests concerning parsing stats from all files ------------------------------

# pdfs and htmls in folder
test_that("stats from all pdfs and htmls in a folder are correctly retrieved 
          & parsed", {
            
            dir <- system.file("test_materials/test_dir", package = "statcheck")
            
            result <- checkdir(dir, subdir = FALSE, messages = FALSE)
            
            # extract 2*11 tests from 2*3 papers
            expect_equal(nrow(result), 22)
            expect_equal(length(unique(result[[VAR_SOURCE]])), 6)
          })

# tests concerning pdf encoding ----------------------------------------------

# for all tests below, we compare statcheck output to manually extracted stats
# from papers with known encoding peculiarities.
# the csv files contain all comlete NHST results reported in full text, so not
# necessarily only APA results.

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
  manual <- read.csv(
    system.file("test_materials/mausbach_manual.csv", package = "statcheck"), 
    header = TRUE)
  
  # remove bottom two "summary" rows
  reference <- manual[manual$raw_result != "", ]
  
  # in this pdf, =, <, and - are wrongly encoded
  # the method pdftools function in statcheck should be able to deal with this
  result <- checkPDF(pdf_file, method = "pdftools", messages = FALSE)
  
  # compare results statcheck with manually extracted stats
  expect_equal(nrow(reference), nrow(result))
  expect_equal(reference$test_value, result$test_value)  
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
  manual <- read.csv(
    system.file("test_materials/costa_manual.csv", package = "statcheck"), 
    header = TRUE)
  
  result <- checkPDF(pdf_file, method = "pdftools", messages = FALSE)
  
  # 3 results in the pdf have incorrect (non-APA) spacing: 
  # t(191) = 8.22, p < . 001
  # t(191) = 9.54, p <. 001
  # t(191) = 4.55, p < . 001
  # remove these from the manual reference for better comparison
  
  reference <- manual[-c(2, 3, 8), ]
  
  # remove bottom two "summary" rows
  reference <- reference[reference$raw_result != "", ]
  
  # compare results statcheck with manually extracted stats
  expect_equal(nrow(reference), nrow(result))
  expect_equal(reference$test_value, result$test_value)
  
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
  manual <- read.csv(
    system.file("test_materials/todd_manual.csv", package = "statcheck"), 
    header = TRUE)
  
  # 3 results are non apa
  # Fs(1, 248)b1.16, psN.28
  # Fs(5, 670)b1.46, psN.20.
  # Fs(1, 702)b1.55, psN.21
  # remove from manual reference
  reference <- manual[-c(9, 18, 20), ]
  
  # remove bottom two "summary" rows
  reference <- reference[reference$raw_result != "", ]
  
  result <- checkPDF(pdf_file, method = "pdftools", messages = FALSE)
  
  # compare results statcheck with manually extracted stats
  expect_equal(nrow(reference), nrow(result))
  expect_equal(reference$test_value, result$test_value)
  
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
  manual <- read.csv(
    system.file("test_materials/zhang_manual.csv", package = "statcheck"), 
    header = TRUE)
  
  # 4 results are not read for unclear reasons
  # all similar results are read, nothing went wrong with the columns,
  # and there is no weird spacing etc.
  
  # x2 (1, N p 3,479) p 3.67, p p .05
  # F(1, 43) p 4.17, p ! .05 
  # t(42) p 3.19, p ! .01
  # F(1, 82) p 6.29, p p .01
  
  # 1 result was reported in non-apa style
  # t(43) p \0011.19, NS
  reference <- manual[-c(8, 31, 38, 41, 44),]
  
  # remove bottom two "summary" rows
  reference <- reference[reference$raw_result != "", ]
  
  # also not read by statcheck: 
  # F(1, 21) p 2.95, NS 
  # t(43) p \001.50, NS
  # t(43) p  2.57, p ! .05.
  # t(43) p  3.01, p ! .01
  # t(42) p \001.83, NS
  # t(42) p 1.09, NS 
  
  result <- checkPDF(pdf_file, method = "pdftools", messages = FALSE)
  
  # compare results statcheck with manually extracted stats
  expect_equal(nrow(reference), nrow(result))
  expect_equal(reference$test_value, result$test_value)
  
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
  
  xpdf <- checkPDF(pdf_file, method = "xpdf", messages = FALSE)
  pdftools <- checkPDF(pdf_file, method = "pdftools", messages = FALSE)
  
  expect_equal(nrow(xpdf), 6)
  expect_equal(nrow(pdftools), 12)
  
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
  manual <- read.csv(
    system.file("test_materials/aloe_manual.csv", package = "statcheck"), 
    header = TRUE)
  
  # 4 QT and 1 QError are extracted manually, but can't be detected by statcheck
  # 1 test has two line breaks in the result: "QBetween(1) = 14.59, p =\r\n.0001"
  # 2 tests were reported in a table incompletely
  # remove these cases
  reference <- manual[-c(1, 2, 3, 8, 10, 13, 24), ]
  
  result <- checkPDF(pdf_file, method = "pdftools", messages = FALSE)
  
  # compare results statcheck with manually extracted stats
  expect_equal(nrow(reference), nrow(result))
  expect_equal(reference$test_value, result$test_value)
  
})


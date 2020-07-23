context("Check if statistics from an article are correctly retrieved & parsed")

# tests concerning parsing stats from pdf files ------------------------------

# individual pdf files
test_that("statistics from a pdf are correctly retrieved and parsed", {
  
  pdf_file <- 
    system.file("test_materials/test_dir/NuijtenEtAl_2016_ReportingErrorsPsychology.pdf",
                          package = "statcheck")
  
  # xpdf
  
  result <- checkPDF(pdf_file, messages = FALSE)
  result_1tailed <- checkPDF(pdf_file, messages = FALSE, OneTailedTxt = TRUE)
  
  # extract 4 tests from paper
  # note: 1 APA F-test is not extracted with xpdf, because it's surrounded by
  # quotation marks, that xpdf transforms into the letter B, turning the result
  # into "BF(2, 56) = 12.03, p < .001^". The B makes that statcheck doesn't 
  # recognize F as the start of a statistic. This problem does not occur with
  # pdftools (see test below)
  expect_equal(nrow(result), 4)
  expect_equal(as.character(result[[VAR_TYPE]]), c("Chi2", "t", "Chi2", "t"))
  expect_equal(result[[VAR_TEST_VALUE]], c(6.9, 2, 6.53, 2))
  
  # check errors
  expect_equal(result[[VAR_ERROR]], c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(result[[VAR_DEC_ERROR]], c(FALSE, FALSE, FALSE, TRUE))
  
  # check errors with one-tailed test detection
  expect_equal(result_1tailed[[VAR_ERROR]], c(FALSE, FALSE, FALSE, FALSE))
  expect_equal(result_1tailed[[VAR_DEC_ERROR]], c(FALSE, FALSE, FALSE, FALSE))
  
  # pdftools
  result <- checkPDF(pdf_file, method = "pdftools", messages = FALSE)
  expect_equal(nrow(result), 5)
  
})

# pdfs in folder
test_that("stats from all pdfs in a folder are correctly retrieved & parsed", {
  
  pdf_folder <- system.file("test_materials/test_dir", package = "statcheck")
  
  result <- checkPDFdir(pdf_folder, messages = FALSE, subdir = FALSE)
  result_1tailed <- checkPDFdir(pdf_folder, messages = FALSE, subdir = FALSE, 
                                OneTailedTxt = TRUE)
  
  # extract 4 tests from paper
  expect_equal(nrow(result), 4)
  expect_equal(as.character(result[[VAR_TYPE]]), c("Chi2", "t", "Chi2", "t"))
  expect_equal(result[[VAR_TEST_VALUE]], c(6.9, 2, 6.53, 2))
  
  # check errors
  expect_equal(result[[VAR_ERROR]], c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(result[[VAR_DEC_ERROR]], c(FALSE, FALSE, FALSE, TRUE))
  
  # check errors with one-tailed test detection
  expect_equal(result_1tailed[[VAR_ERROR]], c(FALSE, FALSE, FALSE, FALSE))
  expect_equal(result_1tailed[[VAR_DEC_ERROR]], c(FALSE, FALSE, FALSE, FALSE))
  
  # extract 5 tests when pdftools is used (pdftools doesn't wrongly convert
  # quotation marks around F test)
  result_pdftools <- checkPDFdir(pdf_folder, method = "pdftools", 
                                 messages = FALSE, subdir = FALSE)
  expect_equal(nrow(result_pdftools), 5)
  
})

# tests concerning parsing stats from html files ------------------------------

# individual html files
test_that("statistics from a html are correctly retrieved and parsed", {
  
  html_file <- system.file(
    "test_materials/test_dir/NuijtenEtAl_2016_ReportingErrorsPsychology.html",
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
test_that("stats from all htmls in a folder are correctly retrieved & parsed", {
  
  html_dir <- system.file("test_materials/test_dir", package = "statcheck")
  
  result <- checkHTMLdir(html_dir, messages = FALSE, subdir = FALSE)
  result_1tailed <- checkHTMLdir(html_dir, messages = FALSE, subdir = FALSE, 
                                 OneTailedTxt = TRUE)
  
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
test_that("stats from all pdfs and htmls in a folder are correctly retrieved 
          & parsed", {
  
  dir <- system.file("test_materials/test_dir", package = "statcheck")
  
  result <- checkdir(dir, subdir = FALSE, messages = FALSE)
  
  # extract 10 tests from papers
  # one test (delta G) isn't detected in the html because it's in a table
  # and the table isn't saved in the html file
  expect_equal(nrow(result), 10)
})

# tests concerning pdf encoding ----------------------------------------------

# stats from mausbach et al. 2012, health psych (apa journal)
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
  
  # 1 extracted statistic is not included because of an error in the conversion 
  # from raw text to text in columns. This seems to be due to the pdf itself,
  # not an error in the pdf_columns function. For some reason, there are not
  # enough whitespaces between some sentences in different columns when 
  # converting.
  
  reference <- manual[-which(manual$test_value == -1.34), ]
  
  # in this pdf, =, <, and - are wrongly encoded
  # the method pdftools function in statcheck should be able to deal with this
  result <- checkPDF(pdf_file, method = "pdftools", messages = FALSE)
  
  # reshuffle rows in result
  # this is necessary because of the same error in restoring the columns in the 
  # pdf as mentioned above. One sentence (with a result) is moved to an 
  # incorrect place in the document
  result_ordered <- result[c(2:8, 1, 9:28), ]
  
  # compare results statcheck with manually extracted stats
  expect_equal(nrow(reference), nrow(result_ordered))
  expect_equal(reference$test_value, result_ordered$test_value)
  
})


# stats from costa et al. 2018, jrnl environmental psych (elsevier journal)
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
  
  # compare results statcheck with manually extracted stats
  expect_equal(nrow(reference), nrow(result))
  expect_equal(reference$test_value, result$test_value)
  
})

# read the letters b and N in a NHST result as < and >
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
  
  result <- checkPDF(pdf_file, method = "pdftools", messages = FALSE)
 
  # compare results statcheck with manually extracted stats
  expect_equal(nrow(reference), nrow(result))
  expect_equal(reference$test_value, result$test_value)
  
})


# read p and ! as = and <
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
  
  result <- checkPDF(pdf_file, method = "pdftools", messages = FALSE)
  
  # compare results statcheck with manually extracted stats
  expect_equal(nrow(reference), nrow(result))
  expect_equal(reference$test_value, result$test_value)
  
})

# don't throw errors in case of unusal spacing
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


# don't read QError as a correlation or QT as t-test
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

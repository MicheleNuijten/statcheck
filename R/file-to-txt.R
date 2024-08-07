# HTML TO TXT ------------------------------------------------------------------

getHTML <- function(x){
  
  strings <- lapply(x, function(fileName){
    con <- file(fileName)
    on.exit(close(con))
    raw_strings <- readChar(con, file.info(fileName)$size, useBytes = TRUE)
    return(raw_strings)
  })
  
  # Remove subscripts (except for p_rep)
  strings <- lapply(strings, gsub, pattern = "<sub>(?!rep).*?</sub>", replacement = "", perl = TRUE)
  
  # Remove HTML tags:
  strings <- lapply(strings, gsub, pattern = "<(.|\n)*?>", replacement = "")
  
  # encode everything in UTF-32 
  # this should ensure the same output accross multiple operating systems
  strings <- stringi::stri_enc_toutf32(strings) 
  
  # substitute a narrow no-break space (UTF-32 Decimal 8239) with a normal space 
  # (UTF-32 Decimal 61) [issue in JESP 2019 papers]
  strings <- lapply(strings, gsub, pattern = "8239",
                     replacement = "32", fixed = TRUE)
  
  # Revert back to UTF-8 encoding
  strings <- stringi::stri_enc_fromutf32(strings)
  
  # Replace html codes:
  # from: https://dev.w3.org/html5/html-author/charref 
  strings <- lapply(strings, gsub, pattern = "&#60;", replacement = "<", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&lt;", replacement = "<", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&LT;", replacement = "<", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#x0003C;", replacement = "<", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#x0003c;", replacement = "<", fixed = TRUE)
  
  strings <- lapply(strings, gsub, pattern = "&#61;", replacement = "=", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&equals;", replacement = "=", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#x0003D;", replacement = "=", fixed = TRUE)
  
  strings <- lapply(strings, gsub, pattern = "&#62;", replacement = ">", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&gt;", replacement = ">", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&GT;", replacement = ">", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#x0003E;", replacement = ">", fixed = TRUE)
  
  strings <- lapply(strings, gsub, pattern = "&#40;", replacement = "(", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#41;", replacement = ")", fixed = TRUE)
  
  strings <- lapply(strings, gsub, pattern = "&thinsp;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&nbsp;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&nnbsp;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#8239;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#x202F;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#32;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#160;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&ensp;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#8194;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&emsp;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#8195;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#8201;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&zwnj;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#8204;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&zwj;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#8205;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&lrm;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#8206;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&rlm;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#8207;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "\\s+", replacement = " ")
  
  strings <- lapply(strings, gsub, pattern = "\n", replacement = "")
  strings <- lapply(strings, gsub, pattern = "\r", replacement = "")
  
  
  strings <- lapply(strings, gsub, pattern = "&minus;", replacement = "-", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#x02212;", replacement = "-", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#8722;", replacement = "-", fixed = TRUE)
  
  strings <- lapply(strings, gsub, pattern = "&chi;", replacement = "X", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#x003C7;", replacement = "X", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#x003c7;", replacement = "X", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#967;", replacement = "X", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&Chi;", replacement = "X", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#x003A7;", replacement = "X", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#935;", replacement = "X", fixed = TRUE)

  return(strings)
}

# PDF TO TXT -------------------------------------------------------------------
getPDF <- function(x){
  
  txtfiles <- sapply(x, pdftools::pdf_text) 
  
  # encode everything in UTF-32 
  # this should ensure the same output accross multiple operating systems
  txtfiles <- stringi::stri_enc_toutf32(txtfiles) 
  
  # Replace known weird characters
  
  # substitute double solidous (UTF-32 Decimal 11005) with equal sign (UTF-32
  # Decimal 61) [issue in APA journals]
  txtfiles <- lapply(txtfiles, gsub, pattern = "11005",
                     replacement = "61", fixed = TRUE)
  
  # substitute 1/4 (UTF-32 decimal 188) with equal sign (UTF-32 Decimal 61);
  # [issue in Elsevier journal: Journal of Environmental Psychology]
  txtfiles <- lapply(txtfiles, gsub, pattern = "188",
                     replacement = "61", fixed = TRUE)
  
  # substitute U+2B0D (C++ \u2b0d; UTF-32 Decimal 11021) with less than
  # sign (UTF-32 Decimal 60) [issue in APA journals]
  txtfiles <- lapply(txtfiles, gsub, pattern = "11021",
                     replacement = "60", fixed = TRUE)
  
  # substitute ! (UTF-32 decimal 33) with less than sign (UTF-32 Decimal 60);
  # [issue in Oxford journal: Journal of Consumer Research]
  txtfiles <- lapply(txtfiles, gsub, pattern = "33",
                     replacement = "60", fixed = TRUE)
  
  # substitute U+2AFA (UTF-32 Decimal 11002) with HYPHEN-MINUS sign (UTF-32
  # Decimal 45) [issue in APA journals]
  txtfiles <- lapply(txtfiles, gsub, pattern = "11002",
                     replacement = "45", fixed = TRUE)
  
  # substitute U+2439 (C++ \u2439; UTF-32 Decimal 9273) with small greek chi
  # (UTF-32 Decimal 967) [issue in APA journals]
  txtfiles <- lapply(txtfiles, gsub, pattern = "9273",
                     replacement = "967", fixed = TRUE)
  
  # Revert to UTF-8 encoding
  txtfiles <- stringi::stri_enc_fromutf32(txtfiles)
  
  
  # Arrange text according to paper column layout
  txtfiles <- pdf_columns(txtfiles)
  
  # Paste the differente pages together, so that each pdf is converted to 
  # one string of text
  txtfiles <- stringr::str_c(unlist(txtfiles), collapse = "")
  
  
  
  # substitute the letter "b" in a NHST result for a "<". This is not feasible
  # in utf32 encoding, because making a regex that only substitutes the b in
  # a statistical result instead of ALL b's in the paper is very hard in 
  # utf32 encoding. [issue in Elsevier journal: JESP]
  txtfiles <- lapply(txtfiles, gsub, 
                     # don't match a b preceded by =<>, because the b itself 
                     # should be the comparison sign.
                     # only match a b followed by a number, that gives further
                     # proof that the b is in fact the comparison sign.
                     pattern = RGX_B_SMALLER,
                     replacement = "<", perl = TRUE)
  
  # substitute the letter "N" in a NHST result for a ">", for the same reason 
  # as above. [issue in Elsevier journal: JESP]
  txtfiles <- lapply(txtfiles, gsub, 
                     # don't match a N preceded by =<>, because the N itself 
                     # should be the comparison sign.
                     # only match a N followed by a number, that gives further
                     # proof that the N is in fact the comparison sign.
                     pattern = RGX_N_LARGER,
                     replacement = ">", perl = TRUE)
  
  # substitute the letter "p" that should be a "=". [issue in Oxford journal:
  # journal of consumer research]
  txtfiles <- lapply(txtfiles, gsub, 
                     # don't match a p preceded by a "," or a ",\\s", because 
                     # that is the actual p-value.
                     # only match a p followed by a number, that gives further
                     # proof that the p is in fact the comparison sign.
                     pattern = RGX_P_EQUAL,
                     replacement = "=", perl = TRUE)
  
  # substitute the letter "B" that should be a '"'. [issue in BRM]
  txtfiles <- lapply(txtfiles, gsub, 
                     # only match a B followed by a letter that could indicate
                     # a test statistic
                     pattern = RGX_B_QUOTE,
                     replacement = '"', perl = TRUE)
  
  
  return(txtfiles)
}


# helper function for getPDF() -------------------------------------------------

# This function helps maintaining the format of pdf files with a multiple 
# columns layout.
# Credits to:  
# https://github.com/fsingletonthorn/EffectSizeScraping/blob/master/R/pdf_process.R 
# for original function

true_false <- function(x, chars) {
  x > chars
}

pdf_columns <- function(x, pattern = "\\p{WHITE_SPACE}{3,}") {
  # \p{L} matches a single code point in the category "letter".
  # {3,} three or more
  
  # This function is slightly adapted from pdfsearch
  # see: https://github.com/lebebr01/pdfsearch/blob/master/R/split_pdf.r
  
  x_lines <- stringi::stri_split_lines(x)
  x_lines <- lapply(x_lines, gsub,
                    pattern = "^\\s{1,20}",
                    # ^ string that starts with
                    # \ creates regular expression containing following...
                    # \s matches any whitespace
                    # {1,20} between 1 and 20 of these [ in your case this will become +]
                    replacement = "")
  
  x_page <- lapply(
    x_lines,
    stringi::stri_split_regex,
    pattern = pattern,
    omit_empty = NA,
    simplify = TRUE
  )
  
  page_lines <- unlist(lapply(x_page, nrow))
  columns <- unlist(lapply(x_page, ncol))
  
  num_chars <- lapply(x_page, base::nchar)
  num_chars_tf <- lapply(num_chars, true_false, chars = 3)
  
  for (xx in seq_along(num_chars_tf)) {
    num_chars_tf[[xx]][is.na(num_chars_tf[[xx]])] <- FALSE
  }
  
  output <- lapply(seq_along(x_page), function(xx)
    x_page[[xx]][num_chars_tf[[xx]]])
  
  output <- lapply(output, paste, collapse = " ")
  return(output)
}
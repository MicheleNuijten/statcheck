extract_stats <- function(txt, apa_style, stat){
  
  # step 1: extract all NHST results from text -----------------------------------
  
  # specify whether to search for APA NHST results or also to include non-APA
  if(apa_style == TRUE){
    rgx_nhst <- RGX_NHST
  } else {
    rgx_nhst <- RGX_NHST_NONAPA
  }
  
  # if there are x NHST results in a text, this returns a character vector of 
  # length x
  nhst_raw <- extract_pattern(txt = txt,
                              # nhst is the regex for nhst results
                              # it came from the regex.R script within the package
                              pattern = rgx_nhst)
  
  # if there are no nhst results in the text, return an empty data frame
  if(is.null(nhst_raw)){
    return(data.frame(NULL))
  }
  
  # step 2: clean non-apa results ----------------------------------------------
  
  # when non-apa results are extracted, it needs to be "translated" in the apa
  # equivalent, to enable parsing of the result
  nhst_clean <- clean_non_apa(nhst_raw)
  
  
  # step 3: parse the extracted results ----------------------------------------
  
  # empty vectors and data frames to store results in
  # where relevant, force the output to be of type "character"
  test_type <- character()
  df_result <- data.frame(NULL)  # will contain both df1 and df2
  test_stats <- data.frame(NULL) # will contain both test comparison and value
  pvals <- data.frame(NULL)      # will contain both p-comparison and p-value
  
  # loop over all extracted, raw nhst results
  # each nhst result is parsed individually, in the order that they were extracted
  # from the text
  for(i in seq_along(nhst_clean)){
    
    # extract the test types from the nhst results 
    test_raw <- extract_pattern(txt = nhst_clean[i],
                                pattern = RGX_TEST_TYPE)
    
    # workaround for wrongly converted chi2 between parentheses:
    # in some very specific cases, chi2 tests can cause this function to break
    # if a chi2 is reported between parentheses "(X2(1) = 2.2, p < .05)", and 
    # the X symbol is not converted, the following text is read: "(2(1) = 2.2, p
    # < .05)". If this happens, 2 tests are extracted: " ", and "(2)", causing
    # an error
    # to solve this, only consider as a test_raw a string that matches the RGX
    # for chi2
    if(length(test_raw) > 1){
      
      chi2_matches <- grepl(RGX_CHI2, test_raw)
      
      # if one test_raw matches chi2, consider that test_raw
      if(sum(chi2_matches == 1)){
        test_raw <- test_raw[chi2_matches]
      } else {
        # if either more or none of the test_raws match chi2, skip this step
        next
      }
    }
    
    # classify the test types in standard classifications
    
    # for each test type, check where the vector with extracted, raw test types
    # matches the regex for each test type, and assign the appropriate category
    # the order of the classification matters: Q has to be tested first, 
    # otherwise the regex for t will also match Qwithin and Qbetween, because
    # both have a t in them. Similarly: first check for Qb, because Qbetween
    # also has a w in it
    if(!is.null(test_raw)){
      if (grepl(pattern = RGX_Q, x = test_raw)){
        
        # distinguish between Q, Qw, and Qb
        if(grepl(pattern = RGX_QB, x = test_raw)){
          test_type[i] <- "Qb"
        } else if (grepl(pattern = RGX_QW, x = test_raw)){
          test_type[i] <- "Qw"
        } else {
          test_type[i] <- "Q"
        }
      } else if(grepl(pattern = RGX_T, x = test_raw)){
        test_type[i] <- "t"
      } else if (grepl(pattern = RGX_F, x = test_raw)){
        test_type[i] <- "F"
      } else if (grepl(pattern = RGX_R, x = test_raw)){
        test_type[i] <- "r"
      } else if (grepl(pattern = RGX_Z, x = test_raw)){
        test_type[i] <- "Z"
      } else if (grepl(pattern = RGX_CHI2, x = test_raw, 
                       perl = TRUE)){
        test_type[i] <- "Chi2"
      }
    } else {
      # if test_raw == NULL, chances are that the result is a chi2
      # check if the nhst_raw matches the regex for chi2 and manually
      # add test type
      if(grepl(pattern = RGX_CHI2_DF, x = nhst_clean[i], 
               perl = TRUE)){
        test_type[i] <- "Chi2"
      }
    }
    
    
    # extract degrees of freedom
    
    dfs <- extract_df(raw = nhst_clean[i],
                      test_type = test_type[i])
    
    df_result <- rbind(df_result, dfs)
    
    # extract test comparison and test value 
    
    test <- extract_test_stats(raw = nhst_clean[i])
    
    test_stats <- rbind(test_stats, test)
    
    # extract p-comparison and p-value
    
    p <- extract_p_value(raw = nhst_clean[i])
    
    pvals <- rbind(pvals, p)
    
  }
  
  # create final data frame ------------------------------------------------------
  
  nhst_parsed <- data.frame(
    # return raw result without leading/trailing whitespaces
    Raw = trimws(nhst_raw, which = "both"),
    Statistic = test_type,
    df1 = df_result$df1,
    df2 = df_result$df2,
    Test.Comparison = test_stats$test_comp,
    Value = test_stats$test_value,
    testdec = test_stats$test_dec,
    Reported.Comparison = pvals$p_comp,
    Reported.P.Value = pvals$p_value,
    dec = pvals$p_dec,
    stringsAsFactors = FALSE)
  
  if (nrow(nhst_parsed) > 0) {
    
    # remove p values greater than one
    nhst_parsed <- nhst_parsed[nhst_parsed$Reported.P.Value <= 1 |
                                 is.na(nhst_parsed$Reported.P.Value), ]
    
    # only return selected stats
    # to that end, rename test-types to match argument stat
    types <- as.vector(nhst_parsed$Statistic)
    
    types[types == "r"] <- "cor"
    types[types == "Chi2"] <- "chisq"
    types[types == "Z"] <- "Z"
    types[types == "Qw"| types == "Qb"] <- "Q"
    
    # only return rows where the test_type matches the selected stats
    nhst_parsed <- nhst_parsed[types %in% stat, ]
    
  }
  
  class(nhst_parsed) <- c("statcheck", "data.frame")
  
  return(nhst_parsed)
}

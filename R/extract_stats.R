extract_stats <- function(txt, stat){
  
  # step 1: extract all NHST results from text -----------------------------------
  
  # if there are x NHST results in a text, this returns a character vector of 
  # length x
  nhst_raw <- extract_pattern(txt = txt,
                              # nhst is the regex for nhst results
                              # it came from the regex.R script within the package
                              pattern = RGX_NHST) 
  
  # step 2: parse the extracted results ------------------------------------------
  
  # empty vectors and data frames to store results in
  # where relevant, force the output to be of type "character"
  test_type <- character()
  df_result <- data.frame(NULL)  # will contain both df1 and df2
  test_stats <- data.frame(NULL) # will contain both test comparison and value
  pvals <- data.frame(NULL)      # will contain both p-comparison and p-value
  
  # loop over all extracted, raw nhst results
  # each nhst result is parsed individually, in the order that they were extracted
  # from the text
  for(i in seq_along(nhst_raw)){
    
    # extract the test types from the nhst results 
    test_raw <- extract_pattern(txt = nhst_raw[i],
                                pattern = RGX_TEST_TYPE)
    
    # classify the test types in standard classifications
    
    # for each test type, check where the vector with extracted, raw test types
    # matches the regex for each test type, and assign the appropriate category
    # the order of the classification matters: Q has to be tested first, 
    # otherwise the regex for t will also match Qwithin and Qbetween, because
    # both have a t in them. Similarly: first check for Qb, because Qbetween
    # also has a w in it
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
    } else if (grepl(pattern = RGX_CHI2, x = test_raw)){
      test_type[i] <- "Chi2"
    }
    
    # extract degrees of freedom
    dfs <- extract_df(raw = nhst_raw[i],
                      test_type = test_type[i])
    
    df_result <- rbind(df_result, dfs)
    
    # extract test comparison and test value 
    
    test <- extract_test_stats(raw = nhst_raw[i])
    
    test_stats <- rbind(test_stats, test)
    
    # extract p-comparison and p-value
    
    p <- extract_p_value(raw = nhst_raw[i])
    
    pvals <- rbind(pvals, p)
    
  }
  
  # create final data frame ------------------------------------------------------
  
  nhst_parsed <- data.frame(Raw = nhst_raw,
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
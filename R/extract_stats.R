extract_stats <- function(txt, stat){
  
  # step 1: extract all NHST results from text -----------------------------------
  
  # if there are x NHST results in a text, this returns a character vector of 
  # length x
  nhst_raw <- extract_pattern(txt = txt,
                              # nhst is the regex for nhst results
                              # it came from the regex.R script within the package
                              pattern = RGX_NHST,
                              # don't ignore case here: we only want to extract
                              # stats reported in a specific way (e.g., t, not T)
                              ignore.case = FALSE) 
  
  # if there are no nhst results in the text, return an empty data frame
  if(is.null(nhst_raw)){
    return(data.frame(NULL))
  }
  
  # step 2: parse the extracted results ------------------------------------------
  
  # Collect each parsed result once, then bind the rows after the loop.
  # Repeatedly growing data frames copies all preceding results each time.
  test_type <- character(length(nhst_raw))
  df_result <- vector("list", length(nhst_raw))
  test_stats <- vector("list", length(nhst_raw))
  pvals <- vector("list", length(nhst_raw))
  
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
      
      df_result[[i]] <- dfs
    
    # extract test comparison and test value 
    
    test <- extract_test_stats(raw = nhst_raw[i])
    
    # ignore any cases with more than 1 test comparison (e.g., z >= 2)
    if(nrow(test) > 1){
      test <- data.frame(test_comp = NA,
                         test_value = NA,
                         test_dec = NA)
    } 
    
    test_stats[[i]] <- test
    
    # extract p-comparison and p-value
    
    p <- extract_p_value(raw = nhst_raw[i])
    
    # ignore any cases with more than 1 test comparison (e.g., p >= .01)
    if(nrow(p) > 1){
      p <- data.frame(p_comp = NA,
                      p_value = NA,
                      p_dec = NA)
    }
    
    pvals[[i]] <- p
    
    
  }
  
  # create final data frame ------------------------------------------------------

  df_result <- do.call(rbind, df_result)
  test_stats <- do.call(rbind, test_stats)
  pvals <- do.call(rbind, pvals)
  
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
    
    # remove correlations greater than one or smaller than -1
    # reason: there is a risk that statcheck simply misread this stat
    # this means that statcheck will not flag such incorrect correlations,
    # which you could also consider a disadvantage
    nhst_parsed <- nhst_parsed[!(nhst_parsed$Statistic == "r" & 
                                 (nhst_parsed$Value > 1|
                                   nhst_parsed$Value < -1)), ]
    
    # remove rows with missing test values
    # reason: this can happen when a test statistic has a weird minus and a 
    # space in front of it. statcheck can't convert the weird minus in that case
    # and would otherwise break down
    nhst_parsed <- nhst_parsed[!is.na(nhst_parsed$Value), ]
    
    # remove missing test comparisons or p comparisons 
    # reason: these are marked as NA in cases where multiple comparisons were
    # reported. E.g., t(23) >= ..., p >= ...
    nhst_parsed <- nhst_parsed[!is.na(nhst_parsed$Test.Comparison) &
                                 !is.na(nhst_parsed$Reported.Comparison), ]
    
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

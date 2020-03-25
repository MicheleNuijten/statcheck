statcheck <- function(texts,
                      stat = c("t", "F", "cor", "chisq", "Z", "Q"),
                      OneTailedTests = FALSE,
                      alpha = .05,
                      pEqualAlphaSig = TRUE,
                      pZeroError = TRUE,
                      OneTailedTxt = FALSE,
                      AllPValues = FALSE,
                      messages = TRUE){
  
  Res <- data.frame(NULL)
  pRes <- data.frame(NULL)
  
  if (is.null(names(texts))){
    names(texts) <-  1:length(texts)
  }
  
  # start progress bar
  if(messages == TRUE){
    message("Extracting statistics...")
    pb <- txtProgressBar(max = length(text), style = 3)
  }
  
  for (i in 1:length(texts)) {
    txt <- texts[i]
    
    # p-values ------------------------------------------
    
    # extract all p values to calculate the ratio (statcheck results)/(total # of p values)
    pvalues <- extract_pvals(txt)
    
    # append and close
    if(nrow(pvalues) > 0){
      pvalues$Source <- names(txt)
      
      pRes <- rbind(pRes, pvalues)
    }
    
    rm(pvalues)
    
    # NHST results ------------------------------------------
    
    # extract all NHST results
    nhst <- extract_stats(txt)
    
    # append and close
    if(nrow(nhst) > 0){
      
      nhst$Source <- names(txt)
      nhst$OneTailedInTxt <- extract_1tail(txt)
      
      Res <- rbind(Res, nhst)
    }
    
    rm(nhst)
    
    if(messages == TRUE){
      setTxtProgressBar(pb, i)
    }
    
  }
  
  # close progressbar
  if(messages == TRUE){
    close(pb)
  }
  
  # reorder data frame based on the location of the stats in the text
  # instead of grouped by test type
  Res <- ddply(Res, .(Source), function(x)
    x[order(x$Location), ])
  
  ###---------------------------------------------------------------------
  
  if (nrow(Res) > 0) {
    
    # compute p-values -----------------------------------------------------
    
    Res$Computed <- rep(NA, nrow(Res))
    
    # if indicated, count all tests as one-sided
    if (OneTailedTests == TRUE) {
      two_tailed <- FALSE
    } else {
      two_tailed <- TRUE
    }
    
    for(i in seq_len(nrow(Res))){
      
      Res$Computed[i] <- compute_p(test_type = Res$Statistic[i],
                                   test_stat = Res$Value[i],
                                   df1 = Res$df1[i],
                                   df2 = Res$df2[i],
                                   two_tailed = two_tailed)
    }
    
    # check for errors & decision errors -------------------------------------
    
    Res$Error <- rep(NA, nrow(Res))
    Res$DecisionError <- rep(NA, nrow(Res))
    
    for(i in seq_len(nrow(Res))){
      Res$Error[i] <- ErrorTest(reported_p = Res$Reported.P.Value[i], 
                                test_type = Res$Statistic[i], 
                                test_stat = Res$Value[i],
                                df1 = Res$df1[i], 
                                df2 = Res$df2[i],
                                p_comparison = Res$Reported.Comparison[i], 
                                test_comparison = Res$Test.Comparison[i], 
                                p_dec = Res$dec[i], 
                                test_dec = Res$testdec[i],
                                two_tailed = two_tailed,
                                alpha = alpha,
                                pZeroError = pZeroError)
      
      if(Res$Error[i] == FALSE){
        # a result can only be a decision error if it is also an erro
        Res$DecisionError[i] <- FALSE
        
      } else if(Res$Error[i] == TRUE) {
        
        Res$DecisionError[i] <-  DecisionErrorTest(reported_p = Res$Reported.P.Value[i], 
                                                   computed_p = Res$Computed[i],
                                                   test_comparison = Res$Test.Comparison[i],
                                                   p_comparison = Res$Reported.Comparison[i],
                                                   alpha = alpha, 
                                                   pEqualAlphaSig = pEqualAlphaSig)
      }
    } 
    # automated 1-tailed test detection -----------------------------------
    
    if(OneTailedTxt == TRUE){
      
      # select only results where the phrase "one-tailed", "one-sided" or
      # "directional" was mentioned in text, and that were an error when
      # we assumed two-tailed tests
      upForCorrection <- Res$Error & Res$OneTailedInTxt
      
      # only start correction procedure if at least 1 result was selected
      if (sum(upForCorrection) > 0) {
        
        # transport relevant cases to separate data frame
        Res_check1tail <- Res[upForCorrection, ]
        
        # for this subset, calculate 1-tailed p-values 
        computed_p                   <- rep(NA, nrow(Res_check1tail))
        ErrorAfterCorrection         <- rep(NA, nrow(Res_check1tail))
        DecisionErrorAfterCorrection <- rep(NA, nrow(Res_check1tail))
        
        for(i in seq_len(nrow(Res_check1tail))){
          computed_p[i] <- compute_p(test_type  = Res_check1tail$Statistic[i],
                                     test_stat  = Res_check1tail$Value[i],
                                     df1        = Res_check1tail$df1[i],
                                     df2        = Res_check1tail$df2[i],
                                     two_tailed = FALSE)
          
          # check whether result would still be an error if 1-tailed
          ErrorAfterCorrection[i] <- 
            ErrorTest(reported_p      = Res_check1tail$Reported.P.Value[i], 
                      test_type       = Res_check1tail$Statistic[i], 
                      test_stat       = Res_check1tail$Value[i],
                      df1             = Res_check1tail$df1[i], 
                      df2             = Res_check1tail$df2[i],
                      p_comparison    = Res_check1tail$Reported.Comparison[i], 
                      test_comparison = Res_check1tail$Test.Comparison[i], 
                      p_dec           = Res_check1tail$dec[i], 
                      test_dec        = Res_check1tail$testdec[i], 
                      two_tailed      = FALSE,
                      alpha           = alpha,
                      pZeroError      = pZeroError)
          
          # only if a result is still an Error after correction, we will 
          # determine if a result would also be a DecisionError. 
          
          DecisionErrorAfterCorrection[i] <- 
            ifelse(
            test = ErrorAfterCorrection[i], 
            yes = DecisionErrorTest(
              reported_p = Res_check1tail$Reported.P.Value[i], 
              computed_p = Res_check1tail$Computed[i],
              test_comparison = Res_check1tail$Test.Comparison[i],
              p_comparison = Res_check1tail$Reported.Comparison[i],
              alpha = alpha, 
              pEqualAlphaSig = pEqualAlphaSig),
            no = FALSE)
        }
      
      # Now, we'll make a vector where we overwrite the original Errors that 
      # were corrected with the actual corrected Error values
      new_Errors <- Res$Error
      new_Errors[upForCorrection] <- ErrorAfterCorrection
      
      # We do the same for the DecisionErrors
      new_DecisionErrors <- Res$DecisionError
      new_DecisionErrors[upForCorrection] <- DecisionErrorAfterCorrection
      
      # And we also save the recomputed p-values
      new_pvalues <- Res$Computed
      new_pvalues[upForCorrection] <- computed_p
      
      # Now, we can determine the index of cases where something has changed
      gotCorrected <- Res$Error != new_Errors
      
      # We want to add this as a column to the data frame
      Res$isOneTailedCorrected <- gotCorrected
      
      # In these cases, we also overwrite the corrected Errors & DecisionErrors
      Res$Error[gotCorrected] <- new_Errors[gotCorrected]
      Res$DecisionError[gotCorrected] <- new_DecisionErrors[gotCorrected]
      
      # Also overwrite the two-tailed p-values with the one-tailed ones
      Res$Computed[gotCorrected] <- new_pvalues[gotCorrected]
    
      }
    }
    
    ###---------------------------------------------------------------------
    
    # APAfactor: proportion of APA results (that statcheck reads) of total number of p values
    
    Res$APAfactor <- calc_APA_factor(pRes, Res)
    
    ###---------------------------------------------------------------------
    
    
    # final data frame
    Res <- data.frame(
      Source = Res$Source,
      Statistic = Res$Statistic,
      df1 = Res$df1,
      df2 = Res$df2,
      Test.Comparison = Res$Test.Comparison,
      Value = Res$Value,
      Reported.Comparison = Res$Reported.Comparison,
      Reported.P.Value = Res$Reported.P.Value,
      Computed = Res$Computed,
      Raw = Res$Raw,
      Error = Res$Error,
      DecisionError = Res$DecisionError,
      # OneTail = Res$OneTail,
      OneTailedInTxt = Res$OneTailedInTxt,
      APAfactor = Res$APAfactor
    )
  }
  
  # Return ------------------------------------------------------------------
  
  if (AllPValues == FALSE) {
    # Return message when there are no results
    if (nrow(Res) > 0) {
      class(Res) <- c("statcheck", "data.frame")
      return(Res)
    } else {
      cat("statcheck did not find any results\n")
    }
  } else {
    return(pRes)
  }
}



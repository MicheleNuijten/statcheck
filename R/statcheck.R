statcheck <- function(texts,
                      stat = c("t", "F", "cor", "chisq", "Z", "Q"),
                      OneTailedTests = FALSE,
                      alpha = .05,
                      pEqualAlphaSig = TRUE,
                      pZeroError = TRUE,
                      OneTailedTxt = FALSE,
                      AllPValues = FALSE,
                      messages = TRUE){
  
  # We need empty data frames to store extracted statistics in
  # One for NHST results (Res) and one for p-values (pRes)
  Res <- data.frame(NULL)
  pRes <- data.frame(NULL)
  
  # to indicate where the statistics came from, we need a name for the input
  # texts. In some cases, this is the name of the file the text came from, but
  # if the text has no name, number them
  if (is.null(names(texts))){
    names(texts) <-  seq_along(texts)
  }
  
  # start progress bar. If the argument messages == FALSE, don't print this 
  # progress bar. This is mainly useful for the unit tests; otherwise hundreds
  # of progress bars would be printed during testing and that makes the test 
  # results hard to read
  if(messages == TRUE){
    message("Extracting statistics...")
    pb <- txtProgressBar(max = length(text), style = 3)
  }
  
  # for each text in the vector of input texts, extract all p-values and all
  # NHST results
  for (i in seq_along(texts)) {
    txt <- texts[i]
    
    # extract p-values ------------------------------------------
    
    # extract all p values. This is based on a pretty rough regular expression 
    # that will extract anything that resembles p =<> .... We need this info 
    # later on to calculate the APA factor: the ratio (statcheck results)/
    # (total # of p values). It is also possible to let statcheck return this
    # dataframe instead of the data frame with NHST results.
    pvalues <- extract_p_value(txt)
    
    # append and close:
    # in each repetition of the loop, the extracted p-values are appended 
    # to the existing pRes data frame, so it grows in each step
    if(nrow(pvalues) > 0){
      pvalues <- cbind(Source = names(txt), pvalues)
      
      pRes <- rbind(pRes, pvalues)
    }
    
    # after appending the pvalues dataframe to the main pRes dataframe,
    # the temporary dataframe pvalues can be removed. 
    rm(pvalues)
    
    # extract NHST results ------------------------------------------
    
    # extract all NHST results. This function scrapes the text for all APA 
    # reported NHST results and parses it so that the separate elements are
    # returned in one large dataframe
    nhst <- extract_stats(txt = txt,
                          stat = stat)
    
    # append and close: same logic as for the pvalues dataframe above
    if(nrow(nhst) > 0){
      
      nhst$Source <- names(txt)
      nhst$OneTailedInTxt <- extract_1tail(txt)
      
      Res <- rbind(Res, nhst)
    }
    
    rm(nhst)
    
    # update the progress bar
    if(messages == TRUE){
      setTxtProgressBar(pb, i)
    }
    
  }
  
  # close progress bar
  if(messages == TRUE){
    close(pb)
  }
  
  ###---------------------------------------------------------------------
  
  if (nrow(Res) > 0) {
    
    # If the argument OneTailedTests == TRUE, it forces statcheck to treat 
    # every encountered NHST result as a one-tailed test. Note: this is not the
    # same as the automated 1-tailed test detection (switched on with the 
    # argument: OneTailedTxt). The latter works more subtly (see comments in 
    # process_stats()). 
    if (OneTailedTests == TRUE) {
      two_tailed <- FALSE
    } else {
      two_tailed <- TRUE
    }
    
    # create empty variables to fill out during the loop
    Res$Computed <- rep(NA, nrow(Res))
    Res$Error <- rep(NA, nrow(Res))
    Res$DecisionError <- rep(NA, nrow(Res))
    
    # row by row, process the extracted statistics in Res. Specifically,
    # compute the p-value, check if the result is an error and a decision error,
    # and if indicated in the options, check & correct for 1-tailed tests
    for(i in seq_len(nrow(Res))){
    
      result <- process_stats(test_type = Res$Statistic[i],
                              test_stat = Res$Value[i],
                              df1 = Res$df1[i], 
                              df2 = Res$df2[i],
                              reported_p = Res$Reported.P.Value[i],
                              p_comparison = Res$Reported.Comparison[i],
                              test_comparison = Res$Test.Comparison[i],
                              p_dec = Res$dec[i],
                              test_dec = Res$testdec[i],
                              OneTailedInTxt = Res$OneTailedInTxt[i],
                              # options:
                              two_tailed = two_tailed,
                              alpha = alpha,
                              pZeroError = pZeroError,
                              pEqualAlphaSig = pEqualAlphaSig,
                              OneTailedTxt = OneTailedTxt,
                              OneTailedTests = OneTailedTests)
      
      Res$Computed[i] <- result$computed_p
      Res$Error[i] <- result$error
      Res$DecisionError[i] <- result$decision_error
    }
    
    ###---------------------------------------------------------------------
    
    # APAfactor: proportion of APA results (that statcheck reads) 
    # in total number of p values
    
    Res$APAfactor <- calc_APA_factor(pRes, Res)
    
    ###---------------------------------------------------------------------
    
    # select & reorder columns for final data frame
    Res <- Res[ , c("Source", "Statistic", "df1", "df2", "Test.Comparison",
                    "Value", "Reported.Comparison", "Reported.P.Value",
                    "Computed", "Raw", "Error", "DecisionError", 
                    "OneTailedInTxt", "APAfactor")]
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



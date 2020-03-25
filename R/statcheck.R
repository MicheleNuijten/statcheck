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
    
    # if indicated, count all tests as one-sided
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
                              OneTailedTxt = OneTailedTxt)
      
      Res$Computed[i] <- result$computed_p
      Res$Error[i] <- result$error
      Res$DecisionError[i] <- result$decision_error
    }
    
    ###---------------------------------------------------------------------
    
    # APAfactor: proportion of APA results (that statcheck reads) of total number of p values
    
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



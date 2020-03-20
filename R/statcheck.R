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
  Source <- NULL
  Res <- ddply(Res, .(Source), function(x)
    x[order(x$Location), ])
  
  ###---------------------------------------------------------------------
  
  if (nrow(Res) > 0) {
    
    # if indicated, count all tests as onesided
    if (OneTailedTests == TRUE) {
      two_tailed <- FALSE
    } else {
      two_tailed <- TRUE
    }
    
    # compute upper and lower bound for p-value ------------------------------
    
    low_stat <- Res$Value - (.5 / 10 ^ Res$testdec)
    up_stat <- Res$Value + (.5 / 10 ^ Res$testdec)
    
    Res$Computed <- rep(NA, nrow(Res))
    Res$up_p <- rep(NA, nrow(Res))
    Res$low_p <- rep(NA, nrow(Res))
    
    for(i in seq_len(nrow(Res))){
      Res$Computed[i] <- compute_p(test_type = Res$Statistic[i],
                                   test_stat = Res$Value[i],
                                   df1 = Res$df1[i],
                                   df2 = Res$df2[i],
                                   two_tailed = two_tailed)
      
      Res$up_p[i] <- compute_p(test_type = Res$Statistic[i],
                               test_stat = low_stat[i],
                               df1 = Res$df1[i],
                               df2 = Res$df2[i],
                               two_tailed = two_tailed)
      
      Res$low_p[i] <- compute_p(test_type = Res$Statistic[i],
                                test_stat = up_stat[i],
                                df1 = Res$df1[i],
                                df2 = Res$df2[i],
                                two_tailed = two_tailed)
    }
    
   # check for errors --------------------------------------------------
    
    Res$Error <- rep(NA, nrow(Res))
    
    for(i in seq_len(nrow(Res))){
      Res$Error[i] <- ErrorTest(reported_p = Res$Reported.P.Value[i], 
                                test_type = Res$Statistic[i], 
                                test_stat = Res$Value[i],
                                computed_p = Res$Computed[i], 
                                low_p = Res$low_p[i],
                                up_p = Res$up_p[i],
                                df1 = Res$df1[i], 
                                df2 = Res$df2[i],
                                p_comparison = Res$Reported.Comparison[i], 
                                test_comparison = Res$Test.Comparison[i], 
                                p_dec = Res$dec[i], 
                                test_dec = Res$testdec[i], 
                                alpha = alpha)
    }
    
    # check for decision errors -------------------------------------------
    
    Res$DecisionError <-  DecisionErrorTest(Res, alpha = alpha, 
                                            pEqualAlphaSig = pEqualAlphaSig)
    
    ### print messages ----------------------------------------------------
    
    # check if there could be one-sided tests in the data set
    if (OneTailedTests == FALSE) {
      Res <- check_presence_1tail(Res,
                                  messages = messages) 
    }
    
    # check if there would also be a decision error if alpha=.01 or .1
    check_alpha_levels(Res, 
                       pEqualAlphaSig = pEqualAlphaSig, messages = messages)
    
    ###---------------------------------------------------------------------
    
    # count errors as correct if they'd be correct one-sided
    # and there was a mention of 'one-sided','one-tailed', or 'directional' in the text
    
    if (OneTailedTxt == TRUE) {
      Res <- correct_4_1tailed(Res, alpha = alpha, pEqualAlphaSig = pEqualAlphaSig)
    }
    
    ###---------------------------------------------------------------------
    
    # "correct" rounding differences
    # e.g. t=2.3 could be 2.25 to 2.34999999... with its range of p values
    correct_round <- numeric()
    
    for (i in seq_len(nrow(Res))) {
      
      correct_round[i] <- check_correct_rounding(Res[i, ], 
                                                 OneTailedTests = OneTailedTests,
                                                 alpha = alpha)
    }
    
    CorrectRound <- as.logical(correct_round)
    
    Res$Error[CorrectRound] <- FALSE
    Res$DecisionError[CorrectRound] <- FALSE
    
    ###---------------------------------------------------------------------
    
    # p values smaller or equal to zero are errors
    
    if (pZeroError == TRUE) {
      ImpossibleP <- (Res$Reported.P.Value <= 0)
    } else {
      ImpossibleP <- (Res$Reported.P.Value < 0)
    }
    
    Res$Error[ImpossibleP] <- TRUE
    
    ###---------------------------------------------------------------------
    
    # p values that are not an error can also not be a decision error
    # this happens sometimes when reported= "p=.05" and e.g. computed=.052...
    # this should be counted as correct
    
    NoErrorDecisionError <-
      Res$Error == FALSE & Res$DecisionError == TRUE
    Res$DecisionError[NoErrorDecisionError] <- FALSE
    
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
      OneTail = Res$OneTail,
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



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
    
    # compute upper and lower bound of test statistic -----------------------
    # in order to be able to calculate range of correct p-values 
    
    low_stat <- Res$Value - (.5 / 10 ^ Res$testdec)
    up_stat <- Res$Value + (.5 / 10 ^ Res$testdec)
    
    # compute p-values -----------------------------------------------------
    
    Res$Computed <- rep(NA, nrow(Res))
    Res$up_p <- rep(NA, nrow(Res))
    Res$low_p <- rep(NA, nrow(Res))
    
    for(i in seq_len(nrow(Res))){
      
      # if indicated, count all tests as one-sided
      if (OneTailedTests == TRUE) {
        two_tailed <- FALSE
      } else {
        two_tailed <- TRUE
      }
      
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
    
    # check for errors & decision errors -------------------------------------
    
    Res$Error <- rep(NA, nrow(Res))
    Res$DecisionError <- rep(NA, nrow(Res))
    
    for(i in seq_len(nrow(Res))){
      Res$Error[i] <- ErrorTest(reported_p = Res$Reported.P.Value[i], 
                                test_type = Res$Statistic[i], 
                                test_stat = Res$Value[i],
                                low_p = Res$low_p[i],
                                up_p = Res$up_p[i],
                                df1 = Res$df1[i], 
                                df2 = Res$df2[i],
                                p_comparison = Res$Reported.Comparison[i], 
                                test_comparison = Res$Test.Comparison[i], 
                                p_dec = Res$dec[i], 
                                test_dec = Res$testdec[i], 
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
      Res_check1tail <- Res[Res$OneTailedInTxt == TRUE & Res$Error == TRUE, ]
      
      if(nrow(Res_check1tail) > 0){
        
        # for this subset, calculate 1-tailed p-values ----------------------
        computed_p <- rep(NA, nrow(Res_check1tail))
        low_p <- rep(NA, nrow(Res_check1tail))
        up_p <- rep(NA, nrow(Res_check1tail))
        Res_check1tail$Error <- rep(NA, nrow(Res_check1tail))
        
        for(i in seq_len(nrow(Res_check1tail))){
          computed_p[i] <- compute_p(test_type = Res_check1tail$Statistic[i],
                               test_stat = Res_check1tail$Value[i],
                               df1 = Res_check1tail$df1[i],
                               df2 = Res_check1tail$df2[i],
                               two_tailed = FALSE)
          
          up_p[i] <- compute_p(test_type = Res_check1tail$Statistic[i],
                               test_stat = low_stat[i],
                               df1 = Res_check1tail$df1[i],
                               df2 = Res_check1tail$df2[i],
                               two_tailed = FALSE)
          
          low_p[i] <- compute_p(test_type = Res_check1tail$Statistic[i],
                                test_stat = up_stat[i],
                                df1 = Res_check1tail$df1[i],
                                df2 = Res_check1tail$df2[i],
                                two_tailed = FALSE)
          
          # check whether result would still be an error if 1-tailed ---------
          Res_check1tail$Error[i] <- 
            ErrorTest(reported_p = Res_check1tail$Reported.P.Value[i], 
                      test_type = Res_check1tail$Statistic[i], 
                      test_stat = Res_check1tail$Value[i],
                      low_p = low_p[i],
                      up_p = up_p[i],
                      df1 = Res_check1tail$df1[i], 
                      df2 = Res_check1tail$df2[i],
                      p_comparison = Res_check1tail$Reported.Comparison[i], 
                      test_comparison = Res_check1tail$Test.Comparison[i], 
                      p_dec = Res_check1tail$dec[i], 
                      test_dec = Res_check1tail$testdec[i], 
                      alpha = alpha,
                      pZeroError = pZeroError)
          
          if(Res_check1tail$Error[i] == FALSE){
            # a result can only be a decision error if it is also an erro
            Res_check1tail$DecisionError[i] <- FALSE
            
          } else if(Res_check1tail$Error[i] == TRUE) {
            
            Res_check1tail$DecisionError[i] <-  
              DecisionErrorTest(reported_p = Res_check1tail$Reported.P.Value[i], 
                                computed_p = Res_check1tail$Computed[i],
                                test_comparison = Res_check1tail$Test.Comparison[i],
                                p_comparison = Res_check1tail$Reported.Comparison[i],
                                alpha = alpha, 
                                pEqualAlphaSig = pEqualAlphaSig)
          }
        }
        
        Res[Res$OneTailedInTxt == TRUE & Res$Error == TRUE, ]$Error <- Res_check1tail$Error
        Res[Res$OneTailedInTxt == TRUE & Res$Error == TRUE, ]$DecisionError <- Res_check1tail$DecisionError
      }
      
    }
    
    ### print messages ----------------------------------------------------
    
    # check if there would also be a decision error if alpha=.01 or .1
    check_alpha_levels(Res, 
                       pEqualAlphaSig = pEqualAlphaSig, messages = messages)
    
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



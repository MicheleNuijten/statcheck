process_stats <- function(test_type,
                          test_stat,
                          df1, 
                          df2,
                          reported_p,
                          p_comparison,
                          test_comparison,
                          p_dec,
                          test_dec,
                          OneTailedInTxt,
                          # options:
                          two_tailed,
                          alpha,
                          pZeroError,
                          pEqualAlphaSig,
                          OneTailedTxt,
                          OneTailedTests){
  
  # compute p-value ----------------------------------------------------------
  computed_p <- compute_p(test_type = test_type,
                          test_stat = test_stat,
                          df1 = df1,
                          df2 = df2,
                          two_tailed = two_tailed)
  
  # check if the result is an error ------------------------------------------
  error <- error_test(reported_p = reported_p, 
                      test_type = test_type, 
                      test_stat = test_stat,
                      df1 = df1,
                      df2 = df2,
                      p_comparison = p_comparison, 
                      test_comparison = test_comparison, 
                      p_dec = p_dec, 
                      test_dec = test_dec,
                      two_tailed = two_tailed,
                      alpha = alpha,
                      pZeroError = pZeroError)
  
  # check if the result is a decision error ----------------------------------
  
  if(!error){
    # if a result is not an error, it's automatically also  not a decision error
    decision_error <- FALSE
  } else {
    # only if a result is an error, it makes sense to check if it's also a 
    # decision error
    decision_error <- decision_error_test(reported_p = reported_p, 
                                          computed_p = computed_p,
                                          test_comparison = test_comparison,
                                          p_comparison = p_comparison,
                                          alpha = alpha, 
                                          pEqualAlphaSig = pEqualAlphaSig)
  }
  
  # correct for one-tailed tests in text ------------------------------------
  
  # OneTailedTxt is an option that determines whether statcheck should
  # try to correct for 1-tailed tests as follows:
  # if the phrase one-tailed/one-sided/directional is in the full text,
  # AND if the reported p-value would have been correct if it was a one-
  # tailed test, classify the result as consistent
  # don't apply this correction if OneTailedTests == FALSE, because this already
  # forces statcheck to treat all results as one-tailed tests
  if(OneTailedTxt == TRUE & OneTailedTests == FALSE){
    
    # select only results where the phrase "one-tailed", "one-sided" or
    # "directional" was mentioned in text, and that were an error when
    # we assumed two-tailed tests
    upForCorrection <- error & OneTailedInTxt
    
    # only start correction procedure if the result fits the criteria above
    if (upForCorrection){
      
      # for this case, recompute the p-value, but this time assuming a 
      # one-tailed test
      computed_p_1tail <- compute_p(test_type = test_type,
                                    test_stat = test_stat,
                                    df1 = df1,
                                    df2 = df2,
                                    two_tailed = FALSE)
      
      # check whether result would still be an error if 1-tailed
      error_1tail <- 
        error_test(reported_p = reported_p, 
                   test_type = test_type, 
                   test_stat = test_stat,
                   df1 = df1,
                   df2 = df2,
                   p_comparison = p_comparison, 
                   test_comparison = test_comparison, 
                   p_dec = p_dec, 
                   test_dec = test_dec,
                   two_tailed = FALSE,
                   alpha = alpha,
                   pZeroError = pZeroError)
      
      if(!error_1tail){
        # if a result is not an error, it's automatically also  not a decision error
        decision_error_1tail <- FALSE
      } else {
        # only if a result is an error, it makes sense to check if it's also a 
        # decision error
        decision_error_1tail <- 
          decision_error_test(reported_p = reported_p, 
                              computed_p = computed_p_1tail,
                              test_comparison = test_comparison,
                              p_comparison = p_comparison,
                              alpha = alpha, 
                              pEqualAlphaSig = pEqualAlphaSig)
      }
      
      # if the 1-tailed p-value is no longer an error, the original values of
      # error, decisionerror, and computed_p should be overwritten with the 1-
      # tailed versions
      if(error != error_1tail){
        computed_p <- computed_p_1tail
        error <- error_1tail
        decision_error <- decision_error_1tail
      }
    }    
  }
  
  result <- data.frame(computed_p = computed_p,
                       error = error,
                       decision_error = decision_error)
  
  return(result) 
}

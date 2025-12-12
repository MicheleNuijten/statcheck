error_test <- function(reported_p, test_type, test_stat,
                       df1, df2,
                       p_comparison, test_comparison, 
                       p_dec, test_dec, 
                       two_tailed,
                       alpha, pZeroError,
                       robust_rounding = FALSE) { 
  
  # replace 'ns' for > alpha -----------------------------------------------
  
  reported_p[p_comparison == "ns"] <- alpha
  p_comparison[p_comparison == "ns"] <- ">"
  
  # compute p-values -------------------------------------------------------
  # take into account that the reported test statistic may have been rounded or truncated.
  # to that end, compute the upper and lower bound of the test statistic
  # based on the number of decimals that it was reported with. 
  
  # Calculate the step size based on decimals (e.g., 2 decimals -> 0.01)
  step <- 1 / (10 ^ test_dec)
  
  if (robust_rounding == FALSE) {
    # --- STANDARD ROUNDING (Default) ---
    # The true value is within +/- 0.5 step.
    # Total Width: 1 step
    
    half_step <- step / 2
    
    if(test_stat >= 0){
      low_stat <- test_stat - half_step
      up_stat  <- test_stat + half_step
    } else {
      # For negatives, reverse directions for "low" and "up" bounds
      low_stat <- test_stat + half_step 
      up_stat  <- test_stat - half_step
    }
    
  } else {
    # --- ROBUST / CATCH-ALL LOGIC (New) ---
    # The true value is within +/- 1.0 step to cover floor, ceiling, or round.
    # Total Width: 2 steps
    
    if(test_stat >= 0){
      low_stat <- test_stat - step
      up_stat  <- test_stat + step
    } else {
      # For negatives:
      # If reported is -2.42, it could be -2.41 (step up) or -2.43 (step down)
      # low_stat (closer to 0) is the "higher" algebraic value
      low_stat <- test_stat + step
      # up_stat (further from 0) is the "lower" algebraic value
      up_stat  <- test_stat - step 
    }
  }
  
  # Compute the p-values that belong to the upper and lower bound of the test
  # statistic. This is the range of p-values that would be correct.
  
  # Note: low_stat (smaller magnitude) -> yields LARGER p-value (up_p)
  up_p <- compute_p(test_type = test_type,
                    test_stat = low_stat,
                    df1 = df1,
                    df2 = df2,
                    two_tailed = two_tailed)
  
  # Note: up_stat (larger magnitude) -> yields SMALLER p-value (low_p)
  low_p <- compute_p(test_type = test_type,
                     test_stat = up_stat,
                     df1 = df1,
                     df2 = df2,
                     two_tailed = two_tailed)
  
  
  # Initialize error result
  error_result <- NA
  
  # p values smaller or equal to zero are errors ---------------------------
  
  if(pZeroError == TRUE & reported_p <= 0){
    error_result <- TRUE
    return(list(error = error_result, lower_bound = low_p, upper_bound = up_p))
  }
  
  # check errors for different combinations of <>= -------------------------
  
  if(test_comparison == "="){
    
    if(p_comparison == "="){
      error_result <- reported_p > round(up_p, p_dec) | reported_p < round(low_p, p_dec)
    } else if(p_comparison == "<"){
      error_result <- reported_p < low_p
    } else if(p_comparison == ">"){
      error_result <- reported_p > up_p
    }
    
  } else if(test_comparison == "<"){
    
    if(p_comparison == "="){
      error_result <- reported_p < round(up_p, p_dec)
    } else if(p_comparison == "<"){
      error_result <- reported_p < up_p
    } else if(p_comparison == ">"){
      error_result <- FALSE
    }
    
  } else if(test_comparison == ">"){
    
    if(p_comparison == "="){
      error_result <- reported_p > round(low_p, p_dec)
    } else if(p_comparison == "<"){
      error_result <- FALSE
    } else if(p_comparison == ">"){
      error_result <- reported_p > low_p
    }
    
  }
  
  # Return the data frame of results 
  return(data.frame(error = error_result, lower_bound = low_p, upper_bound = up_p))
  
}

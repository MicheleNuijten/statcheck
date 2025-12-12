error_test <- function(reported_p, test_type, test_stat,
                       df1, df2,
                       p_comparison, test_comparison, 
                       p_dec, test_dec, 
                       two_tailed,
                       alpha, pZeroError,
                       assume_truncation = FALSE) { 
  
  # replace 'ns' for > alpha -----------------------------------------------
  
  reported_p[p_comparison == "ns"] <- alpha
  p_comparison[p_comparison == "ns"] <- ">"
  
  # compute p-values -------------------------------------------------------
  # take into account that the reported test statistic may have been rounded or truncated.
  # to that end, compute the upper and lower bound of the test statistic
  # based on the number of decimals that it was reported with. 
  
  # Calculate the step size based on decimals (e.g., 2 decimals -> 0.01)
  step <- 1 / (10 ^ test_dec)
  
  if (assume_truncation == FALSE) {
    # --- ROUNDING LOGIC (Original statcheck behavior) ---
    # The true value is within +/- 0.5 step of the reported value.
    # e.g., Reported 2.00 could be [1.995, 2.005]
    
    half_step <- step / 2
    
    if(test_stat >= 0){
      low_stat <- test_stat - half_step
      up_stat <- test_stat + half_step
    } else {
      # For negatives:
      # low_stat (closer to 0, higher p-val) is mathematically larger (e.g. -1.995 vs -2.0)
      low_stat <- test_stat + half_step
      # up_stat (further from 0, lower p-val) is mathematically smaller (e.g. -2.005 vs -2.0)
      up_stat <- test_stat - half_step
    }
    
  } else {
    # --- TRUNCATION LOGIC ---
    # The true value is the reported value extended by the step size.
    # e.g., Reported 2.4 could be [2.40, 2.50)
    
    if(test_stat >= 0){
      # Positive: Range is [Reported, Reported + step]
      low_stat <- test_stat
      up_stat <- test_stat + step
    } else {
      # Negative: Range is [Reported - step, Reported]
      # e.g., Reported -2.4 implied truncation of -2.45...
      # low_stat (closer to 0) is the reported value
      low_stat <- test_stat
      # up_stat (further from 0) extends "downward"
      up_stat <- test_stat - step 
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

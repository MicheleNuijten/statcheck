error_test <- function(reported_p, test_type, test_stat,
                      df1, df2,
                      p_comparison, test_comparison, 
                      p_dec, test_dec, 
                      two_tailed,
                      alpha, pZeroError) {
  
  # replace 'ns' for > alpha -----------------------------------------------
  
  reported_p[p_comparison == "ns"] <- alpha
  p_comparison[p_comparison == "ns"] <- ">"
  
  # compute p-values -------------------------------------------------------
 
  # take into account that the reported test statistic may have been rounded
  # to that end, compute the upper and lower bound of the test statistic
  # based on the number of decimals that it was reported with. E.g., 
  # a t-value of 2.0 could have been rounded from anywhere between 1.95-2.05.
  
  if(test_stat >= 0){
    
    low_stat <- test_stat - (.5 / 10 ^ test_dec)
    up_stat <- test_stat + (.5 / 10 ^ test_dec)
  
    # switch around for negative test statistics
    } else if (test_stat < 0){
    low_stat <- test_stat + (.5 / 10 ^ test_dec)
    up_stat <- test_stat - (.5 / 10 ^ test_dec)
  }
  
  # Compute the p-values that belong to the upper and lower bound of the test
  # statistic. This is the range of p-values that would be correct.
  up_p <- compute_p(test_type = test_type,
                    test_stat = low_stat,
                    df1 = df1,
                    df2 = df2,
                    two_tailed = two_tailed)
  
  low_p <- compute_p(test_type = test_type,
                     test_stat = up_stat,
                     df1 = df1,
                     df2 = df2,
                     two_tailed = two_tailed)
  
  # p values smaller or equal to zero are errors ---------------------------
  
  if(pZeroError && reported_p <= 0){
    error <- TRUE
    return(error)
  }
  
  # check errors for different combinations of <>= -------------------------
  
  if(test_comparison == "="){
    
    if(p_comparison == "="){
      
      error <- reported_p > round(up_p, p_dec) | reported_p < round(low_p, p_dec)
      return(error)
      
    } else if(p_comparison == "<"){
      
      error <- reported_p < low_p
      return(error)
      
    } else if(p_comparison == ">"){
      
      error <- reported_p > up_p
      return(error)
      
    }
    
  } else if(test_comparison == "<"){
    
    if(p_comparison == "="){
      error <- reported_p < round(up_p, p_dec)
      return(error)
      
    } else if(p_comparison == "<"){
    
      error <- reported_p < up_p
      return(error)
    } 
    else if(p_comparison == ">"){
      
      error <- FALSE
      return(error)
      
    }
    
  } else if(test_comparison == ">"){
    
    if(p_comparison == "="){
      
      error <- reported_p > round(low_p, p_dec)
      return(error)
      
    } else if(p_comparison == "<"){
      
      error <- FALSE
      return(error)
      
    } else if(p_comparison == ">"){
      
      error <- reported_p > low_p
      return(error)
      
    }
    
  }
  
  return(NA)
  
}

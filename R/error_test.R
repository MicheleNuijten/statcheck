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
  
  if(test_type %in% c("t", "Z", "r")){
    # These p-values decrease with the absolute statistic. An interval that
    # crosses zero includes the maximum p-value, and correlations cannot
    # extend beyond one even when their rounded value is exactly +/-1.
    low_stat <- max(0, abs(test_stat) - (.5 / 10 ^ test_dec))
    up_stat <- abs(test_stat) + (.5 / 10 ^ test_dec)
    if(test_type == "r") up_stat <- min(1, up_stat)
  } else {
    low_stat <- test_stat - (.5 / 10 ^ test_dec)
    up_stat <- test_stat + (.5 / 10 ^ test_dec)
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
  
  if(reported_p <= 0 & (pZeroError == TRUE | p_comparison == "<")){
    error <- TRUE
    return(error)
  }
  
  # check errors for different combinations of <>= -------------------------

  test_comparison <- magnitude_comparison(test_type, test_stat, test_comparison)

  if(test_comparison == "any"){
    # This signed bound permits statistics arbitrarily close to zero and
    # magnitudes up to the upper end of the test's domain.
    # Under the package's smaller-tail convention the maximum is .5 for
    # one-tailed tests, and 1 for two-tailed tests.
    max_p <- if(two_tailed) 1 else .5
    if(p_comparison == ">") return(reported_p >= max_p)
    if(p_comparison == "<") return(FALSE)
    return(reported_p > max_p)
  }
  
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


magnitude_comparison <- function(test_type, test_stat, test_comparison){
  # Preserve the established interpretation of positive bounds as bounds
  # on magnitude. Negative bounds are signed: x < -a implies |x| > a,
  # whereas x > -a permits both zero and arbitrarily large positive values.
  # Either strict inequality at zero also permits the full magnitude range.
  if(test_type %in% c("t", "Z", "r")){
    if(test_stat == 0 && test_comparison %in% c("<", ">")) return("any")
    if(test_stat < 0){
      if(test_comparison == "<") return(">")
      if(test_comparison == ">") return("any")
    }
  }
  return(test_comparison)
}

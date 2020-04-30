compute_p <- function(test_type, test_stat, df1, df2, two_tailed){
  
  stopifnot(test_type %in% c("t", "F", "Z", "r", "Chi2", "Q", "Qb", "Qw"))
  
  # compute p-values ---------------------------------------------------------
  
  if(test_type == "t"){
    
    computed <- stats::pt(-1 * abs(test_stat), df2)
    
  } else if(test_type == "F"){
    
    computed <- stats::pf(test_stat, df1, df2, lower.tail = FALSE)
    
  } else if(test_type == "Z"){
    
    computed <- stats::pnorm(abs(test_stat), lower.tail = FALSE)
    
  } else if(test_type == "r"){
    
    t <- r2t(test_stat, df2)
    computed <- stats::pt(-1 * abs(t), df2)
    
  } else if(test_type == "Chi2" | 
            test_type == "Q" | test_type == "Qb" | test_type == "Qw"){
    
    computed <- stats::pchisq(test_stat, df1, lower.tail = FALSE)
    
  }
  
  # compute two-tailed ------------------------------------------------------
  
  if (!is.na(computed) &
    (test_type == "t" | test_type == "Z" | test_type == "r") & 
    two_tailed) {
    computed <- computed * 2
  }
  
  # return ------------------------------------------------------------------
  
  return(computed)
}


# Function to transform correlations into t-values by use of raw r and degrees of freedom.
r2t <- function(r, df){
  t <- r / (sqrt((1 - r^2) / df))
  return(t)
}

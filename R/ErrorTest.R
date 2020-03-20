ErrorTest <- function(reported_p, computed_p, 
                      test_type, test_stat,
                      df1, df2,
                      p_comparison, test_comparison, 
                      p_dec, test_dec, alpha) {
  
  # replace 'ns' for > alpha -----------------------------------------------
  
  reported_p[p_comparison == "ns"] <- alpha
  p_comparison[p_comparison == "ns"] <- ">"
  
  # compute upper and lower bound for p-value ------------------------------
  
  low_stat <- test_stat - (.5 / 10 ^ test_dec)
  up_stat <- test_stat + (.5 / 10 ^ test_dec)
  
  low_p <- compute_p(test_type, up_stat, df1, df2)
  up_p <- compute_p(test_type, low_stat, df1, df2)
  
  # check errors for exact test statistics ---------------------------------
  
  if(test_comparison == "="){
    
    if(p_comparison == "="){
      
      error <- reported_p > up_p | reported_p < low_p
      return(error)
      
    } else if(p_comparison == "<"){
      
      error <- reported_p < computed_p
      return(error)
      
    } else if(p_comparison == ">"){
      
      error <- reported_p > computed_p
      return(error)
      
    }
    
  } else if(test_comparison == "<"){
    
    if(p_comparison == "="){
      error <- reported_p < up_p
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
      
      error <- reported_p > low_p
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

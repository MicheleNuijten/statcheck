ErrorTest <- function(reported_p, test_type, test_stat,
                      low_p, up_p,
                      df1, df2,
                      p_comparison, test_comparison, 
                      p_dec, test_dec, 
                      alpha, pZeroError) {
  
  # replace 'ns' for > alpha -----------------------------------------------
  
  reported_p[p_comparison == "ns"] <- alpha
  p_comparison[p_comparison == "ns"] <- ">"
  
  # p values smaller or equal to zero are errors ---------------------------
  
  if(pZeroError == TRUE & reported_p <= 0){
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

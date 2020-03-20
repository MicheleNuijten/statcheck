ErrorTest <- function(reported_p, test_type, test_stat,
                      computed_p, low_p, up_p,
                      df1, df2,
                      p_comparison, test_comparison, 
                      p_dec, test_dec, alpha) {
  
  # replace 'ns' for > alpha -----------------------------------------------
  
  reported_p[p_comparison == "ns"] <- alpha
  p_comparison[p_comparison == "ns"] <- ">"
  
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

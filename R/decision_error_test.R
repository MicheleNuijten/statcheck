decision_error_test <- function(reported_p, computed_p,
                              test_comparison, p_comparison,
                              alpha, pEqualAlphaSig){
  
  # replace 'ns' for > alpha -----------------------------------------------
  reported_p[p_comparison == "ns"] <- alpha
  p_comparison[p_comparison == "ns"] <- ">"
  
  # check errors for different combinations of <>= -------------------------
  
  # treat p = alpha as significant
  if(pEqualAlphaSig == TRUE){
    if(test_comparison == "="){
      
      if(p_comparison == "="){
        
        dec_error <- (reported_p <= alpha & computed_p > alpha) |
          (reported_p > alpha & computed_p <= alpha)
        
        return(dec_error)
        
      } else if(p_comparison == "<"){
        
        dec_error <- reported_p <= alpha & computed_p > alpha
        return(dec_error)
        
      } else if(p_comparison == ">"){
        
        dec_error <- reported_p >= alpha & computed_p <= alpha
        return(dec_error)
        
      }
      
    } else if(test_comparison == "<"){
      
      if(p_comparison == "="){
        dec_error <- reported_p <= alpha & computed_p >= alpha
        return(dec_error)
        
      } else if(p_comparison == "<"){
        
        dec_error <- reported_p <= alpha & computed_p >= alpha
        return(dec_error)
      } 
      else if(p_comparison == ">"){
        
        dec_error <- FALSE
        return(dec_error)
        
      }
      
    } else if(test_comparison == ">"){
      
      if(p_comparison == "="){
        
        dec_error <- reported_p > alpha & computed_p <= alpha
        return(dec_error)
        
      } else if(p_comparison == "<"){
        
        dec_error <- FALSE
        return(dec_error)
        
      } else if(p_comparison == ">"){
        
        dec_error <- reported_p >= alpha & computed_p <= alpha
        return(dec_error)
        
      }
      
    }
    
    return(NA)
    
    # treat p = alpha as significant
  } else if (pEqualAlphaSig == FALSE){
    
    if(test_comparison == "="){
      
      if(p_comparison == "="){
        
        dec_error <- (reported_p < alpha & computed_p >= alpha) |
          (reported_p >= alpha & computed_p < alpha)
        return(dec_error)
        
      } else if(p_comparison == "<"){
        
        dec_error <- reported_p <= alpha & computed_p >= alpha
        return(dec_error)
        
      } else if(p_comparison == ">"){
        
        dec_error <- reported_p >= alpha & computed_p < alpha
        return(dec_error)
        
      }
      
    } else if(test_comparison == "<"){
      
      if(p_comparison == "="){
        dec_error <- reported_p < alpha & computed_p >= alpha
        return(dec_error)
        
      } else if(p_comparison == "<"){
        
        dec_error <- reported_p <= alpha & computed_p >= alpha
        return(dec_error)
      } 
      else if(p_comparison == ">"){
        
        dec_error <- FALSE
        return(dec_error)
        
      }
      
    } else if(test_comparison == ">"){
      
      if(p_comparison == "="){
        
        dec_error <- reported_p >= alpha & computed_p <= alpha
        return(dec_error)
        
      } else if(p_comparison == "<"){
        
        dec_error <- FALSE
        return(dec_error)
        
      } else if(p_comparison == ">"){
        
        dec_error <- reported_p >= alpha & computed_p <= alpha
        return(dec_error)
        
      }
      
    }
    
    return(NA)
  }
}

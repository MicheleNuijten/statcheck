compute_p <- function(Statistic, Value, df1, df2){
  
  if(Statistic == "t"){
    
    computed <- pt(-1 * abs(Value), df2) * 2
    
  } else if(Statistic == "F"){
    
    computed <- pf(Value, df1, df2, lower.tail = FALSE)
    
  } else if(Statistic == "Z"){
    
    computed <- pnorm(abs(Value), lower.tail = FALSE) * 2
    
  } else if(Statistic == "r"){
    
    pComputed <-
      pmin(pt(-1 * abs(r2t(Value, df2)), df2) * 2, 1)
    
    pComputed[is.nan(pComputed)] <- NA
    
    computed <-  pComputed
    
  } else if(Statistic == "Chi2" | 
            Statistic == "Q" | Statistic == "Qb" | Statistic == "Qw"){
    
    computed <- pchisq(Value, df1, lower.tail = FALSE)
    
  } else {
    
    computed <- NA
      
  }
  
  return(computed)
}


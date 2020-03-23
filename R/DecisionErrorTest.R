DecisionErrorTest <- function(reported_p, test_type, test_stat,
                              low_p, up_p,
                              df1, df2,
                              p_comparison, test_comparison, 
                              p_dec, test_dec, 
                              alpha, pEqualAlphaSig){
 
  # replace 'ns' for > alpha -----------------------------------------------
  reported_p[p_comparison == "ns"] <- alpha
  p_comparison[p_comparison == "ns"] <- ">"
  
  #-----------------------------------------------
  
  equalequal <- testcomp == "=" & comparison == "="
  equalsmall <- testcomp == "=" & comparison == "<"
  equalgreat <- testcomp == "=" & comparison == ">"
  
  smallequal <- testcomp == "<" & comparison == "="
  smallsmall <- testcomp == "<" & comparison == "<"
  smallgreat <- testcomp == "<" & comparison == ">"
  
  greatequal <- testcomp == ">" & comparison == "="
  greatsmall <- testcomp == ">" & comparison == "<"
  greatgreat <- testcomp == ">" & comparison == ">"
  
  AllTests <- grepl("=|<|>", comparison)
  
  if (any(AllTests)) {
    if (pEqualAlphaSig == TRUE) {
      AllTests[equalequal] <-
        (reported[equalequal] <= alpha & computed[equalequal]  > alpha) |
        (reported[equalequal] >  alpha &  computed[equalequal] <= alpha)
      AllTests[equalsmall] <-
        reported[equalsmall] <= alpha & computed[equalsmall] > alpha
      AllTests[equalgreat] <-
        reported[equalgreat] >= alpha & computed[equalgreat] <= alpha
      
      
      AllTests[smallequal] <-
        reported[smallequal] <= alpha & computed[smallequal] >= alpha
      AllTests[smallsmall] <-
        reported[smallsmall] <= alpha & computed[smallsmall] >= alpha
      
      AllTests[greatequal] <-
        reported[greatequal] > alpha & computed[greatequal] <= alpha
      AllTests[greatgreat] <-
        reported[greatgreat] >= alpha & computed[greatgreat] <= alpha
      
    } else {
      AllTests[equalequal] <-
        (reported[equalequal] <  alpha & computed[equalequal] >= alpha) |
        (reported[equalequal] >= alpha & computed[equalequal] <  alpha)
      AllTests[equalsmall] <-
        reported[equalsmall] < alpha & computed[equalsmall] >= alpha
      AllTests[equalgreat] <-
        reported[equalgreat] >= alpha & computed[equalgreat] < alpha
      
      
      AllTests[smallequal] <-
        reported[smallequal] < alpha & computed[smallequal] >= alpha
      AllTests[smallsmall] <-
        reported[smallsmall] <= alpha & computed[smallsmall] >= alpha
      
      AllTests[greatequal] <-
        reported[greatequal] >= alpha & computed[greatequal] < alpha
      AllTests[greatgreat] <-
        reported[greatgreat] >= alpha & computed[greatgreat] < alpha
      
    }
    
    # these combinations of < & > are logically always correct
    AllTests[smallgreat] <- FALSE
    AllTests[greatsmall] <- FALSE
  }
  
  
  AllTests <- as.logical(AllTests)
  
  #-----------------------------------------------
  
  return(AllTests)
}
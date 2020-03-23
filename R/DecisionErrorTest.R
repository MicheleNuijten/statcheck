DecisionErrorTest <- function(Res,
                              alpha, pEqualAlphaSig){
 
  test_comparison <- Res$Test.Comparison
  p_comparison <- Res$Comparison
  reported <- Res$Reported.P.Value
  computed <- Res$Computed
  
  # replace 'ns' for > alpha -----------------------------------------------
  reported_p[p_comparison == "ns"] <- alpha
  p_comparison[p_comparison == "ns"] <- ">"
  
  #-----------------------------------------------
  
  equalequal <- test_comparison == "=" & p_comparison == "="
  equalsmall <- test_comparison == "=" & p_comparison == "<"
  equalgreat <- test_comparison == "=" & p_comparison == ">"
  
  smallequal <- test_comparison == "<" & p_comparison == "="
  smallsmall <- test_comparison == "<" & p_comparison == "<"
  smallgreat <- test_comparison == "<" & p_comparison == ">"
  
  greatequal <- test_comparison == ">" & p_comparison == "="
  greatsmall <- test_comparison == ">" & p_comparison == "<"
  greatgreat <- test_comparison == ">" & p_comparison == ">"
  
  AllTests <- grepl("=|<|>", p_comparison)
  
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
        reported[equalsmall] <= alpha & computed[equalsmall] >= alpha
      AllTests[equalgreat] <-
        reported[equalgreat] >= alpha & computed[equalgreat] < alpha
      
      
      AllTests[smallequal] <-
        reported[smallequal] < alpha & computed[smallequal] >= alpha
      AllTests[smallsmall] <-
        reported[smallsmall] <= alpha & computed[smallsmall] >= alpha
      
      AllTests[greatequal] <-
        reported[greatequal] >= alpha & computed[greatequal] <= alpha
      AllTests[greatgreat] <-
        reported[greatgreat] >= alpha & computed[greatgreat] <= alpha
      
    }
    
    # these combinations of < & > are logically always correct
    AllTests[smallgreat] <- FALSE
    AllTests[greatsmall] <- FALSE
  }
  
  
  AllTests <- as.logical(AllTests)
  
  #-----------------------------------------------
  
  return(AllTests)
}
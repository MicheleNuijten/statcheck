DecisionErrorTest <- function(Res,
                              alpha, pEqualAlphaSig){
 
  test_comparison <- Res$Test.Comparison
  p_comparison <- Res$Comparison
  reported_p <- Res$Reported.P.Value
  computed_p <- Res$Computed
  
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
        (reported_p[equalequal] <= alpha & computed_p[equalequal]  > alpha) |
        (reported_p[equalequal] >  alpha &  computed_p[equalequal] <= alpha)
      AllTests[equalsmall] <-
        reported_p[equalsmall] <= alpha & computed_p[equalsmall] > alpha
      AllTests[equalgreat] <-
        reported_p[equalgreat] >= alpha & computed_p[equalgreat] <= alpha
      
      
      AllTests[smallequal] <-
        reported_p[smallequal] <= alpha & computed_p[smallequal] >= alpha
      AllTests[smallsmall] <-
        reported_p[smallsmall] <= alpha & computed_p[smallsmall] >= alpha
      
      AllTests[greatequal] <-
        reported_p[greatequal] > alpha & computed_p[greatequal] <= alpha
      AllTests[greatgreat] <-
        reported_p[greatgreat] >= alpha & computed_p[greatgreat] <= alpha
      
    } else {
      AllTests[equalequal] <-
        (reported_p[equalequal] <  alpha & computed_p[equalequal] >= alpha) |
        (reported_p[equalequal] >= alpha & computed_p[equalequal] <  alpha)
      AllTests[equalsmall] <-
        reported_p[equalsmall] <= alpha & computed_p[equalsmall] >= alpha
      AllTests[equalgreat] <-
        reported_p[equalgreat] >= alpha & computed_p[equalgreat] < alpha
      
      
      AllTests[smallequal] <-
        reported_p[smallequal] < alpha & computed_p[smallequal] >= alpha
      AllTests[smallsmall] <-
        reported_p[smallsmall] <= alpha & computed_p[smallsmall] >= alpha
      
      AllTests[greatequal] <-
        reported_p[greatequal] >= alpha & computed_p[greatequal] <= alpha
      AllTests[greatgreat] <-
        reported_p[greatgreat] >= alpha & computed_p[greatgreat] <= alpha
      
    }
    
    # these combinations of < & > are logically always correct
    AllTests[smallgreat] <- FALSE
    AllTests[greatsmall] <- FALSE
  }
  
  
  AllTests <- as.logical(AllTests)
  
  #-----------------------------------------------
  
  return(AllTests)
}
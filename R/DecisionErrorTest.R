DecisionErrorTest <- function(x, alpha, ...) {
  computed <- x$Computed
  comparison <- x$Reported.Comparison
  reported <- x$Reported.P.Value
  testcomp <-  as.vector(x$Test.Comparison)
  
  # replace 'ns' by > alpha
  reported[comparison == "ns"] <- alpha
  comparison[comparison == "ns"] <- ">"
  
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
        (reported[equalequal] <= alpha &
           computed[equalequal] > alpha) |
        (reported[equalequal] > alpha &
           computed[equalequal] <= alpha)
      AllTests[equalsmall] <-
        reported[equalsmall] <= alpha &
        computed[equalsmall] > alpha
      AllTests[equalgreat] <-
        reported[equalgreat] >= alpha &
        computed[equalgreat] <= alpha
      
      
      AllTests[smallequal] <-
        reported[smallequal] <= alpha &
        computed[smallequal] >= alpha
      AllTests[smallsmall] <-
        reported[smallsmall] <= alpha &
        computed[smallsmall] >= alpha
      
      AllTests[greatequal] <-
        reported[greatequal] > alpha &
        computed[greatequal] <= alpha
      AllTests[greatgreat] <-
        reported[greatgreat] >= alpha &
        computed[greatgreat] <= alpha
      
    } else {
      AllTests[equalequal] <-
        (reported[equalequal] < alpha &
           computed[equalequal] >= alpha) |
        (reported[equalequal] >= alpha &
           computed[equalequal] < alpha)
      AllTests[equalsmall] <-
        reported[equalsmall] < alpha &
        computed[equalsmall] >= alpha
      AllTests[equalgreat] <-
        reported[equalgreat] >= alpha &
        computed[equalgreat] < alpha
      
      
      AllTests[smallequal] <-
        reported[smallequal] < alpha &
        computed[smallequal] >= alpha
      AllTests[smallsmall] <-
        reported[smallsmall] <= alpha &
        computed[smallsmall] >= alpha
      
      AllTests[greatequal] <-
        reported[greatequal] >= alpha &
        computed[greatequal] < alpha
      AllTests[greatgreat] <-
        reported[greatgreat] >= alpha &
        computed[greatgreat] < alpha
      
    }
    
    # these combinations of < & > are logically always correct
    AllTests[smallgreat] <- FALSE
    AllTests[greatsmall] <- FALSE
  }
  
  
  AllTests <- as.logical(AllTests)
  
  #-----------------------------------------------
  
  return(AllTests)
}
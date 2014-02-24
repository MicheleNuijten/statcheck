  ExTest <- function(
    x
    )
  {
    computed <- x$Computed
    reported <- x$Reported.P.Value
    comparison <- x$Reported.Comparison
    ExTests <- comparison=="="
    if (any(ExTests)){
      ExTests[comparison=="="] <- !(round(computed[ExTests],x$dec[ExTests])==round(reported[ExTests],x$dec[ExTests]))
      
      smallequal <- x$Test.Comparison=="<" & comparison=="="
      greatequal <- x$Test.Comparison==">" & comparison=="="
      
      if(any(smallequal)){
        ExTests[smallequal] <- !(round(computed[smallequal],x$dec[smallequal])<round(reported[smallequal],x$dec[smallequal]))
      }
      
      if(any(greatequal)){
        ExTests[greatequal] <- !(round(computed[greatequal],x$dec[greatequal])>round(reported[greatequal],x$dec[greatequal]))
      }
      
    }
    return(ExTests)
  }
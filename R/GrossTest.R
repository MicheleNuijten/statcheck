  GrossTest <- function(x,alpha=.05,...){
    computed <- x$Computed
    comparison <- x$Reported.Comparison
    reported <- x$Reported.P.Value
    
    # replace 'ns' by > alpha
    reported[comparison=="ns"] <- alpha
    comparison[comparison=="ns"] <- ">"
    
    Match <- paste(computed,comparison,reported)
    AllTests <- grepl("=|<|>",Match)
    if (any(AllTests)){
      AllTests[grepl("<",Match)] <- reported[grepl("<",Match)]<=alpha & computed[grepl("<",Match)] >=alpha
      AllTests[grepl(">",Match)] <- reported[grepl(">",Match)] >=alpha & computed[grepl(">",Match)]<alpha
      AllTests[grepl("=",Match)] <- (reported[grepl("=",Match)]<alpha & computed[grepl("=",Match)]>=alpha)|
        (reported[grepl("=",Match)]>=alpha & computed[grepl("=",Match)]<alpha)
    }
    AllTests <- as.logical(AllTests)
    
    return(AllTests)
  }
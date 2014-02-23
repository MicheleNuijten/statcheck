  InExTest <- function(x,...,alpha=.05){
    
    computed <- x$Computed
    comparison <- x$Reported.Comparison
    reported <- x$Reported.P.Value
    testcomp <- x$Test.Comparison
    
    # replace 'ns' for > alpha
    reported[comparison=="ns"] <- alpha
    comparison[comparison=="ns"] <- ">"
    
    Match <- paste(computed,comparison,reported)
    InExTests <- grepl("<|>",Match)
    
    if (any(InExTests)){
      InExTests[grepl("<|>",Match)] <- sapply(Match[grepl("<|>",Match)],function(m)!eval(parse(text=m)))
      
      smallsmall <- x$Test.Comparison=="<" & grepl("<",Match)
      smallgreat <- x$Test.Comparison=="<" & grepl(">",Match)
      greatsmall <- x$Test.Comparison==">" & grepl("<",Match)
      greatgreat <- x$Test.Comparison==">" & grepl(">",Match)
      
      if(any(smallsmall)){
        InExTests[smallsmall] <- !(round(computed[smallsmall],x$dec[smallsmall])<=round(reported[smallsmall],x$dec[smallsmall]))
      }
      
      InExTests[smallgreat] <- FALSE
      InExTests[greatsmall] <- FALSE
      
      if(any(greatgreat)){
        InExTests[greatgreat] <- !(round(computed[greatgreat],x$dec[greatgreat])>=round(reported[greatgreat],x$dec[greatgreat]))
      }
      
    }
    
    return(InExTests)
  }
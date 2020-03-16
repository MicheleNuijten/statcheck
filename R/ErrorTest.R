ErrorTest <- function(x, alpha, ...) {
  computed <- as.vector(x$Computed)
  comparison <- as.vector(x$Reported.Comparison)
  reported <-  as.vector(x$Reported.P.Value)
  testcomp <-  as.vector(x$Test.Comparison)
  
  # replace 'ns' for > alpha
  reported[comparison == "ns"] <- alpha
  comparison[comparison == "ns"] <- ">"
  
  Match <- paste(computed, comparison, reported)
  
  #-----------------------------------------------
  
  # select inexactly reported p values (p<../p>..)
  InExTests <- grepl("<|>", Match)
  
  # evaluate errors when test statistics are reported exactly (t()=.../F(,)=...)
  if (any(InExTests)) {
    InExTests[InExTests] <-
      sapply(Match[InExTests], function(m)
        ! eval(parse(text = m)))
  }
  
  # evaluate errors when test statistics are reported inexactly (t()</>.../F(,)</>...)
  smallsmall <- testcomp == "<" & comparison == "<"
  smallgreat <- testcomp == "<" & comparison == ">"
  greatsmall <- testcomp == ">" & comparison == "<"
  greatgreat <- testcomp == ">" & comparison == ">"
  
  if (any(smallsmall)) {
    InExTests[smallsmall] <-
      round(computed[smallsmall], x$dec[smallsmall]) <= round(reported[smallsmall], x$dec[smallsmall])
  }
  
  if (any(greatgreat)) {
    InExTests[greatgreat] <-
      round(computed[greatgreat], x$dec[greatgreat]) >= round(reported[greatgreat], x$dec[greatgreat])
  }
  
  # these combinations of < & > are logically always correct
  InExTests[smallgreat] <- FALSE
  InExTests[greatsmall] <- FALSE
  
  #-----------------------------------------------
  
  # select exactly reported p values (p=..)
  ExTests <- comparison == "="
  
  # evaluate errors when test statistics are reported exactly (t()=.../F(,)=...)
  if (any(ExTests)) {
    ExTests[ExTests] <-
      !(round(computed[ExTests], x$dec[ExTests]) == round(reported[ExTests], x$dec[ExTests]))
  }
  
  # evaluate errors when test statistics are reported inexactly (t()</>.../F(,)</>...)
  smallequal <- x$Test.Comparison == "<" & comparison == "="
  greatequal <- x$Test.Comparison == ">" & comparison == "="
  
  if (any(smallequal)) {
    ExTests[smallequal] <-
      round(computed[smallequal], x$dec[smallequal]) >= round(reported[smallequal], x$dec[smallequal])
  }
  
  if (any(greatequal)) {
    ExTests[greatequal] <-
      round(computed[greatequal], x$dec[greatequal]) <= round(reported[greatequal], x$dec[greatequal])
  }
  
  #-----------------------------------------------
  
  # a result is an error if InExactError and/or ExactError are TRUE
  Error <- !(InExTests == FALSE & ExTests == FALSE)
  
  return(Error)
}
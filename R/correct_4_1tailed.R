correct_4_1tailed <- function(Res, alpha, pEqualAlphaSig) {
  
  Res1tailed <- Res
  Res1tailed$Computed <- Res1tailed$Computed / 2
  
  Res1tailed$Error <- ErrorTest(Res1tailed, alpha = alpha)
  Res1tailed$DecisionError <- DecisionErrorTest(Res1tailed, alpha = alpha,
                                                pEqualAlphaSig = pEqualAlphaSig)
  
  Res$Error[!((
    Res$Statistic == "F" |
      Res$Statistic == "Chi2" |
      Res$Statistic == "Q"
  ) &
    Res$df1 > 1) &
    Res$OneTailedInTxt == TRUE & Res1tailed$Error == FALSE] <- FALSE
  
  Res$DecisionError[!((
    Res$Statistic == "F" |
      Res$Statistic == "Chi2" |
      Res$Statistic == "Q"
  ) &
    Res$df1 > 1) &
    Res$OneTailedInTxt == TRUE &
    Res1tailed$DecisionError == FALSE] <- FALSE
  
  return(Res)
}
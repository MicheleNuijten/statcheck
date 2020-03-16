check_correct_rounding <- function(Res, OneTailedTests){
  
  lower <- Res$Value - (.5 / 10 ^ Res$testdec)
  upper <- Res$Value + (.5 / 10 ^ Res$testdec)
  
  if (Res$Statistic == "F") {
    upP <- pf(lower, Res$df1, Res$df2, lower.tail = FALSE)
    lowP  <-
      pf(upper, Res$df1, Res$df2, lower.tail = FALSE)
    
  } else if (Res$Statistic == "t") {
    if (lower < 0) {
      lowP <- pt(lower, Res$df2) * 2
      upP  <- pt(upper, Res$df2) * 2
    } else{
      upP <- pt(-1 * lower, Res$df2) * 2
      lowP  <- pt(-1 * upper, Res$df2) * 2
    }
    
  } else if (Res$Statistic == "Chi2" |
             Res$Statistic == "Q" |
             Res$Statistic == "Qw" | 
             Res$Statistic == "Qb") {
    upP <- pchisq(lower, Res$df1, lower.tail = FALSE)
    lowP  <- pchisq(upper, Res$df1, lower.tail = FALSE)
    
  } else if (Res$Statistic == "r") {
    if (lower < 0) {
      lowP <- pmin(pt(r2t(lower, Res$df2), Res$df2) * 2, 1)
      upP  <-
        pmin(pt(r2t(upper, Res$df2), Res$df2) * 2, 1)
    } else {
      upP <- pmin(pt(-1 * r2t(lower, Res$df2), Res$df2) * 2, 1)
      lowP  <-
        pmin(pt(-1 * r2t(upper, Res$df2), Res$df2) * 2, 1)
    }
    
  } else if (Res$Statistic == "Z" |
             Res$Statistic == "z") {
    if (lower < 0) {
      lowP <- pnorm(abs(lower), lower.tail = FALSE) * 2
      upP  <- pnorm(abs(upper), lower.tail = FALSE) * 2
    } else {
      upP <- pnorm(lower, lower.tail = FALSE) * 2
      lowP  <- pnorm(upper, lower.tail = FALSE) * 2
    }
    
  }
  
  if (OneTailedTests == TRUE) {
    upP <- upP / 2
    lowP <- lowP / 2
  }
  
  if (Res$Reported.Comparison == "=") {
    correct_round <-
      ifelse(
        Res$Error == TRUE &
          Res$Reported.P.Value >= round(lowP, Res$dec) &
          Res$Reported.P.Value <= round(upP, Res$dec),
        TRUE,
        FALSE
      )
  }
  
  if (Res$Reported.Comparison == "<") {
    correct_round <-
      ifelse(Res$Error == TRUE &
               Res$Reported.P.Value > lowP, TRUE, FALSE)
  }
  
  if (Res$Reported.Comparison  == ">") {
    correct_round <-
      ifelse(Res$Error == TRUE &
               Res$Reported.P.Value < upP, TRUE, FALSE)
    
  }
  
  return(correct_round)
  
}
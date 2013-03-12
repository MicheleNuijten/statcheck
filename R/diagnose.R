diagnose <- function(x){
  # x = a statcheck output
  
  # create shorter variable names
  computed <- x$Computed
  comparison <- x$Reported.Comparison
  reported <- x$Reported.P.Value
  raw <- x$Raw
  
  # one-sided test
  # this is not necessarily an error, it is entirely possible that the 
  # researchers just made use of a one-sided test and reported it
  # statcheck cannot detect this
  onetail <- computed/2
  
  OneTail <- ifelse(!(x$InExactError==FALSE & x$ExactError==FALSE) &
    (grepl("=",comparison)[!is.na(onetail)] & round(reported,2)==round(onetail,2))
    | (grepl("<",comparison) & reported==.05 & onetail < reported & computed > reported)[!is.na(onetail)],
    TRUE,FALSE)
  
  # "correct" rounding differences
  # e.g. t=2.3 could be 2.25 to 2.34 with its range of p values
  correct_round <- numeric()
  
  lower <- x$Value-.005
  upper <- x$Value+.004
  
  r2t <- function(r,df){
    r / (sqrt((1-r^2)/df))
  }
  
  for(i in 1:nrow(x)){
    
    if(x[i,]$Statistic=="F"){
      upP <- pf(lower[i],x[i,]$df1,x[i,]$df2,lower.tail=FALSE)
      lowP  <- pf(upper[i],x[i,]$df1,x[i,]$df2,lower.tail=FALSE)
      
    } else if(x[i,]$Statistic=="t"){
      upP <- pt(-1*abs(lower[i]),x[i,]$df1)*2
      lowP  <- pt(-1*abs(upper[i]),x[i,]$df1)*2
      
    } else if(x[i,]$Statistic=="Chi2"){
      upP <- pchisq(lower[i],x[i,]$df1,lower.tail=FALSE)
      lowP  <- pchisq(upper[i],x[i,]$df1,lower.tail=FALSE)
      
    } else if(x[i,]$Statistic=="r"){
      upP <- pmin(pt(-1*abs(r2t(lower[i],x[i,]$df1)),x[i,]$df1)*2,1)
      lowP  <- pmin(pt(-1*abs(r2t(upper[i],x[i,]$df1)),x[i,]$df1)*2,1)
    }
    
    correct_round[i] <- ifelse(!(x[i,]$InExactError==FALSE & x[i,]$ExactError==FALSE) & reported[i]>lowP & reported[i]<upP,TRUE,FALSE)
  }
  
  CorrectRound <- as.logical(correct_round)
  
  # rounding errors
  # e.g. p=.049 is rounded to p=.04 instead of .05
  RoundError <- ifelse(!(x$InExactError==FALSE & x$ExactError==FALSE) & CorrectRound==FALSE & (reported==trunc(computed*100)/100|reported-.01==trunc(computed*100)/100),TRUE,FALSE)
  
  # reported p < .000
  PSmallerThanZero <- ifelse(!(x$InExactError==FALSE & x$ExactError==FALSE) & grepl("<",comparison) & reported==0,TRUE,FALSE)
  
  # reported < when = would be correct
  # e.g. p < .123 when p = .123
  SmallerInsteadEqual <- ifelse(!(x$InExactError==FALSE & x$ExactError==FALSE) & grepl("<|>",comparison) & (round(reported,3)==round(computed,3)|round(reported,2)==round(computed,2)),TRUE,FALSE)
  
  # Bonferroni correction
  # e.g. when there are six tests reported and the p values are multiplied by 6
  CorrectedP <- numeric()
  
  for (i in 1:nrow(x)){
    CorrectedP[i] <- computed[i]*as.numeric(table(x$Source)[x$Source[i]])
  }
  
  Bonferroni <- ifelse(!(x$InExactError==FALSE & x$ExactError==FALSE) & grepl("=",comparison) & (round(CorrectedP,3)==round(reported,3)|round(CorrectedP,2)==round(reported,2)),TRUE,FALSE)
  
  # unidentifiable error
  Unidentifiable <- ifelse(!(x$InExactError==FALSE & x$ExactError==FALSE) & !(OneTail|CorrectRound|RoundError|PSmallerThanZero|SmallerInsteadEqual|Bonferroni),TRUE,FALSE)
  
  # copy paste errors
  # same string of results elsewhere in article?
  CopyPaste <- numeric()
  for (i in 1:length(x$Raw)){
    x_new <- x[-i,]
    CopyPaste[i] <- x$Raw[i]%in%x_new$Raw[x_new$Source==x_new$Source[i]]
  }
  CopyPaste <- as.logical(CopyPaste)
  
  # results
  res_full <- data.frame(Source=x$Source,
                         Raw=x$Raw,
                         Computed=round(x$Computed,3),
                         OneTail=OneTail,
                         CorrectRound=CorrectRound,
                         RoundError=RoundError,
                         PSmallerThanZero=PSmallerThanZero,
                         SmallerInsteadEqual=SmallerInsteadEqual,
                         Bonferroni=Bonferroni,
                         Unidentifiable=Unidentifiable,
                         CopyPaste=CopyPaste)
  
  res_error <- res_full[!(x$InExactError==FALSE & x$ExactError==FALSE),]
  
  res_copy <- res_full[res_full$CopyPaste,]
  res_copy <- res_copy[order(res_copy$Raw),]
  
  # complete result
  res <- list(FullDiagnosis=res_full,ErrorDiagnosis=res_error,CopyPaste=res_copy)
  
  class(res) <- c("statcheck","diagnose","list")
  
  return(res)
  
}
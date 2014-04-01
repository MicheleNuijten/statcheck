diagnose <- structure(function(# Diagnose the most likely cause why a p value is inconsistent with its test statistic and degrees of freedom.
  ### This function analyzes whether inconsistent results in articles (as diagnosed with \code{\link{statcheck}} could be due to one of the most frequently observed causes (Bakker & Wicherts, 2011).
  x
  ### a \code{statcheck} output.
  ){ 
  ##details<<
  ## This dataframe contains the error diagnosis of the analyzed articles that contained an error and has the following components: 
  ## \item{Source: Name of the file of which the statistic is extracted}
  ## \item{Raw: Raw string of the statistical reference that is extracted}
  ## \item{Computed: The recomputed p-value}
  ## \item{OneTail: Logical. Is it likely that the reported p value resulted from a correction for one-sided testing?}
  ## \item{RoundError: Logical. P value wrongly rounded upward or downward. E.g. rounding a p value of .049 to .04.}
  ## \item{PSmallerThanZero: Logical. Reported p <.000.}
  ## \item{SmallerInsteadEqual: Logical. Reported "<" when "=" would be correct. E.g. F(2,20)=2.33, p <.123, whereas the correct p value is equal to .123.}
  ## \item{Bonferroni: Logical. Could the reported p value have resulted from a Bonferroni correction? Note: The corrected p values were calculated by multiplying the reported p values by the number of statistical results reported in one article. This is a rough estimation.}
  ## \item{Unidentifiable: Logical. The error could not be classified on the basis of the extracted information.}
  ## \item{CopyPaste: Logical. Does the exact string of the extracted raw results occur anywhere else in the article?}
  ## This dataframe contains an overview of the detected copy-paste errors in all the articles. Its components are the same as the ones in ErrorDiagnosis. Note that a copy-paste error could still be congruent in terms of test statistic and p value, so it is possible that these errors do not show up in ErrorDiagnosis. 
  ## This dataframe contains the error diagnosis for every article, not just the articles that contained an error. The components are the same as in ErrorDiagnosis. This dataframe is not automatically printed but can be obtained through $FullDiagnosis.
  ##references<<
  ## Bakker, M. & Wicherts, J. M. (2011). The (mis)reporting of statistical results in psychology journals. Behavior Research Methods, 43, 666-678.
  ##note<<
  ## Please note that the resulting diagnosis is more a rough indication of what could be going on than it is a clear cut verdict. The function is therefore most suitable if you want to get a rough idea of the type of errors made in a certain set of articles. 
  ##seealso<<
  ## \code{\link{statcheck}}, \code{\link{identify.statcheck}}
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
  
  OneTail <- ifelse(x$Error==TRUE &
    (grepl("=",comparison)[!is.na(onetail)] & round(reported,2)==round(onetail,2))
    | (grepl("<",comparison) & reported==.05 & onetail < reported & computed > reported)[!is.na(onetail)],
    TRUE,FALSE)
  

  # rounding errors
  # e.g. p=.049 is rounded to p=.04 instead of .05
  RoundError <- ifelse(x$Error==TRUE & (reported==trunc(computed*100)/100|reported-.01==trunc(computed*100)/100),TRUE,FALSE)
  
  # reported p < .000
  PSmallerThanZero <- ifelse(x$Error==TRUE & grepl("<",comparison) & reported==0,TRUE,FALSE)
  
  # reported < when = would be correct
  # e.g. p < .123 when p = .123
  SmallerInsteadEqual <- ifelse(x$Error==TRUE & grepl("<|>",comparison) & (round(reported,3)==round(computed,3)|round(reported,2)==round(computed,2)),TRUE,FALSE)
  
  # Bonferroni correction
  # e.g. when there are six tests reported and the p values are multiplied by 6
  CorrectedP <- numeric()
  
  for (i in 1:nrow(x)){
    CorrectedP[i] <- computed[i]*as.numeric(table(x$Source)[x$Source[i]])
  }
  
  Bonferroni <- ifelse(x$Error==TRUE & grepl("=",comparison) & (round(CorrectedP,3)==round(reported,3)|round(CorrectedP,2)==round(reported,2)),TRUE,FALSE)
  
  # unidentifiable error
  Unidentifiable <- ifelse(x$Error==TRUE & !(OneTail|RoundError|PSmallerThanZero|SmallerInsteadEqual|Bonferroni),TRUE,FALSE)
  
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
                         RoundError=RoundError,
                         PSmallerThanZero=PSmallerThanZero,
                         SmallerInsteadEqual=SmallerInsteadEqual,
                         Bonferroni=Bonferroni,
                         Unidentifiable=Unidentifiable,
                         CopyPaste=CopyPaste)
  
  res_error <- res_full[x$Error==TRUE,]
  
  res_copy <- res_full[res_full$CopyPaste,]
  res_copy <- res_copy[order(res_copy$Raw),]
  
  # complete result
  res <- list(FullDiagnosis=res_full,ErrorDiagnosis=res_error,CopyPaste=res_copy)
  
  class(res) <- c("statcheck","diagnose","list")
  
  return(res)
  ### a list that contains four dataframes: ErrorDiagnosis, CopyPaste, Summary, and FullDiagnosis. In the following sections the content of these dataframes will be described in detail. 
},ex=function(){
  # given that the articles of interest are saved in "DIR"
DIR <- "C:/mydocuments/articles"
stat_result <- checkdir(DIR)

diagnosis <- diagnose(stat_result)

# print diagnosis
diagnosis

# print diagnosis for all articles
diagnosis$FullDiagnosis
})
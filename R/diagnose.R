###
# create a function that diagnoses the most likely cause for an incorrectly reported result.
# option:
# calculate p values under every type of error
# see which error renders a p value closest to the reported result
# if closest match still differs too much (what is too much?) categorize under "different"
# maybe "onetail" last, because that will be the biggest possible difference
# otherwise all errors will be classified as a onetail error

diagnose <- function(x,...){
  # x = a statcheck output
  
  # select wrong results (there's no need to diagnose sound results)
  x <- x[!(x$InExactError==FALSE & x$ExactError==FALSE),]
  
  # create shorter variable names
  computed <- x$Computed
  comparison <- x$Reported.Comparison
  reported <- x$Reported.P.Value
  raw <- x$Raw
  
  # one-sided test
  onetail <- computed/2
  
  OneTail <- ifelse(
    (!grepl("<|>",comparison)[!is.na(onetail)] & abs(reported - onetail)[!is.na(onetail)] <
       abs(reported - computed)[!is.na(onetail)]) 
    | (grepl("<",comparison) & onetail < reported & computed > reported)[!is.na(onetail)]
    | (grepl(">",comparison) & onetail > reported & computed < reported)[!is.na(onetail)],  
    TRUE,FALSE)
  
  computed[!is.na(onetail)] <- ifelse(
    (!grepl("<|>",comparison)[!is.na(onetail)] & abs(x$Reported.P.Value - x$OneTail)[!is.na(x$OneTail)] <
       abs(x$Reported.P.Value - x$Computed)[!is.na(x$OneTail)]) 
    | (grepl("<",comparison) & x$OneTail < x$Reported.P.Value & x$Computed > x$Reported.P.Value)[!is.na(x$OneTail)]
    | (grepl(">",comparison) & x$OneTail > x$Reported.P.Value & x$Computed < x$Reported.P.Value)[!is.na(x$OneTail)],  
    x$OneTail[!is.na(x$OneTail)],x$Computed[!is.na(x$OneTail)])  
  
  # "correct" rounding differences
  # e.g. t=2.3 could be 2.25 to 2.34 with its range of p values
  correct_round <- 
    
  
  # rounding errors
  # e.g. p=.049 is rounded to p=.04 instead of .05
  
  
  # incorrect reporting of smallest p values
  # e.g. p=.000097 reported as p = .001 instead of p < .001
  
  
  # copy paste errors
  # same string of results elsewhere in article?
  # don't just analyze the erroneous results, but all the results
  CopyPaste <- ifelse(grepl())
  
  # reported p < .000
  PSmallerThanZero <- ifelse(grepl("<.000|< .000",raw),TRUE,FALSE)
  
  # reported < when = would be correct
  # e.g. p < .123 when p = .123
  SmallerInsteadEqual <- ifelse(grepl("<|>",comparison) & round(reported,3)==round(computed,3),TRUE,FALSE)
  
  # unidentifiable error
  Unidentifiable <- ifelse(!(OneTail|correct_round|PSmallerThanZero|SmallerInsteadEqual),TRUE,FALSE)
  
  # result
  res <- data.frame(Source=x$Source,
                    OneTail=OneTail)
  
}
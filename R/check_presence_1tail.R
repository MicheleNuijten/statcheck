check_presence_1tail <- function(Res, messages, OneTailedTxt) {
  
  computed <- Res$Computed
  comparison <- Res$Reported.Comparison
  reported <- Res$Reported.P.Value
  raw <- Res$Raw
  onetail <- computed / 2
  onetailtxt <- Res$OneTailedInTxt
  
  OneTail <- ifelse(
    Res$Error == TRUE & (grepl("=", comparison) 
                         & round(reported, 2) == round(onetail, 2)) |
      (grepl("<", comparison) &
         onetail < reported & computed > reported),
    TRUE, FALSE)
  
  Res$OneTail <- OneTail
  
  if (messages == TRUE & 
      any(OneTail[!is.na(OneTail)] == TRUE &
          onetailtxt[!is.na(onetailtxt)] == FALSE)) {
    message(
      "\n Check for one tailed tests. \n \n Some of the p value incongruencies might in fact be one tailed tests. It is recommended to check this in the actual paper or text. Check if the p values would also be incongruent if the test is indeed one sided by running statcheck again with 'OneTailedTests' set to TRUE. To see which Sources probably contain a one tailed test, try unique(x$Source[x$OneTail]) (where x is the statcheck output). \n "
    )
  }
}

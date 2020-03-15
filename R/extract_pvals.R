extract_pvals <- function(text){
 
   # Create empty data frame for p values:
  pRes <- data.frame(
    Source = NULL,
    Statistic = NULL,
    Reported.Comparison = NULL,
    Reported.P.Value = NULL,
    Raw = NULL,
    stringsAsFactors = FALSE
  )
  
  if (length(text) == 0){
    return(Res)
  }
  
  if (is.null(names(text))){
    names(text) <-  1:length(text)
  }
  
  for (i in 1:length(text)) {
    txt <- text[i]
    
    #---------------------------
    
    # extract all p values in order to calculate the ratio (statcheck results)/(total # of p values)
    
    # p-values
    # Get location of p-values in text:
    pLoc <-
      gregexpr("([^a-z]ns)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*)",
               txt,
               ignore.case = TRUE)[[1]]
    
    if (pLoc[1] != -1) {
      # Get raw text of p-values:
      pRaw <-
        substring(txt, pLoc, pLoc + attr(pLoc, "match.length") - 1)
      
      nums <-
        gregexpr("(\\d*\\.?\\d+\\s?e?-?\\d*)|ns", pRaw, ignore.case = TRUE)
      
      # Extract p-values
      suppressWarnings(pValsChar <-
                         substring(
                           pRaw,
                           sapply(nums, '[', 1),
                           sapply(nums, function(x)
                             x[1] + attr(x, "match.length")[1] - 1)
                         ))
      
      suppressWarnings(pVals <- as.numeric(pValsChar))
      
      # Extract (in)equality
      eqLoc <- gregexpr("p\\s?.?", pRaw)
      pEq <- substring(
        pRaw,
        sapply(eqLoc, function(x)
          x[1] + attr(x, "match.length")[1] - 1),
        sapply(eqLoc, function(x)
          x[1] + attr(x, "match.length")[1] - 1)
      )
      pEq[grepl("ns", pRaw, ignore.case = TRUE)] <- "ns"
      
      pvalues <- data.frame(
        Source = names(text)[i],
        Statistic = "p",
        Reported.Comparison = pEq,
        Reported.P.Value = pVals,
        Raw = pRaw,
        stringsAsFactors = FALSE
      )
      
      # remove p values greater than one
      pvalues <-
        pvalues[pvalues$Reported.P.Value <= 1 |
                  is.na(pvalues$Reported.P.Value), ]
      
      pRes <- rbind(pRes, pvalues)
      rm(pvalues)
      
      return(pRes)
      
    }
  }
}
extract_1tail <- function(text){
  for (i in 1:length(text)) {
    txt <- text[i]
    
    # search for "one-sided"/"one-tailed"/"directional" in full text to detect one-sided testing
    onesided <-
      gregexpr("one.?sided|one.?tailed|directional", txt, ignore.case = TRUE)[[1]]
    
    if (onesided[1] != -1) {
      onesided <- 1
    } else {
      onesided <- 0
    }
    
    OneTailedInTxt <- as.logical(onesided)
    
    return(OneTailedInTxt)
  }
}


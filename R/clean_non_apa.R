clean_non_apa <- function(nhst_raw){
  
  # remove all spaces
  # the parsing functions can handle no spaces, but it would be too complicated
  # to take into account every possible number of spaces, so removing them
  # all is easiest
  nhst_clean <- gsub(pattern = "\\s+", replacement = "",
                     nhst_raw)
  
  # replace square or curly brackets with parentheses
  nhst_clean <- gsub(pattern = RGX_SQ_CURLY1, replacement = RGX_PRTS_1, 
                     nhst_clean)
  nhst_clean <- gsub(pattern = RGX_SQ_CURLY2, replacement = RGX_PRTS_2, 
                     nhst_clean)
  
  # add parentheses around degrees of freedom that were in subscripts
  # locate a letter followed by a digit and put a parentheses between these 2 
  # "regex groups" next, locate a digit followed by a comparison sign, and put 
  # a closing parenthesis between these two groups
  
  nhst_clean <- gsub(pattern = "^([a-zA-Z])(\\d\\s?(?!\\())", 
                     replacement = "\\1\\(\\2", 
                     nhst_clean,
                     perl = TRUE)
  nhst_clean <- gsub(pattern = "(\\d)([=<>])", 
                     replacement = "\\1\\)\\2", 
                     nhst_clean)
  
  
  # remove "DF = " from degrees of freedom
  nhst_clean <- gsub(pattern = RGX_DF_TXT, replacement = "", nhst_clean)
  
  # replace semi-colons with commas
  nhst_clean <- gsub(pattern = ";", replacement = ",", nhst_clean)
  
  return(nhst_clean)
}



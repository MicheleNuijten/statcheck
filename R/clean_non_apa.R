clean_non_apa <- function(nhst_raw){
  
  # replace square or curly brackets with parentheses
  nhst_clean <- gsub(pattern = RGX_SQ_CURLY1, replacement = RGX_PRTS_1, 
                         nhst_raw)
  nhst_clean <- gsub(pattern = RGX_SQ_CURLY2, replacement = RGX_PRTS_2, 
                     nhst_clean)
  
  # remove "DF = " from degrees of freedom
  nhst_clean <- gsub(pattern = RGX_DF_TXT, replacement = "", nhst_clean)
  
  return(nhst_clean)
}

clean_non_apa <- function(nhst_raw){
  
  # replace square or curly brackets with parentheses
  nhst_raw_prts1 <- gsub(pattern = RGX_BRACK_1, replacement = RGX_PRTS_1, nhst_raw)
  nhst_raw_prts12 <- gsub(pattern = RGX_BRACK_2, replacement = RGX_PRTS_2, nhst_raw_prts1)
  
  return(nhst_raw_prts12)
}
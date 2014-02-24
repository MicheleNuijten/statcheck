getClosest <- function(# Inner function to extract closest computed match
	x
	)
{
  computed <- x$Computed
  comparison <- x$Reported.Comparison
  
  computed[!is.na(x$OneTail)] <- ifelse(
    (!grepl("<|>",comparison)[!is.na(x$OneTail)] & abs(x$Reported.P.Value - x$OneTail)[!is.na(x$OneTail)] <
      abs(x$Reported.P.Value - x$Computed)[!is.na(x$OneTail)]) 
    | (grepl("<",comparison) & x$OneTail < x$Reported.P.Value & x$Computed > x$Reported.P.Value)[!is.na(x$OneTail)]
    | (grepl(">",comparison) & x$OneTail > x$Reported.P.Value & x$Computed < x$Reported.P.Value)[!is.na(x$OneTail)],  
    x$OneTail[!is.na(x$OneTail)],x$Computed[!is.na(x$OneTail)])  

  return(computed)
}
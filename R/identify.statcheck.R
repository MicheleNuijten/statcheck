identify.statcheck <- function(x){
  
  reported <- x$Reported.P.Value
  computed <- x$Computed
  
  plot(x)
  ID <- identify(reported,computed)
  
  res <- x[ID,]
  class(res) <- c("statcheck","data.frame")
  
  return(res)
}
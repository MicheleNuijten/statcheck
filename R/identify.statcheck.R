identify.statcheck <- function(x){
  
  reported <- x$Reported.P.Value
  computed <- x$Computed
  
  plot(x) # makes use of the plot.statcheck() function
  ID <- identify(reported,computed)
  
  res <- x[ID,]
  class(res) <- c("statcheck","data.frame")
  
  return(res)
}
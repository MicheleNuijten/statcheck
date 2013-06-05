summary.statcheck <- function(object,...){
  
  x <- object
  
  # Source
  Source <- c(ddply(x,"Source",function(x) unique(x$Source))[,2],"Total")
  
  # Number of p values extracted per article and in total
  pValues <- c(ddply(x,"Source",function(x) nrow(x))[,2],nrow(x))
  
  # Number of errors per article and in total
  Errors <- c(ddply(x,"Source",function(x) sum(x$Error))[,2],sum(x$Error))
  
  # Number of decision errors per article and in total
  DecisionErrors <- c(ddply(x,"Source",function(x) sum(x$DecisionError))[,2],sum(x$DecisionError))
  
  # Results in dataframe
  res <- data.frame(Source=c(unique(x$Source),"Total"),
                    pValues=pValues,
                    Errors=Errors,
                    DecisionErrors=DecisionErrors)
  
  class(res) <- c("statcheck","data.frame")
  
  return(res)
}
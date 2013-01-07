summary.statcheck <- function(object,...){
  
  x <- object
  
  # Source
  Source <- c(ddply(x,"Source",function(x) unique(x$Source))[,2],"Total")
  
  # Number of p values extracted per article and in total
  pValues <- c(ddply(x,"Source",function(x) nrow(x))[,2],nrow(x))
  
  # Number of exact errors per article and in total
  ExactErrors <- c(ddply(x,"Source",function(x) sum(x$ExactError))[,2],sum(x$ExactError))
  
  # Number of exact errors per article and in total
  InExactErrors <- c(ddply(x,"Source",function(x) sum(x$InExactError))[,2],sum(x$InExactError))
  
  # Number of decision errors per article and in total
  DecisionErrors <- c(ddply(x,"Source",function(x) sum(x$DecisionError))[,2],sum(x$DecisionError))
  
  # Results in dataframe
  res <- data.frame(Source=c(unique(x$Source),"Total"),
                    pValues=pValues,
                    ExactErrors=ExactErrors,
                    InExactErrors=InExactErrors,
                    DecisionErrors=DecisionErrors)
  
  class(res) <- c("statcheck","data.frame")
  
  return(res)
}
summary.statcheck <- structure(function(# Summary method for \code{statcheck}.
  ### Gives the summaries for a \code{statcheck} object.  
  object,
  ### a \code{statcheck} object.
  ...
  ### additional arguments affecting the summary produced.
  ){
  ##seealso<<
  ## \code{\link{statcheck}}
  x <- object
  
  # Source
  Source <- c(ddply(x,"Source",function(x) unique(x$Source))[,2],"Total")
  
  # Number of p values extracted per article and in total
  pValues <- c(ddply(x,"Source",function(x) nrow(x))[,2],nrow(x))
  
  # Number of errors per article and in total
  Errors <- c(ddply(x,"Source",function(x) sum(x$Error,na.rm=TRUE))[,2],sum(x$Error,na.rm=TRUE))
  
  # Number of decision errors per article and in total
  DecisionErrors <- c(ddply(x,"Source",function(x) sum(x$DecisionError,na.rm=TRUE))[,2],sum(x$DecisionError,na.rm=TRUE))
  
  # Results in dataframe
  res <- data.frame(Source=c(unique(x$Source),"Total"),
                    pValues=pValues,
                    Errors=Errors,
                    DecisionErrors=DecisionErrors)
  
  class(res) <- c("statcheck","data.frame")
  
  return(res)
  ##value<<
  ## A data frame containing for each extracted statistic:
  ## \item{Source}{Name of the file of which the statistic is extracted}
  ## \item{pValues}{The number of reported p values per article}
  ## \item{Errors}{The number of errors per article}
  ## \item{DecisionErrors}{The number of errors that caused a non-significant result to be reported as significant (or vice versa) per article}
},ex=function(){
  Text <- "blablabla the effect was very significant (t(100)=1, p < 0.001)"
Stat <- statcheck(Text)

summary(Stat)
  })
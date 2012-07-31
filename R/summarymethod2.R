summary.statcheck <- function(object,...){
  
  x <- object
  
  # Papers analyzed
  Paper <- c(daply(x,.(Source),function(x)unique(x$Source)),"Total")
  
  # Number of p values extracted per article and in total
  pValues <- c(ddply(x,"Source",function(x) nrow(x))[,2],nrow(x))
  
  
  # Choose onetailed if more appropriate:
  computed <- x$Computed
  
  computed[!is.na(x$OneTail)] <- ifelse(
    abs(x$Reported.P.Value - x$OneTail)[!is.na(x$OneTail)] <
      abs(x$Reported.P.Value - x$Computed)[!is.na(x$OneTail)],  
    x$OneTail[!is.na(x$OneTail)],x$Computed[!is.na(x$OneTail)])  
  
  x <- cbind(x,computed=computed)
  
  # Significant results reported as non significant per paper and in total
  SigAsNonSig <- ddply(x,"Source",function(x){
    sum((x$Reported.P.Value<.05 & x$computed>.05)[x$Reported.Comparison=="="],na.rm=TRUE) 
  })[,2]
  
  SigAsNonSig <- c(SigAsNonSig,sum(SigAsNonSig))
  
  # Non significant results reported as significant per paper and in total
  NonSigAsSig <- ddply(x,"Source",function(x){
    sum((x$Reported.P.Value>.05 & x$computed<.05)[x$Reported.Comparison=="="],na.rm=TRUE)
  })[,2]
  
  NonSigAsSig <- c(NonSigAsSig, sum(NonSigAsSig))
  
  # Total amount of errors in exact p values
  TotalExactErrors <- ddply(x,"Source",function(x){
    sum(abs(x$Reported.P.Value[x$Reported.Comparison=="="]-
      x$computed[x$Reported.Comparison=="="])>.01,na.rm=TRUE)
  })[,2]
  
  TotalExactErrors <- c(TotalExactErrors,sum(TotalExactErrors))
  
  # Mean deviation between reported and computed exact p values
  MeanDeviationExact <- ddply(x,"Source",function(x){
    mean(abs(x$Reported.P.Value[x$Reported.Comparison=="="]-
      x$computed[x$Reported.Comparison=="="]),na.rm=TRUE)
  })[,2]
  
  MeanDeviationExact <- c(MeanDeviationExact,mean(MeanDeviationExact,na.rm=TRUE))
  
  InExErrors <- function(x)
  {
    comparison <- gsub("=","==",x$Reported.Comparison)
    computed <- x$computed
    reported <- x$Reported.P.Value
    Match <- paste(computed,comparison,reported)
    InExMatch <- Match[grepl("<|>",Match)]
    InExTests <- sapply(InExMatch,function(m)eval(parse(text=m)))  
    return(sum(!InExTests,na.rm=TRUE))
  }
  
  
  # Results in dataframe
  res <- data.frame(Paper=Paper,
                    pValues=pValues,
                    NonSigAsSig=NonSigAsSig,
                    SigAsNonSig=SigAsNonSig,
                    TotalExactErrors=TotalExactErrors,
                    MeanDeviationExact=MeanDeviationExact,
                    TotalInExactErrors =c(daply(x,.(Source),InExErrors),InExErrors(x))
  )  
  class(res) <- c("statcheck","data.frame")
  
  return(res)
}
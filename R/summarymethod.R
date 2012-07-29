# library('plyr')
# library('devtools')
# install_github("statcheck","sachaepskamp")
# library('statcheck')
# dat <- checkPDFdir()


summary.statcheck <- function(x,...){
  
  
  
  # Papers analyzed
  Paper <- c(unique(x$Source),"Total")
  
  # Number of p values extracted per article and in total
  pValues <- c(ddply(x,"Source",function(x) nrow(x))[,2],nrow(x))
 
  
  # Choose onetailed if more appropriate:
  computed <- x$Computed
  
  computed[!is.na(x$OneTail)] <- ifelse(
    abs(x$Reported.P.Value - x$OneTail)[!is.na(x$OneTail)] <
      abs(x$Reported.P.Value - x$Computed)[!is.na(x$OneTail)],  
    x$OneTail[!is.na(x$OneTail)],x$Computed[!is.na(x$OneTail)])  
  
  x <- cbind(x,computed)
  
  
  # Significant results reported as non significant per paper and in total
  SigAsNonSig <- ddply(x,"Source",function(x){
    sum(x$Reported.P.Value<.05 & x$computed>.05) 
  })[,2]
  
  SigAsNonSig <- c(SigAsNonSig,sum(SigAsNonSig))
  
  # Non significant results reported as significant per paper and in total
  NonSigAsSig <- ddply(x,"Source",function(x){
    sum(x$Reported.P.Value>.05 & x$computed<.05)
  })[,2]
  
  NonSigAsSig <- c(NonSigAsSig, sum(NonSigAsSig))
  
  # Number of gross errors per paper and in total
  GrossErrors <- ddply(x,"Source",function(x){
    sum(x$Reported.P.Value>.05 & x$computed<.05,
        x$Reported.P.Value<.05 & x$computed>.05) 
  })[,2]
  
  GrossErrors <- c(GrossErrors,sum(GrossErrors))
    
  # Results in dataframe
  res <- data.frame(Paper=Paper,
                    Number_of_p_values=pValues,
                    NonSigAsSig=NonSigAsSig,
                    SigAsNonSig=SigAsNonSig,
                    TotalGrossErrors=GrossErrors)

  class(res) <- c("statcheck","data.frame")
  
  return(res)
}
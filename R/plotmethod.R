plot.statcheck <- function(x,...) {
  
  # Extract limit args:
  args <- list(...)
  if (is.null(args$xlim)) args$xlim <- c(0,1)
  if (is.null(args$ylim)) args$ylim <- c(0,1)
   
  reported <- x$Reported.P.Value
  computed <- x$Computed
  
  # Choose onetailed if more appropriate:
  computed[!is.na(x$OneTail)] <- ifelse(
    abs(x$Reported.P.Value - x$OneTail)[!is.na(x$OneTail)] <
    abs(x$Reported.P.Value - x$Computed)[!is.na(x$OneTail)],  
    x$OneTail[!is.na(x$OneTail)],x$Computed[!is.na(x$OneTail)])
  
  # scatterplot of reported and recalculated p values
  do.call(plot.default,c(list(x=reported,y=computed,
               xlab="reported p value",
               ylab="recalculated p value",                         
               pch=20),args))
  
  ## Gross error if:
    # Inexact p value wrongly specified
    # Exact p value leads to different conclusion
  
    comparison <- x$Reported.Comparison
  
    computed <- x$computed
    reported <- x$Reported.P.Value
    Match <- paste(computed,comparison,reported)
    InExTests <- grepl("<|>",Match)
    InExTests[grepl("<|>",Match)] <- sapply(Match[grepl("<|>",Match)],function(m)!eval(parse(text=m)))  
    
    ExTests <- grepl("=",comparison)
    ExTests[grepl("=",comparison)] <- ((reported>.05 & computed<.05)|(reported<.05 & computed>.05))[grepl("=",comparison)]
  
  
  # red dot for gross error (non-sig reported as sig and vice versa)
  points(reported[InExTests|ExTests],
         computed[InExTests|ExTests],
         pch=20,col="red")
  
  # triangles for exact p values
  points(x$Reported.P.Value[x$Reported.Comparison=="="],
         computed[x$Reported.Comparison=="="],
         pch=5)
  
  
  # general layout of figure:
  # lines & text to indicate under- and overestimates
  abline(h=.05)
  abline(v=.05)
  abline(0,1)
  
  text(.8,.4,"overestimated")
  text(.4,.8,"underestimated")
  
  text(0,.53,"non-sig",cex=.7)
  text(0,.50,"reported",cex=.7)
  text(0,.47,"as sig",cex=.7)
  
  text(.5,0,"sig reported as non-sig",cex=.7)
  
  par(xpd=TRUE)
  legend(.88,-.15,
         pch=c(20,5),
         col=c("red","black"),
         legend=c("gross error","exact"),
         cex=.8)
  par(xpd=FALSE)
}
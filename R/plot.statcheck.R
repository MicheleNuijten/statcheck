plot.statcheck <- function(x,...) {
  
  # Extract limit args:
  args <- list(...)
  if (is.null(args$xlim)) args$xlim <- c(0,1)
  if (is.null(args$ylim)) args$ylim <- c(0,1)
   
  reported <- x$Reported.P.Value
  computed <- x$Computed
    
  # scatterplot of reported and recalculated p values
  do.call(plot.default,c(list(x=reported,y=computed,
               xlab="reported p value",
               ylab="recalculated p value",                         
               pch=20),args))
  
  # orange dot for error 
  points(reported[x$ExactError],
         computed[x$ExactError],
         pch=20,col="orange")
  
  points(reported[x$InExactError],
         computed[x$InExactError],
         pch=20,col="orange")
  
  # red dot for gross error (non-sig reported as sig and vice versa)
  points(reported[x$DecisionError],
         computed[x$DecisionError],
         pch=20,col="red")
   
  # indicate exact p values with diamond
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
         pch=c(20,20,5),
         col=c("orange","red","black"),
         legend=c("p inconsistency","decision error","exact"),
         cex=.8)
  par(xpd=FALSE)
}
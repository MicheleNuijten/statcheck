plot.statcheck <- function(x,...) {
  
  # scatterplot of reported and recalculated p values
  plot.default(x$Reported.P.Value,x$Computed,
               xlab="reported p value",
               ylab="recalculated p value",
               xlim=c(0,1),ylim=c(0,1),
               pch=20)
  
  
  # red dot for gross error (non-sig reported as sig and vice versa)
  points(x$Reported.P.Value[which(x$Reported.P.Value>.05 & x$Computed<.05)],
         x$Computed[which(x$Reported.P.Value>.05 & x$Computed<.05)],
         pch=20,col="red")
  
  points(x$Reported.P.Value[which(x$Reported.P.Value<.05 & x$Computed>.05)],
         x$Computed[which(x$Reported.P.Value<.05 & x$Computed>.05)],
         pch=20,col="red")
  
  
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
  
}

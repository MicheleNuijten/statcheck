plot.statcheck <- function(# Plot method for "statcheck"
  ### Function for plotting of "statcheck" objects. Reported p values are plotted against recalculated p values, which allows the user to easily spot if articles contain miscalculations of statistical results. 
  x,
  ### a "statcheck" object. See \code{\link{statcheck}}.
  alpha=.05,
  ### assumed level of significance in the scanned texts. Defaults to .05. 
  ...
  ### arguments to be passed to methods, such as graphical parameters (see \code{\link{par}}).
  ) {
  ##details<<
  ## Inconsistencies between the reported and the recalculated p value are indicated with an orange dot. Recalculations of the p value that render a previously non significant result (p >= .5) as significant (p < .05), and vice versa, are considered gross errors, and are indicated with a red dot. Exactly reported p values (i.e. p = ..., as opposed to p < ... or p > ...) are indicated with a diamond.
  ##seealso<<
  ## \code{\link{statcheck}}
  # Extract limit args:
  args <- list(...)
  if (is.null(args$xlim)) args$xlim <- c(0,1)
  if (is.null(args$ylim)) args$ylim <- c(0,1)
   
  reported <- x$Reported.P.Value
  computed <- x$Computed
    
  # replace 'ns' for > alpha
  reported[x$Reported.Comparison=="ns"] <- alpha
  
  # scatterplot of reported and recalculated p values
  do.call(plot.default,c(list(x=reported,y=computed,
               xlab="reported p value",
               ylab="recalculated p value",                         
               pch=20),args))
  
  # orange dot for error 
  points(reported[x$Error],
         computed[x$Error],
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
         legend=c("p inconsistency","decision error","exact (p = ...)"),
         cex=.8)
  par(xpd=FALSE)
}
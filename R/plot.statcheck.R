plot.statcheck <- function(# Plot method for "statcheck"
  ### Function for plotting of "statcheck" objects. Reported p values are plotted against recalculated p values, which allows the user to easily spot if articles contain miscalculations of statistical results. 
  x,
  ### a "statcheck" object. See \code{\link{statcheck}}.
  alpha=.05,
  ### assumed level of significance in the scanned texts. Defaults to .05. 
  APAstyle=TRUE,
  ### if TRUE, prints plot in APA style
  group=NULL,
  ### indicate grouping variable to facet plot. Only works when APAstyle==TRUE
  ...
  ### arguments to be passed to methods, such as graphical parameters (see \code{\link{par}}).
) {
  ##details<<
  ## If APAstyle = FALSE, inconsistencies between the reported and the recalculated p value are indicated with an orange dot. Recalculations of the p value that render a previously non significant result (p >= .5) as significant (p < .05), and vice versa, are considered gross errors, and are indicated with a red dot. Exactly reported p values (i.e. p = ..., as opposed to p < ... or p > ...) are indicated with a diamond.
  ##seealso<<
  ## \code{\link{statcheck}}
  
  if(APAstyle==TRUE){
    
    # Many thanks for John Sakaluk for writing the code for APA style plotting
    
    # Add vector "Type" to statcheck object, specifying whether observations are 
    # correctly reported, reporting inconsistencies, or decision errors. 
    x$Type[x$Error=="FALSE" & x$DecisionError=="FALSE"] <- "Correctly Reported"
    x$Type[x$Error=="TRUE" & x$DecisionError=="FALSE"] <- "Reporting Inconsistency"
    x$Type[x$Error=="TRUE" & x$DecisionError=="TRUE"] <- "Decision Error"
    
    #Create ggplot "APA format" theme
    apatheme <- theme_bw()+
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.line=element_line())
    
    #If no grouping variable is specified, don't use faceting
    if(is.null(group)){
      
      #Create plot "p"; map computed p-values to x-axis, reported p-values to y-axis, and
      #color to the Type variable created earlier. Environment command allows apatheme to 
      #be applied later because of bug when creating functions with ggplot2
      p <- ggplot(x, 
                  aes(y = Computed,
                      x = Reported.P.Value,
                      col= Type),
                  environment = environment())  
      
      #Add data points to plot
      p + geom_point(size=2.5)+
        #Add vertical grey dashed line, located at specified alpha level
        geom_vline(xintercept=alpha, color="grey60",linetype="dashed")+
        #Add horizontal grey dashed line, located at specified alpha level
        geom_hline(yintercept=alpha, color="grey60",linetype="dashed")+
        #Add a line showing where accurately reported p-values should fall
        geom_abline(intercept=0, slope=1, color="grey60")+
        #Add text annotations demarcating over-/under-estimated areas of the plot
        annotate("text", x= 0.5, y = .10, label="overestimated")+
        annotate("text", x= 0.5, y = .90, label="underestimated")+
        #Rename the x- and y-axis, and manually specify breaks
        scale_x_continuous(name="Reported p-values", breaks=c(0.00, 0.05, 0.10, 0.25, 0.50, 0.75, 1.0))+
        scale_y_continuous(name="Computed p-values", breaks=c(0.00, 0.05, 0.10, 0.25, 0.50, 0.75, 1.0))+
        #Manually specify greyscale colors for different levels of Type
        scale_color_manual(breaks=c("Correctly Reported","Reporting Inconsistency","Decision Error"), 
                           values = c("grey80", "black","grey50"))+
        apatheme
      
    } else {
      
      #If grouping variable is specified, use for faceting
      
      #Create plot "p"; map computed p-values to x-axis, reported p-values to y-axis, and
      #color to the Type variable created earlier. Environment command allows apatheme to 
      #be applied later because of bug when creating functions with ggplot2
      p <- ggplot(x, 
                  aes(y = Computed,
                      x = Reported.P.Value,
                      col= Type),
                  environment = environment())  
      
      #Add data points to plot
      p + geom_point(size=2.5)+
        #Add vertical grey dashed line, located at specified alpha level
        geom_vline(xintercept=alpha, color="grey60",linetype="dashed")+
        #Add horizontal grey dashed line, located at specified alpha level
        geom_hline(yintercept=alpha, color="grey60",linetype="dashed")+
        #Add a line showing where accurately reported p-values should fall
        geom_abline(intercept=0, slope=1, color="grey60")+
        #Add text annotations demarcating over-/under-estimated areas of the plot
        annotate("text", x= 0.5, y = .10, label="overestimated")+
        annotate("text", x= 0.5, y = .90, label="underestimated")+
        #Rename the x- and y-axis, and manually specify breaks
        scale_x_continuous(name="Reported p-values", breaks=c(0.00, 0.05, 0.10, 0.25, 0.50, 0.75, 1.0))+
        scale_y_continuous(name="Computed p-values", breaks=c(0.00, 0.05, 0.10, 0.25, 0.50, 0.75, 1.0))+
        #Manually specify greyscale colors for different levels of Type
        scale_color_manual(breaks=c("Correctly Reported","Reporting Inconsistency","Decision Error"), 
                           values = c("grey80", "black","grey50"))+
        facet_grid(as.formula(paste(group, "~ .")))+
        apatheme
    }
    
  } else {
    
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
}
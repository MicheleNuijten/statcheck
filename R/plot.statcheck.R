#' Plot method for statcheck
#' 
#' Function for plotting of \code{statcheck} objects. Reported p values are 
#' plotted against recalculated p values, which allows the user to easily spot 
#' if articles contain miscalculations of statistical results.
#' 
#' If APAstyle = FALSE, inconsistencies between the reported and the recalculated p value are indicated with an orange dot. Recalculations of the p value that render a previously non significant result (p >= .5) as significant (p < .05), and vice versa, are considered decision errors, and are indicated with a red dot. Exactly reported p values (i.e. p = ..., as opposed to p < ... or p > ...) are indicated with a diamond.
#' 
#' @section Acknowledgements:
#' Many thanks to John Sakaluk who adapted the plot code to create graphs in 
#' APA style.
#' 
#' @seealso \code{\link{statcheck}}
#' 
#' @param x A statcheck object. See \code{\link{statcheck}}.
#' @param alpha assumed level of significance in the scanned texts. Defaults to 
#' .05.
#' @param APAstyle If TRUE, prints plot in APA style.
#' @param group Indicate grouping variable to facet plot. Only works when 
#' \code{APAstyle==TRUE}
#' @param ... arguments to be passed to methods, such as graphical parameters 
#' (see \code{\link{par}}).
#' 
#' @examples 
#' # First we need a statcheck object
#' # Here, we create one by running statcheck on some raw text
#' 
#' txt <- "This test is consistent t(28) = 0.2, p = .84, but this one is 
#' inconsistent: F(2, 28) = 4.2, p = .01. This final test is even a
#' gross/decision inconsistency: z = 1.23, p = .03"
#' 
#' result <- statcheck(txt)
#' 
#' # We can then plot the statcheck object 'result' by simply calling plot() on 
#' # "result". R will know what kind of plot to make, because "result" is of 
#' # class "statcheck"
#' plot(result)
#' 
#' @importFrom ggplot2 theme theme_bw element_blank element_line ggplot aes 
#' geom_point geom_vline geom_hline geom_abline annotate scale_x_continuous
#' scale_y_continuous scale_color_manual facet_grid
#' @importFrom rlang .data
#' @importFrom graphics plot.default points abline text par legend
#' 
#' @export

plot.statcheck <- function(
  x,
  alpha = .05,
  APAstyle = TRUE,
  group = NULL,
  ...
){
  
  # replace 'ns' for > alpha
  ns <- x[[VAR_P_COMPARISON]] == "ns"
  x[[VAR_P_COMPARISON]][ns] <- ">"
  x[[VAR_REPORTED_P]][ns] <- alpha
  
  if (APAstyle == TRUE) {
    
    # Add vector "Type" to statcheck object, specifying whether observations are
    # correctly reported, reporting inconsistencies, or decision errors.
    # First create an empty variable for Type to avoid a NOTE in the R CMD Check
    # that there is "no visible binding for global variable"
    Type <- rep(NA, nrow(x))
    x <- cbind(x, Type)
    
    x$Type[x[[VAR_ERROR]] == "FALSE" &
             x[[VAR_DEC_ERROR]] == "FALSE"] <- "Correctly Reported"
    x$Type[x[[VAR_ERROR]] == "TRUE" &
             x[[VAR_DEC_ERROR]] == "FALSE"] <- "Reporting Inconsistency"
    x$Type[x[[VAR_ERROR]] == "TRUE" &
             x[[VAR_DEC_ERROR]] == "TRUE"] <- "Decision Error"
    
    #Create ggplot "APA format" theme
    apatheme <- theme_bw() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line()
      )
    
    #If no grouping variable is specified, don't use faceting
    if (is.null(group)) {
      #Create plot "p"; map computed p-values to x-axis, reported p-values to y-axis, and
      #color to the Type variable created earlier. Environment command allows apatheme to
      #be applied later because of bug when creating functions with ggplot2
      p <- ggplot(x,
                  aes(y = .data[[VAR_COMPUTED_P]],
                      x = .data[[VAR_REPORTED_P]],
                      col = Type),
                  environment = environment())
      
      #Add data points to plot
      p + geom_point(size = 2.5) +
        #Add vertical grey dashed line, located at specified alpha level
        geom_vline(xintercept = alpha,
                   color = "grey60",
                   linetype = "dashed") +
        #Add horizontal grey dashed line, located at specified alpha level
        geom_hline(yintercept = alpha,
                   color = "grey60",
                   linetype = "dashed") +
        #Add a line showing where accurately reported p-values should fall
        geom_abline(intercept = 0,
                    slope = 1,
                    color = "grey60") +
        #Add text annotations demarcating over-/under-estimated areas of the plot
        annotate("text",
                 x = 0.5,
                 y = .10,
                 label = "overestimated") +
        annotate("text",
                 x = 0.5,
                 y = .90,
                 label = "underestimated") +
        #Rename the x- and y-axis, and manually specify breaks
        scale_x_continuous(
          name = "Reported p-values",
          breaks = c(0.00, 0.05, 0.10, 0.25, 0.50, 0.75, 1.0),
          limits = c(0, 1)
        ) +
        scale_y_continuous(
          name = "Computed p-values",
          breaks = c(0.00, 0.05, 0.10, 0.25, 0.50, 0.75, 1.0),
          limits = c(0, 1)
        ) +
        #Manually specify greyscale colors for different levels of Type
        scale_color_manual(
          breaks = c(
            "Correctly Reported",
            "Reporting Inconsistency",
            "Decision Error"
          ),
          values = c("grey80", "black", "grey50")
        ) +
        apatheme
      
    } else {
      #If grouping variable is specified, use for faceting
      
      #Create plot "p"; map computed p-values to x-axis, reported p-values to y-axis, and
      #color to the Type variable created earlier. Environment command allows apatheme to
      #be applied later because of bug when creating functions with ggplot2
      p <- ggplot(x,
                  aes(y = rlang::.data[[VAR_COMPUTED_P]],
                      x = rlang::.data[[VAR_REPORTED_P]],
                      col = Type),
                  environment = environment())
      
      #Add data points to plot
      p + geom_point(size = 2.5) +
        #Add vertical grey dashed line, located at specified alpha level
        geom_vline(xintercept = alpha,
                   color = "grey60",
                   linetype = "dashed") +
        #Add horizontal grey dashed line, located at specified alpha level
        geom_hline(yintercept = alpha,
                   color = "grey60",
                   linetype = "dashed") +
        #Add a line showing where accurately reported p-values should fall
        geom_abline(intercept = 0,
                    slope = 1,
                    color = "grey60") +
        #Add text annotations demarcating over-/under-estimated areas of the plot
        annotate("text",
                 x = 0.5,
                 y = .10,
                 label = "overestimated") +
        annotate("text",
                 x = 0.5,
                 y = .90,
                 label = "underestimated") +
        #Rename the x- and y-axis, and manually specify breaks
        scale_x_continuous(name = "Reported p-values",
                           breaks = c(0.00, 0.05, 0.10, 0.25, 0.50, 0.75, 1.0)) +
        scale_y_continuous(name = "Computed p-values",
                           breaks = c(0.00, 0.05, 0.10, 0.25, 0.50, 0.75, 1.0)) +
        #Manually specify greyscale colors for different levels of Type
        scale_color_manual(
          breaks = c(
            "Correctly Reported",
            "Reporting Inconsistency",
            "Decision Error"
          ),
          values = c("grey80", "black", "grey50")
        ) +
        facet_grid(stats::as.formula(paste(group, "~ ."))) +
        apatheme
    }
    
  } else {
    # Extract limit args:
    args <- list(...)
    if (is.null(args$xlim))
      args$xlim <- c(0, 1)
    if (is.null(args$ylim))
      args$ylim <- c(0, 1)
    
    reported <- x[[VAR_REPORTED_P]]
    computed <- x[[VAR_COMPUTED_P]]
    
    # replace 'ns' for > alpha
    reported[x[[VAR_P_COMPARISON]] == "ns"] <- alpha
    
    # scatterplot of reported and recalculated p values
    do.call(plot.default, c(
      list(
        x = reported,
        y = computed,
        xlab = "reported p value",
        ylab = "recalculated p value",
        pch = 20
      ),
      args
    ))
    
    # orange dot for error
    points(reported[x[[VAR_ERROR]]],
           computed[x[[VAR_ERROR]]],
           pch = 20, col = "orange")
    
    # red dot for gross error (non-sig reported as sig and vice versa)
    points(reported[x[[VAR_DEC_ERROR]]],
           computed[x[[VAR_DEC_ERROR]]],
           pch = 20, col = "red")
    
    # indicate exact p values with diamond
    points(x[[VAR_REPORTED_P]][x[[VAR_P_COMPARISON]] == "="],
           computed[x[[VAR_P_COMPARISON]] == "="],
           pch = 5)
    
    # general layout of figure:
    # lines & text to indicate under- and overestimates
    abline(h = .05)
    abline(v = .05)
    abline(0, 1)
    
    text(.8, .4, "overestimated")
    text(.4, .8, "underestimated")
    
    text(0, .53, "non-sig", cex = .7)
    text(0, .50, "reported", cex = .7)
    text(0, .47, "as sig", cex = .7)
    
    text(.5, 0, "sig reported as non-sig", cex = .7)
    
    par(xpd = TRUE)
    legend(
      .88,
      -.15,
      pch = c(20, 20, 5),
      col = c("orange", "red", "black"),
      legend = c("p inconsistency", "decision error", "exact (p = ...)"),
      cex = .8
    )
    par(xpd = FALSE)
  }
}
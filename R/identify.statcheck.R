#' Identify specific points in a statcheck plot.
#' 
#' With this function you can simply point and click on the datapoints in the 
#' plot to see the corresponding statcheck details, such as the paper from which 
#' the data came and the exact statistical results.
#' 
#' @inheritParams plot.statcheck
#'
#' @examples \dontrun{
#' 
#' # First we need a statcheck object
#' # Here, we create one by running statcheck on some raw text
#' 
#' txt <- "This test is consistent t(28) = 0.2, p = .84, but this one is 
#' inconsistent: F(2, 28) = 4.2, p = .01. This final test is even a
#' gross/decision inconsistency: z = 1.23, p = .03"
#' 
#' result <- statcheck(txt)
#' 
#' # Now, we can run identify.statcheck(), or shorter, simply identify():
#' identify(result)
#' 
#' # Further instructions:
#' # click on one or multiple points of interest
#' # press Esc
#' # a dataframe with information on the selected points will appear
#' 
#' }
#' 
#' @export

identify.statcheck <-
  function(x,
           alpha = .05,
           ...) {
    reported <- x[[VAR_REPORTED_P]]
    computed <- x[[VAR_COMPUTED_P]]
    
    # replace 'ns' for > alpha
    reported[x[[VAR_P_COMPARISON]] == "ns"] <- alpha
    
    plot(x, APAstyle = FALSE, ...) # makes use of the plot.statcheck() function
    ID <- identify(reported, computed)
    
    res <- x[ID,]
    class(res) <- c("statcheck", "data.frame")
    
    return(res)
  }

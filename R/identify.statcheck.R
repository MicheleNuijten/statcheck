identify.statcheck <-
  function(x,
           alpha = .05,
           ...) {
    reported <- x$Reported.P.Value
    computed <- x$Computed
    
    # replace 'ns' for > alpha
    reported[x$Reported.Comparison == "ns"] <- alpha
    
    plot(x, APAstyle = FALSE, ...) # makes use of the plot.statcheck() function
    ID <- identify(reported, computed)
    
    res <- x[ID,]
    class(res) <- c("statcheck", "data.frame")
    
    return(res)
  }
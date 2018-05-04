summary.statcheck <-
  function(object, ...) {
    x <- object
    
    # Source
    Source <-
      c(as.vector(ddply(x, "Source", function(x)
        unique(x$Source))[, 1]), "Total")
    
    # Number of p values extracted per article and in total
    pValues <- c(ddply(x, "Source", function(x)
      nrow(x))[, 2], nrow(x))
    
    # Number of errors per article and in total
    Errors <-
      c(ddply(x, "Source", function(x)
        sum(x$Error, na.rm = TRUE))[, 2], sum(x$Error, na.rm = TRUE))
    
    # Number of decision errors per article and in total
    DecisionErrors <-
      c(ddply(x, "Source", function(x)
        sum(x$DecisionError, na.rm = TRUE))[, 2],
        sum(x$DecisionError, na.rm = TRUE))
    
    # Results in dataframe
    res <- data.frame(
      Source = Source,
      pValues = pValues,
      Errors = Errors,
      DecisionErrors = DecisionErrors
    )
    
    class(res) <- c("statcheck", "data.frame")
    
    return(res)
  }
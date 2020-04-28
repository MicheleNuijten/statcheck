summary.statcheck <-
  function(object, ...) {
    x <- object
    
    # Source
    Source <-
      c(as.vector(plyr::ddply(x, VAR_SOURCE, function(x)
        unique(x[[VAR_SOURCE]]))[, 1]), "Total")
    
    # Number of p values extracted per article and in total
    pValues <- c(plyr::ddply(x, VAR_SOURCE, function(x)
      nrow(x))[, 2], nrow(x))
    
    # Number of errors per article and in total
    Errors <-
      c(plyr::ddply(x, VAR_SOURCE, function(x)
        sum(x[[VAR_ERROR]], na.rm = TRUE))[, 2], 
        sum(x[[VAR_ERROR]], na.rm = TRUE))
    
    # Number of decision errors per article and in total
    DecisionErrors <-
      c(plyr::ddply(x, VAR_SOURCE, function(x)
        sum(x[[VAR_DEC_ERROR]], na.rm = TRUE))[, 2],
        sum(x[[VAR_DEC_ERROR]], na.rm = TRUE))
    
    # Results in dataframe
    res <- data.frame(
      Source = Source,
      pValues = pValues,
      Errors = Errors,
      DecisionErrors = DecisionErrors
    )
    
    # Rename columns based on constants in constants.R file
    colnames(res) <- c(VAR_SOURCE, VAR_NR_PVALUES, VAR_NR_ERRORS, 
                       VAR_NR_DEC_ERRORS)
    
    class(res) <- c("statcheck", "data.frame")
    
    return(res)
  }
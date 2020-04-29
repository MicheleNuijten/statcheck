#' Summary method for statcheck
#' 
#' Gives the summaries for a \code{statcheck} object.
#' 
#' @param object a \code{statcheck} object.
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A data frame containing for each source of statistics:
#' \describe{
#'     \item{source}{Name of the file/origin of which the statistics are 
#'     extracted}
#'     \item{nr_p_values}{The number of extracted reported p values per article}
#'     \item{nr_errors}{The number of errors per article}
#'     \item{nr_decision_errors}{The number of decision errors per article}
#' }
#' 
#' @examples 
#' txt <- "blablabla the effect was very significant (t(100)=1, p < 0.001)"
#' stat <- statcheck(txt)
#' summary(stat)
#' 
#' @export


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
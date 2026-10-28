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
    
    # Group the rows once for all counts, preserving plyr's source ordering.
    counts <- plyr::ddply(x, VAR_SOURCE, function(group) {
      c(
        pValues = nrow(group),
        Errors = sum(group[[VAR_ERROR]], na.rm = TRUE),
        DecisionErrors = sum(group[[VAR_DEC_ERROR]], na.rm = TRUE)
      )
    })

    Source <- c(as.vector(counts[[VAR_SOURCE]]), "Total")
    pValues <- c(counts$pValues, nrow(x))
    Errors <- c(counts$Errors, sum(counts$Errors))
    DecisionErrors <- c(counts$DecisionErrors, sum(counts$DecisionErrors))
    
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

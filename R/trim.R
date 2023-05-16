#' Trimming method for statcheck output
#' 
#' Returns a subset of columns of a \code{statcheck} object.
#' 
#' @param object a \code{statcheck} object.
#' @param ... additional arguments affecting the trimmed output.
#' 
#' @return A data frame containing for each source of statistics:
#' \describe{
#'     \item{source}{Name of the file/origin of which the statistics are 
#'     extracted}
#'     \item{raw}{Raw string of the statistical reference that is extracted}
#'     \item{computed_p}{The recomputed p-value}
#'     \item{error}{The computed p value is not congruent with the reported 
#'     p-value}
#'     \item{decision_error}{The reported result is significant whereas the 
#'     recomputed result is not, or vice versa.}
#' }
#' 
#' @examples 
#' txt <- "blablabla the effect was very significant (t(100)=1, p < 0.001)"
#' stat <- statcheck(txt)
#' trim(stat)
#' 
#' @export

trim <- 
  function(object, ...){
   
    x <- object
    
    colnames_concise <- c(VAR_SOURCE, VAR_RAW, VAR_COMPUTED_P,
                          VAR_ERROR, VAR_DEC_ERROR)
    
    concise_output <- x[ , colnames_concise]
        
    class(concise_output) <- c("statcheck", "data.frame")
    
    return(concise_output)
  }
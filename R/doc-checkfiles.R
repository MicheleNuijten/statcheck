#' Extract statistics from PDF/HTML articles and recalculate p-values
#' 
#' These functions search for NHST results in PDF and/or HTML articles and send 
#' the extracted statistics to \code{statcheck}.
#' 
#' @param files Vector of strings containing file paths to HTML files to check.
#' @param ... Arguments sent to \code{statcheck}.
#' 
#' @name checkfiles
#' 
#' @return A statcheck data frame with the extracted statistics. See 
#' \code{\link{statcheck}} for details.

NULL
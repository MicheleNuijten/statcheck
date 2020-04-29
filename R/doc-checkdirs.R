#' Extract statistics from folders with PDF/HTML articles and recalculate 
#' p-values
#' 
#' These functions search for NHST results in all PDF and/or HTML articles in a 
#' certain folder and send the extracted statistics to \code{statcheck}.
#' 
#' @param dir String indicating the directory to be used. If this is left empty, 
#' a window will pop up from which you can choose a directory.
#' @param subdir Logical. Indicates whether you also want to check subfolders. 
#' Defaults to TRUE
#' @param extension Logical. Indicates whether the HTML extension should be 
#' checked. Defaults to TRUE
#' @param ... Arguments sent to \code{statcheck}.
#' 
#' @name checkdirs
#' 
#' @return A statcheck data frame with the extracted statistics. See 
#' \code{\link{statcheck}} for details.

NULL
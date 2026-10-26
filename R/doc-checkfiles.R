#' Extract statistics from PDF/HTML articles and recalculate p-values
#'
#' These functions search for NHST results in PDF and/or HTML articles and send
#' the extracted statistics to \code{statcheck}.
#'
#' @param files Vector of strings containing file paths to HTML or PDF files to check.
#' @param ... Arguments sent to \code{statcheck}.
#'
#' @return A \code{statcheck} data frame with the extracted statistics. See 
#'   \code{\link{statcheck}} for details.
#'
#' @details
#' On macOS, file selection dialogs used in related functions (e.g., via \code{tk_choose.files})
#' can be unreliable. If you encounter problems, it is recommended to type the full
#' file path manually instead of relying on the dialog.
#'
#' Also, make sure that the files you provide are actual HTML or PDF documents.
#' Passing other file types may result in errors.
#'
#' @note
#' This package has been primarily tested on Windows. Mac users may experience
#' issues with multi-file selection dialogs.
#'
#' @name checkfiles
#' @rdname checkfiles
#' @aliases checkHTML
#' @aliases checkPDF
#' @aliases checkfiles
#'
#' @examples
#' \dontrun{
#' # Check a single HTML file
#' checkHTML("path/to/file.html")
#'
#' # Check multiple PDF files
#' checkPDF(c("file1.pdf", "file2.pdf"))
#'
#' # If dialogs fail on macOS, type the full file path manually:
#' checkHTML("/Users/username/Documents/file.html")
#' }
NULL

#' Extract statistics from folders with PDF/HTML articles and recalculate p-values
#'
#' These functions search for NHST results in all PDF and/or HTML articles in a 
#' certain folder and send the extracted statistics to \code{statcheck}.
#'
#' @param dir String indicating the directory to be used. If left empty, a window 
#'   will pop up from which you can choose a directory.
#' @param subdir Logical. Indicates whether you also want to check subfolders. 
#'   Defaults to TRUE.
#' @param extension (Only relevant for \code{checkHTMLdir()}) If TRUE (default), only files with a `.html` or `.htm`
#'   extension are processed. If FALSE, all files in the specified directory
#'   (and subdirectories if `subdir = TRUE`) are included, regardless of extension.
#'   Setting this to FALSE may cause errors if non-HTML files are present.
#' @param ... Arguments sent to \code{statcheck}.
#'
#' @return A \code{statcheck} data frame with the extracted statistics. See 
#'   \code{\link{statcheck}} for details.
#'   
#' @details
#' On macOS, the file selection dialog can sometimes be unreliable or fail to open
#' when using `tk_choose.files()`. If you encounter this issue, it is recommended
#' to manually type the filename or path instead of using the dialog.
#'
#' @note
#' This package was primarily tested on Windows. Mac users may experience issues
#' with multi-file selection dialogs.
#'
#' @name checkdirs
#' @rdname checkdirs
#' @aliases checkHTMLdir
#' @aliases checkPDFdir
#' @aliases checkdir
#' @aliases checkdirs
#'
#' @examples
#' \dontrun{
#' # Scan a folder of PDFs
#' checkPDFdir("path/to/pdfs")
#'
#' # Scan a folder of HTML articles
#' checkHTMLdir("path/to/html")
#'
#' # Scan a folder of mixed PDFs/HTML
#' checkdir("path/to/articles")
#' }
NULL

#' @rdname checkdirs
#' @export

checkdir <-
  function(dir, subdir = TRUE, ...) {
    if (missing(dir))
      dir <- tcltk::tk_choose.dir()
    
    files <- list.files(dir, recursive = subdir)
    pdfs <- any(grepl("\\.pdf$", files, ignore.case = TRUE))
    htmls <- any(grepl("\\.html?$", files, ignore.case = TRUE))

    if (!pdfs && !htmls)
      stop("No PDF or HTML found")

    pdfres <- NULL
    htmlres <- NULL
    
    if (pdfs)
      pdfres <- checkPDFdir(dir, subdir, ...)
    if (htmls)
      htmlres <- checkHTMLdir(dir, subdir, ...)
    
    Res <- rbind(pdfres, htmlres)
    if (is.null(Res))
      stop("statcheck did not find any results")
    
    
    class(Res) <- c("statcheck", "data.frame")
    return(Res)
    
  }

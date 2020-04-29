#' @rdname checkdirs
#' @export

checkdir <-
  function(dir, subdir = TRUE, ...) {
    if (missing(dir))
      dir <- tk_choose.dir()
    
    pdfs <-
      any(grepl("\\.pdf$", list.files(dir, recursive = subdir), ignore.case =
                  TRUE))
    htmls <-
      any(grepl(
        "\\.html?$",
        list.files(dir, recursive = subdir),
        ignore.case = TRUE
      ))
    
    if (pdfs)
      pdfres <- checkPDFdir(dir, subdir, ...)
    if (htmls)
      htmlres <- checkHTMLdir(dir, subdir, ...)
    
    if (pdfs & htmls) {
      if (!is.null(pdfres) & !is.null(htmlres))
        Res <- rbind(pdfres, htmlres)
      else
        stop("statcheck did not find any results")
      
    } else
      if (pdfs & !htmls) {
        if (!is.null(pdfres))
          Res <- pdfres
        else
          stop("statcheck did not find any results")
      }
    
    else
      if (!pdfs & htmls) {
        if (!is.null(htmlres))
          Res <- htmlres
        else
          stop("statcheck did not find any results")
      }
    
    else
      if (!pdfs & !htmls)
        stop("No PDF or HTML found")
    
    
    class(Res) <- c("statcheck", "data.frame")
    return(Res)
    
  }
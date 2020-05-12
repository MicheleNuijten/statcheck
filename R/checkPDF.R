#' @rdname checkfiles
#' @export

checkPDF <-
  function(files, 
           method = c("xpdf", "pdftools"), 
           ...) {
    if (missing(files))
      files <- tcltk::tk_choose.files()
    
    txts <-  sapply(files, getPDF, method)
    names(txts) <-
      gsub("\\.pdf$", "", basename(files), perl = TRUE)
    return(statcheck(txts, ...))
  }

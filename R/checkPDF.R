#' @rdname checkfiles
#' @export

checkPDF <-
  function(files, ...) {
    if (missing(files))
      files <- tcltk::tk_choose.files()
    
    txts <-  sapply(files, getPDF)
    names(txts) <-  basename(files)
    return(statcheck(txts, ...))
  }

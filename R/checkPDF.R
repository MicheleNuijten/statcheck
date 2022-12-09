#' @rdname checkfiles
#' @export

checkPDF <-
  function(files, method, ...) {
    if (missing(files))
      files <- tcltk::tk_choose.files()
    
    txts <-  sapply(files, getPDF)
    names(txts) <-
      gsub("\\.pdf$", "", basename(files), perl = TRUE)
    return(statcheck(txts, ...))
  }

#' @rdname checkfiles
#' @export

checkPDF <-
  function(files, ...) {
    if (missing(files))
      files <- tk_choose.files()
    
    txts <-  sapply(files, getPDF)
    names(txts) <-
      gsub("\\.pdf$", "", basename(files), perl = TRUE)
    return(statcheck(txts, ...))
  }

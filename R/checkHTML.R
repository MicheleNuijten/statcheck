#' @rdname checkfiles

checkHTML <- function(files,
                      ...)
{
  if (missing(files))
    files <- tk_choose.files()
  
  txts <-  sapply(files, getHTML)
  names(txts) <- gsub(".html", "", basename(files))
  names(txts) <- gsub(".htm", "", names(txts))
  return(statcheck(txts, ...))
  
}

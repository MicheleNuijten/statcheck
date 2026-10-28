#' @rdname checkfiles
#' @export

checkHTML <- function(files,
                      ...)
{
  if (missing(files))
    files <- tcltk::tk_choose.files()
  
  txts <-  sapply(files, getHTML)
  names(txts) <- file_sources(files)
  return(statcheck(txts, ...))
  
}

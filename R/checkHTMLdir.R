#' @rdname checkdirs
#' @export

checkHTMLdir <- function(dir,
                         subdir = TRUE,
                         extension = TRUE,
                         ...) {
  if (missing(dir)) {
    dir <- tcltk::tk_choose.dir()
  }
  
  if (extension == TRUE) {
    pat = ".html|.htm"
  }
  
  if (extension == FALSE) {
    pat = ""
  }
  
  files <-
    list.files(dir,
               pattern = pat,
               full.names = TRUE,
               recursive = subdir)
  
  if (length(files) == 0) {
    stop("No HTML found")
  }
  
  txts <- character(length(files))
  message("Importing HTML files...")
  pb <- utils::txtProgressBar(max = length(files), style = 3)
  
  for (i in 1:length(files)) {
    txts[i] <-  getHTML(files[i])
    utils::setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  names(txts) <- basename(files)
  return(statcheck(txts, ...))
}

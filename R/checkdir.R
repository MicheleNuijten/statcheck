

## Function to check directory of PDFs:
checkdir <- function(dir,...)
{
  if (missing(dir)) dir <- tk_choose.dir()
#   if (any(grepl("\\.pdf",list.files(dir)))) 
    pdfres <- checkPDFdir(dir,...)
#   if (any(grepl("\\.html",list.files(dir)))) 
    htmlres <- checkHTMLdir(dir,...)
  Res <- rbind(pdfres,htmlres)
  class(Res) <- c("statcheck","data.frame")
  return(Res)
}
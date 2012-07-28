

## Function to check directory of PDFs:
checkdir <- function(dir,...)
{
  if (missing(dir)) dir <- tk_choose.dir()
  pdfres <- checkPDFdir(dir,...)
  htmlres <- checkHTMLdir(dir,...)
  Res <- rbind(pdfres,htmlres)
  class(Res) <- c("statcheck","data.frame")
  return(Res)
}
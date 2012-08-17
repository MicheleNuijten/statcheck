

## Function to check directory of PDFs:
checkdir <- function(dir,...)
{
  pdfs <- any(grepl("\\.pdf",list.files(dir)))
  htmls <- any(grepl("\\.html",list.files(dir)))
  
  if (missing(dir)) dir <- tk_choose.dir()
   if (pdfs) pdfres <- checkPDFdir(dir,...)
   if (htmls) htmlres <- checkHTMLdir(dir,...)
  
  if (pdfs & htmls)
  {
    Res <- rbind(pdfres,htmlres)
  } else if (pdfs & !htmls) Res <- pdfres else if (!pdfs & htmls) Res <- htmlres else stop("No PDF or HTML found")
  class(Res) <- c("statcheck","data.frame")
  return(Res)
}
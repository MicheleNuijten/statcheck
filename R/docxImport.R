if(!require(textreadr)){install.packages('textreadr')}
library(textreadr)

getdocx <- function(
  x
  )
{
  
  strings <- read_docx(x,function(fileName))
  return(strings)
}


checkdocxdir <- structure(function(dir, subdir = TRUE, extension = TRUE,...){
  if (missing(dir)) dir <- tk_choose.dir()
  if (extension == TRUE) pat = ".docx"
  if (extension == FALSE) pat = ""
  
  files <- list.files(dir,pattern = pat, full.names = TRUE, recursive = subdir)
  
  if(length(files)==0) stop("No docx files found")
  
  txts <- character(length(files))
  message("Importing docx files...")
  pb <- txtProgressBar(max=length(files),style=3)
  for (i in 1:length(files))
  {
    txts[i] <-  getHTML(files[i])    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  names(txts) <- gsub(".docx","",basename(files))
  return(statcheck(txts,...))
 
  },ex=function(){

})

checkdocx <- structure(function(files,...
  )
{
  if (missing(files)) files <- tk_choose.files()
  txts <-  sapply(files, getdocx)
  names(txts) <- gsub(".docx","",basename(files))
  return(statcheck(txts,...))

  },ex=function(){

})


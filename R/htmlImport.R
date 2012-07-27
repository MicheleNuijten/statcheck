
getHTML <- function(x)
{
  strings <- lapply(x,function(fileName)readChar(fileName, file.info(fileName)$size))
  
  # Remove HTML tags:
  strings <- lapply(strings,gsub,pattern="<(.|\n)*?>",replacement="")
  
  # Replace html codes:
  strings <- lapply(strings,gsub,pattern="&#60;",replacement="<",fixed=TRUE)
  strings <- lapply(strings,gsub,pattern="&lt;",replacement="<",fixed=TRUE)
  strings <- lapply(strings,gsub,pattern="&#61;",replacement="=",fixed=TRUE)
  strings <- lapply(strings,gsub,pattern="&#62;",replacement=">",fixed=TRUE)
  strings <- lapply(strings,gsub,pattern="&gt;",replacement=".",fixed=TRUE)
  strings <- lapply(strings,gsub,pattern="&#40;",replacement="(",fixed=TRUE)
  strings <- lapply(strings,gsub,pattern="&#41;",replacement=")",fixed=TRUE)

  return(strings)
}


## Function to check directory of HTMLs:
checkHTMLdir <- function(dir,...)
{
  if (missing(dir)) dir <- tk_choose.dir()
  files <- list.files(dir,pattern=".html",full.names=TRUE)
  txts <-  sapply(files,getHTML,dir=dir)
  names(txts) <- gsub(".html","",list.files(dir,pattern=".html"))
  return(statcheck(txts,...))
}

## Function to given HTMLs:
checkHTML <- function(files,...)
{
  txts <-  sapply(files,getHTML,dir=dir)
  names(txts) <- gsub(".html","",list.files(dir,pattern=".html"))
  return(statcheck(txts,...))
}


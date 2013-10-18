
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
  strings <- lapply(strings,gsub,pattern="&gt;",replacement=">",fixed=TRUE)
  strings <- lapply(strings,gsub,pattern="&#40;",replacement="(",fixed=TRUE)
  strings <- lapply(strings,gsub,pattern="&#41;",replacement=")",fixed=TRUE)
  strings <- lapply(strings,gsub,pattern="&thinsp;",replacement=" ",fixed=TRUE)
  strings <- lapply(strings,gsub,pattern="&nbsp;",replacement=" ",fixed=TRUE) # these are used in JCPP
  strings <- lapply(strings,gsub,pattern="\n",replacement="")
  strings <- lapply(strings,gsub,pattern="\r",replacement="")
  strings <- lapply(strings,gsub,pattern="\\s+",replacement=" ")
  
  return(strings)
}


## Function to check directory of HTMLs:
checkHTMLdir <- function(dir,...)
{
  if (missing(dir)) dir <- tk_choose.dir()
  files <- list.files(dir,pattern=".html|.htm",full.names=TRUE)
  txts <- character(length(files))
  message("Importing HTML files...")
  pb <- txtProgressBar(max=length(files),style=3)
  for (i in 1:length(files))
  {
    txts[i] <-  getHTML(files[i])    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  names(txts) <- gsub(".html","",basename(files))
  names(txts) <- gsub(".htm","",basename(files))
  return(statcheck(txts,...))
}

## Function to given HTMLs:
checkHTML <- function(files,...)
{
  txts <-  sapply(files,getHTML)
  names(txts) <- gsub(".html","",basename(files))
  return(statcheck(txts,...))
}


getHTML <- function(
  x
  )
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
  strings <- lapply(strings,gsub,pattern="&#935",replacement="χ")
  strings <- lapply(strings,gsub,pattern="&#967",replacement="χ")
 
  return(strings)
}


checkHTMLdir <- structure(function(# Extract test statistics from all HTML files in a folder.
  ### Extracts statistical references from a directory with HTML versions of articles. By default a gui window is opened that allows you to choose the directory (using tcltk).
  dir,
  ### String indicating the directory to be used.
  subdir = TRUE,
  ### Logical indicating whether you also want to check subfolders. Defaults to TRUE
  ...
  ### Arguments sent to  \code{\link{statcheck}}
  )
{
  ##details<<
  ## See \code{\link{statcheck}} for more details. Use \code{\link{checkHTML}} to import individual HTML files. 
  ## Note that the conversion to plain text and extraction of statistics can result in errors. Some statistical values can be missed, especially if the notation is unconventional. It is recommended to manually check some of the results.
  ##seealso<<
  ## \code{\link{statcheck}}, \code{\link{checkPDF}}, \code{\link{checkPDFdir}}, \code{\link{checkHTML}}, \code{\link{checkdir}}
  if (missing(dir)) dir <- tk_choose.dir()
  files <- list.files(dir,pattern=".html|.htm",full.names=TRUE,recursive=subdir)
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
  ##value<<
  ## A data frame containing for each extracted statistic:
  ## \item{Source}{Name of the file of which the statistic is extracted}
  ## \item{Statistic}{Character indicating the statistic that is extracted}
  ## \item{df1}{First degree of freedom}
  ## \item{df2}{Second degree of freedom (if applicable)}
  ## \item{Value}{Reported value of the statistic}
  ## \item{Reported.Comparison}{Reported comparison, when importing from pdf this will often not be converted properly}
  ## \item{Reported.P.Value}{The reported p-value, or NA if the reported value was NS}
  ## \item{Computed}{The recomputed p-value}
  ## \item{Raw}{Raw string of the statistical reference that is extracted}
  ## \item{InExactError}{Error in inexactly reported p values as compared to the recalculated p values}
  ## \item{ExactError}{Error in exactly reported p values as compared to the recalculated p values}
  ## \item{DecisionError}{The reported result is significant whereas the recomputed result is not, or vice versa.}
  },ex=function(){
  # with this command a menu will pop up from which you can select the directory with HTML articles
  checkHTMLdir()

# you could also specify the directory beforehand
# for instance:
# DIR <- "C:/mydocuments/articles"
# checkHTMLdir(DIR)
})

checkHTML <- structure(function(# Extract test statistics from HTML file.
  ### Extracts statistical references from given HTML files.
  files,
  ### Vector of strings containing file paths to HTML files to check.
  ...
  ### Arguments sent to  \code{\link{statcheck}}.
  )
{
  ##details<<
  ## See \code{\link{statcheck}} for more details. Use \code{\link{checkHTMLdir}} to import al HTML files in a given directory at once. 
  ## Note that the conversion to plain text and extraction of statistics can result in errors. Some statistical values can be missed, especially if the notation is unconvetional. It is recommended to manually check some of the results.
  ##seealso<<
  ## \code{\link{statcheck}}, \code{\link{checkPDF}}, \code{\link{checkPDFdir}}, \code{\link{checkHTMLdir}}, \code{\link{checkdir}}
  txts <-  sapply(files,getHTML)
  names(txts) <- gsub(".html","",basename(files))
  return(statcheck(txts,...))
  ##value<<
  ## A data frame containing for each extracted statistic:
  ## \item{Source}{Name of the file of which the statistic is extracted}
  ## \item{Statistic}{Character indicating the statistic that is extracted}
  ## \item{df1}{First degree of freedom}
  ## \item{df2}{Second degree of freedom (if applicable)}
  ## \item{Value}{Reported value of the statistic}
  ## \item{Reported.Comparison}{Reported comparison, when importing from pdf this will often not be converted properly}
  ## \item{Reported.P.Value}{The reported p-value, or NA if the reported value was NS}
  ## \item{Computed}{The recomputed p-value}
  ## \item{Raw}{Raw string of the statistical reference that is extracted}
  ## \item{InExactError}{Error in inexactly reported p values as compared to the recalculated p values}
  ## \item{ExactError}{Error in exactly reported p values as compared to the recalculated p values}
  ## \item{DecisionError}{The reported result is significant whereas the recomputed result is not, or vice versa.}
  },ex=function(){
  # given that my HTML file is called "article.html"
# and I saved it in "C:/mydocuments/articles"

    #checkHTML("C:/mydocuments/articles/article.html")
})


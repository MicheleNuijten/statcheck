getHTML <- function(x){
  
  strings <- lapply(x, function(fileName)readChar(file(fileName), file.info(fileName)$size, useBytes = TRUE))
  
  # Remove subscripts (except for p_rep)
  strings <- lapply(strings, gsub, pattern = "<sub>(?!rep).*?</sub>", replacement = "", perl = TRUE)
  
  # Remove HTML tags:
  strings <- lapply(strings, gsub, pattern = "<(.|\n)*?>", replacement = "")
  
  # Replace html codes:
  strings <- lapply(strings, gsub, pattern = "&#60;", replacement = "<", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&lt;", replacement = "<", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#61;", replacement = "=", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#62;", replacement = ">", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&gt;", replacement = ">", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#40;", replacement = "(", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#41;", replacement = ")", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&thinsp;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&nbsp;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "\n", replacement = "")
  strings <- lapply(strings, gsub, pattern = "\r", replacement = "")
  strings <- lapply(strings, gsub, pattern = "\\s+", replacement = " ")
  strings <- lapply(strings, gsub, pattern = "&minus;", replacement = "-", fixed = TRUE)
  
  return(strings)
}




checkHTMLdir <- function(dir,
                         subdir = TRUE,
                         extension = TRUE,
                         ...) {
  if (missing(dir)) {
    dir <- tk_choose.dir()
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
  pb <- txtProgressBar(max = length(files), style = 3)
  
  for (i in 1:length(files)) {
    txts[i] <-  getHTML(files[i])
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  names(txts) <- gsub(".html", "", basename(files))
  names(txts) <- gsub(".htm", "", names(txts))
  return(statcheck(txts, ...))
}

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

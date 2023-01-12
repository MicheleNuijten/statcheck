# HTML TO TXT ------------------------------------------------------------------

getHTML <- function(x){
  
  strings <- lapply(x, function(fileName){
    con <- file(fileName)
    on.exit(close(con))
    raw_strings <- readChar(con, file.info(fileName)$size, useBytes = TRUE)
    return(raw_strings)
  })
  
  # Remove subscripts (except for p_rep)
  strings <- lapply(strings, gsub, pattern = "<sub>(?!rep).*?</sub>", replacement = "", perl = TRUE)
  
  # Remove HTML tags:
  strings <- lapply(strings, gsub, pattern = "<(.|\n)*?>", replacement = "")
  
  # Replace html codes:
  # from: https://dev.w3.org/html5/html-author/charref 
  strings <- lapply(strings, gsub, pattern = "&#60;", replacement = "<", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&lt;", replacement = "<", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&LT;", replacement = "<", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#x0003C;", replacement = "<", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#x0003c;", replacement = "<", fixed = TRUE)
  
  strings <- lapply(strings, gsub, pattern = "&#61;", replacement = "=", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&equals;", replacement = "=", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#x0003D;", replacement = "=", fixed = TRUE)
  
  strings <- lapply(strings, gsub, pattern = "&#62;", replacement = ">", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&gt;", replacement = ">", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&GT;", replacement = ">", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#x0003E;", replacement = ">", fixed = TRUE)
  
  strings <- lapply(strings, gsub, pattern = "&#40;", replacement = "(", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#41;", replacement = ")", fixed = TRUE)
  
  strings <- lapply(strings, gsub, pattern = "&thinsp;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&nbsp;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&nnbsp;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#8239;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#x202F;", replacement = " ", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "\\s+", replacement = " ")
  
  strings <- lapply(strings, gsub, pattern = "\n", replacement = "")
  strings <- lapply(strings, gsub, pattern = "\r", replacement = "")
  
  
  strings <- lapply(strings, gsub, pattern = "&minus;", replacement = "-", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#x02212;", replacement = "-", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#8722;", replacement = "-", fixed = TRUE)
  
  strings <- lapply(strings, gsub, pattern = "&chi;", replacement = "X", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#x003C7;", replacement = "X", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#x003c7;", replacement = "X", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#967;", replacement = "X", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&Chi;", replacement = "X", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#x003A7;", replacement = "X", fixed = TRUE)
  strings <- lapply(strings, gsub, pattern = "&#935;", replacement = "X", fixed = TRUE)

  return(strings)
}

# PDF TO TXT -------------------------------------------------------------------
getPDF <- function(x){
  
  txtfiles <- character(length(x))
  for (i in 1:length(x)){
    
    system(paste('pdftotext -q -enc "ASCII7" "', x[i], '"', sep = ""))
    if (file.exists(gsub("\\.pdf$", "\\.txt", x[i]))) {
      fileName <- gsub("\\.pdf$", "\\.txt", x[i])
      strings <- readChar(fileName, file.info(fileName)$size)
      
      # remove carriage returns and new lines
      strings <- gsub(x = strings, pattern = "[\r\n]", replacement = "")
      
      # save result in vector
      txtfiles[i] <- strings
      
    } else{
      
      warning(paste("Failure in file", x[i]))
      txtfiles[i] <- ""
      
    }
  }
  
  return(txtfiles)
}

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

# PDF TO TXT -------------------------------------------------------------------
getPDF <- function(x)
{
  txtfiles <- character(length(x))
  for (i in 1:length(x))
  {
    system(paste('pdftotext -q -enc "ASCII7" "', x[i], '"', sep = ""))
    if (file.exists(gsub("\\.pdf$", "\\.txt", x[i]))) {
      fileName <- gsub("\\.pdf$", "\\.txt", x[i])
      txtfiles[i] <- readChar(fileName, file.info(fileName)$size)
    } else{
      warning(paste("Failure in file", x[i]))
      txtfiles[i] <- ""
    }
  }
  return(txtfiles)
}

#' @rdname checkdirs
#' @export

checkPDFdir <-
  function(dir,
           subdir = TRUE,
           ...) {
    if (missing(dir))
      dir <- tcltk::tk_choose.dir()
    
    all.files <-
      list.files(dir,
                 pattern = "\\.pdf",
                 full.names = TRUE,
                 recursive = subdir)
    files <- all.files[grepl("\\.pdf$", all.files)]
    
    if (length(files) == 0)
      stop("No PDF found")
    
    txts <- character(length(files))
    message("Importing PDF files...")
    pb <- utils::txtProgressBar(max = length(files), style = 3)
    
    for (i in 1:length(files)){
      txts[i] <- read_directory_file(files[i], getPDF)
      utils::setTxtProgressBar(pb, i)
    }
    
    close(pb)
    
    names(txts) <- basename(files)
    
    return(statcheck(txts, ...))
  }

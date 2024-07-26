# this script contains a helper function for the unit tests to load the 
# manual reference file and select relevant rows to compare statcheck output to


load_manual <- function(
    path_manual, # path to the reference file
    apa = TRUE, # only consider apa reported stats
    pdf_conversion_issues = FALSE, # exclude cases where pdf conversion led to weird characters
    typesetting_issues = FALSE, # exclude cases where typesetting issues led to weird situations
    file_type = c("all", "pdf", "html"), # select specific file types
    file_id = NULL # select specific files based on file_id variable
){
  
  # load the reference file with manually extracted statistics
  manual <- read.csv2(system.file(path_manual, package = "statcheck"), header = TRUE)
  
  # row selection based on arguments
  if(apa == TRUE){
    manual <- manual[manual$extract_apa == 1, ]
  }
  
  if(pdf_conversion_issues == FALSE){
    manual <- manual[manual$pdf_conversion_issues == 0, ]
  }
  
  if(typesetting_issues == FALSE){
    manual <- manual[manual$typesetting_issues == 0, ]
  }
  
  if(file_type[1] == "pdf"){
    manual <- manual[manual$file_type == "pdf", ]
  } else if(file_type[1] == "html") {
    manual <- manual[manual$file_type == "pdf", ]
  } 
  
  if(!is.null(file_id)){
    manual <- manual[manual$file_id == file_id, ]
  }
  
  return(manual)
}
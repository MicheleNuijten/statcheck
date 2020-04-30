#' Generate HTML report for statcheck output
#' 
#' This function uses R Markdown to generate a nicely formatted HTML report of 
#' \code{\link{statcheck}} output.
#' 
#' This function temporarily saves the inserted \code{statcheck} output as an 
#' .RData file in the "output" folder in the statcheck package directory. This 
#' file is then called by the .Rmd template that is saved in the folder "rmd", 
#' also in the statcheck package directory. After the HTML report is generated, 
#' the .RData file is removed again.
#' 
#' @param statcheckOutput statcheck output of one of the following functions: 
#' \code{\link{statcheck}}, \code{\link{checkPDFdir}}, \code{\link{checkPDF}}, 
#' \code{\link{checkHTMLdir}}, \code{\link{checkHTML}}, or
#' \code{\link{checkdir}}.
#' @param outputFileName String specifying the file name under which you want to 
#' save the generated HTML report. The extension ".html" is automatically added, 
#' so doesn't need to be specified in this argument.
#' @param outputDir String specifying the directory in which you want to save 
#' the generated HTML report.
#' 
#' @return An HTML report, saved in the directory specified in the argument 
#' "outputDir".
#' 
#' @examples \dontrun{
#' 
#' # first generate statcheck output, for instance by using the statcheck() 
#' function
#' 
#' txt <- "blablabla the effect was very significant (t(100)=1, p < 0.001)"
#' stat <- statcheck(txt)
#' 
#' # next, use this output to generate a nice HTML report of the results
#' statcheckReport(stat, outputFileName="statcheckHTMLReport", 
#'                 outputDir="C:/mydocuments/results")
#' 
#' # you can now find your HTML report in the folder 
#' # "C:/mydocuments/results" under the name "statcheckHTMLReport.html".
#' 
#' }
#' 
#' @export

statcheckReport <-
  function(statcheckOutput,
           outputFileName,
           outputDir) {
    
    # set working directory to output file in statcheck package library
    currentWD <- getwd()
    setwd(system.file("inst/rmd", package = "statcheck"))
    
    # temporarily save statcheck output as RData in the selected working directory
    save(statcheckOutput, file = "statcheckOutput.RData")
    
    # run the markdown/knitr script
    statcheckReport_template <-
      system.file("rmd/statcheckReport_template.Rmd", package = "statcheck")
    rmarkdown::render(statcheckReport_template)
    
    # save/move the file in/to the specified output directory
    curDir <- system.file("rmd", package = "statcheck")
    file.rename(
      from = paste(curDir, "statcheckReport_template.html", sep = "/"),
      to = paste(outputDir, "/", outputFileName, ".html", sep = "")
    )
    
    # remove .RData file from package library folder
    file.remove(paste(curDir, "statcheckOutput.RData", sep = "/"))
    
    setwd(currentWD)
  }
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
#' @param outputFile String specifying the file name under which you want to 
#' save the generated HTML report. The extension ".html" is automatically added, 
#' so doesn't need to be specified in this argument.
#' @param outputDir String specifying the directory in which you want to save 
#' the generated HTML report.
#' 
#' @return An HTML report, saved in the directory specified in the argument 
#' "output_dir".
#' 
#' @examples \dontrun{
#' 
#' # first generate statcheck output, for instance by using the statcheck() 
#' function
#' 
#' txt <- "blablabla the effect was very significant (t(100)=1, p < 0.001)"
#' stat <- statcheck(txt)
#' 
#' # next, use this output to generate a formatted HTML report of the results
#' statcheckReport(stat, output_file = "statcheckHTMLReport", 
#'                 output_dir = "C:/mydocuments/results")
#' 
#' # you can now find your HTML report in the folder 
#' # "C:/mydocuments/results" under the name "statcheckHTMLReport.html".
#' 
#' }
#' 
#' @export

statcheckReport <-
  function(statcheckOutput,
           outputFile,
           outputDir = getwd()) {
    
    template <- system.file(
      "rmd/statcheckReport_template.Rmd",
      package = "statcheck"
    )
    
    tmp <- tempfile(fileext = ".Rmd")
    file.copy(template, tmp)
    
    rmarkdown::render(
      input = tmp,
      output_file = outputFile,
      output_dir = outputDir,
      params = list(statcheckOutput = statcheckOutput),
      envir = new.env(parent = globalenv())
    )
  }

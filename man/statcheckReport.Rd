\name{statcheckReport}

\alias{statcheckReport}

\title{Generate HTML report for statcheck output.}

\description{This function uses R Markdown to generate a nicely formatted HTML report of statcheck output.}

\usage{statcheckReport(statcheckOutput, outputFileName, outputDir)}

\arguments{
  
  \item{statcheckOutput}{statcheck output of one of the following functions: statcheck(), checkPDF(), checkHTML(), checkdir(), checkPDFdir(), checkHTMLdir().}
  
  \item{outputFileName}{String specifying the file name under which you want to save the generated HTML report. The extension ".html" is automatically added, so doesn't need to be specified in this argument.}  
  
  \item{outputDir}{String specifying the directory in which you want to save the generated HTML report.}
  
}

\details{This function temporarily saves the inserted statcheck output as an .RData file in the "output" folder in the statcheck package directory. This file is then called by the .Rmd template that is saved in the folder "rmd", also in the statcheck package directory. After the HTML report is generated, the .RData file is removed again.}

\value{An HTML report, saved in the directory specified in the argument "outputDir".}



\author{Sacha Epskamp <mail@sachaepskamp.com> & Michele B. Nuijten
  
  <m.b.nuijten@uvt.nl>}



\seealso{\code{\link{statcheck}}, \code{\link{checkPDF}}, \code{\link{checkHTMLdir}}, \code{\link{checkHTML}}, \code{\link{checkHTMLdir}}}

\examples{\dontrun{
  
  # first generate statcheck output, for instance by using the statcheck() function
  
  txt <- "blablabla the effect was very significant (t(100)=1, p < 0.001)"
  stat <- statcheck(txt)
  
  # next, use this output to generate a nice HTML report of the results
  
  statcheckReport(stat, outputFileName="statcheckHTMLReport", outputDir="C:/mydocuments/results")
  }
  
  # you can now find your HTML report in the folder 
  # "C:/mydocuments/results" under the name "statcheckHTMLReport.html".
  
}


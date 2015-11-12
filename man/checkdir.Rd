\name{checkdir}
\alias{checkdir}
\title{Extract test statistics from all HTML and PDF files in a folder.}
\description{Extracts statistical references from a directory with HTML and PDF files. The "pdftotext" program is used to convert PDF files to plain text files. This must be installed and PATH variables must be properly set so that this program can be used from command line.
By default a gui window is opened that allows you to choose the directory (using tcltk).}
\usage{checkdir(dir, subdir = TRUE, ...)}
\arguments{
  \item{dir}{String indicating the directory to be used. If this is left empty, a window will pop up from which you can choose a directory.}
  \item{subdir}{Logical indicating whether you also want to check subfolders. Defaults to TRUE}
  \item{\dots}{Arguments sent to  \code{\link{statcheck}}.}
}
\details{See \code{\link{statcheck}} for more details. This function is a wrapper around both \code{\link{checkPDFdir}} for PDF files and \code{\link{checkHTMLdir}} for HTML files.
Depending on the PDF file the comparison operators (=/</>) can sometimes not be converted correctly, causing these to not be reported in the output. Using html versions of articles is reccomended for more stable results.
Note that the conversion to plain text and extraction of statistics can result in errors. Some statistical values can be missed, especially if the notation is unconventional. It is recommended to manually check some of the results.}
\value{A data frame containing for each extracted statistic:
\item{Source}{Name of the file of which the statistic is extracted}
\item{Statistic}{Character indicating the statistic that is extracted}
\item{df1}{First degree of freedom}
\item{df2}{Second degree of freedom (if applicable)}
\item{Test.Comparison}{Reported comparison of the test statistic, when importing from pdf this will often not be converted properly}
\item{Value}{Reported value of the statistic}
\item{Reported.Comparison}{Reported comparison, when importing from pdf this might not be converted properly}
\item{Reported.P.Value}{The reported p-value, or NA if the reported value was NS}
\item{Computed}{The recomputed p-value}
\item{Raw}{Raw string of the statistical reference that is extracted}
\item{Error}{The computed p value is not congruent with the reported p value}
\item{DecisionError}{The reported result is significant whereas the recomputed result is not, or vice versa.}
\item{OneTail}{Logical. Is it likely that the reported p value resulted from a correction for one-sided testing?}
\item{OneTailedInTxt}{Logical. Does the text contain the string "sided", "tailed", and/or "directional"?}
\item{CopyPaste}{Logical. Does the exact string of the extracted raw results occur anywhere else in the article?}}

\author{Sacha Epskamp <mail@sachaepskamp.com> & Michele B. Nuijten
<m.b.nuijten@uvt.nl>}



\seealso{\code{\link{statcheck}}, \code{\link{checkPDF}}, \code{\link{checkHTMLdir}}, \code{\link{checkHTML}}, \code{\link{checkHTMLdir}}}
\examples{
# with this command a menu will pop up from which you can select the directory with articles
# checkdir()

# you could also specify the directory beforehand
# for instance:
# DIR <- "C:/mydocuments/articles"
# checkdir(DIR)
}

\name{checkPDF}
\alias{checkPDF}
\title{Extract statistics and recompute p-values from pdf files.}
\description{Extracts statistical values (currently only t and F statistics) from PDF files. To this end the "pdftotext" program is used to convert PDF files to plain text files. This must be installed and PATH variables must be properly set so that this program can be used from command line.}
\usage{checkPDF(files, ...)}\arguments{
  \item{files}{Vector with paths to the PDF files.}
  \item{\dots}{Arguments sent to  \code{\link{statcheck}}}
}
\details{See \code{\link{statcheck}} for more details. Use \code{\link{checkPDFdir}} to import every PDF file in a given directory. Currently only statistics in the form "(stat (df1, df2) = value, p = value)" are extracted.
Note that this function is still in devellopment. Some statistical values can be missed, especially if the notation is unconvetional. It is recommended to manually check some of the results.}
\value{A data frame containing for each extracted statistic:
\item{Source}{Name of the file of which the statistic is extracted}
\item{Statistic}{Character indicating the statistic that is extracted}
\item{df1}{First degree of freedom}
\item{df2}{Second degree of freedom (if applicable)}
\item{Value}{Reported value of the statistic}
\item{Reported.Comparison}{Reported comparison, when importing from pdf this will often not be converted properly}
\item{Reported.P.Value}{The reported p-value, or NA if the reported value was NS}
\item{Computed}{The recomputed p-value}
\item{Raw}{Raw string of the statistical reference that is extracted}
\item{InExactError}{Error in inexactly reported p values as compared to the recalculated p values}
\item{ExactError}{Error in exactly reported p values as compared to the recalculated p values}
\item{DecisionError}{The reported result is significant whereas the recomputed result is not, or vice versa.}}

\author{Sacha Epskamp <mail@sachaepskamp.com> & Michele B. Nuijten
<m.b.nuijten@uvt.nl>}



\seealso{\code{\link{statcheck}}, \code{\link{checkPDFdir}}}
\examples{
# given that my PDF file is called "article.pdf"
# and I saved it in "C:/mydocuments/articles"

 # checkPDF("C:/mydocuments/articles/article.pdf")
}

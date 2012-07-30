\name{statcheck}
\alias{statcheck}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract statistics and recompute p-values.
}
\description{
This function extracts statistics from strings and returns the extracted values, reported p-values and recomputed p-values.
}
\usage{
statcheck(x, stat = c("t", "F", "cor", "chisq"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A vector of strings containing whole articles.
}
  \item{stat}{
"t" to extract t-values and "F" to extract F-values.
}
}
\details{
This function can be used if the text of articles has already been imported in R. To import text from pdf files and automatically send the results to this function use \code{\link{checkPDFdir}} or \code{\link{checkPDF}}. To import text from HTML files use the similar functions \code{\link{checkHTMLdir}} or \code{\link{checkHTML}}. Finally, \code{\link{checkdir}} can be used to import text from both PDF and HTML files in a folder.

For PDF files the "pdftotext" program is used to convert PDF files to plain text files. This must be installed and PATH variables must be properly set so that this program can be used from command line.

This function uses regular expressions to find statistial references in the form "stat (df1, df2) = value, p = value". If the statistic is "t" and there is only one degree of freedom a t value is extracted, if the statistic is "r" with one degree of freedom a correlation is extraced and if the statistic is "F" with two degrees of freedom a F value is extracted. Finally, chi-square values are extracted by finding all statistical references with one degree of freedom that do not follow a "t" or "r".

Note that the conversion to plain text and extraction of statistics can result in errors. Some statistical values can be missed, especially if the notation is unconvetional. It is recommended to manually check some of the results.
}
\value{
A data frame containing for each extracted statistic:
\item{Source}{Name of the file of which the statistic is extracted}
\item{Statistic}{Character indicating the statistic that is extracted}
\item{df1}{First degree of freedom}
\item{df2}{Second degree of freedom (if applicable)}
\item{Value}{Reported value of the statistic}
\item{Reported.Comparison}{Reported comparison, when importing from pdf this will often not be converted properly}
\item{Reported.P.Value}{The reported p-value, or NA if the reported value was NS}
\item{Computed}{The re-computed p-value}
\item{OneTail}{Smallest one-tailed recoputed p-value (if applicable)}
\item{Location}{Location of the statistic in the string.}
\item{Raw}{Raw string of the statistical reference that is extracted}
}
\author{
Sacha Epskamp <mail@sachaepskamp.com> & Michele Nuijten
}
\seealso{
\code{\link{statcheck}}, \code{\link{checkPDF}}, \code{\link{checkHTMLdir}}, \code{\link{checkHTML}}, \code{\link{checkdir}}
}

\examples{
Text <- "blablabla the effect was very signigicant (t(100)=1, p < 0.001)"

statcheck(Text)
}

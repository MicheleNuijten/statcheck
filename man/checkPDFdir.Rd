\name{checkPDFdir}
\alias{checkPDFdir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract statistics and recompute p values from a directory with pdf files.
}
\description{
Extracts statistical references from a directory with PDF files. The "pdftotext" program is used to convert PDF files to plain text files. This must be installed and PATH variables must be properly set so that this program can be used from command line.

By default a gui window is opened that allows you to choose the directory (using tcltk).
}
\usage{
checkPDFdir(dir, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{
String indicating the directory to be used.
}
  \item{\dots}{
Arguments sent to  \code{\link{statcheck}}.
}
}
\details{
See \code{\link{statcheck}} for more details. Use \code{\link{checkPDF}} to import individual PDF files. Currently only statistics in the form "stat (df1, df2) = value, p = value" are extracted. Because the Chi-square symbol can not be repressented in plain text it is often lost in the conversion. Because of this Chi-square values are extracted by finding all statistical references with one degree of freedom that do not follow the symbol "t" or "r". While this does extract most Chi-square values it is possible that other statistics, possibly due to unconventional notation, are also extracted and reported as chi-square values.

Depending on the PDF file the comparison operators can sometimes not be converted correctly, causing these to not be reported in the output. Using html versions of articles and the similar function \code{\link{checkHTMLdir}} is reccomended for more stable results.

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
\dontrun{

# with this command a menu will pop up from which you can select the directory with PDF articles
checkPDFdir()

# you could also specify the directory beforehand
# for instance:
DIR <- "C:/mydocuments/articles"
checkPDFdir(DIR)
}
}
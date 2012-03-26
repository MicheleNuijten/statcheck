\name{checkPDFdir}
\alias{checkPDFdir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract statistics and recompute p-values from directory with pdf files.
}
\description{
Extracts statistical values (currently only t and F statistics) from directory withPDF files. To this end the "pdftotext" program is used to convert PDF files to plain text files. This must be installed and PATH variables must be properly set so that this program can be used from command line.

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
Arguments sent to  \code{\link{statcheck}}
}
}
\details{
See \code{\link{statcheck}} for more details. Use \code{\link{checkPDF}} to import individual PDF files. Currently only statistics in the form "(stat (df1, df2) = value, p = value)" are extracted.

Note that this function is still in devellopment. Some statistical values can be missed, especially if the notation is unconvetional. It is recommended to manually check some of the results.
}
\value{
A data frame containing for each extracted statistic:
\item{Source}{Name of the element in the vector 'x' of which the statistic is extracted}
\item{Statistic}{Character indicating the statistic that is extracted}
\item{df1}{First degree of freedom}
\item{df2}{Second degree of freedom (if applicable)}
\item{Value}{Reported value of the statistic}
\item{Reported.Comparison}{Reported comparison, when importing from pdf this will often not be converted properly}
\item{Reported.P.Value}{The reported p-value, or NA if the reported value was NS}
\item{Computed}{The re-computed p-value}
\item{OneTail}{Smallest one-tailed recoputed p-value (if applicable)}
\item{Location}{Location of the statistic in the string.}
}
\author{
Sacha Epskamp (mail@sachaepskamp.com)
}
\seealso{
\code{\link{statcheck}}, \code{\link{checkPDF}}
}

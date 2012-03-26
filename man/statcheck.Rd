\name{statcheck}
\alias{statcheck}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract statistics and recompute p-values.
}
\description{
This function extracts statistics (currently t and F statistics in APA format with no effect size) from strings and returns the extracted values, reported p-values and recomputed p-values.
}
\usage{
statcheck(x, stat = c("t", "F"))
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
This function can be used if the text of articles has already been imported in R. To import text from pdf files and automatically send the results to this function use \code{\link{checkPDFdir}} or \code{\link{checkPDF}}. Currently only statistics in the form "(stat (df1, df2) = value, p = value)" are extracted.

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
\code{\link{checkPDFdir}}, \code{\link{checkPDF}}
}
\examples{
Text <- "blablabla the effect was very signigicant (t(100)=1, p < 0.001)"

statcheck(Text)
}

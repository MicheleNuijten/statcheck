\name{statcheck}
\alias{statcheck}
\title{Extract statistics and recompute p-values.}
\description{This function extracts statistics from strings and returns the extracted values, reported p-values and recomputed p-values. The package relies on the program "pdftotext", see the paragraph "Note" for details on the installation.}
\usage{statcheck(x, stat = c("t", "F", "cor", "chisq", "Z", "Wald"), 
    OneTailedTests = FALSE, alpha = 0.05, OneTailedTxt = FALSE)}
\arguments{
  \item{x}{A vector of strings containing whole articles.}
  \item{stat}{"t" to extract t-values, "F" to extract F-values, "cor" to extract correlations, "chisq"to extract chisquare values, "Z" to extract Z-values, and "Wald" to extract the results of the Wald test.}
  \item{OneTailedTests}{Logical. Do we assume that all reported tests are one tailed (TRUE) or two tailed (FALSE, default).}
  \item{alpha}{Assumed level of significance in the scanned texts. Defaults to .05. }
  \item{OneTailedTxt}{Logical. If TRUE, statcheck searches the text for "sided", "tailed", and "directional" to identify the possible use of one-sided tests. If one or more of these strings is found in the text AND the result would have been correct if it was a one-sided test, the result is assumed to be indeed one-sided and is counted as correct.}
}
\details{This function can be used if the text of articles has already been imported in R. To import text from pdf files and automatically send the results to this function use \code{\link{checkPDFdir}} or \code{\link{checkPDF}}. To import text from HTML files use the similar functions \code{\link{checkHTMLdir}} or \code{\link{checkHTML}}. Finally, \code{\link{checkdir}} can be used to import text from both PDF and HTML files in a folder.
This function uses regular expressions to find statistical references in the form "stat (df1, df2) = value, p = value". If the statistic is "t" and there is only one degree of freedom a t value is extracted, if the statistic is "r" with one degree of freedom a correlation is extraced and if the statistic is "F" with two degrees of freedom a F value is extracted. Finally, chi-square values are extracted by finding all statistical references with one degree of freedom that do not follow a "t" or "r".
Note that the conversion to plain text and extraction of statistics can result in errors. Some statistical values can be missed, especially if the notation is unconventional. It is recommended to manually check some of the results.
For PDF files the "pdftotext" program is used to convert PDF files to plain text files. You can obtain pdftotext from \code{http://www.foolabs.com/xpdf/download.html}. Download and unzip the precompiled binaries. Next, add the folder with the binaries to the PATH variables so that this program can be used from command line.
Note that a seemingly inconsistent p value can still be correct when we take the possibility into account that the test statistic was rounded after calculating the corresponding p value. For instance, a reported t value of 2.35 could correspond to an actual value of 2.345 to 2.354 with a range of p values that can slightly deviate from the recomputed p value but still be correct.}
\value{A data frame containing for each extracted statistic:}

\author{Sacha Epskamp <mail@sachaepskamp.com> & Michele B. Nuijten <m.b.nuijten@uvt.nl>}



\seealso{\code{\link{checkPDF}}, \code{\link{checkHTMLdir}}, \code{\link{checkHTML}}, \code{\link{checkdir}}}
\examples{
txt <- "blablabla the effect was very significant (t(100)=1, p < 0.001)"
statcheck(txt)
}

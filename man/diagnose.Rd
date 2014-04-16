\name{diagnose}
\alias{diagnose}
\title{Diagnose the most likely cause why a p value is inconsistent with its test statistic and degrees of freedom.}
\description{This function analyzes whether inconsistent results in articles (as diagnosed with \code{\link{statcheck}} could be due to one of the most frequently observed causes (Bakker & Wicherts, 2011).}
\usage{diagnose(x)}
\arguments{
  \item{x}{a \code{statcheck} output.}
}
\details{This dataframe contains the error diagnosis of the analyzed articles that contained an error and has the following components: 
Source: Name of the file of which the statistic is extracted 

Raw: Raw string of the statistical reference that is extracted

Computed: The recomputed p-value 

OneTail: Logical. Is it likely that the reported p value resulted from a correction for one-sided testing?

RoundError: Logical. P value wrongly rounded upward or downward. E.g. rounding a p value of .049 to .04.

PSmallerThanZero: Logical. Reported p <.000.

SmallerInsteadEqual: Logical. Reported "<" when "=" would be correct. E.g. F(2,20)=2.33, p <.123, whereas the correct p value is equal to .123.

Bonferroni: Logical. Could the reported p value have resulted from a Bonferroni correction? Note: The corrected p values were calculated by multiplying the reported p values by the number of statistical results reported in one article. This is a rough estimation.

Unidentifiable: Logical. The error could not be classified on the basis of the extracted information.

CopyPaste: Logical. Does the exact string of the extracted raw results occur anywhere else in the article?

This dataframe contains an overview of the detected copy-paste errors in all the articles. Its components are the same as the ones in ErrorDiagnosis. Note that a copy-paste error could still be congruent in terms of test statistic and p value, so it is possible that these errors do not show up in ErrorDiagnosis. 
This dataframe contains the error diagnosis for every article, not just the articles that contained an error. The components are the same as in ErrorDiagnosis. This dataframe is not automatically printed but can be obtained through $FullDiagnosis.}
\value{a list that contains four dataframes: ErrorDiagnosis, CopyPaste, Summary, and FullDiagnosis. In the following sections the content of these dataframes will be described in detail. }
\references{Bakker, M. & Wicherts, J. M. (2011). The (mis)reporting of statistical results in psychology journals. Behavior Research Methods, 43, 666-678.}
\author{Sacha Epskamp <mail@sachaepskamp.com> & Michele B. Nuijten <m.b.nuijten@uvt.nl>}
\note{Please note that the resulting diagnosis is more a rough indication of what could be going on than it is a clear cut verdict. The function is therefore most suitable if you want to get a rough idea of the type of errors made in a certain set of articles. }


\seealso{\code{\link{statcheck}}, \code{\link{identify.statcheck}}}
\examples{
  # given that the articles of interest are saved in "DIR"
# DIR <- "C:/mydocuments/articles"
# stat_result <- checkdir(DIR)

# diagnosis <- diagnose(stat_result)

# print diagnosis
# diagnosis

# print diagnosis for all articles
# diagnosis$FullDiagnosis
}

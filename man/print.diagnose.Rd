\name{print.diagnose}
\alias{print.diagnose}
\title{Print diagnose output.}
\description{Print the output of a \code{diagnose} object.}
\usage{\method{print}{diagnose}(x, ...)}
\arguments{
  \item{x}{
}
  \item{\dots}{
}
}
\details{This dataframe contains the error diagnosis of the analyzed articles that contained an error and has the following components: 
Source: Name of the file of which the statistic is extracted

Raw: Raw string of the statistical reference that is extracted

Computed: The recomputed p-value

OneTail: Logical. Is it likely that the reported p value resulted from a correction for one-sided testing?

CorrectRound: Logical. Is the reported p value correct when we take the possibility into account that the test statistic was rounded after calculating the corresponding p value? For instance, a reported t value of 2.35 could correspond to an actual value of 2.345 to 2.354 with a range of p values that can slightly deviate from the recomputed p value but still be correct.
RoundError: Logical. P value wrongly rounded upward or downward. E.g. rounding a p value of .049 to .04.

PSmallerThanZero: Logical. Reported p <.000.

SmallerInsteadEqual: Logical. Reported "<" when "=" would be correct. E.g. F(2,20)=2.33, p <.123, whereas the correct p value is equal to .123.

Bonferroni: Logical. Could the reported p value have resulted from a Bonferroni correction? Note: The corrected p values were calculated by multiplying the reported p values by the number of statistical results reported in one article. This is a rough estimation.

Unidentifiable: Logical. The error could not be classified on the basis of the extracted information.

CopyPaste: Logical. Does the exact string of the extracted raw results occur anywhere else in the article?

This dataframe contains an overview of the detected copy-paste errors in all the articles. Its components are the same as the ones in ErrorDiagnosis. Note that a copy-paste error could still be congruent in terms of test statistic and p value, so it is possible that these errors do not show up in ErrorDiagnosis. 
This dataframe provides a summary of all detected errors. It summarizes the number of detected errors in each of the aforementioned categories per article. The copy-paste errors are counted in a way that two identical strings of results are counted as one copy-paste error.}
\value{a list that contains four dataframes: ErrorDiagnosis, CopyPaste, Summary, and FullDiagnosis. In the following sections the content of these dataframes will be described in detail. }

\author{Sacha Epskamp <mail@sachaepskamp.com> & Michele B. Nuijten <m.b.nuijten@uvt.nl>}



\seealso{\code{\link{diagnose}}, \code{\link{statcheck}}}
\examples{
	# given that the articles of interest are saved in "DIR"
DIR <- "C:/mydocuments/articles"
stat_result <- checkdir(DIR)

diagnose(stat_result)
}

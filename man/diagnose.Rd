\name{diagnose}
\alias{diagnose}
\title{Diagnose the most likely cause why a p value is inconsistent with its test statistic and degrees of freedom.}
\description{This function analyzes whether inconsistent results in articles (as diagnosed with \code{\link{statcheck}} could be due to one of the most frequently observed causes (Bakker & Wicherts, 2011).}
\usage{diagnose(x)}
\arguments{
  \item{x}{a \code{statcheck} output.}
}
\details{This dataframe contains the error diagnosis of the analyzed articles that contained an error and has the following components: 
This dataframe contains an overview of the detected copy-paste errors in all the articles. Its components are the same as the ones in ErrorDiagnosis. Note that a copy-paste error could still be congruent in terms of test statistic and p value, so it is possible that these errors do not show up in ErrorDiagnosis. 
This dataframe contains the error diagnosis for every article, not just the articles that contained an error. The components are the same as in ErrorDiagnosis. This dataframe is not automatically printed but can be obtained through $FullDiagnosis.}
\value{a list that contains four dataframes: ErrorDiagnosis, CopyPaste, Summary, and FullDiagnosis. In the following sections the content of these dataframes will be described in detail. }
\references{Bakker, M. & Wicherts, J. M. (2011). The (mis)reporting of statistical results in psychology journals. Behavior Research Methods, 43, 666-678.}
\author{Sacha Epskamp <mail@sachaepskamp.com> & Michele B. Nuijten <m.b.nuijten@uvt.nl>}
\note{Please note that the resulting diagnosis is more a rough indication of what could be going on than it is a clear cut verdict. The function is therefore most suitable if you want to get a rough idea of the type of errors made in a certain set of articles. }


\seealso{\code{\link{statcheck}}, \code{\link{identify.statcheck}}}
\examples{
  # given that the articles of interest are saved in "DIR"
DIR <- "C:/mydocuments/articles"
stat_result <- checkdir(DIR)

diagnosis <- diagnose(stat_result)

# print diagnosis
diagnosis

# print diagnosis for all articles
diagnosis$FullDiagnosis
}

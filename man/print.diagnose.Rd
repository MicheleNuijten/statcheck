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
\details{This dataframe contains the error diagnosis of the analyzed articles that contained an error and has the following components: }
\value{a list that contains four dataframes: ErrorDiagnosis, CopyPaste, Summary, and FullDiagnosis. In the following sections the content of these dataframes will be described in detail. }

\author{Sacha Epskamp <mail@sachaepskamp.com> & Michele B. Nuijten <m.b.nuijten@uvt.nl>}



\seealso{\code{\link{diagnose}}, \code{\link{statcheck}}}
\examples{
	# given that the articles of interest are saved in "DIR"
DIR <- "C:/mydocuments/articles"
stat_result <- checkdir(DIR)

diagnose(stat_result)
}

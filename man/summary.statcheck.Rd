\name{summary.statcheck}
\alias{summary.statcheck}
\title{Summary method for \code{statcheck}.}
\description{Gives the summaries for a \code{statcheck} object.  }
\usage{\method{summary}{statcheck}(object, ...)}\arguments{
  \item{object}{a \code{statcheck} object.}
  \item{\dots}{additional arguments affecting the summary produced.}
}

\value{A data frame containing for each extracted statistic:
\item{Source}{Name of the file of which the statistic is extracted}
\item{pValues}{The number of reported p values per article}
\item{Errors}{The number of errors per article}
\item{DecisionErrors}{The number of errors that caused a non-significant result to be reported as significant (or vice versa) per article}}

\author{Sacha Epskamp <mail@sachaepskamp.com> & Michele B. Nuijten
<m.b.nuijten@uvt.nl>}



\seealso{\code{\link{statcheck}}}
\examples{
  Text <- "blablabla the effect was very significant (t(100)=1, p < 0.001)"
Stat <- statcheck(Text)

summary(Stat)
}

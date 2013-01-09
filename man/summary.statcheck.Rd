\name{summary.statcheck}
\alias{summary.statcheck}
\title{
Summary method for \code{statcheck}.
}
\description{
Gives the summaries for a \code{statcheck} object.
}
\usage{
\method{summary}{statcheck}(object, ...)
}
\arguments{
  \item{object}{
a \code{statcheck} object.
}
  \item{\dots}{
additional arguments affecting the summary produced.
}
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
\item{Computed}{The recomputed p-value}
\item{Raw}{Raw string of the statistical reference that is extracted}
\item{InExactError}{Error in inexactly reported p values as compared to the recalculated p values}
\item{ExactError}{Error in exactly reported p values as compared to the recalculated p values}
\item{DecisionError}{The reported result is significant whereas the recomputed result is not, or vice versa.}
}
\author{
Sacha Epskamp <mail@sachaepskamp.com> & Michele B. Nuijten <m.b.nuijten@uvt.nl>
}
\seealso{
\code{\link{statcheck}}
}
\examples{
Text <- "blablabla the effect was very significant (t(100)=1, p < 0.001)"
Stat <- statcheck(Text)

summary(Stat)
}
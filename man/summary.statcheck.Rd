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
summary.statcheck returns a data.frame of class \code{statcheck} with the following summaries reported per paper and in total:
\item{Source}{Source of the extracted statistic. Usually the name of the file, if no file name is available the sources will be numbered.}
\item{pValues}{The number of p values extracted.}
\item{NonSigAsSig}{The number of exact results that were reported as significant, but turned out to be non significant after recalculation.}
\item{SigAsNonSig}{The number of exact results that were reported as non significant, but turned out to be significant after recalculation.}
\item{ExactErrors}{The total number of errors in the exactly reported p values as compared to the recalculated p values. A difference between the reported and recalculated was counted as an error when it was greater than .01.}
\item{MeanDeviationExact}{The mean deviation of the exactly reported p values from the recalculated p values.}
\item{InExactErrors}{Total number of errors in inexact reported p values as compared to the recalculated p values.}
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
\name{summary.statcheck}
\alias{summary.statcheck}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary method for "statcheck"
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Gives the summaries for a "statcheck" object.
}
\usage{
\method{summary}{statcheck}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{x} here~~
a "statcheck" object.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
summary.statcheck returns a data.frame of class "statcheck" with the following summaries reported per paper and in total:
\item{pValues}{The number of p values extracted.}
\item{NonSigAsSig}{The number of exact results that were reported as significant, but turned out to be non significant after recalculation.}
\item{SigAsNonSig}{The number of exact results that were reported as non significant, but turned out to be significant after recalculation.}
\item{TotalExactErrors}{The total number of errors in the exactly reported p values as compared to the recalculated p values. A difference between the reported and recalculated was counted as an error when it was greater than .01.}
\item{MeanDeviationExact}{The mean deviation of the exactly reported p values from the recalculated p values.}
\item{TotalInExactErrors}{Total number of errors in inexact reported p values as compared to the recalculated p values.}
}
\author{
%%  ~~who you are~~
Sacha Epskamp <mail@sachaepskamp.com> & Michele Nuijten
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{statcheck}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

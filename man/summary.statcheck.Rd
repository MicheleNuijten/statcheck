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
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\verb|summary.statcheck| returns a data.frame of class "statcheck" with the following summaries reported per paper and in total:
\item{pValues} The number of p values extracted.
\item{NonSigAsSig} The number of results that were reported as significant, but turned out to be non significant after recalculation.
\item{SigAsNonSig} The number of results that were reported as non significant, but turned out to be significant after recalculation.
\item{TotalExactErrors} The total number of errors in the exactly reported p values as compared to the recalculated p values. A difference between the reported and recalculated was counted as an error when it was greater than .01.
\item{MeanDeviationExact} The mean deviation of the exactly reported p values from the recalculated p values.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Sacha Epskamp <mail@sachaepskamp.com> and Mich\`ele Nuijten
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, ...) 
{
    Paper <- c(unique(x$Source), "Total")
    pValues <- c(ddply(x, "Source", function(x) nrow(x))[, 2], 
        nrow(x))
    computed <- x$Computed
    computed[!is.na(x$OneTail)] <- ifelse(abs(x$Reported.P.Value - 
        x$OneTail)[!is.na(x$OneTail)] < abs(x$Reported.P.Value - 
        x$Computed)[!is.na(x$OneTail)], x$OneTail[!is.na(x$OneTail)], 
        x$Computed[!is.na(x$OneTail)])
    x <- cbind(x, computed)
    SigAsNonSig <- ddply(x, "Source", function(x) {
        sum(x$Reported.P.Value < 0.05 & x$computed > 0.05)
    })[, 2]
    SigAsNonSig <- c(SigAsNonSig, sum(SigAsNonSig))
    NonSigAsSig <- ddply(x, "Source", function(x) {
        sum(x$Reported.P.Value > 0.05 & x$computed < 0.05)
    })[, 2]
    NonSigAsSig <- c(NonSigAsSig, sum(NonSigAsSig))
    GrossErrors <- ddply(x, "Source", function(x) {
        sum(x$Reported.P.Value > 0.05 & x$computed < 0.05, x$Reported.P.Value < 
            0.05 & x$computed > 0.05)
    })[, 2]
    GrossErrors <- c(GrossErrors, sum(GrossErrors))
    res <- data.frame(Paper = Paper, Number_of_p_values = pValues, 
        NonSigAsSig = NonSigAsSig, SigAsNonSig = SigAsNonSig, 
        TotalGrossErrors = GrossErrors)
    class(res) <- c("statcheck", "data.frame")
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

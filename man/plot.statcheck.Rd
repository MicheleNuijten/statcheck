\name{plot.statcheck}
\alias{plot.statcheck}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot method for "statcheck"
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Function for plotting of "statcheck" objects.Reported p values are plotted against recalculated p values, which allows the user to easily spot if articles contain miscalculations of statistical results. 
}
\usage{
\method{plot}{statcheck}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
a "statcheck" object.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
arguments to be passed to methods, such as graphical parameters (see par).
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Recalculations of the p value that render a previously non significant result (p >= .5) as significant (p < .05), and vice versa, are considered gross errors, and are indicated with a red dot. Exactly reported p values (i.e. p = ..., as opposed to p < ... or p > ...) are indicated with a diamond.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Sacha Epskamp (mail $@$ sachaepskamp.com) & Mich\`ele Nuijten
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

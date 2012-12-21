\name{identify.statcheck}
\alias{identify.statcheck}
\title{
Identify specific points in a \code{statcheck} plot.
}
\description{
With this function you can simply point and click on the datapoints in the plot to see the corresponding statcheck details, such as the paper from which the data came and the exact statistical results.
}
\usage{
\method{identify}{statcheck}(x, ...)
}
\arguments{
  \item{x}{
a \code{statcheck} object.
}
  \item{\dots}{
additional arguments to be passed on to the plot method.
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
}

\author{
Sacha Epskamp <mail@sachaepskamp.com> and Michele B. Nuijten <m.b.nuijten@uvt.nl>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# given that the articles of interest are saved in "DIR"
DIR <- "C:/mydocuments/articles"
stat_result <- checkdir(DIR)

identify(stat_result)
}
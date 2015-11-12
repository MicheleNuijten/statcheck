\name{identify.statcheck}
\alias{identify.statcheck}
\title{Identify specific points in a \code{statcheck} plot.}
\description{With this function you can simply point and click on the datapoints in the plot to see the corresponding statcheck details, such as the paper from which the data came and the exact statistical results.}
\usage{\method{identify}{statcheck}(x, alpha = 0.05, ...)}
\arguments{
  \item{x}{a \code{statcheck} object.}
  \item{alpha}{assumed level of significance in the scanned texts. Defaults to .05.}
  \item{\dots}{additional arguments to be passed on to the plot method.}
}

\value{This function returns both a plot and a dataframe. For the contents of the dataframe see \code{\link{statcheck}}.}

\author{Sacha Epskamp <mail@sachaepskamp.com> & Michele B. Nuijten
<m.b.nuijten@uvt.nl>}



\seealso{\code{\link{statcheck}}}
\examples{
	# given that the articles of interest are saved in "DIR"
# DIR <- "C:/mydocuments/articles"
# stat_result <- checkdir(DIR)

# identify(stat_result)

## Further instructions:
# click on one or multiple points of interest
# press Esc
# a dataframe with information on the selected points will appear
}

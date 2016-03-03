\name{plot.statcheck}

\alias{plot.statcheck}

\title{Plot method for "statcheck"}

\description{Function for plotting of "statcheck" objects. Reported p values are plotted against recalculated p values, which allows the user to easily spot if articles contain miscalculations of statistical results. }

\usage{\method{plot}{statcheck}(x, alpha = 0.05, APAstyle = TRUE, group = NULL, 

    ...)}

\arguments{

  \item{x}{a "statcheck" object. See \code{\link{statcheck}}.}

  \item{alpha}{assumed level of significance in the scanned texts. Defaults to .05. }

  \item{APAstyle}{if TRUE, prints plot in APA style}

  \item{group}{indicate grouping variable to facet plot. Only works when APAstyle==TRUE}

  \item{\dots}{arguments to be passed to methods, such as graphical parameters (see \code{\link{par}}).}

}

\details{If APAstyle = FALSE, inconsistencies between the reported and the recalculated p value are indicated with an orange dot. Recalculations of the p value that render a previously non significant result (p >= .5) as significant (p < .05), and vice versa, are considered gross errors, and are indicated with a red dot. Exactly reported p values (i.e. p = ..., as opposed to p < ... or p > ...) are indicated with a diamond.}





\author{Sacha Epskamp <mail@sachaepskamp.com> & Michele B. Nuijten

<m.b.nuijten@uvt.nl>. Many thanks to John Sakaluk who adapted the plot code to create graphs in APA style.}







\seealso{\code{\link{statcheck}}}




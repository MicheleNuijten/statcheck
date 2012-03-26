\name{swstNP}
\alias{swstNP}
\title{LaTeX code for statistical reference (no paranthesis version)}
\usage{
  swstNP(x, ...)
}
\arguments{
  \item{x}{An input object. Currently supported are a
  numerical vector and objects of classes "htest"
  (depending on statistic), "lm", "aov" and "anova".}

  \item{\dots}{Arguments sent to \code{\link{swp}}}
}
\value{
  A strings containing LaTeX code, or a named vector of
  strings containing LaTeX code
}
\description{
  The same as \code{\link{swst}} except that no parantheses
  are printed
}

\author{
  Sacha Epskamp (mail@sachaepskamp.com)
}
\seealso{
  \code{\link{swst}} \code{\link{swp}}
}


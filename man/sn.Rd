\name{sn}
\alias{sn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
LaTeX code for scientific notation
}
\description{
This function will return LaTeX code for scientific notation of it's input, taken from my answer on Stackoverflow (http://stackoverflow.com/a/8367233/567015).
}
\usage{
sn(x, digits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Number to be formatted
}
  \item{digits}{
Number of digits to be displayed
}
}

\author{
Sacha Epskamp (mail@sachaepskamp.com)
}

\examples{
## Example in Sweave:
## Blablabla this is a pretty formatted number $\Sexpr{sn(0.00134,2)}$.

## Some examples:
sn(2000000)
sn(0.001)
sn(0.00005)
sn(10.1203)
sn(-0.00013)
sn(0)

}


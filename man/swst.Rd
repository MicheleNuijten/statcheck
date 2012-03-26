\name{swst}
\alias{swst}
\alias{swst.anova}
\alias{swst.Anova.mlm}
\alias{swst.aov}
\alias{swst.default}
\alias{swst.htest}
\alias{swst.lm}
\title{swst:LaTeX code for statistical reference}

\usage{
  swst(x, ...)

  \method{swst}{htest} (x, ...)

  \method{swst}{aov} (x, ...)

  \method{swst}{lm} (x, ...)

  \method{swst}{anova} (x, ...)

  \method{swst}{Anova.mlm} (x, ...)

  \method{swst}{default} (x, ...)
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
  swst:LaTeX code for statistical reference

  This function attempts to extract information on
  statistic, degrees of freedom and p-value from a given
  object, and returns a LaTeX code.

  This function attemps to extract information needed to
  make a LaTeX code of the statistic in the following
  format (for an F value):

  (F(df1,df2)=..., p=...)

  Some arguments can be passed to \code{\link{swp}} to
  modify the way the statistic is printed. For example,
  'digits' an be used to change the amount of digits
  printed and 'sig' can be used to change the way to show
  only significance cutoff scores. For example, with p=0.02
  this would return: (F(df1,df2)=..., p<0.05)

  This is aimed to facilitate the use of Sweave especially
  for users new to R. This is currently in development and
  is so far only a few commonly used statistics are
  implemented, but this should change in future versions.

  If you have any ideas on other ways to facilitate using
  Sweave (except of course existing methods such as xtable)
  please contact me. Also, if you know of an object that is
  not implemented please contact me as well. 
}
\examples{
### Using examples from documentation of the used functions
# t-test:
require(graphics)
tTest <- t.test(1:10,y=c(7:20))

swst(tTest)

# Significance:
swst(tTest,sig=TRUE)

tTest <- t.test(1:10,y=1:10)
swst(tTest,sig=TRUE)

# Correlation test:
## Hollander & Wolfe (1973), p. 187f.
## Assessment of tuna quality.  We compare the Hunter L measure of
##  lightness to the averages of consumer panel scores (recoded as
##  integer values from 1 to 6 and averaged over 80 such values) in
##  9 lots of canned tuna.

x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)

##  The alternative hypothesis of interest is that the
##  Hunter L value is positively associated with the panel score.
corTest <- cor.test(x, y, method = "kendall", alternative = "greater")

swst(corTest)

# Chi-square test:
M <- as.table(rbind(c(762, 327, 468), c(484,239,477)))
dimnames(M) <- list(gender=c("M","F"),
                    party=c("Democrat","Independent", "Republican"))
chisqTest <- chisq.test(M)

swst(chisqTest)

# Linear model:
## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
## Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2,10,20, labels=c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
lm.D90 <- lm(weight ~ group - 1) # omitting intercept

swst(lm.D9)
swst(lm.D90)

\dontrun{
# ANOVA:
## From Venables and Ripley (2002) p.165.
utils::data(npk, package="MASS")

## Set orthogonal contrasts.
op <- options(contrasts=c("contr.helmert", "contr.poly"))
npk.aov <- aov(yield ~ block + N*P*K, npk)

swst(npk.aov)
}
}
\author{
  Sacha Epskamp (mail@sachaepskamp.com)
}
\seealso{
  \code{\link{swp}} \code{\link{swstNP}}
}


## No paranthesis version:
swstNP <- function(x,...)
{
  swst(x,parantheses=FALSE)
}

## Generic method:
swst <- function (x, ...) {
   UseMethod("swst", x)
}

## 'htest' method:
swst.htest <- function(x,...)
{

### chisq.test() ###
   if (any(grepl("X-squared|chi-square",names(x$statistic))))
   {
     # Extract Statistics:
     stat <- x$statistic['X-squared']
     df <- x$parameter['df']
     pval <- x$p.value

     return(swp("\\\\chi^2",stat,pval,df,...))
   }


# If all else fails:
     stat <- x$statistic
     df <- x$parameter
     pval <- x$p.value

     return(swp(names(x$statistic),stat,pval,df,...))
 }

## 'aov' method:
swst.aov <- function(x,...)
{
   return(swst(anova(x),...))
}

## 'lm' method:
swst.lm <- function(x,...)
{
    sum <- summary(x)
    stat <- sum$fstatistic['value']
    df1 <- sum$fstatistic['numdf']
    df2 <- sum$fstatistic['dendf']
    pval <- pf(stat,df1,df2,lower.tail=FALSE)

    return(swp("F",stat,pval,c(df1,df2),...))
 }

## 'anova' method:
swst.anova <- function(x,...)
{
    n <- nrow(x)-1
    res <- character(n)
    names(res) <- rownames(x)[1:n]
    for (i in 1:n)
    {
      if ("num Df"%in%names(x) & "den Df"%in%names(x))
      {
        res[i] <- swp("F",x[i,grepl('approx F|F value',names(x))],x[i,'Pr(>F)'],c(x[i,'num Df'],x[i,'den Df']),...)
      } else
      {
        res[i] <- swp("F",x[i,grepl('approx F|F value',names(x))],x[i,'Pr(>F)'],c(x[i,'Df'],x[n+1,'Df']),...)
      }
    }
    return(res)
  }


## 'Anova.mlm' method:
swst.Anova.mlm <- function(x,...)
{
    ### DATA EXTRACTION COPIED FROM car:::Anova.mlm
    test <- x$test
    repeated <- x$repeated
    ntests <- length(x$terms)
    tests <- matrix(NA, ntests, 4)
    if (!repeated) 
        SSPE.qr <- qr(x$SSPE)
    for (term in 1:ntests) {
        eigs <- Re(eigen(qr.coef(if (repeated) qr(x$SSPE[[term]]) else SSPE.qr, 
            x$SSP[[term]]), symmetric = FALSE)$values)
        tests[term, 1:4] <- switch(test, Pillai = stats:::Pillai(eigs, 
            x$df[term], x$error.df), Wilks = stats:::Wilks(eigs, 
            x$df[term], x$error.df), `Hotelling-Lawley` = stats:::HL(eigs, 
            x$df[term], x$error.df), Roy = stats:::Roy(eigs, 
            x$df[term], x$error.df))
    }
    ok <- tests[, 2] >= 0 & tests[, 3] > 0 & tests[, 4] > 0
    ok <- !is.na(ok) & ok
    tests <- cbind(x$df, tests, pf(tests[ok, 2], tests[ok, 3], 
        tests[ok, 4], lower.tail = FALSE))
    rownames(tests) <- x$terms
    colnames(tests) <- c("Df", "test stat", "approx F", "num Df", 
        "den Df", "Pr(>F)")
    tests <- structure(as.data.frame(tests), heading = paste("\nType ", 
        x$type, if (repeated) 
            " Repeated Measures", " MANOVA Tests: ", test, " test statistic", 
        sep = ""), class = c("anova", "data.frame"))
    ### END CODE COPIED FROM car:::Anova.mlm
    
    n <- nrow(tests) - 1
    res <- character(n)
    names(res) <- rownames(tests)[-1]
    for (i in 1:n)
    {
      res[i] <- swp("F",tests[['approx F']][i+1], tests[['Pr(>F)']][i+1], c(tests[['num Df']][i+1],tests[['den Df']][i+1]), ...)
    }
    return(res)
}



## Default method (simply an error)
swst.default <- function(x,...) stop("The class of your object is not yet supported by swst.\n\nPlease contact me (sacha.epskamp@gmail.com) with information on your object.")

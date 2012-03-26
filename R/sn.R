

sn <- function(x,digits)
{
  if (x==0) return("0")
  ord <- floor(log(abs(x),10))
  x <- x / 10^ord
  if (!missing(digits)) x <- format(x,digits=digits)
  if (ord==0) return(as.character(x))
  return(paste(x,"\\\\times 10^{",ord,"}",sep=""))
}

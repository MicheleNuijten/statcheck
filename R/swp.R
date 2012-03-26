
swp <- function(statistic,value,pval,df=NULL,digits=3,sig=FALSE,alpha=c(0.05,0.01,0.001),output=c("Sweave","knitr"),parantheses=TRUE)
{
  po <- ifelse(parantheses,"(","")
  pc <- ifelse(parantheses,")","")  
  
  if (sig)
  {
    if (pval > max(alpha))
    {
      res <- paste(
        po,"$",
        statistic,
        "",
        ifelse(is.null(df),"",paste("(",paste(format(round(df,digits),scientific=FALSE),collapse=","),")",sep="")),
        " = ",
        format(round(value,digits),scientific=FALSE),
        "$, ns",pc,sep="")
    } else
    {
      whichsig <- which(alpha == min(alpha[pval<=alpha]))
      res <- paste(
        po,"$",
        statistic,
        "",
        ifelse(is.null(df),"",paste("(",paste(format(round(df,digits),scientific=FALSE),collapse=","),")",sep="")),
        " = ",
        format(round(value,digits),scientific=FALSE),
        "$, $p<",
        format(alpha[whichsig],scientific=FALSE),
        "$",pc,sep="")
    } 
  } else
  {
    if (pval > 10^(-1*digits))
    {
      res <- paste(
        po,"$",
        statistic,
        "",
        ifelse(is.null(df),"",paste("(",paste(format(round(df,digits),scientific=FALSE),collapse=","),")",sep="")),
        " = ",
        format(round(value,digits),scientific=FALSE),
        "$, $p=",
        format(round(pval,digits),scientific=FALSE),
        "$",pc,sep="")
    } else
    {
      res <- paste(
        po,"$",
        statistic,
        "",
        ifelse(is.null(df),"",paste("(",paste(format(round(df,digits),scientific=FALSE),collapse=","),")",sep="")),
        " = ",
        format(round(value,digits),scientific=FALSE),
        "$, $p<",
        format(round(10^(-1*digits),digits),scientific=FALSE),
        "$",pc,sep="")    
    }
  }
  if (all(output=="plain"))
  {
    res <- gsub("\\$","",res)
  }
  return(res)  
}

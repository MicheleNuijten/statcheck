
## Main function, checks statistics of vector of strings (articles).
statcheck <- function(x,stat=c("t","F","cor","chisq"))
{
  '%rem%'<- function(x,y)
  {
    at <- attr(x,"match.length")
    x <- x[-y]
    at <- at[-y]
    attr(x,"match.length") <- at
    return(x)
  }
  
  # Create empty data frame:
  Res <- data.frame(Source = NULL, Statistic=NULL,df1=NULL,df2=NULL,Value=NULL,Reported.Comparison=NULL,Reported.P.Value=NULL, Computed = NULL, oneTail = NULL, Location = NULL,stringsAsFactors=FALSE)
  class(Res) <- c("statcheck","data.frame")
  
  if (length(x)==0) return(Res)
  
  if (is.null(names(x))) names(x) <-  1:length(x)
  for (i in 1:length(x))
  {
    
    txt <- x[i]
    
    # t-values:
    if ("t"%in%stat)
    {
      # Get location of t-values in text:
      tLoc <- gregexpr("t\\s?\\(\\s?\\d*\\.?\\d+\\s?\\)\\s?.?\\s?\\-?\\s?\\d*\\.?\\d+\\s?,\\s?(ns|p\\s?.?\\s?\\d?\\.\\d+)",txt,ignore.case=TRUE)[[1]]
      
      if (tLoc[1] != -1)
      {
        # Get raw text of t-values:
        tRaw <- substring(txt,tLoc,tLoc+attr(tLoc,"match.length")-1)
        # Extract location of numbers:
        nums <- gregexpr("(\\-?\\s?\\d*\\.?\\d+)|ns",tRaw)
        
        for (k in 1:length(nums))
        {
          if (length(nums[[k]]) == 5) nums[[k]] <- nums[[k]]%rem%c(2,4)
          if (length(nums[[k]]) == 4) nums[[k]] <- nums[[k]]%rem%2
          if (length(nums[[k]]) != 3) warning(paste("Could not extract statistics properly from",tRaw[k]))
        }
        # Extract df:
        df <- as.numeric(substring(tRaw,sapply(nums,'[',1),sapply(nums,function(x)x[1]+attr(x,"match.length")[1]-1)))
        
        # Extract t-values
        tVals <- as.numeric(substring(tRaw,sapply(nums,'[',2),sapply(nums,function(x)x[2]+attr(x,"match.length")[2]-1)))
        
        # Extract p-values
        pVals <- as.numeric(substring(tRaw,sapply(nums,'[',3),sapply(nums,function(x)x[3]+attr(x,"match.length")[3]-1)))
        
        # Extract (in)equality
        #pEq <- ifelse(grepl("\\=|<",tRaw),substring(tRaw,sapply(gregexpr("\\=|<",tRaw),'[',2),sapply(gregexpr("\\=|<",tRaw),'[',2)),"?")
        eqLoc <- gregexpr("p\\s?.?",tRaw)
        pEq <- substring(tRaw,
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
        
        # Create data frame:
        tRes <- data.frame(Source = names(x)[i], 
                          Statistic="t", 
                           df1= df, 
                           df2=NA, 
                           Value = tVals, 
                           Reported.Comparison= pEq, 
                           Reported.P.Value=pVals, 
                           Computed = pt(-1*abs(tVals),df)*2, 
                           OneTail = ifelse(abs(pVals - pt(tVals,df,lower.tail=TRUE)) < abs(pVals - (pt(tVals,df,lower.tail=FALSE))),pt(tVals,df,lower.tail=TRUE),pt(tVals,df,lower.tail=FALSE)), 
                           Location = tLoc,
                           Raw = tRaw,
                           stringsAsFactors=FALSE)
        
        # Append, clean and close:
        Res <- rbind(Res,tRes)
        rm(tRes)
      }
    }
    
    # F-values:
    if ("F"%in%stat)
    {
      # Get location of t-values in text:
      tLoc <- gregexpr("F\\s?\\(\\s?\\d*\\.?\\d+\\s?,\\s?\\d*\\.?\\d+\\s?\\)\\s?.?\\s?\\d*\\.?\\d+\\s?,\\s?(ns|p)\\s?.?\\s?\\d?\\.\\d+",txt,ignore.case=TRUE)[[1]]
      
      if (tLoc[1] != -1)
      {
        # Get raw text of t-values:
        tRaw <- substring(txt,tLoc,tLoc+attr(tLoc,"match.length")-1)
        
        # Extract location of numbers:
        nums <- gregexpr("(\\d*\\.?\\d+)|ns",tRaw)
        
        for (k in 1:length(nums))
        {
          #if (length(nums[[k]])!=4) browser()
          if (length(nums[[k]]) == 6) nums[[k]] <- nums[[k]]%rem%c(3,5)
          if (length(nums[[k]]) == 5) nums[[k]] <- nums[[k]]%rem%3
          if (length(nums[[k]]) != 4) warning(paste("Could not extract statistics properly from",tRaw[k]))
        }
        
        # Extract df1:
        df1 <- as.numeric(substring(tRaw,sapply(nums,'[',1),sapply(nums,function(x)x[1]+attr(x,"match.length")[1]-1)))
        
        # Extract df2:
        df2 <- as.numeric(substring(tRaw,sapply(nums,'[',2),sapply(nums,function(x)x[2]+attr(x,"match.length")[2]-1)))
        
        # Extract t-values
        FVals <- as.numeric(substring(tRaw,sapply(nums,'[',3),sapply(nums,function(x)x[3]+attr(x,"match.length")[3]-1)))
        
        # Extract p-values
        pVals <- as.numeric(substring(tRaw,sapply(nums,'[',4),sapply(nums,function(x)x[4]+attr(x,"match.length")[4]-1)))
        
        # Extract (in)equality
        #pEq <- ifelse(grepl("\\=|<",tRaw),substring(tRaw,sapply(gregexpr("\\=|<",tRaw),'[',2),sapply(gregexpr("\\=|<",tRaw),'[',2)),"?")
        #pEq <- substring(tRaw,sapply(gregexpr("p",tRaw),'[',1)+1,sapply(nums,'[',4)-1)
        eqLoc <- gregexpr("p\\s?.?",tRaw)
        pEq <- substring(tRaw,
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
        
        # Create data frame:
        tRes <- data.frame(
          Source = names(x)[i], 
          Statistic="F", 
          df1= df1, 
          df2= df2, 
          Value = FVals,  
          Reported.Comparison= pEq, 
          Reported.P.Value=pVals, 
          Computed = pf(FVals,df1,df2,lower.tail=FALSE), 
          OneTail = NA, Location = tLoc,
          Raw = tRaw,
          stringsAsFactors=FALSE)
        
        # Append, clean and close:
        Res <- rbind(Res,tRes)
        rm(tRes)
      }
    }
    
    
    # correlations:
    if (any(c("r","cor","correlations")%in%stat))
    {
      # Get location of t-values in text:
      tLoc <- gregexpr("r\\s?\\(\\s?\\d*\\.?\\d+\\s?\\)\\s?.?\\s?\\-?\\s?\\d*\\.?\\d+\\s?,\\s?(ns|p\\s?.?\\s?\\d?\\.\\d+)",txt,ignore.case=TRUE)[[1]]
      
      if (tLoc[1] != -1)
      {
        # Get raw text of t-values:
        tRaw <- substring(txt,tLoc,tLoc+attr(tLoc,"match.length")-1)
        # Extract location of numbers:
        nums <- gregexpr("(\\-?\\s?\\d*\\.?\\d+)|ns",tRaw)
        
        for (k in 1:length(nums))
        {
          if (length(nums[[k]]) == 5) nums[[k]] <- nums[[k]]%rem%c(2,4)
          if (length(nums[[k]]) == 4) nums[[k]] <- nums[[k]]%rem%2
          if (length(nums[[k]]) != 3) warning(paste("Could not extract statistics properly from",tRaw[k]))
        }
        # Extract df:
        df <- as.numeric(substring(tRaw,sapply(nums,'[',1),sapply(nums,function(x)x[1]+attr(x,"match.length")[1]-1)))
        
        # Extract t-values
        tVals <- as.numeric(substring(tRaw,sapply(nums,'[',2),sapply(nums,function(x)x[2]+attr(x,"match.length")[2]-1)))
        
        # Extract p-values
        pVals <- as.numeric(substring(tRaw,sapply(nums,'[',3),sapply(nums,function(x)x[3]+attr(x,"match.length")[3]-1)))
        
        # Extract (in)equality
        #pEq <- ifelse(grepl("\\=|<",tRaw),substring(tRaw,sapply(gregexpr("\\=|<",tRaw),'[',2),sapply(gregexpr("\\=|<",tRaw),'[',2)),"?")
        eqLoc <- gregexpr("p\\s?.?",tRaw)
        pEq <- substring(tRaw,
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
        
        r2t <- function(r,df)
        {
          r / (sqrt((1-r^2)/df))
        }
        
        # Create data frame:
        tRes <- data.frame(Source = names(x)[i], 
                           Statistic="r", 
                           df1= df, 
                           df2=NA, 
                           Value = tVals, 
                           Reported.Comparison= pEq, 
                           Reported.P.Value=pVals, 
                           Computed = pmin(pt(-1*abs(r2t(tVals,df)),df)*2,1), 
                           OneTail = ifelse(abs(pVals - pt(r2t(tVals,df),df,lower.tail=TRUE)) < abs(pVals - (pt(r2t(tVals,df),df,lower.tail=FALSE))),pt(r2t(tVals,df),df,lower.tail=TRUE),pt(r2t(tVals,df),df,lower.tail=FALSE)), 
                           Location = tLoc,
                           Raw = tRaw,
                           stringsAsFactors=FALSE)
        
        # Append, clean and close:
        Res <- rbind(Res,tRes)
        rm(tRes)
      }
    }
    
    
    # Chis2-values:
    if ("chisq"%in%stat)
    {
      # Get location of not-t-values in text:
      tLoc <- gregexpr("((\\[CHI\\]|\\[DELTA\\]G)2?\\s?|[^(t|r|\\s)]\\s+)\\(\\s?\\d*\\.?\\d+\\s?\\)\\s?.?\\s?\\-?\\s?\\d*\\.?\\d+\\s?,\\s?(ns|p\\s?.?\\s?\\d?\\.\\d+)",txt,perl=TRUE,ignore.case=TRUE)[[1]]
      
      if (tLoc[1] != -1)
      {
        # Get raw text of t-values:
        tRaw <- substring(txt,tLoc,tLoc+attr(tLoc,"match.length")-1)
        # Extract location of numbers:
        nums <- gregexpr("(\\-?\\s?\\d*\\.?\\d+)|ns",tRaw)
        
        for (k in 1:length(nums))
        {
          if (length(nums[[k]]) == 5) nums[[k]] <- nums[[k]]%rem%c(2,4)
          if (length(nums[[k]]) == 4) nums[[k]] <- nums[[k]]%rem%2
          if (length(nums[[k]]) != 3) warning(paste("Could not extract statistics properly from",tRaw[k]))
        }
        # Extract df:
        df <- as.numeric(substring(tRaw,sapply(nums,'[',1),sapply(nums,function(x)x[1]+attr(x,"match.length")[1]-1)))
        
        # Extract t-values
        tVals <- as.numeric(substring(tRaw,sapply(nums,'[',2),sapply(nums,function(x)x[2]+attr(x,"match.length")[2]-1)))
        
        # Extract p-values
        pVals <- as.numeric(substring(tRaw,sapply(nums,'[',3),sapply(nums,function(x)x[3]+attr(x,"match.length")[3]-1)))
        
        # Extract (in)equality
        #pEq <- ifelse(grepl("\\=|<",tRaw),substring(tRaw,sapply(gregexpr("\\=|<",tRaw),'[',2),sapply(gregexpr("\\=|<",tRaw),'[',2)),"?")
        eqLoc <- gregexpr("p\\s?.?",tRaw)
        pEq <- substring(tRaw,
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
        
        # Create data frame:
        tRes <- data.frame(Source = names(x)[i], 
                           Statistic="Chi2", 
                           df1= df, 
                           df2=NA, 
                           Value = tVals, 
                           Reported.Comparison= pEq, 
                           Reported.P.Value=pVals, 
                           Computed = pchisq(tVals,df,lower.tail=FALSE), 
                           OneTail = NA, 
                           Location = tLoc,
                           Raw = tRaw,
                           stringsAsFactors=FALSE)
        
        # Append, clean and close:
        Res <- rbind(Res,tRes)
        rm(tRes)
      }
    }
    
    
    
  }
  Source <- NULL
  Res <- ddply(Res,.(Source),function(x)x[order(x$Location),])
  Res[['Reported.Comparison']] <- gsub("5","=",Res[['Reported.Comparison']])
  Res[['Reported.Comparison']] <- gsub(",","<",Res[['Reported.Comparison']])
  class(Res) <- c("statcheck","data.frame")
  return(Res)
}


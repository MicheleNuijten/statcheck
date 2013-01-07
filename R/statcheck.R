
## Main function, checks statistics of vector of strings (articles).
statcheck <- function(x,stat=c("t","F","cor","chisq")){
  '%rem%'<- function(x,y){
    at <- attr(x,"match.length")
    x <- x[-y]
    at <- at[-y]
    attr(x,"match.length") <- at
    return(x)
  }
  
  # Create empty data frame:
  Res <- data.frame(Source = NULL, Statistic=NULL,df1=NULL,df2=NULL,Value=NULL,Reported.Comparison=NULL,Reported.P.Value=NULL, Computed = NULL, oneTail = NULL, InExactError = NULL, ExactError=NULL, DecisionError=NULL, Location = NULL,stringsAsFactors=FALSE)
  class(Res) <- c("statcheck","data.frame")
  
  if (length(x)==0) return(Res)
  
  if (is.null(names(x))) names(x) <-  1:length(x)
  
  message("Extracting statistics...")
  pb <- txtProgressBar(max=length(x),style=3)
  for (i in 1:length(x)){
    
    txt <- x[i]
    
    # t-values:
    if ("t"%in%stat){
      # Get location of t-values in text:
      tLoc <- gregexpr("t\\s?\\(\\s?\\d*\\.?\\d+\\s?\\)\\s?.?\\s?\\-?\\s?\\d*\\.?\\d+\\s?,\\s?(ns|p\\s?.?\\s?\\d?\\.\\d+)",txt,ignore.case=TRUE)[[1]]
      
      if (tLoc[1] != -1){
        # Get raw text of t-values:
        tRaw <- substring(txt,tLoc,tLoc+attr(tLoc,"match.length")-1)
        # Extract location of numbers:
        nums <- gregexpr("(\\-?\\s?\\d*\\.?\\d+)|ns",tRaw)
        
        for (k in 1:length(nums)){
          if (length(nums[[k]]) == 5) nums[[k]] <- nums[[k]]%rem%c(2,4)
          if (length(nums[[k]]) == 4) nums[[k]] <- nums[[k]]%rem%2
          if (length(nums[[k]]) != 3) warning(paste("Could not extract statistics properly from",tRaw[k]))
        }
        # Extract df:
        df <- as.numeric(substring(tRaw,sapply(nums,'[',1),sapply(nums,function(x)x[1]+attr(x,"match.length")[1]-1)))
        
        # Extract t-values
        tVals <- as.numeric(substring(tRaw,sapply(nums,'[',2),sapply(nums,function(x)x[2]+attr(x,"match.length")[2]-1)))
        
        # Extract p-values
        suppressWarnings(
          pVals <- as.numeric(substring(tRaw,sapply(nums,'[',3),sapply(nums,function(x)x[3]+attr(x,"match.length")[3]-1))))
        # Extract (in)equality
        eqLoc <- gregexpr("p\\s?.?",tRaw)
        pEq <- substring(tRaw,
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
        pEq[grepl("ns",tRaw,ignore.case=TRUE)] <- "ns"
        
        # Create data frame:
        tRes <- data.frame(Source = names(x)[i], 
                           Statistic="t", 
                           df1= df, 
                           df2=NA, 
                           Value = tVals, 
                           Reported.Comparison= pEq, 
                           Reported.P.Value=pVals, 
                           Computed = pt(-1*abs(tVals),df)*2, 
                           Location = tLoc,
                           Raw = tRaw,
                           stringsAsFactors=FALSE)
        
        # Append, clean and close:
        Res <- rbind(Res,tRes)
        rm(tRes)
      }
    }
    
    # F-values:
    if ("F"%in%stat){
      # Get location of F-values in text:
      FLoc <- gregexpr("F\\s?\\(\\s?\\d*\\.?\\d+\\s?,\\s?\\d*\\.?\\d+\\s?\\)\\s?.?\\s?\\d*\\.?\\d+\\s?,\\s?(ns|p)\\s?.?\\s?\\d?\\.\\d+",txt,ignore.case=TRUE)[[1]]
      
      if (FLoc[1] != -1){
        # Get raw text of F-values:
        FRaw <- substring(txt,FLoc,FLoc+attr(FLoc,"match.length")-1)
        
        # Extract location of numbers:
        nums <- gregexpr("(\\d*\\.?\\d+)|ns",FRaw)
        
        for (k in 1:length(nums)){
          if (length(nums[[k]]) == 6) nums[[k]] <- nums[[k]]%rem%c(3,5)
          if (length(nums[[k]]) == 5) nums[[k]] <- nums[[k]]%rem%3
          if (length(nums[[k]]) != 4) warning(paste("Could not extract statistics properly from",FRaw[k]))
        }
        
        # Extract df1:
        df1 <- as.numeric(substring(FRaw,sapply(nums,'[',1),sapply(nums,function(x)x[1]+attr(x,"match.length")[1]-1)))
        
        # Extract df2:
        df2 <- as.numeric(substring(FRaw,sapply(nums,'[',2),sapply(nums,function(x)x[2]+attr(x,"match.length")[2]-1)))
        
        # Extract t-values
        FVals <- as.numeric(substring(FRaw,sapply(nums,'[',3),sapply(nums,function(x)x[3]+attr(x,"match.length")[3]-1)))
        
        # Extract p-values
        suppressWarnings(
        pVals <- as.numeric(substring(FRaw,sapply(nums,'[',4),sapply(nums,function(x)x[4]+attr(x,"match.length")[4]-1))))
                  
        # Extract (in)equality
        eqLoc <- gregexpr("p\\s?.?",FRaw)
        pEq <- substring(FRaw,
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
        pEq[grepl("ns",FRaw,ignore.case=TRUE)] <- "ns"
        
        # Create data frame:
        FRes <- data.frame(
          Source = names(x)[i], 
          Statistic="F", 
          df1= df1, 
          df2= df2, 
          Value = FVals,  
          Reported.Comparison= pEq, 
          Reported.P.Value=pVals, 
          Computed = pf(FVals,df1,df2,lower.tail=FALSE), 
          Location = FLoc,
          Raw = FRaw,
          stringsAsFactors=FALSE)
        
        # Append, clean and close:
        Res <- rbind(Res,FRes)
        rm(FRes)
      }
    }
    
    
    # correlations:
    if (any(c("r","cor","correlations")%in%stat)){
      # Get location of r-values in text:
      rLoc <- gregexpr("r\\s?\\(\\s?\\d*\\.?\\d+\\s?\\)\\s?.?\\s?\\-?\\s?\\d*\\.?\\d+\\s?,\\s?(ns|p\\s?.?\\s?\\d?\\.\\d+)",txt,ignore.case=TRUE)[[1]]
      
      if (rLoc[1] != -1){
        # Get raw text of r-values:
        rRaw <- substring(txt,rLoc,rLoc+attr(rLoc,"match.length")-1)
        # Extract location of numbers:
        nums <- gregexpr("(\\-?\\s?\\d*\\.?\\d+)|ns",rRaw)
        
        for (k in 1:length(nums)){
          if (length(nums[[k]]) == 5) nums[[k]] <- nums[[k]]%rem%c(2,4)
          if (length(nums[[k]]) == 4) nums[[k]] <- nums[[k]]%rem%2
          if (length(nums[[k]]) != 3) warning(paste("Could not extract statistics properly from",rRaw[k]))
        }
        # Extract df:
        df <- as.numeric(substring(rRaw,sapply(nums,'[',1),sapply(nums,function(x)x[1]+attr(x,"match.length")[1]-1)))
        
        # Extract r-values
        rVals <- as.numeric(substring(rRaw,sapply(nums,'[',2),sapply(nums,function(x)x[2]+attr(x,"match.length")[2]-1)))
        
        # Extract p-values
        suppressWarnings(
        pVals <- as.numeric(substring(rRaw,sapply(nums,'[',3),sapply(nums,function(x)x[3]+attr(x,"match.length")[3]-1))))
                  
        # Extract (in)equality
        eqLoc <- gregexpr("p\\s?.?",rRaw)
        pEq <- substring(rRaw,
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
        pEq[grepl("ns",rRaw,ignore.case=TRUE)] <- "ns"
        
        r2t <- function(r,df){
          r / (sqrt((1-r^2)/df))
        }
        
        # Create data frame:
        rRes <- data.frame(Source = names(x)[i], 
                           Statistic="r", 
                           df1= df, 
                           df2=NA, 
                           Value = rVals, 
                           Reported.Comparison= pEq, 
                           Reported.P.Value=pVals, 
                           Computed = pmin(pt(-1*abs(r2t(rVals,df)),df)*2,1), 
                           Location = rLoc,
                           Raw = rRaw,
                           stringsAsFactors=FALSE)
        
        # Append, clean and close:
        Res <- rbind(Res,rRes)
        rm(rRes)
      }
    }
    
    
    # Chis2-values:
    if ("chisq"%in%stat){
      # Get location of not-t-values in text: (?)
      chi2Loc <- gregexpr("((\\[CHI\\]|\\[DELTA\\]G)2?\\s?|[^(t|r|\\s)]\\s+)\\(\\s?\\d*\\.?\\d+\\s?\\)\\s?.?\\s?\\-?\\s?\\d*\\.?\\d+\\s?,\\s?(ns|p\\s?.?\\s?\\d?\\.\\d+)",txt,perl=TRUE,ignore.case=TRUE)[[1]]
      
      if (chi2Loc[1] != -1){
        # Get raw text of chi2-values:
        chi2Raw <- substring(txt,chi2Loc,chi2Loc+attr(chi2Loc,"match.length")-1)
        substr(chi2Raw,1,1)[grepl("\\d",substr(chi2Raw,1,1))] <- " "
        # Extract location of numbers:
        nums <- gregexpr("(\\-?\\s?\\d*\\.?\\d+)|ns",chi2Raw)
        
        for (k in 1:length(nums)){
          if (length(nums[[k]]) == 5) nums[[k]] <- nums[[k]]%rem%c(2,4)
          if (length(nums[[k]]) == 4) nums[[k]] <- nums[[k]]%rem%2
          if (length(nums[[k]]) != 3) warning(paste("Could not extract statistics properly from",chi2Raw[k]))
        }
        # Extract df:
        df <- as.numeric(substring(chi2Raw,sapply(nums,'[',1),sapply(nums,function(x)x[1]+attr(x,"match.length")[1]-1)))
        
        # Extract t-values
        chi2Vals <- as.numeric(substring(chi2Raw,sapply(nums,'[',2),sapply(nums,function(x)x[2]+attr(x,"match.length")[2]-1)))
        
        # Extract p-values
        suppressWarnings(
        pVals <- as.numeric(substring(chi2Raw,sapply(nums,'[',3),sapply(nums,function(x)x[3]+attr(x,"match.length")[3]-1))))
                  
        # Extract (in)equality
        eqLoc <- gregexpr("p\\s?.?",chi2Raw)
        pEq <- substring(chi2Raw,
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
        pEq[grepl("ns",chi2Raw,ignore.case=TRUE)] <- "ns"
        
        # Create data frame:
        chi2Res <- data.frame(Source = names(x)[i], 
                           Statistic="Chi2", 
                           df1= df, 
                           df2=NA, 
                           Value = chi2Vals, 
                           Reported.Comparison= pEq, 
                           Reported.P.Value=pVals, 
                           Computed = pchisq(chi2Vals,df,lower.tail=FALSE), 
                           Location = chi2Loc,
                           Raw = chi2Raw,
                           stringsAsFactors=FALSE)
        
        # Append, clean and close:
        Res <- rbind(Res,chi2Res)
        rm(chi2Res)
      }
    }
    
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  Source <- NULL
  Res <- ddply(Res,.(Source),function(x)x[order(x$Location),])
  Res[['Reported.Comparison']] <- gsub("5","=",Res[['Reported.Comparison']])
  Res[['Reported.Comparison']] <- gsub(",","<",Res[['Reported.Comparison']])
  
  InExTest <- function(x){
    computed <- x$Computed
    comparison <- x$Reported.Comparison
    reported <- x$Reported.P.Value
    Match <- paste(computed,comparison,reported)
    InExTests <- grepl("<|>",Match)
    if (any(InExTests)){
      InExTests[grepl("<|>",Match)] <- sapply(Match[grepl("<|>",Match)],function(m)!eval(parse(text=m)))
    }
    
    return(InExTests)
  }
  
  ExTest <- function(x){
    computed <- x$Computed
    comparison <- x$Reported.Comparison
    reported <- x$Reported.P.Value
    Match <- paste(computed,comparison,reported)
    ExTests <- grepl("=",Match)
    if (any(ExTests)){
      ExTests[grepl("=",Match)] <- !(round(computed[ExTests],3)==reported[ExTests]|round(computed[ExTests],2)==round(reported[ExTests],2))
    }
    return(ExTests)
  }
  
  GrossTest <- function(x){
    computed <- x$Computed
    comparison <- x$Reported.Comparison
    reported <- x$Reported.P.Value
    Match <- paste(computed,comparison,reported)
    AllTests <- grepl("=|<|>",Match)
    if (any(AllTests)){
      AllTests[grepl("<",Match)] <- reported[grepl("<",Match)]<=.05 & computed[grepl("<",Match)] >=.05
      AllTests[grepl(">",Match)] <- reported[grepl(">",Match)] >=.05 & computed[grepl(">",Match)]<.05
      AllTests[grepl("=",Match)] <- (reported[grepl("=",Match)]<.05 & computed[grepl("=",Match)]>=.5)|
      (reported[grepl("=",Match)]>=.05 & computed[grepl("=",Match)]<.05)
    }
      AllTests <- as.logical(AllTests)
    
    return(AllTests)
  }
    
  Res$InExactError <- InExTest(Res)
  Res$ExactError <- ExTest(Res)
  Res$DecisionError <- GrossTest(Res)
  
  class(Res) <- c("statcheck","data.frame")
  return(Res[,!(names(Res)%in%"Location")])
}


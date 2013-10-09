
## Main function, checks statistics of vector of strings (articles).
statcheck <- function(x,stat=c("t","F","cor","chisq","Z","Wald"),OneTailedTests=FALSE,alpha=.05,OneTailedTxt=FALSE){
  '%rem%'<- function(x,y){
    at <- attr(x,"match.length")
    x <- x[-y]
    at <- at[-y]
    attr(x,"match.length") <- at
    return(x)
  }
  
  # Create empty data frame:
  Res <- data.frame(Source = NULL, Statistic=NULL,df1=NULL,df2=NULL,Test.Comparison=NULL,Value=NULL,Reported.Comparison=NULL,Reported.P.Value=NULL, Computed = NULL, Error = NULL, InExactError = NULL, ExactError=NULL, DecisionError=NULL, CopyPaste=NULL, Location = NULL,stringsAsFactors=FALSE,dec=NULL,testdec=NULL,OneTail=NULL,OneTailedInTxt=NULL)
  class(Res) <- c("statcheck","data.frame")
  OneTailedInTxt <- NULL
  
  if (length(x)==0) return(Res)
  
  if (is.null(names(x))) names(x) <-  1:length(x)
  
  message("Extracting statistics...")
  pb <- txtProgressBar(max=length(x),style=3)
  for (i in 1:length(x)){
    
    txt <- x[i]
    
    #---------------------------
    
    # search for "sided"/"tailed"/"directional" in full text to detect one-sided testing
    
    onesided <- gregexpr("sided|tailed|directional",txt,ignore.case=TRUE)[[1]]
    
    if(onesided[1] != -1){
      onesided <- 1
    } else {
      onesided <- 0
    }
    
    OneTailedInTxt <- as.logical(onesided)
    
    #---------------------------
    
    # t-values:
    if ("t"%in%stat){
      # Get location of t-values in text:
      tLoc <- gregexpr("t\\s?\\(\\s?\\d*\\.?\\d+\\s?\\)\\s?.?\\s?\\D{0,3}\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(ns|p\\s?.?\\s?\\d?\\.?\\d+)",txt,ignore.case=TRUE)[[1]]
      
      if (tLoc[1] != -1){
        # Get raw text of t-values:
        tRaw <- substring(txt,tLoc,tLoc+attr(tLoc,"match.length")-1)
        
        # remove commas (thousands separators)
        tRaw <- gsub("(?<=\\d),(?=\\d+)","",tRaw,perl=TRUE)
        
        # Replace weird codings of a minus sign with actual minus sign:
        # First remove spaces
        tRaw <- gsub("(?<=\\=)\\s+(?=.*\\,)","",tRaw,perl=TRUE)
        
        # Replace any weird string with a minus sign
        tRaw <- gsub("(?<=\\=)\\s?[^\\d\\.]+(?=.*\\,)"," -",tRaw,perl=TRUE)
        
        # Add spaces again:
        tRaw <- gsub("(?<=\\=)(?=(\\.|\\d))"," ",tRaw,perl=TRUE)  
        
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
        suppressWarnings(
          tValsChar <- substring(tRaw,sapply(nums,'[',2),sapply(nums,function(x)x[2]+attr(x,"match.length")[2]-1)))
        
        suppressWarnings(
          tVals <- as.numeric(tValsChar))
        
        # Extract number of decimals test statistic
        testdec <- attr(regexpr("\\.\\d+",tValsChar),"match.length")-1
        testdec[testdec<0] <- 0
        
        # Extract (in)equality test statistic
        testEqLoc <- gregexpr("\\)\\s?.?",tRaw)
        testEq <- substring(tRaw,
                            sapply(testEqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                            sapply(testEqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
        
        
        # Extract p-values
        suppressWarnings(
          pValsChar <- substring(tRaw,sapply(nums,'[',3),sapply(nums,function(x)x[3]+attr(x,"match.length")[3]-1)))
        
        suppressWarnings(
          pVals <- as.numeric(pValsChar))
        
        # Extract (in)equality
        eqLoc <- gregexpr("p\\s?.?",tRaw)
        pEq <- substring(tRaw,
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
        pEq[grepl("ns",tRaw,ignore.case=TRUE)] <- "ns"
        
        # determine number of decimals of p value
        dec <- attr(regexpr("\\.\\d+",pValsChar),"match.length")-1
        dec[dec<0] <- 0
        
        # Create data frame:
        tRes <- data.frame(Source = names(x)[i], 
                           Statistic="t", 
                           df1= df, 
                           df2=NA,
                           Test.Comparison=testEq,
                           Value = tVals, 
                           Reported.Comparison= pEq, 
                           Reported.P.Value=pVals, 
                           Computed = pt(-1*abs(tVals),df)*2, 
                           Location = tLoc,
                           Raw = tRaw,
                           stringsAsFactors=FALSE,
                           dec = dec,
                           testdec=testdec,
                           OneTailedInTxt=OneTailedInTxt)
        
        # Append, clean and close:
        Res <- rbind(Res,tRes)
        rm(tRes)
      }
    }
    
    # F-values:
    if ("F"%in%stat){
      # Get location of F-values in text:
      FLoc <- gregexpr("F\\s?\\(\\s?\\d*\\.?\\d+\\s?,\\s?\\d*\\.?\\d+\\s?\\)\\s?.?\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(ns|p\\s?.?\\s?\\d?\\.\\d+)",txt,ignore.case=TRUE)[[1]]
      
      if (FLoc[1] != -1){
        # Get raw text of F-values:
        FRaw <- substring(txt,FLoc,FLoc+attr(FLoc,"match.length")-1)
        
        # remove commas (thousands separators)
        FRaw <- gsub("(?<=\\d),(?=\\d+\\.)","",FRaw,perl=TRUE)
        
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
        
        # Extract F-values
        suppressWarnings(
          FValsChar <- substring(FRaw,sapply(nums,'[',3),sapply(nums,function(x)x[3]+attr(x,"match.length")[3]-1)))
        
        suppressWarnings(
          FVals <- as.numeric(FValsChar))
        
        # Extract number of decimals test statistic
        testdec <- attr(regexpr("\\.\\d+",FValsChar),"match.length")-1
        testdec[testdec<0] <- 0
        
        # Extract (in)equality test statistic
        testEqLoc <- gregexpr("\\)\\s?.?",FRaw)
        testEq <- substring(FRaw,
                            sapply(testEqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                            sapply(testEqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
        
        
        # Extract p-values
        suppressWarnings(
          pValsChar <- substring(FRaw,sapply(nums,'[',4),sapply(nums,function(x)x[4]+attr(x,"match.length")[4]-1)))
        
        suppressWarnings(
          pVals <- as.numeric(pValsChar))
        
        # Extract (in)equality
        eqLoc <- gregexpr("p\\s?.?",FRaw)
        pEq <- substring(FRaw,
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
        pEq[grepl("ns",FRaw,ignore.case=TRUE)] <- "ns"
        
        # determine number of decimals of p value
        dec <- attr(regexpr("\\.\\d+",pValsChar),"match.length")-1
        dec[dec<0] <- NA
        
        # Create data frame:
        FRes <- data.frame(Source = names(x)[i], 
                           Statistic="F", 
                           df1= df1, 
                           df2= df2,
                           Test.Comparison=testEq,
                           Value = FVals,  
                           Reported.Comparison= pEq, 
                           Reported.P.Value=pVals, 
                           Computed = pf(FVals,df1,df2,lower.tail=FALSE), 
                           Location = FLoc,
                           Raw = FRaw,
                           stringsAsFactors=FALSE,
                           dec=dec,
                           testdec=testdec,
                           OneTailedInTxt=OneTailedInTxt)
        
        # Append, clean and close:
        Res <- rbind(Res,FRes)
        rm(FRes)
      }
    }
    
    
    # correlations:
    if (any(c("r","cor","correlations")%in%stat)){
      # Get location of r-values in text:
      rLoc <- gregexpr("r\\s?\\(\\s?\\d*\\.?\\d+\\s?\\)\\s?.?\\s?\\D{0,3}\\s?\\d*\\.?\\d+\\s?,\\s?(ns|p\\s?.?\\s?\\d?\\.\\d+)",txt,ignore.case=TRUE)[[1]]
      
      if (rLoc[1] != -1){
        # Get raw text of r-values:
        rRaw <- substring(txt,rLoc,rLoc+attr(rLoc,"match.length")-1)
        
        # Replace weird codings of a minus sign with actual minus sign:
        # First remove spaces
        rRaw <- gsub("(?<=\\=)\\s+(?=.*\\,)","",rRaw,perl=TRUE)
        
        # Replace any weird string with a minus sign
        rRaw <- gsub("(?<=\\=)\\s?[^\\d\\.]+(?=.*\\,)"," -",rRaw,perl=TRUE)
        
        # Add spaces again:
        rRaw <- gsub("(?<=\\=)(?=(\\.|\\d))"," ",rRaw,perl=TRUE) 
        
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
        suppressWarnings(
          rValsChar <- substring(rRaw,sapply(nums,'[',2),sapply(nums,function(x)x[2]+attr(x,"match.length")[2]-1)))
        
        suppressWarnings(
          rVals <- as.numeric(rValsChar))
        
        # Extract number of decimals test statistic
        testdec <- attr(regexpr("\\.\\d+",rValsChar),"match.length")-1
        testdec[testdec<0] <- 0
        
        
        # Extract (in)equality test statistic
        testEqLoc <- gregexpr("\\)\\s?.?",rRaw)
        testEq <- substring(rRaw,
                            sapply(testEqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                            sapply(testEqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
        
        
        # Extract p-values
        suppressWarnings(
          pValsChar <- substring(rRaw,sapply(nums,'[',3),sapply(nums,function(x)x[3]+attr(x,"match.length")[3]-1)))
        
        suppressWarnings(
          pVals <- as.numeric(pValsChar))
        
        # Extract (in)equality
        eqLoc <- gregexpr("p\\s?.?",rRaw)
        pEq <- substring(rRaw,
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
        pEq[grepl("ns",rRaw,ignore.case=TRUE)] <- "ns"
        
        r2t <- function(r,df){
          r / (sqrt((1-r^2)/df))
        }
        
        
        # determine number of decimals of p value
        dec <- attr(regexpr("\\.\\d+",pValsChar),"match.length")-1
        dec[dec<0] <- 0
        
        # Create data frame:
        rRes <- data.frame(Source = names(x)[i], 
                           Statistic="r", 
                           df1= df, 
                           df2=NA,
                           Test.Comparison=testEq,
                           Value = rVals, 
                           Reported.Comparison= pEq, 
                           Reported.P.Value=pVals, 
                           Computed = pmin(pt(-1*abs(r2t(rVals,df)),df)*2,1), 
                           Location = rLoc,
                           Raw = rRaw,
                           stringsAsFactors=FALSE,
                           dec=dec,
                           testdec=testdec,
                           OneTailedInTxt=OneTailedInTxt)
        
        # Append, clean and close:
        Res <- rbind(Res,rRes)
        rm(rRes)
      }
    }
    
    # z-values:
    if ("Z"%in%stat){
      # Get location of z-values in text:
      zLoc <- gregexpr("\\s(\\z|\\Z)\\s?.?\\s?\\D{0,3}\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(ns|p\\s?.?\\s?\\d?\\.\\d+)",txt,ignore.case=TRUE)[[1]]
      
      if (zLoc[1] != -1){
        # Get raw text of z-values:
        zRaw <- substring(txt,zLoc,zLoc+attr(zLoc,"match.length")-1)
        
        # remove commas (thousands separators)
        zRaw <- gsub("(?<=\\d),(?=\\d+\\.)","",zRaw,perl=TRUE)
        
        # Replace weird codings of a minus sign with actual minus sign:
        # First remove spaces
        zRaw <- gsub("(?<=\\=)\\s+(?=.*\\,)","",zRaw,perl=TRUE)
        
        # Replace any weird string with a minus sign
        zRaw <- gsub("(?<=\\=)\\s?[^\\d\\.]+(?=.*\\,)"," -",zRaw,perl=TRUE)
        
        # Add spaces again:
        zRaw <- gsub("(?<=\\=)(?=(\\.|\\d))"," ",zRaw,perl=TRUE) 
        
        # Extract location of numbers:
        nums <- gregexpr("(\\-?\\s?\\d*\\.?\\d+)|ns",zRaw)
        
        for (k in 1:length(nums)){
          if (length(nums[[k]]) == 4) nums[[k]] <- nums[[k]]%rem%c(2,4)
          if (length(nums[[k]]) == 3) nums[[k]] <- nums[[k]]%rem%2
          if (length(nums[[k]]) != 2) warning(paste("Could not extract statistics properly from",zRaw[k]))
        }
        # Extract z-values
        suppressWarnings(
          zValsChar <- substring(zRaw,sapply(nums,'[',1),sapply(nums,function(x)x[1]+attr(x,"match.length")[1]-1)))
        
        suppressWarnings(
          zVals <- as.numeric(zValsChar))
        
        # Extract number of decimals test statistic
        testdec <- attr(regexpr("\\.\\d+",zValsChar),"match.length")-1
        testdec[testdec<0] <- 0
        
        # Extract (in)equality test statistic
        testEqLoc <- gregexpr("(z|Z|z'|Z')\\s?.?",zRaw)
        testEq <- substring(zRaw,
                            sapply(testEqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                            sapply(testEqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
        
        
        # Extract p-values
        suppressWarnings(
          pValsChar <- substring(zRaw,sapply(nums,'[',2),sapply(nums,function(x)x[2]+attr(x,"match.length")[2]-1)))
        
        suppressWarnings(
          pVals <- as.numeric(pValsChar))
        
        # Extract (in)equality
        eqLoc <- gregexpr("p\\s?.?",zRaw)
        pEq <- substring(zRaw,
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
        pEq[grepl("ns",zRaw,ignore.case=TRUE)] <- "ns"
        
        # determine number of decimals of p value
        dec <- attr(regexpr("\\.\\d+",pValsChar),"match.length")-1
        dec[dec<0] <- 0
        
        # Create data frame:
        zRes <- data.frame(Source = names(x)[i], 
                           Statistic="Z", 
                           df1= NA, 
                           df2=NA,
                           Test.Comparison=testEq,
                           Value = zVals, 
                           Reported.Comparison= pEq, 
                           Reported.P.Value=pVals, 
                           Computed = pnorm(abs(zVals),lower.tail=FALSE)*2, 
                           Location = zLoc,
                           Raw = zRaw,
                           stringsAsFactors=FALSE,
                           dec=dec,
                           testdec=testdec,
                           OneTailedInTxt=OneTailedInTxt)
        
        # Append, clean and close:
        Res <- rbind(Res,zRes)
        rm(zRes)
      }
    }
    
    # Wald test results
    if ("Wald"%in%stat){
      # Get location of Wald results in text:
      wLoc <- gregexpr("\\s(\\wald|\\Wald)\\s?\\D?\\s?\\D{0,3}\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(ns|p\\s?.?\\s?\\d?\\.\\d+)",txt,ignore.case=TRUE)[[1]]
      
      if (wLoc[1] != -1){
        # Get raw text of Wald results:
        wRaw <- substring(txt,wLoc,wLoc+attr(wLoc,"match.length")-1)
        
        # remove commas (thousands separators)
        wRaw <- gsub("(?<=\\d),(?=\\d+\\.)","",wRaw,perl=TRUE)
        
        # Replace weird codings of a minus sign with actual minus sign:
        # First remove spaces
        wRaw <- gsub("(?<=\\=)\\s+(?=.*\\,)","",wRaw,perl=TRUE)
        
        # Replace any weird string with a minus sign
        wRaw <- gsub("(?<=\\=)\\s?[^\\d\\.]+(?=.*\\,)"," -",wRaw,perl=TRUE)
        
        # Add spaces again:
        wRaw <- gsub("(?<=\\=)(?=(\\.|\\d))"," ",wRaw,perl=TRUE) 
        
        # Extract location of numbers:
        nums <- gregexpr("(\\-?\\s?\\d*\\.?\\d+)|ns",wRaw)
        
        for (k in 1:length(nums)){
          if (length(nums[[k]]) == 4) nums[[k]] <- nums[[k]]%rem%c(1,3)
          if (length(nums[[k]]) == 3) nums[[k]] <- nums[[k]]%rem%3
          if (length(nums[[k]]) != 2) warning(paste("Could not extract statistics properly from",wRaw[k]))
        }
        # Extract test statistic (Z or chisq2)
        suppressWarnings(
          wValsChar <- substring(wRaw,sapply(nums,'[',1),sapply(nums,function(x)x[1]+attr(x,"match.length")[1]-1)))
        
        suppressWarnings(
          wVals <- as.numeric(wValsChar))
        
        # Extract number of decimals test statistic
        testdec <- attr(regexpr("\\.\\d+",wValsChar),"match.length")-1
        testdec[testdec<0] <- 0
        
        # Extract (in)equality test statistic
        testEqLoc <- gregexpr("(Wald|wald)\\s?.?",wRaw)
        testEq <- substring(wRaw,
                            sapply(testEqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                            sapply(testEqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
        
        
        # Extract p-values
        suppressWarnings(
          pValsChar <- substring(wRaw,sapply(nums,'[',2),sapply(nums,function(x)x[2]+attr(x,"match.length")[2]-1)))
        
        suppressWarnings(
          pVals <- as.numeric(pValsChar))
        
        # Extract (in)equality
        eqLoc <- gregexpr("p\\s?.?",wRaw)
        pEq <- substring(wRaw,
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
        pEq[grepl("ns",wRaw,ignore.case=TRUE)] <- "ns"
        
        # recompute p-values and decide whether a Z or chisq test is more appropriate
        comp <- numeric()
        df <- numeric()
        
        
        for(j in 1:length(wVals)){
          if(!(is.na(pVals[j]))){
            
            pZ <- pnorm(wVals[j],lower.tail=FALSE)*2
            pChisq <- pchisq(wVals[j],1,lower.tail=FALSE)
            
            if(abs(pVals[j]-pZ)<abs(pVals[j]-pChisq)){
              comp[j] <- pZ
              df[j] <- NA
              
            } else{
              comp[j] <- pChisq
              df[j] <- 1
            }
          } else {
            comp[j] <- NA
            df[j] <- NA
          }
        }
        
        # determine number of decimals of p value
        dec <- attr(regexpr("\\.\\d+",pValsChar),"match.length")-1
        dec[dec<0] <- 0
        
        # Create data frame:
        wRes <- data.frame(Source = names(x)[i], 
                           Statistic="Wald", 
                           df1= df, 
                           df2=NA,
                           Test.Comparison=testEq,
                           Value = wVals, 
                           Reported.Comparison= pEq, 
                           Reported.P.Value=pVals, 
                           Computed = comp, 
                           Location = wLoc,
                           Raw = wRaw,
                           stringsAsFactors=FALSE,
                           dec=dec,
                           testdec=testdec,
                           OneTailedInTxt=OneTailedInTxt)
        
        # Append, clean and close:
        Res <- rbind(Res,wRes)
        rm(wRes)
      }
    }
    
    # Chis2-values:
    if ("chisq"%in%stat){
      # Get location of not-t-values in text: (?)
      chi2Loc <- gregexpr("((\\[CHI\\]|\\[DELTA\\]G)2?\\s?|[^(t|r|\\s)]\\s+)\\(\\s?\\d*\\.?\\d+\\s?\\)\\s?.?\\s?\\s?\\d*\\,?\\d*\\.?\\d+\\s?,\\s?(ns|p\\s?.?\\s?\\d?\\.\\d+)",txt,perl=TRUE,ignore.case=TRUE)[[1]]
      
      if (chi2Loc[1] != -1){
        # Get raw text of chi2-values:
        chi2Raw <- substring(txt,chi2Loc,chi2Loc+attr(chi2Loc,"match.length")-1)
        substr(chi2Raw,1,1)[grepl("\\d",substr(chi2Raw,1,1))] <- " "
        
        # remove commas (thousands separators)
        chi2Raw <- gsub("(?<=\\d),(?=\\d+\\.)","",chi2Raw,perl=TRUE)
        
        # Extract location of numbers:
        nums <- gregexpr("(\\-?\\s?\\d*\\.?\\d+)|ns",sub("^.*?\\(","",chi2Raw))
        
        for (k in 1:length(nums)){
          if (length(nums[[k]]) == 5) nums[[k]] <- nums[[k]]%rem%c(2,4)
          if (length(nums[[k]]) == 4) nums[[k]] <- nums[[k]]%rem%2
          if (length(nums[[k]]) != 3) warning(paste("Could not extract statistics properly from",chi2Raw[k]))
        }
        # Extract df:
        df <- as.numeric(substring(sub("^.*?\\(","",chi2Raw),sapply(nums,'[',1),sapply(nums,function(x)x[1]+attr(x,"match.length")[1]-1)))
        
        # Extract chi2-values
        suppressWarnings(
          chi2ValsChar <- substring(sub("^.*?\\(","",chi2Raw),sapply(nums,'[',2),sapply(nums,function(x)x[2]+attr(x,"match.length")[2]-1)))
        
        suppressWarnings(
          chi2Vals <- as.numeric(chi2ValsChar))
        
        # Extract number of decimals test statistic
        testdec <- attr(regexpr("\\.\\d+",chi2ValsChar),"match.length")-1
        testdec[testdec<0] <- 0
        
        # Extract (in)equality test statistic
        testEqLoc <- gregexpr("\\)\\s?.?",chi2Raw)
        testEq <- substring(chi2Raw,
                            sapply(testEqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                            sapply(testEqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
        
        
        # Extract p-values
        suppressWarnings(
          pValsChar <- substring(sub("^.*?\\(","",chi2Raw),sapply(nums,'[',3),sapply(nums,function(x)x[3]+attr(x,"match.length")[3]-1)))
        
        suppressWarnings(
          pVals <- as.numeric(pValsChar))
        
        # Extract (in)equality
        eqLoc <- gregexpr("p\\s?.?",chi2Raw)
        pEq <- substring(chi2Raw,
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
        pEq[grepl("ns",chi2Raw,ignore.case=TRUE)] <- "ns"
        
        # determine number of decimals of p value
        dec <- attr(regexpr("\\.\\d+",pValsChar),"match.length")-1
        dec[dec<0] <- 0
        
        # Create data frame:
        chi2Res <- data.frame(Source = names(x)[i], 
                              Statistic="Chi2", 
                              df1= df, 
                              df2=NA,
                              Test.Comparison=testEq,
                              Value = chi2Vals, 
                              Reported.Comparison= pEq, 
                              Reported.P.Value=pVals, 
                              Computed = pchisq(chi2Vals,df,lower.tail=FALSE), 
                              Location = chi2Loc,
                              Raw = chi2Raw,
                              stringsAsFactors=FALSE,
                              dec=dec,
                              testdec=testdec,
                              OneTailedInTxt=OneTailedInTxt)
        
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
  
  Res <- cbind(Res,OneTailedInTxt)
  
  ###---------------------------------------------------------------------
  
  InExTest <- function(x,...){
    
    computed <- x$Computed
    comparison <- x$Reported.Comparison
    reported <- x$Reported.P.Value
    testcomp <- x$Test.Comparison
    
    # replace 'ns' for > alpha
    reported[comparison=="ns"] <- alpha
    comparison[comparison=="ns"] <- ">"
    
    Match <- paste(computed,comparison,reported)
    InExTests <- grepl("<|>",Match)
    
    if (any(InExTests)){
      InExTests[grepl("<|>",Match)] <- sapply(Match[grepl("<|>",Match)],function(m)!eval(parse(text=m)))
      
      smallsmall <- x$Test.Comparison=="<" & grepl("<",Match)
      smallgreat <- x$Test.Comparison=="<" & grepl(">",Match)
      greatsmall <- x$Test.Comparison==">" & grepl("<",Match)
      greatgreat <- x$Test.Comparison==">" & grepl(">",Match)
      
      if(any(smallsmall)){
        InExTests[smallsmall] <- !(round(computed[smallsmall],x$dec[smallsmall])<=round(reported[smallsmall],x$dec[smallsmall]))
      }
      
      InExTests[smallgreat] <- FALSE
      InExTests[greatsmall] <- FALSE
      
      if(any(greatgreat)){
        InExTests[greatgreat] <- !(round(computed[greatgreat],x$dec[greatgreat])>=round(reported[greatgreat],x$dec[greatgreat]))
      }
      
    }
    
    return(InExTests)
  }
  
  ExTest <- function(x){
    computed <- x$Computed
    reported <- x$Reported.P.Value
    comparison <- x$Reported.Comparison
    ExTests <- comparison=="="
    if (any(ExTests)){
      ExTests[comparison=="="] <- !(round(computed[ExTests],x$dec[ExTests])==round(reported[ExTests],x$dec[ExTests]))
      
      smallequal <- x$Test.Comparison=="<" & comparison=="="
      greatequal <- x$Test.Comparison==">" & comparison=="="
      
      if(any(smallequal)){
        ExTests[smallequal] <- !(round(computed[smallequal],x$dec[smallequal])<round(reported[smallequal],x$dec[smallequal]))
      }
      
      if(any(greatequal)){
        ExTests[greatequal] <- !(round(computed[greatequal],x$dec[greatequal])>round(reported[greatequal],x$dec[greatequal]))
      }
      
    }
    return(ExTests)
  }
  
  GrossTest <- function(x,...){
    computed <- x$Computed
    comparison <- x$Reported.Comparison
    reported <- x$Reported.P.Value
    
    # replace 'ns' by > alpha
    reported[comparison=="ns"] <- alpha
    comparison[comparison=="ns"] <- ">"
    
    Match <- paste(computed,comparison,reported)
    AllTests <- grepl("=|<|>",Match)
    if (any(AllTests)){
      AllTests[grepl("<",Match)] <- reported[grepl("<",Match)]<=alpha & computed[grepl("<",Match)] >=alpha
      AllTests[grepl(">",Match)] <- reported[grepl(">",Match)] >=alpha & computed[grepl(">",Match)]<alpha
      AllTests[grepl("=",Match)] <- (reported[grepl("=",Match)]<alpha & computed[grepl("=",Match)]>=alpha)|
        (reported[grepl("=",Match)]>=alpha & computed[grepl("=",Match)]<alpha)
    }
    AllTests <- as.logical(AllTests)
    
    return(AllTests)
  }
  
  if(OneTailedTests==TRUE){
    Res$Computed <- Res$Computed/2
  } 
  
  Res$InExactError <- InExTest(Res)
  Res$ExactError <- ExTest(Res)
  
  Error <- !(Res$InExactError==FALSE & Res$ExactError==FALSE)
  
  # p values smaller or equal to zero and p values larger than one are errors
  ImpossibleP <- (Res$Reported.P.Value<=0|Res$Reported.P.Value>1)
  Error[ImpossibleP] <- TRUE
  
  Res$Error <- Error
  Res$DecisionError <- GrossTest(Res)  
  
  ###---------------------------------------------------------------------
  
  # check if there would also be a decision error if alpha=.01 or .1
  DecisionErrorAlphas <- logical()
  alphas <- c(.01,.1)
  
  for(a in alphas){
    alpha <- a
    DecisionErrorAlphas <- c(DecisionErrorAlphas,GrossTest(Res))
  }
  
  if(any(DecisionErrorAlphas)){
    cat("\n Check the significance level. \n \n Some of the p value incongruencies are decision errors if the significance level is .1 or .01 instead of the conventional .05. It is recommended to check the actual significance level in the paper or text. Check if the reported p values are a decision error at a different significane level by running statcheck again with 'alpha' set to .1 and/or .01. \n ",fill=TRUE)
  }
  
  ###---------------------------------------------------------------------
  
  if(OneTailedTests==FALSE){
    
    # check if there could be one-sided tests in the data set
    
    computed <- Res$Computed
    comparison <- Res$Reported.Comparison
    reported <- Res$Reported.P.Value
    raw <- Res$Raw
    onetail <- computed/2
    
    OneTail <- ifelse(!(Res$InExactError==FALSE & Res$ExactError==FALSE) &
                        (grepl("=",comparison)[!is.na(onetail)] & round(reported,2)==round(onetail,2))
                      | (grepl("<",comparison) & reported==.05 & onetail < reported & computed > reported)[!is.na(onetail)],
                      TRUE,FALSE)
    Res$OneTail <- OneTail
    
    if(any(OneTail==TRUE & OneTailedTxt==FALSE)){
      cat("\n Check for one tailed tests. \n \n Some of the p value incongruencies might in fact be one tailed tests. It is recommended to check this in the actual paper or text. Check if the p values would also be incongruent if the test is indeed one sided by running statcheck again with 'OneTailedTests' set to TRUE. To see which Sources probably contain a one tailed test, try unique(x$Source[x$OneTail]) (where x is the statcheck output). \n ",fill=TRUE)
    }
    
  }
  
  ###---------------------------------------------------------------------
  
  # count errors as correct if they'd be correct one-sided
  
  if(OneTailedTxt==TRUE){
    Res$Error[Res$OneTailedInTxt==TRUE & Res$OneTail==TRUE] <- FALSE
    Res$DecisionError[Res$OneTailedInTxt==TRUE & Res$OneTail==TRUE] <- FALSE
  }
  
  ###---------------------------------------------------------------------
  
  # copy paste errors
  # same string of results elsewhere in article?
  CopyPaste <- numeric()
  for (i in 1:length(Res$Raw)){
    Res_new <- Res[-i,]
    CopyPaste[i] <- Res$Raw[i]%in%Res_new$Raw[Res_new$Source==Res_new$Source[i]]
  }
  CopyPaste <- as.logical(CopyPaste)
  
  Res$CopyPaste <- CopyPaste
  
  ###---------------------------------------------------------------------
  
  # "correct" rounding differences
  # e.g. t=2.3 could be 2.25 to 2.34 with its range of p values
  correct_round <- numeric()
  
  lower <- Res$Value-(.5/10^Res$testdec)
  upper <- Res$Value+(.4/10^Res$testdec)
  
  r2t <- function(r,df){
    r / (sqrt((1-r^2)/df))
  }
  
  for(i in seq_len(nrow(Res))){
    
    if(Res[i,]$Statistic=="F"){
      upP <- pf(lower[i],Res[i,]$df1,Res[i,]$df2,lower.tail=FALSE)
      lowP  <- pf(upper[i],Res[i,]$df1,Res[i,]$df2,lower.tail=FALSE)
      
    } else if(Res[i,]$Statistic=="t"){
      upP <- pt(-1*abs(lower[i]),Res[i,]$df1)*2
      lowP  <- pt(-1*abs(upper[i]),Res[i,]$df1)*2
      
    } else if(Res[i,]$Statistic=="Chi2"){
      upP <- pchisq(lower[i],Res[i,]$df1,lower.tail=FALSE)
      lowP  <- pchisq(upper[i],Res[i,]$df1,lower.tail=FALSE)
      
    } else if(Res[i,]$Statistic=="r"){
      upP <- pmin(pt(-1*abs(r2t(lower[i],Res[i,]$df1)),Res[i,]$df1)*2,1)
      lowP  <- pmin(pt(-1*abs(r2t(upper[i],Res[i,]$df1)),Res[i,]$df1)*2,1)
      
    } else if(Res[i,]$Statistic=="Z"|Res[i,]$Statistic=="z"){
      upP <- pnorm(abs(lower[i]),lower.tail=FALSE)*2
      lowP  <- pnorm(abs(upper[i]),lower.tail=FALSE)*2
      
    } else if(Res[i,]$Statistic=="Wald"|Res[i,]$Statistic=="wald"){
      upP <- min(pnorm(lower[i],lower.tail=FALSE)*2,pchisq(lower,1,lower.tail=FALSE))
      lowP  <- max(pnorm(upper[i],lower.tail=FALSE)*2,pchisq(lower,1,lower.tail=FALSE))
    }
    
    if(OneTailedTests==FALSE){
      correct_round[i] <- ifelse(Res[i,]$Error==TRUE & Res$Reported.P.Value[i]>=lowP & Res$Reported.P.Value[i]<=upP,TRUE,FALSE)
    } else {
      correct_round[i] <- ifelse(Res[i,]$Error==TRUE & Res$Reported.P.Value[i]/2>=lowP & Res$Reported.P.Value[i]/2<=upP,TRUE,FALSE)
    }
  }
  
  CorrectRound <- as.logical(correct_round)
  
  ###---------------------------------------------------------------------
  
  Res$InExactError[CorrectRound] <- FALSE
  Res$ExactError[CorrectRound] <- FALSE
  Res$Error[CorrectRound] <- FALSE
  Res$DecisionError[CorrectRound] <- FALSE
  
  # final data frame
  Res <- data.frame(Source = Res$Source, 
                    Statistic = Res$Statistic, 
                    df1 = Res$df1, 
                    df2 = Res$df2,
                    Test.Comparison = Res$Test.Comparison,
                    Value = Res$Value, 
                    Reported.Comparison = Res$Reported.Comparison, 
                    Reported.P.Value = Res$Reported.P.Value, 
                    Computed = Res$Computed, 
                    Raw = Res$Raw,
                    Error = Res$Error,
                    DecisionError = Res$DecisionError,
                    OneTail = Res$OneTail,
                    OneTailedInTxt = Res$OneTailedInTxt,
                    CopyPaste = Res$CopyPaste
  )
  
  class(Res) <- c("statcheck","data.frame")
  
  return(Res)
}

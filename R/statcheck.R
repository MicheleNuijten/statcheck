statcheck <- structure(function(# Extract statistics and recompute p-values.
  ### This function extracts statistics from strings and returns the extracted values, reported p-values and recomputed p-values. The package relies on the program "pdftotext", see the paragraph "Note" for details on the installation.
  x,
  ### A vector of strings containing whole articles.
  stat=c("t","F","cor","chisq","Z","Wald"),
  ### "t" to extract t-values, "F" to extract F-values, "cor" to extract correlations, "chisq"to extract chisquare values, "Z" to extract Z-values, and "Wald" to extract the results of the Wald test.
  OneTailedTests=FALSE,
  ### Logical. Do we assume that all reported tests are one tailed (TRUE) or two tailed (FALSE, default).
  alpha=.05,
  ### Assumed level of significance in the scanned texts. Defaults to .05. 
  OneTailedTxt=FALSE
  ### Logical. If TRUE, statcheck searches the text for "sided", "tailed", and "directional" to identify the possible use of one-sided tests. If one or more of these strings is found in the text AND the result would have been correct if it was a one-sided test, the result is assumed to be indeed one-sided and is counted as correct.
  )
{
  ##details<<
  ## This function can be used if the text of articles has already been imported in R. To import text from pdf files and automatically send the results to this function use \code{\link{checkPDFdir}} or \code{\link{checkPDF}}. To import text from HTML files use the similar functions \code{\link{checkHTMLdir}} or \code{\link{checkHTML}}. Finally, \code{\link{checkdir}} can be used to import text from both PDF and HTML files in a folder.
  ## This function uses regular expressions to find statistical references in the form "stat (df1, df2) = value, p = value". If the statistic is "t" and there is only one degree of freedom a t value is extracted, if the statistic is "r" with one degree of freedom a correlation is extraced and if the statistic is "F" with two degrees of freedom a F value is extracted. Finally, chi-square values are extracted by finding all statistical references with one degree of freedom that do not follow a "t" or "r".
  ## Note that the conversion to plain text and extraction of statistics can result in errors. Some statistical values can be missed, especially if the notation is unconventional. It is recommended to manually check some of the results.
  ##\section{Installing pdftotext}
  ## {For PDF files the "pdftotext" program is used to convert PDF files to plain text files. You can obtain pdftotext from \code{http://www.foolabs.com/xpdf/download.html}. Download and unzip the precompiled binaries. Next, add the folder with the binaries to the PATH variables so that this program can be used from command line.}
  ##\section{Note}
  ## {Note that a seemingly inconsistent p value can still be correct when we take the possibility into account that the test statistic was rounded after calculating the corresponding p value. For instance, a reported t value of 2.35 could correspond to an actual value of 2.345 to 2.354 with a range of p values that can slightly deviate from the recomputed p value but still be correct.
  ##seealso<<
  ## \code{\link{checkPDF}}, \code{\link{checkHTMLdir}}, \code{\link{checkHTML}}, \code{\link{checkdir}}
  '%rem%'<- function(x,y){
    at <- attr(x,"match.length")
    x <- x[-y]
    at <- at[-y]
    attr(x,"match.length") <- at
    return(x)
  }
  
  # Create empty data frame:
  Res <- data.frame(Source = NULL,Statistic=NULL,df1=NULL,df2=NULL,Test.Comparison=NULL,
                    Value=NULL,Reported.Comparison=NULL,Reported.P.Value=NULL, Computed = NULL, 
                    Error = NULL,DecisionError=NULL,CopyPaste=NULL, Location = NULL,
                    stringsAsFactors=FALSE,dec=NULL,testdec=NULL,OneTail=NULL,OneTailedInTxt=NULL)
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
      zLoc <- gregexpr("[^a-z]?(z|Z)\\s?.?\\s?\\D{0,3}\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(ns|p\\s?.?\\s?\\d?\\.\\d+)",txt,ignore.case=TRUE)[[1]]
      
      if (zLoc[1] != -1){
        # Get raw text of z-values:
        zRaw <- substring(txt,zLoc,zLoc+attr(zLoc,"match.length")-1)
        
        # remove any character before test statistic
        zRaw <- gsub(".?(z|Z)","Z",zRaw,perl=TRUE)
        
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
      wLoc <- gregexpr("[^a-z]?(\\wald|\\Wald)\\s?\\D?\\s?\\D{0,3}\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(ns|p\\s?.?\\s?\\d?\\.\\d+)",txt,ignore.case=TRUE)[[1]]
      
      if (wLoc[1] != -1){
        # Get raw text of Wald results:
        wRaw <- substring(txt,wLoc,wLoc+attr(wLoc,"match.length")-1)
        
        # remove any character before test statistic
        wRaw <- gsub(".?(wald|Wald)","Wald",wRaw,perl=TRUE)
        
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
  
  ###---------------------------------------------------------------------
  ErrorTest <- function(x,...){
    
    computed <- as.vector(x$Computed)
    comparison <- as.vector(x$Reported.Comparison)
    reported <-  as.vector(x$Reported.P.Value)
    testcomp <-  as.vector(x$Test.Comparison)
    
    # replace 'ns' for > alpha
    reported[comparison=="ns"] <- alpha
    comparison[comparison=="ns"] <- ">"
    
    Match <- paste(computed,comparison,reported)
    
    #-----------------------------------------------
    
    # select inexactly reported p values (p<../p>..)
    InExTests <- grepl("<|>",Match)
    
    # evaluate errors when test statistics are reported exactly (t()=.../F(,)=...)
    if(any(InExTests)){
      InExTests[InExTests] <- sapply(Match[InExTests],function(m)!eval(parse(text=m)))
    }
    
    # evaluate errors when test statistics are reported inexactly (t()</>.../F(,)</>...)
    smallsmall <- testcomp=="<" & comparison=="<"
    smallgreat <- testcomp=="<" & comparison==">"
    greatsmall <- testcomp==">" & comparison=="<"
    greatgreat <- testcomp==">" & comparison==">"
    
    if(any(smallsmall)){
      InExTests[smallsmall] <- !(round(computed[smallsmall],x$dec[smallsmall])<=round(reported[smallsmall],x$dec[smallsmall]))
    }
    
    if(any(greatgreat)){
      InExTests[greatgreat] <- !(round(computed[greatgreat],x$dec[greatgreat])>=round(reported[greatgreat],x$dec[greatgreat]))
    }
    
    # these combinations of < & > are logically always correct
    InExTests[smallgreat] <- FALSE
    InExTests[greatsmall] <- FALSE
    
    #-----------------------------------------------
    
    # select exactly reported p values (p=..)
    ExTests <- comparison=="="
    
    # evaluate errors when test statistics are reported exactly (t()=.../F(,)=...)
    if(any(ExTests)){
      ExTests[ExTests] <- !(round(computed[ExTests],x$dec[ExTests])==round(reported[ExTests],x$dec[ExTests]))
    }
    
    # evaluate errors when test statistics are reported inexactly (t()</>.../F(,)</>...)
    smallequal <- x$Test.Comparison=="<" & comparison=="="
    greatequal <- x$Test.Comparison==">" & comparison=="="
    
    if(any(smallequal)){
      ExTests[smallequal] <- !(round(computed[smallequal],x$dec[smallequal])<round(reported[smallequal],x$dec[smallequal]))
    }
    
    if(any(greatequal)){
      ExTests[greatequal] <- !(round(computed[greatequal],x$dec[greatequal])>round(reported[greatequal],x$dec[greatequal]))
    }
    
    #-----------------------------------------------
    
    # a result is an error if InExactError and/or ExactError are TRUE
    Error <- !(InExTests==FALSE & ExTests==FALSE)
    
    return(Error)
  }
  
  ###---------------------------------------------------------------------
  
  DecisionErrorTest <- function(x,...){
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
  
  ###---------------------------------------------------------------------
  
  if(OneTailedTests==TRUE){
    Res$Computed <- Res$Computed/2
  } 
  
  Res$Error <- ErrorTest(Res)
  
  # p values smaller or equal to zero and p values larger than one are errors
  ImpossibleP <- (Res$Reported.P.Value<=0|Res$Reported.P.Value>1)
  Res$Error[ImpossibleP] <- TRUE
  
  Res$DecisionError <-  DecisionErrorTest(Res)  
  
  ###---------------------------------------------------------------------
  
  # check if there would also be a decision error if alpha=.01 or .1
  DecisionErrorAlphas <- logical()
  alphas <- c(.01,.1)
  
  for(a in alphas){
    alpha <- a
    DecisionErrorAlphas <- c(DecisionErrorAlphas, DecisionErrorTest(Res))
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
    
    OneTail <- ifelse(Res$Error==TRUE &
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
  # and there was a mention of 'sided','tailed', or 'directional' in the text
  
  if(OneTailedTxt==TRUE){
    
    Res1tailed <- Res
    Res1tailed$Computed <- Res1tailed$Computed/2
    
    Res1tailed$Error <- ErrorTest(Res1tailed)
    
    Res$Error[Res$OneTailedInTxt==TRUE & Res1tailed$Error==FALSE] <- FALSE
    Res$DecisionError[Res$OneTailedInTxt==TRUE & Res1tailed$DecisionError==FALSE] <- FALSE
    
    #     Res$Error[Res$OneTailedInTxt==TRUE & Res$OneTail==TRUE] <- FALSE
    #     Res$DecisionError[Res$OneTailedInTxt==TRUE & Res$OneTail==TRUE] <- FALSE
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
  ### A data frame containing for each extracted statistic:
  ### \item{Source}{Name of the file of which the statistic is extracted}
  ### \item{Statistic}{Character indicating the statistic that is extracted}
  ### \item{df1}{First degree of freedom}
  ### \item{df2}{Second degree of freedom (if applicable)}
  ### \item{Test.Comparison}{Reported comparison of the test statistic, when importing from pdf this will often not be converted properly}
  ### \item{Value}{Reported value of the statistic}
  ### \item{Reported.Comparison}{Reported comparison of the p value, when importing from pdf this will often not be converted properly}
  ### \item{Reported.P.Value}{The reported p-value, or NA if the reported value was NS}
  ### \item{Computed}{The recomputed p-value}
  ### \item{Raw}{Raw string of the statistical reference that is extracted}
  ### \item{Error}{The computed p value is not congruent with the reported p value}
  ### \item{DecisionError}{The reported result is significant whereas the computed result is not, or vice versa.}
  ### \item{OneTail}{Logical. Is it likely that the reported p value resulted from a correction for one-sided testing?}
  ### \item{OneTailedInTxt}{Logical. Does the text contain the string "sided", "tailed", and/or "directional"?}
  ### \item{CopyPaste}{Logical. Does the exact string of the extracted raw results occur anywhere else in the article?}
},ex=function(){
  txt <- "blablabla the effect was very significant (t(100)=1, p < 0.001)"
  statcheck(txt)
  })


###########################

r2t <- function(# Transform r values into t values
  ### Function to transform r values into t values by use of raw r and degrees of freedom.
  r,
  ### Raw correlation value
  df
  ### Degrees of freedom (N-1)
  ){
          r / (sqrt((1-r^2)/df))
        }
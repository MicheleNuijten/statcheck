statcheck <- structure(function(# Extract statistics and recompute p-values.
  ### This function extracts statistics from strings and returns the extracted values, reported p-values and recomputed p-values. The package relies on the program "pdftotext", see the paragraph "Note" for details on the installation.
  x,
  ### A vector of strings.
  stat=c("t","F","cor","chisq","Z"),
  ### "t" to extract t-values, "F" to extract F-values, "cor" to extract correlations, "chisq"to extract chi-square values, and "Z" to extract Z-values.
  OneTailedTests=FALSE,
  ### Logical. Do we assume that all reported tests are one tailed (TRUE) or two tailed (FALSE, default)?
  alpha=.05,
  ### Assumed level of significance in the scanned texts. Defaults to .05. 
  pEqualAlphaSig=TRUE,
  ### Logical. If TRUE, statcheck counts p <= alpha as significant (default), if FALSE, statcheck counts p < alpha as significant
  OneTailedTxt=FALSE,
  ### Logical. If TRUE, statcheck searches the text for "one-sided", "one-tailed", and "directional" to identify the possible use of one-sided tests. If one or more of these strings is found in the text AND the result would have been correct if it was a one-sided test, the result is assumed to be indeed one-sided and is counted as correct.
  AllPValues=FALSE
  ### Logical. If TRUE, the output will consist of a dataframe with all detected p values, also the ones that were not part of the full results in APA format
){
  ##details<<
  ## Statcheck uses regular expressions to find statistical results in APA format. When a statistical result deviates from APA format, statcheck will not find it. The APA formats that statcheck uses are: t(df) = value, p = value; F(df1,df2) = value, p = value; r(df) = value, p = value; [chi]2 (df, N = value) = value, p = value (N is optional, delta G is also included); Z = value, p = value. All regular expressions take into account that test statistics and p values may be exactly (=) or inexactly (< or >) reported. Different spacing has also been taken into account.
  ## This function can be used if the text of articles has already been imported in R. To import text from pdf files and automatically send the results to this function use \code{\link{checkPDFdir}} or \code{\link{checkPDF}}. To import text from HTML files use the similar functions \code{\link{checkHTMLdir}} or \code{\link{checkHTML}}. Finally, \code{\link{checkdir}} can be used to import text from both PDF and HTML files in a folder.
  ## Note that the conversion from PDF (and sometimes also HTML) to plain text and extraction of statistics can result in errors. Some statistical values can be missed, especially if the notation is unconventional. It is recommended to manually check some of the results.
  ## PDF files should automatically be converted to plain text files. However, if this does not work, it might help to manually install the program "pdftotext". You can obtain pdftotext from \code{http://www.foolabs.com/xpdf/download.html}. Download and unzip the precompiled binaries. Next, add the folder with the binaries to the PATH variables so that this program can be used from command line.
  ## Also, note that a seemingly inconsistent p value can still be correct when we take into account that the test statistic might have been rounded after calculating the corresponding p value. For instance, a reported t value of 2.35 could correspond to an actual value of 2.345 to 2.354 with a range of p values that can slightly deviate from the recomputed p value. Statcheck will not count cases like this as errors.
  ##seealso<<
  ## \code{\link{checkPDF}}, \code{\link{checkHTMLdir}}, \code{\link{checkHTML}}, \code{\link{checkdir}}
  
  # Create empty data frame for main result:
  Res <- data.frame(Source = NULL,Statistic=NULL,df1=NULL,df2=NULL,Test.Comparison=NULL,
                    Value=NULL,Reported.Comparison=NULL,Reported.P.Value=NULL, Computed = NULL, 
                    Error = NULL,DecisionError=NULL,CopyPaste=NULL, Location = NULL,
                    stringsAsFactors=FALSE,dec=NULL,testdec=NULL,OneTail=NULL,OneTailedInTxt=NULL,
                    APAfactor = NULL)
  class(Res) <- c("statcheck","data.frame")
  OneTailedInTxt <- NULL
  
  # Create empty data frame for p values:
  pRes <- data.frame(Source = NULL, 
                     Statistic=NULL, 
                     Reported.Comparison= NULL, 
                     Reported.P.Value=NULL, 
                     Raw = NULL,
                     stringsAsFactors=FALSE
  )
  
  if (length(x)==0) return(Res)
  
  if (is.null(names(x))) names(x) <-  1:length(x)
  
  message("Extracting statistics...")
  pb <- txtProgressBar(max=length(x),style=3)
  for (i in 1:length(x)){
    
    txt <- x[i]
    
    #---------------------------
    
    # extract all p values in order to calculate the ratio statcheck results/total # of p values
    
    # p-values
    # Get location of p-values in text:
    pLoc <- gregexpr("([^a-z]ns)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*)",txt,ignore.case=TRUE)[[1]]
    
    if (pLoc[1] != -1){
      # Get raw text of p-values:
      pRaw <- substring(txt,pLoc,pLoc+attr(pLoc,"match.length")-1)
      
      nums <- gregexpr("(\\d*\\.?\\d+\\s?e?-?\\d*)|ns",pRaw,ignore.case=TRUE)
      
      # Extract p-values
      suppressWarnings(
        pValsChar <- substring(pRaw,sapply(nums,'[',1),sapply(nums,function(x)x[1]+attr(x,"match.length")[1]-1)))
      
      suppressWarnings(
        pVals <- as.numeric(pValsChar))
      
      # Extract (in)equality
      eqLoc <- gregexpr("p\\s?.?",pRaw)
      pEq <- substring(pRaw,
                       sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                       sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
      pEq[grepl("ns",pRaw,ignore.case=TRUE)] <- "ns"
      
      
      
      pvalues <- data.frame(Source = names(x)[i], 
                            Statistic="p", 
                            Reported.Comparison= pEq, 
                            Reported.P.Value=pVals, 
                            Raw = pRaw,
                            stringsAsFactors=FALSE)
      
      # remove p values greater than one
      pvalues <- pvalues[pvalues$Reported.P.Value<=1|is.na(pvalues$Reported.P.Value),]
      
      pRes <- rbind(pRes,pvalues)
      rm(pvalues)
      
    }
    
    #---------------------------
    
    # search for "one-sided"/"one-tailed"/"directional" in full text to detect one-sided testing
    
    #     onesided <- gregexpr("sided|tailed|directional",txt,ignore.case=TRUE)[[1]]
    onesided <- gregexpr("one.?sided|one.?tailed|directional",txt,ignore.case=TRUE)[[1]]
    
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
      tLoc <- gregexpr("t\\s?\\(\\s?\\d*\\.?\\d+\\s?\\)\\s?[<>=]\\s?[^a-z\\d]{0,3}\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(([^a-z]ns)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*))",txt,ignore.case=TRUE)[[1]]
      
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
        nums <- gregexpr("(\\-?\\s?\\d*\\.?\\d+\\s?e?-?\\d*)|ns",tRaw,ignore.case=TRUE)
        
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
        testEqLoc <- gregexpr("\\)\\s?[<>=]",tRaw)
        testEq <- substring(tRaw,
                            sapply(testEqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                            sapply(testEqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
        
        
        # Extract p-values
        suppressWarnings(
          pValsChar <- substring(tRaw,sapply(nums,'[',3),sapply(nums,function(x)x[3]+attr(x,"match.length")[3]-1)))
        
        suppressWarnings(
          pVals <- as.numeric(pValsChar))
        
        # Extract (in)equality
        eqLoc <- gregexpr("p\\s?[<>=]",tRaw,ignore.case=TRUE)
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
                           df1= NA, 
                           df2=df,
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
      FLoc <- gregexpr("F\\s?\\(\\s?\\d*\\.?\\d+\\s?,\\s?\\d*\\.?\\d+\\s?\\)\\s?[<>=]\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(([^a-z]ns)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*))",txt,ignore.case=TRUE)[[1]]
      
      if (FLoc[1] != -1){
        # Get raw text of F-values:
        FRaw <- substring(txt,FLoc,FLoc+attr(FLoc,"match.length")-1)
        
        # Extract location of numbers:
        nums <- gregexpr("(\\d*\\.?\\d+\\s?e?-?\\d*)|ns",FRaw,ignore.case=TRUE)
        
        # Extract df1:
        df1 <- as.numeric(substring(FRaw,sapply(nums,'[',1),sapply(nums,function(x)x[1]+attr(x,"match.length")[1]-1)))
        
        # Extract df2:
        df2 <- as.numeric(substring(FRaw,sapply(nums,'[',2),sapply(nums,function(x)x[2]+attr(x,"match.length")[2]-1)))
        
        # remove commas (thousands separators)
        Fsplit <- strsplit(FRaw,"\\)",perl=TRUE)
        
        FValsRaw <- lapply(Fsplit,function(x) x[2])
        FandDF <- lapply(Fsplit,function(x) x[1])
        
        FValsRaw <- gsub("(?<=\\d),(?=\\d+)","",FValsRaw,perl=TRUE)
        
        FRaw <- paste(FandDF,")",FValsRaw,sep="")
        
        # Extract F-values
        numsF <- gregexpr("(\\d*\\.?\\d+)|ns",FValsRaw)
        suppressWarnings(
          FValsChar <- substring(FValsRaw,sapply(numsF,'[',1),sapply(numsF,function(x)x[1]+attr(x,"match.length")[1]-1)))
        
        suppressWarnings(
          FVals <- as.numeric(FValsChar))
        
        # Extract number of decimals test statistic
        testdec <- attr(regexpr("\\.\\d+",FValsChar),"match.length")-1
        testdec[testdec<0] <- 0
        
        # Extract (in)equality test statistic
        testEqLoc <- gregexpr("\\)\\s?[<>=]",FRaw)
        testEq <- substring(FRaw,
                            sapply(testEqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                            sapply(testEqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
        
        
        # Extract p-values
        suppressWarnings(
          pValsChar <- substring(FValsRaw,sapply(numsF,'[',2),sapply(numsF,function(x)x[2]+attr(x,"match.length")[2]-1)))
        
        suppressWarnings(
          pVals <- as.numeric(pValsChar))
        
        # Extract (in)equality
        eqLoc <- gregexpr("p\\s?[<>=]",FRaw,ignore.case=TRUE)
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
      rLoc <- gregexpr("r\\s?\\(\\s?\\d*\\.?\\d+\\s?\\)\\s?[<>=]\\s?[^a-z\\d]{0,3}\\s?\\d*\\.?\\d+\\s?,\\s?(([^a-z]ns)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*))",txt,ignore.case=TRUE)[[1]]
      
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
        nums <- gregexpr("(\\-?\\s?\\d*\\.?\\d+\\s?e?-?\\d*)|ns",rRaw,ignore.case=TRUE)
        
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
        testEqLoc <- gregexpr("\\)\\s?[<>=]",rRaw)
        testEq <- substring(rRaw,
                            sapply(testEqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                            sapply(testEqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
        
        
        # Extract p-values
        suppressWarnings(
          pValsChar <- substring(rRaw,sapply(nums,'[',3),sapply(nums,function(x)x[3]+attr(x,"match.length")[3]-1)))
        
        suppressWarnings(
          pVals <- as.numeric(pValsChar))
        
        # Extract (in)equality
        eqLoc <- gregexpr("p\\s?[<>=]",rRaw,ignore.case=TRUE)
        pEq <- substring(rRaw,
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                         sapply(eqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
        pEq[grepl("ns",rRaw,ignore.case=TRUE)] <- "ns"
        
        
        # determine number of decimals of p value
        dec <- attr(regexpr("\\.\\d+",pValsChar),"match.length")-1
        dec[dec<0] <- 0
        
        # computed p = NA for correlations reported as >1 
        pComputed <- pmin(pt(-1*abs(r2t(rVals,df)),df)*2,1)
        pComputed[is.nan(pComputed)] <- NA
        
        # Create data frame:
        rRes <- data.frame(Source = names(x)[i], 
                           Statistic="r", 
                           df1= NA, 
                           df2=df,
                           Test.Comparison=testEq,
                           Value = rVals, 
                           Reported.Comparison= pEq, 
                           Reported.P.Value=pVals, 
                           Computed = pComputed, 
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
      zLoc <- gregexpr("[^a-z]z\\s?[<>=]\\s?[^a-z\\d]{0,3}\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(([^a-z]ns)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*))",txt,ignore.case=TRUE)[[1]]
      
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
        nums <- gregexpr("(\\-?\\s?\\d*\\.?\\d+\\s?e?-?\\d*)|ns",zRaw,ignore.case=TRUE)
        
        # Extract z-values
        suppressWarnings(
          zValsChar <- substring(zRaw,sapply(nums,'[',1),sapply(nums,function(x)x[1]+attr(x,"match.length")[1]-1)))
        
        suppressWarnings(
          zVals <- as.numeric(zValsChar))
        
        # Extract number of decimals test statistic
        testdec <- attr(regexpr("\\.\\d+",zValsChar),"match.length")-1
        testdec[testdec<0] <- 0
        
        # Extract (in)equality test statistic
        testEqLoc <- gregexpr("(z|Z|z'|Z')\\s?[<>=]",zRaw)
        testEq <- substring(zRaw,
                            sapply(testEqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                            sapply(testEqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
        
        
        # Extract p-values
        suppressWarnings(
          pValsChar <- substring(zRaw,sapply(nums,'[',2),sapply(nums,function(x)x[2]+attr(x,"match.length")[2]-1)))
        
        suppressWarnings(
          pVals <- as.numeric(pValsChar))
        
        # Extract (in)equality
        eqLoc <- gregexpr("p\\s?[<>=]",zRaw,ignore.case=TRUE)
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
    
    # Chis2-values:
    if ("chisq"%in%stat){
      # Get location of chi values or delta G in text:
      chi2Loc <- gregexpr("((\\[CHI\\]|\\[DELTA\\]G)\\s?|(\\s[^trF ]\\s?)|([^trF]2\\s?))2?\\(\\s?\\d*\\.?\\d+\\s?(,\\s?N\\s?\\=\\s?\\d*\\,?\\d*\\,?\\d+\\s?)?\\)\\s?[<>=]\\s?\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(([^a-z]ns)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*))",txt,ignore.case=TRUE)[[1]]
      
      if (chi2Loc[1] != -1){
        # Get raw text of chi2-values:
        chi2Raw <- substring(txt,chi2Loc,chi2Loc+attr(chi2Loc,"match.length")-1)
        substr(chi2Raw,1,1)[grepl("\\d",substr(chi2Raw,1,1))] <- " "
        
        # remove sample size if reported for calculations
        # save full result for "Raw" in final data frame
        chi2Raw_inclN <- chi2Raw
        chi2Raw <- gsub("N\\s?=\\s?\\d*\\,?\\d*\\,?\\d*","",chi2Raw,ignore.case=TRUE)
        
        # remove commas (thousands separators)
        chi2Raw <- gsub("(?<=\\d),(?=\\d+\\.)","",chi2Raw,perl=TRUE)
        
        # bug fix: remove extra opening brackets 
        # if a chi2 result is reported between brackets, and the chi is not read by statcheck
        # the opening bracket is translated as the chi symbol, and extracting the numerics goes wrong
        chi2Raw <- gsub("\\((?=2\\s?\\()","",chi2Raw,perl=TRUE)
        
        # Extract location of numbers:
        nums <- gregexpr("(\\-?\\s?\\d*\\.?\\d+\\s?e?-?\\d*)|ns",sub("^.*?\\(","",chi2Raw),ignore.case=TRUE)
        
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
        testEqLoc <- gregexpr("\\)\\s?[<>=]",chi2Raw)
        testEq <- substring(chi2Raw,
                            sapply(testEqLoc,function(x)x[1]+attr(x,"match.length")[1]-1),
                            sapply(testEqLoc,function(x)x[1]+attr(x,"match.length")[1]-1))
        
        
        # Extract p-values
        suppressWarnings(
          pValsChar <- substring(sub("^.*?\\(","",chi2Raw),sapply(nums,'[',3),sapply(nums,function(x)x[3]+attr(x,"match.length")[3]-1)))
        
        suppressWarnings(
          pVals <- as.numeric(pValsChar))
        
        # Extract (in)equality
        eqLoc <- gregexpr("p\\s?[<>=]",chi2Raw,ignore.case=TRUE)
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
                              Raw = chi2Raw_inclN,
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
  
  if(nrow(Res)>0){
    # remove p values greater than one
    Res <- Res[Res$Reported.P.Value<=1|is.na(Res$Reported.P.Value),]
  }
  
  
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
      InExTests[smallsmall] <- round(computed[smallsmall],x$dec[smallsmall])<=round(reported[smallsmall],x$dec[smallsmall])
    }
    
    if(any(greatgreat)){
      InExTests[greatgreat] <- round(computed[greatgreat],x$dec[greatgreat])>=round(reported[greatgreat],x$dec[greatgreat])
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
      ExTests[smallequal] <- round(computed[smallequal],x$dec[smallequal])<=round(reported[smallequal],x$dec[smallequal])
    }
    
    if(any(greatequal)){
      ExTests[greatequal] <- round(computed[greatequal],x$dec[greatequal])>=round(reported[greatequal],x$dec[greatequal])
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
    testcomp <-  as.vector(x$Test.Comparison)
    
    # replace 'ns' by > alpha
    reported[comparison=="ns"] <- alpha
    comparison[comparison=="ns"] <- ">"
    
    #-----------------------------------------------
    
    equalequal <- testcomp=="=" & comparison=="="
    equalsmall <- testcomp=="=" & comparison=="<"
    equalgreat <- testcomp=="=" & comparison==">"
    
    smallequal <- testcomp=="<" & comparison=="="
    smallsmall <- testcomp=="<" & comparison=="<"
    smallgreat <- testcomp=="<" & comparison==">"
    
    greatequal <- testcomp==">" & comparison=="="
    greatsmall <- testcomp==">" & comparison=="<"
    greatgreat <- testcomp==">" & comparison==">"
    
    
    
    AllTests <- grepl("=|<|>",comparison)
    
    if (any(AllTests)){
      
      if(pEqualAlphaSig==TRUE){
        
        AllTests[equalequal] <- (reported[equalequal]<=alpha & computed[equalequal]>alpha)|
          (reported[equalequal]>alpha & computed[equalequal]<=alpha)
        AllTests[equalsmall] <- reported[equalsmall]<=alpha & computed[equalsmall] >alpha
        AllTests[equalgreat] <- reported[equalgreat] >=alpha & computed[equalgreat]<=alpha
        
        
        AllTests[smallequal] <- reported[smallequal]<=alpha & computed[smallequal]>=alpha
        AllTests[smallsmall] <- reported[smallsmall]<=alpha & computed[smallsmall]>=alpha
        
        AllTests[greatequal] <- reported[greatequal]>alpha & computed[greatequal]<=alpha
        AllTests[greatgreat] <- reported[greatgreat]>=alpha & computed[greatgreat]<=alpha
        
        
      } else {
        
        AllTests[equalequal] <- (reported[equalequal]<alpha & computed[equalequal]>=alpha)|
          (reported[equalequal]>=alpha & computed[equalequal]<alpha)
        AllTests[equalsmall] <- reported[equalsmall]<alpha & computed[equalsmall] >=alpha
        AllTests[equalgreat] <- reported[equalgreat] >=alpha & computed[equalgreat]<alpha
        
        
        AllTests[smallequal] <- reported[smallequal]<alpha & computed[smallequal]>=alpha
        AllTests[smallsmall] <- reported[smallsmall]<=alpha & computed[smallsmall]>=alpha
        
        AllTests[greatequal] <- reported[greatequal]>=alpha & computed[greatequal]<alpha
        AllTests[greatgreat] <- reported[greatgreat]>=alpha & computed[greatgreat]<alpha
        
      }
      
      
      # these combinations of < & > are logically always correct
      AllTests[smallgreat] <- FALSE
      AllTests[greatsmall] <- FALSE
    }
    
    
    AllTests <- as.logical(AllTests)
    
    #-----------------------------------------------
    
    
    return(AllTests)
  }
  
  ###---------------------------------------------------------------------
  
  if(nrow(Res)>0){
    
    # if indicated, count all tests as onesided
    if(OneTailedTests==TRUE){
      Res$Computed <- Res$Computed/2
    } 
    
    # check for errors
    Res$Error <- ErrorTest(Res)
    
    Res$DecisionError <-  DecisionErrorTest(Res)  
    
    ###---------------------------------------------------------------------

    # check if there would also be a decision error if alpha=.01 or .1
    DecisionErrorAlphas <- logical()
    alphas <- c(.01,.1)

    for(a in alphas){
      alpha <- a
      DecisionErrorAlphas <- c(DecisionErrorAlphas, DecisionErrorTest(Res))
    }

    if(any(DecisionErrorAlphas[!is.na(DecisionErrorAlphas) & !is.nan(DecisionErrorAlphas)])){
      cat("\n Check the significance level. \n \n Some of the p value incongruencies are decision errors if the significance level is .1 or .01 instead of the conventional .05. It is recommended to check the actual significance level in the paper or text. Check if the reported p values are a decision error at a different significance level by running statcheck again with 'alpha' set to .1 and/or .01. \n ",fill=TRUE)
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
                          (grepl("=",comparison) & round(reported,2)==round(onetail,2))
                        | (grepl("<",comparison) & reported==.05 & onetail < reported & computed > reported),
                        TRUE,FALSE)
      Res$OneTail <- OneTail
      
      if(any(OneTail[!is.na(OneTail)]==TRUE & OneTailedTxt[!is.na(OneTailedTxt)]==FALSE)){
        cat("\n Check for one tailed tests. \n \n Some of the p value incongruencies might in fact be one tailed tests. It is recommended to check this in the actual paper or text. Check if the p values would also be incongruent if the test is indeed one sided by running statcheck again with 'OneTailedTests' set to TRUE. To see which Sources probably contain a one tailed test, try unique(x$Source[x$OneTail]) (where x is the statcheck output). \n ",fill=TRUE)
      }
      
    }
    
    ###---------------------------------------------------------------------
    
    # count errors as correct if they'd be correct one-sided
    # and there was a mention of 'one-sided','one-tailed', or 'directional' in the text
    
    if(OneTailedTxt==TRUE){
      
      Res1tailed <- Res
      Res1tailed$Computed <- Res1tailed$Computed/2
      
      Res1tailed$Error <- ErrorTest(Res1tailed)
      Res1tailed$DecisionError <- DecisionErrorTest(Res1tailed)
      
      Res$Error[!((Res$Statistic=="F"|Res$Statistic=="Chi2") & Res$df1>1) & Res$OneTailedInTxt==TRUE & Res1tailed$Error==FALSE] <- FALSE
      Res$DecisionError[!((Res$Statistic=="F"|Res$Statistic=="Chi2") & Res$df1>1) & Res$OneTailedInTxt==TRUE & Res1tailed$DecisionError==FALSE] <- FALSE
      

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
# e.g. t=2.3 could be 2.25 to 2.34999999... with its range of p values
correct_round <- numeric()

lower <- Res$Value-(.5/10^Res$testdec)
upper <- Res$Value+(.5/10^Res$testdec)

for(i in seq_len(nrow(Res))){
  
  if(Res[i,]$Statistic=="F"){
    upP <- pf(lower[i],Res[i,]$df1,Res[i,]$df2,lower.tail=FALSE)
    lowP  <- pf(upper[i],Res[i,]$df1,Res[i,]$df2,lower.tail=FALSE)
    
  } else if(Res[i,]$Statistic=="t"){
    
    if(lower[i]<0){
      lowP <- pt(lower[i],Res[i,]$df2)*2
      upP  <- pt(upper[i],Res[i,]$df2)*2
    } else{
      upP <- pt(-1*lower[i],Res[i,]$df2)*2
      lowP  <- pt(-1*upper[i],Res[i,]$df2)*2
    }
    
  } else if(Res[i,]$Statistic=="Chi2"){
    upP <- pchisq(lower[i],Res[i,]$df1,lower.tail=FALSE)
    lowP  <- pchisq(upper[i],Res[i,]$df1,lower.tail=FALSE)
    
  } else if(Res[i,]$Statistic=="r"){
    
    if(lower[i]<0){
      lowP <- pmin(pt(r2t(lower[i],Res[i,]$df2),Res[i,]$df2)*2,1)
      upP  <- pmin(pt(r2t(upper[i],Res[i,]$df2),Res[i,]$df2)*2,1)
    } else {
      upP <- pmin(pt(-1*r2t(lower[i],Res[i,]$df2),Res[i,]$df2)*2,1)
      lowP  <- pmin(pt(-1*r2t(upper[i],Res[i,]$df2),Res[i,]$df2)*2,1)
    }
    
  } else if(Res[i,]$Statistic=="Z"|Res[i,]$Statistic=="z"){
    
    if(lower[i]<0){
      lowP <- pnorm(abs(lower[i]),lower.tail=FALSE)*2
      upP  <- pnorm(abs(upper[i]),lower.tail=FALSE)*2
    } else {
      upP <- pnorm(lower[i],lower.tail=FALSE)*2
      lowP  <- pnorm(upper[i],lower.tail=FALSE)*2
    }
    
  } 
  
  if(OneTailedTests==TRUE){
    upP <- upP/2
    lowP <- lowP/2
  }
  
  if(Res[i,"Reported.Comparison"]=="="){
    correct_round[i] <- ifelse(Res[i,]$Error==TRUE & Res$Reported.P.Value[i]>=round(lowP,Res$dec[i]) & Res$Reported.P.Value[i]<=round(upP,Res$dec[i]),TRUE,FALSE)        
  }
  
  if(Res[i,"Reported.Comparison"]=="<"){
    correct_round[i] <- ifelse(Res[i,]$Error==TRUE & Res$Reported.P.Value[i]>lowP,TRUE,FALSE)
  }
  
  if(Res[i,"Reported.Comparison"]==">"){
    correct_round[i] <- ifelse(Res[i,]$Error==TRUE & Res$Reported.P.Value[i]<upP,TRUE,FALSE)
  }
  
  
}

CorrectRound <- as.logical(correct_round)

###---------------------------------------------------------------------

# p values smaller or equal to zero are errors
ImpossibleP <- (Res$Reported.P.Value<=0)
Res$Error[ImpossibleP] <- TRUE

###---------------------------------------------------------------------

# p values that are not an error can also not be a decision error
# this happens sometimes when reported= "p=.05" and e.g. computed=.052...
# this should be counted as correct

NoErrorDecisionError <- Res$Error==FALSE & Res$DecisionError==TRUE
Res$DecisionError[NoErrorDecisionError] <- FALSE

###---------------------------------------------------------------------

# APAfactor: proportion of APA results (that statcheck reads) of total number of p values

# select only the results of pRes that are from articles with at least 1 statcheck result
pRes_selection <- pRes[pRes$Source%in%Res$Source,]

# select only the statcheck results that are from an article with at least one p value
# this is relevant, because it sometimes happens that statcheck extracts less p values 
# p values than statcheck results. For instance in cases when a p value appears to be
# greater than 1.

Res_selection <- Res[Res$Source%in%pRes_selection$Source,]
APA <- by(Res_selection,Res_selection$Source,nrow)/by(pRes_selection,pRes_selection$Source,nrow)
Res$APAfactor <- round(as.numeric(apply(Res,1,function(x) APA[which(names(APA)==x["Source"])])),2)

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
                  CopyPaste = Res$CopyPaste,
                  APAfactor = Res$APAfactor
)

class(Res) <- c("statcheck","data.frame")
  }

###--------------------------------------------------------------------- 

if(AllPValues==FALSE){
  
  
  
  # Return message when there are no results
  if(nrow(Res)>0){
    
    
    return(Res) 
  } else {
    Res <- cat("statcheck did not find any results\n")
  }
  
} else {
  return(pRes)
}

### A data frame containing for each extracted statistic:
### \item{Source}{Name of the file of which the statistic is extracted}
### \item{Statistic}{Character indicating the statistic that is extracted}
### \item{df1}{First degree of freedom}
### \item{df2}{Second degree of freedom (if applicable)}
### \item{Test.Comparison}{Reported comparison of the test statistic, when importing from pdf this will often not be converted properly}
### \item{Value}{Reported value of the statistic}
### \item{Reported.Comparison}{Reported comparison, when importing from pdf this might not be converted properly}
### \item{Reported.P.Value}{The reported p-value, or NA if the reported value was NS}
### \item{Computed}{The recomputed p-value}
### \item{Raw}{Raw string of the statistical reference that is extracted}
### \item{Error}{The computed p value is not congruent with the reported p value}
### \item{DecisionError}{The reported result is significant whereas the recomputed result is not, or vice versa.}
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
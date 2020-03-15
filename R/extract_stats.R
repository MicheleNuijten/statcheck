extract_stats <- function(text, 
                         stat = c("t", "F", "cor", "chisq", "Z", "Q"),
                         messages = TRUE){
  
  # Create empty data frame for main result:
  Res <-
    data.frame(
      Source = NULL,
      Statistic = NULL,
      df1 = NULL,
      df2 = NULL,
      Test.Comparison = NULL,
      Value = NULL,
      Reported.Comparison = NULL,
      Reported.P.Value = NULL,
      Location = NULL,
      stringsAsFactors = FALSE,
      dec = NULL,
      testdec = NULL
    )
  
  class(Res) <- c("statcheck", "data.frame")
  
  if (length(text) == 0){
    return(Res)
  }
  
  if (is.null(names(text))){
    names(text) <-  1:length(text)
  }
  
  # start progress bar
  if(messages == TRUE){
    message("Extracting statistics...")
    pb <- txtProgressBar(max = length(text), style = 3)
  }
  
  for (i in 1:length(text)) {
    txt <- text[i]
    
    #---------------------------
    
    # t-values:
    if ("t" %in% stat) {
      # Get location of t-values in text:
      tLoc <-
        gregexpr(
          "t\\s?\\(\\s?\\d*\\.?\\d+\\s?\\)\\s?[<>=]\\s?[^a-z\\d]{0,3}\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(([^a-z]ns)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*))",
          txt,
          ignore.case = TRUE
        )[[1]]
      
      if (tLoc[1] != -1) {
        # Get raw text of t-values:
        tRaw <-
          substring(txt, tLoc, tLoc + attr(tLoc, "match.length") - 1)
        
        # remove commas (thousands separators)
        tRaw <- gsub("(?<=\\d),(?=\\d+)", "", tRaw, perl = TRUE)
        
        # Replace weird codings of a minus sign with actual minus sign:
        # First remove spaces
        tRaw <-
          gsub("(?<=\\=)\\s+(?=.*\\,)", "", tRaw, perl = TRUE)
        
        # Replace any weird string with a minus sign
        tRaw <-
          gsub("(?<=\\=)\\s?[^\\d\\.]+(?=.*\\,)", " -", tRaw, perl = TRUE)
        
        # Add spaces again:
        tRaw <-
          gsub("(?<=\\=)(?=(\\.|\\d))", " ", tRaw, perl = TRUE)
        
        # Extract location of numbers:
        nums <-
          gregexpr("(\\-?\\s?\\d*\\.?\\d+\\s?e?-?\\d*)|ns",
                   tRaw,
                   ignore.case = TRUE)
        
        # Extract df:
        df <-
          as.numeric(substring(
            tRaw,
            sapply(nums, '[', 1),
            sapply(nums, function(x)
              x[1] + attr(x, "match.length")[1] - 1)
          ))
        
        # Extract t-values
        suppressWarnings(tValsChar <-
                           substring(
                             tRaw,
                             sapply(nums, '[', 2),
                             sapply(nums, function(x)
                               x[2] + attr(x, "match.length")[2] - 1)
                           ))
        
        suppressWarnings(tVals <- as.numeric(tValsChar))
        
        # Extract number of decimals test statistic
        testdec <-
          attr(regexpr("\\.\\d+", tValsChar), "match.length") - 1
        testdec[testdec < 0] <- 0
        
        # Extract (in)equality test statistic
        testEqLoc <- gregexpr("\\)\\s?[<>=]", tRaw)
        testEq <- substring(
          tRaw,
          sapply(testEqLoc, function(x)
            x[1] + attr(x, "match.length")[1] - 1),
          sapply(testEqLoc, function(x)
            x[1] + attr(x, "match.length")[1] - 1)
        )
        
        # Extract p-values
        suppressWarnings(pValsChar <-
                           substring(
                             tRaw,
                             sapply(nums, '[', 3),
                             sapply(nums, function(x)
                               x[3] + attr(x, "match.length")[3] - 1)
                           ))
        
        suppressWarnings(pVals <- as.numeric(pValsChar))
        
        # Extract (in)equality
        eqLoc <-
          gregexpr("p\\s?[<>=]", tRaw, ignore.case = TRUE)
        pEq <- substring(
          tRaw,
          sapply(eqLoc, function(x)
            x[1] + attr(x, "match.length")[1] - 1),
          sapply(eqLoc, function(x)
            x[1] + attr(x, "match.length")[1] - 1)
        )
        pEq[grepl("ns", tRaw, ignore.case = TRUE)] <- "ns"
        
        # determine number of decimals of p value
        dec <-
          attr(regexpr("\\.\\d+", pValsChar), "match.length") - 1
        dec[dec < 0] <- 0
        
        # Create data frame:
        tRes <- data.frame(
          Source = names(txt)[i],
          Statistic = "t",
          df1 = NA,
          df2 = df,
          Test.Comparison = testEq,
          Value = tVals,
          Reported.Comparison = pEq,
          Reported.P.Value = pVals,
          Location = tLoc,
          Raw = tRaw,
          stringsAsFactors = FALSE,
          dec = dec,
          testdec = testdec
        )
        
        # Append, clean and close:
        Res <- rbind(Res, tRes)
        rm(tRes)
      }
    }
    
    #---------------------------
    
    # F-values:
    if ("F" %in% stat) {
      # Get location of F-values in text:
      # also pick up degrees of freedom wrongly converted into letters:
      # 1 --> l or I
      FLoc <-
        gregexpr(
          "F\\s?\\(\\s?\\d*\\.?(I|l|\\d+)\\s?,\\s?\\d*\\.?\\d+\\s?\\)\\s?[<>=]\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(([^a-z]ns)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*))",
          txt,
          ignore.case = TRUE
        )[[1]]
      
      if (FLoc[1] != -1) {
        # Get raw text of F-values:
        FRaw <-
          substring(txt, FLoc, FLoc + attr(FLoc, "match.length") - 1)
        
        # convert wrongly printed "l" or "I" into 1
        FRaw <- gsub("l|I", 1, FRaw)
        
        # Extract location of numbers:
        nums <-
          gregexpr("(\\d*\\.?\\d+\\s?e?-?\\d*)|ns", FRaw, ignore.case = TRUE)
        
        # Extract df1:
        df1 <-
          as.numeric(substring(
            FRaw,
            sapply(nums, '[', 1),
            sapply(nums, function(x)
              x[1] + attr(x, "match.length")[1] - 1)
          ))
        
        # Extract df2:
        df2 <-
          as.numeric(substring(
            FRaw,
            sapply(nums, '[', 2),
            sapply(nums, function(x)
              x[2] + attr(x, "match.length")[2] - 1)
          ))
        
        # remove commas (thousands separators)
        Fsplit <- strsplit(FRaw, "\\)", perl = TRUE)
        
        FValsRaw <- lapply(Fsplit, function(x)
          x[2])
        FandDF <- lapply(Fsplit, function(x)
          x[1])
        
        FValsRaw <-
          gsub("(?<=\\d),(?=\\d+)", "", FValsRaw, perl = TRUE)
        
        FRaw <- paste(FandDF, ")", FValsRaw, sep = "")
        
        # Extract F-values
        numsF <- gregexpr("(\\d*\\.?\\d+)|ns", FValsRaw)
        suppressWarnings(FValsChar <-
                           substring(
                             FValsRaw,
                             sapply(numsF, '[', 1),
                             sapply(numsF, function(x)
                               x[1] + attr(x, "match.length")[1] - 1)
                           ))
        
        suppressWarnings(FVals <- as.numeric(FValsChar))
        
        # Extract number of decimals test statistic
        testdec <-
          attr(regexpr("\\.\\d+", FValsChar), "match.length") - 1
        testdec[testdec < 0] <- 0
        
        # Extract (in)equality test statistic
        testEqLoc <- gregexpr("\\)\\s?[<>=]", FRaw)
        testEq <- substring(
          FRaw,
          sapply(testEqLoc, function(x)
            x[1] + attr(x, "match.length")[1] - 1),
          sapply(testEqLoc, function(x)
            x[1] + attr(x, "match.length")[1] - 1)
        )
        
        # Extract p-values
        suppressWarnings(pValsChar <-
                           substring(
                             FValsRaw,
                             sapply(numsF, '[', 2),
                             sapply(numsF, function(x)
                               x[2] + attr(x, "match.length")[2] - 1)
                           ))
        
        suppressWarnings(pVals <- as.numeric(pValsChar))
        
        # Extract (in)equality
        eqLoc <-
          gregexpr("p\\s?[<>=]", FRaw, ignore.case = TRUE)
        pEq <- substring(
          FRaw,
          sapply(eqLoc, function(x)
            x[1] + attr(x, "match.length")[1] - 1),
          sapply(eqLoc, function(x)
            x[1] + attr(x, "match.length")[1] - 1)
        )
        pEq[grepl("ns", FRaw, ignore.case = TRUE)] <- "ns"
        
        # determine number of decimals of p value
        dec <-
          attr(regexpr("\\.\\d+", pValsChar), "match.length") - 1
        dec[dec < 0] <- NA
        
        # Create data frame:
        FRes <- data.frame(
          Source = names(txt)[i],
          Statistic = "F",
          df1 = df1,
          df2 = df2,
          Test.Comparison = testEq,
          Value = FVals,
          Reported.Comparison = pEq,
          Reported.P.Value = pVals,
          Location = FLoc,
          Raw = FRaw,
          stringsAsFactors = FALSE,
          dec = dec,
          testdec = testdec
        )
        
        # Append, clean and close:
        Res <- rbind(Res, FRes)
        rm(FRes)
      }
    }
    
    #---------------------------
    
    # correlations:
    if (any(c("r", "cor", "correlations") %in% stat)) {
      # Get location of r-values in text:
      rLoc <-
        gregexpr(
          "r\\s?\\(\\s?\\d*\\.?\\d+\\s?\\)\\s?[<>=]\\s?[^a-z\\d]{0,3}\\s?\\d*\\.?\\d+\\s?,\\s?(([^a-z]ns)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*))",
          txt,
          ignore.case = TRUE
        )[[1]]
      
      if (rLoc[1] != -1) {
        # Get raw text of r-values:
        rRaw <-
          substring(txt, rLoc, rLoc + attr(rLoc, "match.length") - 1)
        
        # Replace weird codings of a minus sign with actual minus sign:
        # First remove spaces
        rRaw <-
          gsub("(?<=\\=)\\s+(?=.*\\,)", "", rRaw, perl = TRUE)
        
        # Replace any weird string with a minus sign
        rRaw <-
          gsub("(?<=\\=)\\s?[^\\d\\.]+(?=.*\\,)", " -", rRaw, perl = TRUE)
        
        # Add spaces again:
        rRaw <-
          gsub("(?<=\\=)(?=(\\.|\\d))", " ", rRaw, perl = TRUE)
        
        # Extract location of numbers:
        nums <-
          gregexpr("(\\-?\\s?\\d*\\.?\\d+\\s?e?-?\\d*)|ns",
                   rRaw,
                   ignore.case = TRUE)
        
        # Extract df:
        df <-
          as.numeric(substring(
            rRaw,
            sapply(nums, '[', 1),
            sapply(nums, function(x)
              x[1] + attr(x, "match.length")[1] - 1)
          ))
        
        # Extract r-values
        suppressWarnings(rValsChar <-
                           substring(
                             rRaw,
                             sapply(nums, '[', 2),
                             sapply(nums, function(x)
                               x[2] + attr(x, "match.length")[2] - 1)
                           ))
        
        suppressWarnings(rVals <- as.numeric(rValsChar))
        
        # Extract number of decimals test statistic
        testdec <-
          attr(regexpr("\\.\\d+", rValsChar), "match.length") - 1
        testdec[testdec < 0] <- 0
        
        
        # Extract (in)equality test statistic
        testEqLoc <- gregexpr("\\)\\s?[<>=]", rRaw)
        testEq <- substring(
          rRaw,
          sapply(testEqLoc, function(x)
            x[1] + attr(x, "match.length")[1] - 1),
          sapply(testEqLoc, function(x)
            x[1] + attr(x, "match.length")[1] - 1)
        )
        
        # Extract p-values
        suppressWarnings(pValsChar <-
                           substring(
                             rRaw,
                             sapply(nums, '[', 3),
                             sapply(nums, function(x)
                               x[3] + attr(x, "match.length")[3] - 1)
                           ))
        
        suppressWarnings(pVals <- as.numeric(pValsChar))
        
        # Extract (in)equality
        eqLoc <-
          gregexpr("p\\s?[<>=]", rRaw, ignore.case = TRUE)
        pEq <- substring(
          rRaw,
          sapply(eqLoc, function(x)
            x[1] + attr(x, "match.length")[1] - 1),
          sapply(eqLoc, function(x)
            x[1] + attr(x, "match.length")[1] - 1)
        )
        pEq[grepl("ns", rRaw, ignore.case = TRUE)] <- "ns"
        
        
        # determine number of decimals of p value
        dec <-
          attr(regexpr("\\.\\d+", pValsChar), "match.length") - 1
        dec[dec < 0] <- 0
        
        # Create data frame:
        rRes <- data.frame(
          Source = names(txt)[i],
          Statistic = "r",
          df1 = NA,
          df2 = df,
          Test.Comparison = testEq,
          Value = rVals,
          Reported.Comparison = pEq,
          Reported.P.Value = pVals,
          Location = rLoc,
          Raw = rRaw,
          stringsAsFactors = FALSE,
          dec = dec,
          testdec = testdec
        )
        
        # Append, clean and close:
        Res <- rbind(Res, rRes)
        rm(rRes)
      }
    }
    
    #---------------------------
    
    # z-values:
    if ("Z" %in% stat) {
      # Get location of z-values in text:
      zLoc <-
        gregexpr(
          "[^a-z]z\\s?[<>=]\\s?[^a-z\\d]{0,3}\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(([^a-z]ns)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*))",
          txt,
          ignore.case = TRUE
        )[[1]]
      
      if (zLoc[1] != -1) {
        # Get raw text of z-values:
        zRaw <-
          substring(txt, zLoc, zLoc + attr(zLoc, "match.length") - 1)
        
        # remove any character before test statistic
        zRaw <- gsub(".?(z|Z)", "Z", zRaw, perl = TRUE)
        
        # remove commas (thousands separators)
        zRaw <-
          gsub("(?<=\\d),(?=\\d+\\.)", "", zRaw, perl = TRUE)
        
        # Replace weird codings of a minus sign with actual minus sign:
        # First remove spaces
        zRaw <-
          gsub("(?<=\\=)\\s+(?=.*\\,)", "", zRaw, perl = TRUE)
        
        # Replace any weird string with a minus sign
        zRaw <-
          gsub("(?<=\\=)\\s?[^\\d\\.]+(?=.*\\,)", " -", zRaw, perl = TRUE)
        
        # Add spaces again:
        zRaw <-
          gsub("(?<=\\=)(?=(\\.|\\d))", " ", zRaw, perl = TRUE)
        
        # Extract location of numbers:
        nums <-
          gregexpr("(\\-?\\s?\\d*\\.?\\d+\\s?e?-?\\d*)|ns",
                   zRaw,
                   ignore.case = TRUE)
        
        # Extract z-values
        suppressWarnings(zValsChar <-
                           substring(
                             zRaw,
                             sapply(nums, '[', 1),
                             sapply(nums, function(x)
                               x[1] + attr(x, "match.length")[1] - 1)
                           ))
        
        suppressWarnings(zVals <- as.numeric(zValsChar))
        
        # Extract number of decimals test statistic
        testdec <-
          attr(regexpr("\\.\\d+", zValsChar), "match.length") - 1
        testdec[testdec < 0] <- 0
        
        # Extract (in)equality test statistic
        testEqLoc <- gregexpr("(z|Z|z'|Z')\\s?[<>=]", zRaw)
        testEq <- substring(
          zRaw,
          sapply(testEqLoc, function(x)
            x[1] + attr(x, "match.length")[1] - 1),
          sapply(testEqLoc, function(x)
            x[1] + attr(x, "match.length")[1] - 1)
        )
        
        # Extract p-values
        suppressWarnings(pValsChar <-
                           substring(
                             zRaw,
                             sapply(nums, '[', 2),
                             sapply(nums, function(x)
                               x[2] + attr(x, "match.length")[2] - 1)
                           ))
        
        suppressWarnings(pVals <- as.numeric(pValsChar))
        
        # Extract (in)equality
        eqLoc <-
          gregexpr("p\\s?[<>=]", zRaw, ignore.case = TRUE)
        pEq <- substring(
          zRaw,
          sapply(eqLoc, function(x)
            x[1] + attr(x, "match.length")[1] - 1),
          sapply(eqLoc, function(x)
            x[1] + attr(x, "match.length")[1] - 1)
        )
        pEq[grepl("ns", zRaw, ignore.case = TRUE)] <- "ns"
        
        # determine number of decimals of p value
        dec <-
          attr(regexpr("\\.\\d+", pValsChar), "match.length") - 1
        dec[dec < 0] <- 0
        
        # Create data frame:
        zRes <- data.frame(
          Source = names(txt)[i],
          Statistic = "Z",
          df1 = NA,
          df2 = NA,
          Test.Comparison = testEq,
          Value = zVals,
          Reported.Comparison = pEq,
          Reported.P.Value = pVals,
          Location = zLoc,
          Raw = zRaw,
          stringsAsFactors = FALSE,
          dec = dec,
          testdec = testdec
        )
        
        # Append, clean and close:
        Res <- rbind(Res, zRes)
        rm(zRes)
      }
    }
    
    #---------------------------
    
    # Chis2-values:
    if ("chisq" %in% stat) {
      # Get location of chi values or delta G in text:
      chi2Loc <-
        gregexpr(
          "((\\[CHI\\]|\\[DELTA\\]G)\\s?|(\\s[^trFzQWBnD ]\\s?)|([^trFzQWBnD ]2\\s?))2?\\(\\s?\\d*\\.?\\d+\\s?(,\\s?N\\s?\\=\\s?\\d*\\,?\\d*\\,?\\d+\\s?)?\\)\\s?[<>=]\\s?\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(([^a-z]ns)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*))",
          txt,
          ignore.case = TRUE
        )[[1]]
      
      if (chi2Loc[1] != -1) {
        # Get raw text of chi2-values:
        chi2Raw <-
          substring(txt, chi2Loc, chi2Loc + attr(chi2Loc, "match.length") - 1)
        substr(chi2Raw, 1, 1)[grepl("\\d", substr(chi2Raw, 1, 1))] <-
          " "
        
        # remove sample size if reported for calculations
        # save full result for "Raw" in final data frame
        chi2Raw_inclN <- chi2Raw
        chi2Raw <-
          gsub("N\\s?=\\s?\\d*\\,?\\d*\\,?\\d*",
               "",
               chi2Raw,
               ignore.case = TRUE)
        
        # remove commas (thousands separators)
        chi2Raw <-
          gsub("(?<=\\d),(?=\\d+\\.)", "", chi2Raw, perl = TRUE)
        
        # bug fix: remove extra opening brackets
        # if a chi2 result is reported between brackets, and the chi is not read by statcheck
        # the opening bracket is translated as the chi symbol, and extracting the numerics goes wrong
        chi2Raw <-
          gsub("\\((?=2\\s?\\()", "", chi2Raw, perl = TRUE)
        
        # Extract location of numbers:
        nums <-
          gregexpr(
            "(\\-?\\s?\\d*\\.?\\d+\\s?e?-?\\d*)|ns",
            sub("^.*?\\(", "", chi2Raw),
            ignore.case = TRUE
          )
        
        # Extract df:
        df <-
          as.numeric(substring(
            sub("^.*?\\(", "", chi2Raw),
            sapply(nums, '[', 1),
            sapply(nums, function(x)
              x[1] + attr(x, "match.length")[1] - 1)
          ))
        
        # Extract chi2-values
        suppressWarnings(chi2ValsChar <-
                           substring(
                             sub("^.*?\\(", "", chi2Raw),
                             sapply(nums, '[', 2),
                             sapply(nums, function(x)
                               x[2] + attr(x, "match.length")[2] - 1)
                           ))
        
        suppressWarnings(chi2Vals <- as.numeric(chi2ValsChar))
        
        # Extract number of decimals test statistic
        testdec <-
          attr(regexpr("\\.\\d+", chi2ValsChar), "match.length") - 1
        testdec[testdec < 0] <- 0
        
        # Extract (in)equality test statistic
        testEqLoc <- gregexpr("\\)\\s?[<>=]", chi2Raw)
        testEq <- substring(
          chi2Raw,
          sapply(testEqLoc, function(x)
            x[1] + attr(x, "match.length")[1] - 1),
          sapply(testEqLoc, function(x)
            x[1] + attr(x, "match.length")[1] - 1)
        )
        
        # Extract p-values
        suppressWarnings(pValsChar <-
                           substring(
                             sub("^.*?\\(", "", chi2Raw),
                             sapply(nums, '[', 3),
                             sapply(nums, function(x)
                               x[3] + attr(x, "match.length")[3] - 1)
                           ))
        
        suppressWarnings(pVals <- as.numeric(pValsChar))
        
        # Extract (in)equality
        eqLoc <-
          gregexpr("p\\s?[<>=]", chi2Raw, ignore.case = TRUE)
        pEq <- substring(
          chi2Raw,
          sapply(eqLoc, function(x)
            x[1] + attr(x, "match.length")[1] - 1),
          sapply(eqLoc, function(x)
            x[1] + attr(x, "match.length")[1] - 1)
        )
        pEq[grepl("ns", chi2Raw, ignore.case = TRUE)] <- "ns"
        
        # determine number of decimals of p value
        dec <-
          attr(regexpr("\\.\\d+", pValsChar), "match.length") - 1
        dec[dec < 0] <- 0
        
        # Create data frame:
        chi2Res <- data.frame(
          Source = names(txt)[i],
          Statistic = "Chi2",
          df1 = df,
          df2 = NA,
          Test.Comparison = testEq,
          Value = chi2Vals,
          Reported.Comparison = pEq,
          Reported.P.Value = pVals,
          Location = chi2Loc,
          Raw = chi2Raw_inclN,
          stringsAsFactors = FALSE,
          dec = dec,
          testdec = testdec
        )
        
        # Append, clean and close:
        Res <- rbind(Res, chi2Res)
        rm(chi2Res)
      }
    }
    
    #---------------------------
    
    # Q-values:
    if ("Q" %in% stat) {
      # Get location of Q-values in text:
      QLoc <-
        gregexpr(
          "Q\\s?-?\\s?(w|within|b|between)?\\s?\\(\\s?\\d*\\.?\\d+\\s?\\)\\s?[<>=]\\s?[^a-z\\d]{0,3}\\s?\\d*,?\\d*\\.?\\d+\\s?,\\s?(([^a-z]ns)|(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*))",
          txt,
          ignore.case = FALSE
        )[[1]]
      
      if (QLoc[1] != -1) {
        # Get raw text of t-values:
        QRaw <-
          substring(txt, QLoc, QLoc + attr(QLoc, "match.length") - 1)
        
        # remove commas (thousands separators)
        QRaw <- gsub("(?<=\\d),(?=\\d+)", "", QRaw, perl = TRUE)
        
        # Replace weird codings of a minus sign with actual minus sign:
        # First remove spaces
        QRaw <-
          gsub("(?<=\\=)\\s+(?=.*\\,)", "", QRaw, perl = TRUE)
        
        # Replace any weird string with a minus sign
        QRaw <-
          gsub("(?<=\\=)\\s?[^\\d\\.]+(?=.*\\,)", " -", QRaw, perl = TRUE)
        
        # Add spaces again:
        QRaw <-
          gsub("(?<=\\=)(?=(\\.|\\d))", " ", QRaw, perl = TRUE)
        
        # Extract type of Q-test (general, within, or between)
        QtypeLoc <-
          gregexpr("Q\\s?-?\\s?(w|within|b|between)?",
                   QRaw,
                   ignore.case = TRUE)
        QtypeRaw <-
          substring(QRaw,
                    sapply(QtypeLoc, '[', 1),
                    sapply(QtypeLoc, function(x)
                      x[1] + attr(x, "match.length")[1] - 1))
        
        Qtype <- rep(NA, length(QtypeRaw))
        
        Qtype[grepl("Q\\s?-?\\s?(w|within)", QtypeRaw, ignore.case = TRUE)] <-
          "Qw"
        Qtype[grepl("Q\\s?-?\\s?(b|between)", QtypeRaw, ignore.case = TRUE)] <-
          "Qb"
        Qtype[is.na(Qtype)] <- "Q"
        
        # Extract location of numbers:
        nums <-
          gregexpr("(\\-?\\s?\\d*\\.?\\d+\\s?e?-?\\d*)|ns",
                   QRaw,
                   ignore.case = TRUE)
        
        # Extract df:
        df <-
          as.numeric(substring(
            QRaw,
            sapply(nums, '[', 1),
            sapply(nums, function(x)
              x[1] + attr(x, "match.length")[1] - 1)
          ))
        
        # Extract Q-values
        suppressWarnings(QValsChar <-
                           substring(
                             QRaw,
                             sapply(nums, '[', 2),
                             sapply(nums, function(x)
                               x[2] + attr(x, "match.length")[2] - 1)
                           ))
        
        suppressWarnings(QVals <- as.numeric(QValsChar))
        
        # Extract number of decimals test statistic
        testdec <-
          attr(regexpr("\\.\\d+", QValsChar), "match.length") - 1
        testdec[testdec < 0] <- 0
        
        # Extract (in)equality test statistic
        testEqLoc <- gregexpr("\\)\\s?[<>=]", QRaw)
        testEq <- substring(
          QRaw,
          sapply(testEqLoc, function(x)
            x[1] + attr(x, "match.length")[1] - 1),
          sapply(testEqLoc, function(x)
            x[1] + attr(x, "match.length")[1] - 1)
        )
        
        # Extract p-values
        suppressWarnings(pValsChar <-
                           substring(
                             QRaw,
                             sapply(nums, '[', 3),
                             sapply(nums, function(x)
                               x[3] + attr(x, "match.length")[3] - 1)
                           ))
        
        suppressWarnings(pVals <- as.numeric(pValsChar))
        
        # Extract (in)equality
        eqLoc <-
          gregexpr("p\\s?[<>=]", QRaw, ignore.case = TRUE)
        pEq <- substring(QRaw,
                         sapply(eqLoc, function(x)
                           x[1] + attr(x, "match.length")[1] - 1),
                         sapply(eqLoc, function(x)
                           x[1] + attr(x, "match.length")[1] - 1))
        pEq[grepl("ns", QRaw, ignore.case = TRUE)] <- "ns"
        
        # determine number of decimals of p value
        dec <-
          attr(regexpr("\\.\\d+", pValsChar), "match.length") - 1
        dec[dec < 0] <- 0
        
        # Create data frame:
        QRes <- data.frame(
          Source = names(txt)[i],
          Statistic = Qtype,
          df1 = NA,
          df2 = df,
          Test.Comparison = testEq,
          Value = QVals,
          Reported.Comparison = pEq,
          Reported.P.Value = pVals,
          Location = QLoc,
          Raw = QRaw,
          stringsAsFactors = FALSE,
          dec = dec,
          testdec = testdec
        )
        
        # Append, clean and close:
        Res <- rbind(Res, QRes)
        rm(QRes)
      }
    }
    
    if(messages == TRUE){
      setTxtProgressBar(pb, i)
    }
  }
  
  # close progressbar
  if(messages == TRUE){
    close(pb)
  }
  
  Source <- NULL
  Res <- ddply(Res, .(Source), function(x)
    x[order(x$Location), ])
  
  if (nrow(Res) > 0) {
    # remove p values greater than one
    Res <- Res[Res$Reported.P.Value <= 1 |
                 is.na(Res$Reported.P.Value), ]
  }
  
  return(Res)
}
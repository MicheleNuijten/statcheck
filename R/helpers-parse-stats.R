# this script contains helper functions to extract and parse nhst results from 
# text

# function to extract snippets of text from a string ---------------------------

extract_pattern <- function(txt, pattern) {
  
  # extract the locations of the matches in the text:
  # gregexpr returns the position of every match in a string
  # if there are multiple matches in the text, gregexpr will flag them all
  # the output is in list format, but the relevant information is all in [[1]]
  string_loc <- gregexpr(pattern = pattern, 
                         text = txt, 
                         ignore.case = TRUE,
                         perl = TRUE)[[1]] # perl is necessary for lookbehinds
  
  # if no match is found, return NULL
  if(string_loc[1] == -1){
    return(NULL)
  }
  
  # if a match is found:
  # extract the raw text of the regex match:
  # retrieve a 'substring' from the text that starts at string_loc: string_loc
  # is a vector of integers that indicate the location of the first characters of
  # the match. The end point of the substring is determined using the 
  # attribute 'match.length' of string_loc This is a vector of integers indicating
  # how long each matched string is. By adding the length of the string to the
  # position of the first character, and subtracting 1, you obtain the location
  # of the last character of the string.
  string <- substring(text = txt, 
                      first = string_loc, 
                      last = string_loc + attr(string_loc, "match.length") - 1)
  
  return(string)
}

# function to extract dfs from a raw nhst result -------------------------------

extract_df <- function(raw, test_type){
  
  # z tests do not have dfs, so return df1 = NA, and df2 = NA for z-tests
  if(test_type == "Z"){
    
    df1 <- NA
    df2 <- NA
    
  } else {
    # for all other test types, extract dfs from the raw nhst result
    df_raw <- extract_pattern(txt = raw,
                              pattern = RGX_DF)[[1]]
    
    # remove parentheses to only keep numbers
    df <- gsub("\\(|\\)", "", df_raw)
    
    # split string on comma to separate df1 and df2 / N
    # note: there can be no commas as thousand separators in the dfs; statcheck
    # would not have recognized these in the first place, so we don't have to take
    # this possibility into account here
    df <- strsplit(df, ",")[[1]]
    
    # remove leading/trailing whitespaces
    df <- trimws(df, which = "both")
    
    # there are three different types of degrees of freedom:
    # - t-tests, correlations, and Q-tests (a single number between brackets)
    # - F-tests (two degrees of freedom separated by a comma)
    # - chi2 (can also contain sample size)
    
    if(test_type %in% c("t", "r")){
      
      df1 <- NA
      df2 <- df
      
    } else if(test_type == "F"){
      
      df1 <- df[1]
      df2 <- df[2]
      
    } else if(test_type %in% c("Chi2", "Q", "Qw", "Qb")){
      
      df1 <- df[1]
      df2 <- NA
      
    } else {
      
      df1 <- NA
      df2 <- NA
      
    }
  }
  
  # return dfs as numeric values (to avoid R turning them into factors)
  return(data.frame(df1 = as.numeric(df1), 
                    df2 = as.numeric(df2)))
  
}

# function to remove commas that serve as thousands separators -----------------

remove_1000_sep <- function(raw){
  
  # replace all matches in the raw nhst results with nothing
  output <- gsub(pattern = RGX_1000_SEP,
                 replacement = "", 
                 x = raw,
                 perl = TRUE) # for the lookaheads & lookbehinds in the regex
  
  return(output)
  
}

# function to replace weird symbols with a minus sign --------------------------

# sometimes the mathematical symbol for a minus sign is wrongly converted into
# a strange symbol. Since it is very likely that any weird symbol in front of
# a test statistic is in fact a minus sign, replace all such weird codings with
# an actual minus sign

recover_minus_sign <- function(raw){
  
  # replace any weird string before the test value with a minus sign
  return(gsub(RGX_WEIRD_MINUS, " -", raw, perl = TRUE))
  
}



# function to extract test-values and test comparisons -------------------------

extract_test_stats <- function(raw){
  
  test_raw <- extract_pattern(txt = raw,
                              pattern = RGX_TEST_VALUE)
  
  # extract test comparison
  test_comp <- extract_pattern(txt = test_raw,
                               pattern = RGX_COMP)
  
  # remove test comparison to only keep numbers
  test_value <- gsub(RGX_COMP, "", test_raw)
  
  # remove thousand separators
  test_value <- remove_1000_sep(test_value)
  
  # replace weird coding before a test value with a minus sign
  test_value <- recover_minus_sign(test_value)
  
  # remove leading/trailing whitespaces 
  test_value <- trimws(test_value, which = "both")
  
  # remove comma at the end of the value
  test_value <- gsub(",$", "", test_value)
  
  # record the number of decimals of the test statistic
  test_dec <- attr(regexpr(RGX_DEC, test_value), "match.length") - 1
  test_dec[test_dec < 0] <- 0
  
  return(data.frame(test_comp = test_comp,
                    test_value = as.numeric(test_value),
                    test_dec = test_dec,
                    stringsAsFactors = FALSE))
  
}

# function to extract and parse p-values --------------------------------------

extract_p_value <- function(raw){
  
  p_raw <- extract_pattern(txt = raw,
                           pattern = RGX_P_NS)
  
  p_comp <- character()
  p_value <- numeric()
  p_dec <- numeric()
  
  for(i in seq_along(p_raw)){
    
    if(grepl(RGX_NS, p_raw[i], ignore.case = TRUE)){
      
      p_comp[i] <- "ns"
      p_value[i] <- NA
      p_dec[i] <- NA
      
    } else {
      
      # extract p-comparison
      p_comp[i] <- extract_pattern(txt = p_raw[i],
                                pattern = RGX_COMP)
      
      # remove p comparison to only keep numbers
      # split the string on the comparison, that splits the string into a p and
      # the actual value. Only select the second element: the value
      p_value[i] <- strsplit(p_raw[i], RGX_COMP)[[1]][2]
      
      # remove leading/trailing whitespaces 
      p_value[i] <- trimws(p_value[i], which = "both")
      
      # record the number of decimals of the p-value
      dec <- attr(regexpr(RGX_DEC, p_value[i]), "match.length") - 1
      dec[dec < 0] <- 0
      
      p_dec[i] <- dec
      
    }
  }
  
  return(data.frame(p_comp = p_comp,
                    p_value = as.numeric(p_value),
                    p_dec = p_dec,
                    stringsAsFactors = FALSE))
  
}

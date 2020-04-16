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

extract_df <- function(raw, pattern, test_type){
  
  df_raw <- extract_pattern(txt = raw,
                            pattern = pattern)[[1]]
  
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
  
  if(test_type %in% c("t", "r", "Q", "Qw", "Qb")){
    
    df1 <- NA
    df2 <- df
    
  } else if(test_type == "F"){
    
    df1 <- df[1]
    df2 <- df[2]
    
  } else if(test_type == "chi2"){
    
    df1 <- df[1]
    df2 <- NA
    
  } else if(test_type == "z"){
    
    df1 <- NA
    df2 <- NA
    
  }
  
  # return dfs as numeric values (to avoid R turning them into factors)
  return(data.frame(df1 = as.numeric(df1), 
                    df2 = as.numeric(df2)))
  
}

# function to remove commas that serve as thousands separators -----------------

remove_1000_sep <- function(raw){
  
  # this regex matches commas flanked by digits on both sides
  regex_thousands_sep <- "(?<=\\d),(?=\\d+)"
  
  # replace all matches in the raw nhst results with nothing
  output <- gsub(pattern = regex_thousands_sep,
                 replacement = "", 
                 x = raw,
                 perl = TRUE) # for the lookaheads & lookbehinds in the regex
  
  return(output)
  
}

# function to extract test-values and test comparisons -------------------------

extract_test_stats <- function(raw, pattern){
  
  regex_comp <- "[<>=]"
  
  test_raw <- extract_pattern(txt = raw,
                              pattern = pattern)
  
  # extract test comparison
  test_comp <- extract_pattern(txt = test_raw,
                               pattern = regex_comp)
  
  # remove test comparison to only keep numbers
  test_value <- gsub(regex_comp, "", test_raw)
  
  # remove thousand separators
  test_value <- remove_1000_sep(test_value)
  
  # remove leading/trailing whitespaces 
  test_value <- trimws(test_value, which = "both")
  
  # remove comma at the end of the value
  test_value <- gsub(",$", "", test_value)
  
  return(data.frame(test_comp = test_comp,
                    test_value = as.numeric(test_value)))
  
}

# function to extract p-values and p comparisons -------------------------------

extract_p_value <- function(raw, pattern){
  
  regex_comp <- "[<>=]"
  
  p_raw <- extract_pattern(txt = raw,
                           pattern = pattern)
  
  if(grepl(ns, p_raw, ignore.case = FALSE)){
    
    p_comp <- "ns"
    p_value <- NA
    
  } else {
    
    # extract p-comparison
    p_comp <- extract_pattern(txt = p_raw,
                              pattern = regex_comp)
    
    # remove p comparison to only keep numbers
    # split the string on the comparison, that splits the string into a p and
    # the actual value. Only select the second element: the value
    p_value <- strsplit(p_raw, regex_comp)[[1]][2]
    
    # remove leading/trailing whitespaces 
    p_value <- trimws(p_value, which = "both")
    
  }
  
  return(data.frame(p_comp = p_comp,
                    p_value = as.numeric(p_value)))
  
}

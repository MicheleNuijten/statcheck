# this script contains the regular expressions that statcheck uses to extract
# and parse NHST results from text

################################################################################
########## MAIN REGEXES TO EXTRACT APA REPORTED NHST RESULTS FROM TEXT #########
################################################################################

# test types -----------------------------------------
RGX_T <- "t"
RGX_R <- "r"
RGX_Q <- "Q\\s?-?\\s?(w|within|b|between)?"
RGX_F <- "F"
RGX_CHI2 <- 
  "((\\[CHI\\]|\\[DELTA\\]G)\\s?|(\\s[^trFzQWBnD ]\\s?)|([^trFzQWBnD ]2\\s?))2?"
RGX_Z <- "([^a-z](z|Z))"

# degrees of freedom ---------------------------------

# the way dfs are reported differs per test type, except for t, r, and Q, where
# they are always in the format "(28)". The regex for these tests can therefore
# be combined
# z-tests do not have dfs

# regex for parentheses
# make this flexible to allow replacement with other bracket types for non-APA 
RGX_PRTS_1 <- "\\("
RGX_PRTS_2 <- "\\)"

# combine into full df regexes
RGX_DF_T_R_Q <- 
  paste0(RGX_PRTS_1, "\\s?\\d*\\.?\\d+\\s?", RGX_PRTS_2)
RGX_DF_F <- 
  paste0(RGX_PRTS_1,"\\s?\\d*\\.?(I|l|\\d+)\\s?,\\s?\\d*\\.?\\d+\\s?", 
         RGX_PRTS_2)
RGX_DF_CHI2 <- 
  paste0(RGX_PRTS_1, 
         "\\s?\\d*\\.?\\d+\\s?(,\\s?N\\s?\\=\\s?\\d*\\,?\\d*\\,?\\d+\\s?)?", 
         RGX_PRTS_2)

# combine test types with the correct type of df -----
# put regex between () to create regex groups
RGX_T_DF <- paste0("(", RGX_T, "\\s?", RGX_DF_T_R_Q, ")")
RGX_R_DF <- paste0("(", RGX_R, "\\s?", RGX_DF_T_R_Q, ")")
RGX_Q_DF <- paste0("(", RGX_Q, "\\s?", RGX_DF_T_R_Q, ")")
RGX_F_DF <- paste0("(", RGX_F, "\\s?", RGX_DF_F, ")")
RGX_CHI2_DF <- paste0("(", RGX_CHI2, "\\s?", RGX_DF_CHI2, ")")

RGX_TEST_DF <- paste0("(", RGX_T_DF, "|", RGX_R_DF, "|", RGX_Q_DF, "|", 
                      RGX_F_DF, "|", RGX_CHI2_DF, "|", RGX_Z, ")")

# test value ------------------------------------------

# this is the same for every type of test
# the snippet [^a-z\\d]{0,3} searches for weird symbols that could indicate a
# minus sign
RGX_TEST_VALUE <- "[<>=]\\s?[^a-z\\d]{0,3}\\s?\\d*,?\\d*\\.?\\d+\\s?,"

# combine test type, df, value ------------------------

RGX_TEST_DF_VALUE <- paste(RGX_TEST_DF, RGX_TEST_VALUE, sep = "\\s?")

# p-values --------------------------------------------

# this is the same for every type of test
RGX_NS <- "([^a-z]n\\.?s\\.?)"
RGX_P <- "(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*)"
RGX_P_NS <- paste0("(", RGX_NS, "|", RGX_P, ")")

# full result ----------------------------------------
RGX_NHST <- paste(RGX_TEST_DF_VALUE, RGX_P_NS, sep = "\\s?")

################################################################################
####################### HELPER REGEXES TO PARSE NHST ###########################
################################################################################

# regex to recognize test type ------------------------

# match everything up until the first occurence of a "(" with a positive look
# ahead. A "(" signals the start of the degrees of freedom, so everything before
# that should be the test statistic. Also match the regex for a z-test 
# (because a z-test has no df)

RGX_OPEN_BRACKET <- "(.+?(?=\\())"
RGX_TEST_TYPE <- paste(RGX_Z, RGX_OPEN_BRACKET, sep = "|")

# regex for Q-test ------------------------------------

# for the Q-test, we also need to distinguish between Q, Qw, and Qb
# select all raw_nhst results that seem to have a Q-test in them
# it suffices to simply search for the letters "w" and "b"
RGX_QW <- "w"
RGX_QB <- "b"

# regex for degrees of freedom ------------------------

# combine the separate regexes for the different types of dfs
# in one all-encompassing regex. Group the df-types with parentheses and 
# separate with an OR sign
RGX_DF <- paste0("(", RGX_DF_T_R_Q, ")|(", RGX_DF_F, ")|(", RGX_DF_CHI2, ")")

# regex for comparison symbols ------------------------

RGX_COMP <- "[<>=]"

# regex for thousands separator -----------------------
# this regex matches commas flanked by digits on both sides
RGX_1000_SEP <- "(?<=\\d),(?=\\d+)"

# regex for numbers after a point ---------------------
# used to determine number of decimals
RGX_DEC <- "\\.\\d+"

# regex for weirdly converted minus signs -------------
# match potentially a space, followed by one or more characters that are not a 
# digit, period, or space, followed by a digit or period (using a positive 
# lookahead)
RGX_WEIRD_MINUS <- "\\s?[^\\d\\.\\s]+(?=\\d|\\.)"

# regex for weird df1 in F-tests ----------------------
# for some reason, typesetting in articles sometimes goes wrong with 
# F-tests and when df1 == 1, it gets typeset as the letter l or I 
RGX_DF1_I_L <- "I|l"

################################################################################
######################### REGEXES FOR NON-APA STYLE ############################
################################################################################

# F with MSE ------------------------------------------

RGX_MSE <- "MSE"
RGX_F_MSE <- paste(RGX_F_DF, RGX_TEST_VALUE, RGX_MSE, RGX_TEST_VALUE, RGX_P_NS, 
                   sep = "\\s?")

# Square or curly brackets ----------------------------

# regex for square or curly brackets
RGX_BRACK_1 <- "(\\[|\\{)"
RGX_BRACK_2 <- "(\\]|\\})"

# combine into full df regexes
RGX_DF_T_R_Q_BRACK <- 
  paste0(RGX_BRACK_1, "\\s?\\d*\\.?\\d+\\s?", RGX_BRACK_2)
RGX_DF_F_BRACK <- 
  paste0(RGX_BRACK_1,"\\s?\\d*\\.?(I|l|\\d+)\\s?,\\s?\\d*\\.?\\d+\\s?", 
         RGX_BRACK_2)
RGX_DF_CHI2_BRACK <- 
  paste0(RGX_BRACK_1, 
         "\\s?\\d*\\.?\\d+\\s?(,\\s?N\\s?\\=\\s?\\d*\\,?\\d*\\,?\\d+\\s?)?", 
         RGX_BRACK_2)

# combine test types with the correct type of df -----
# put regex between () to create regex groups
RGX_T_DF_BRACK <- paste0("(", RGX_T, "\\s?", RGX_DF_T_R_Q_BRACK, ")")
RGX_R_DF_BRACK <- paste0("(", RGX_R, "\\s?", RGX_DF_T_R_Q_BRACK, ")")
RGX_Q_DF_BRACK <- paste0("(", RGX_Q, "\\s?", RGX_DF_T_R_Q_BRACK, ")")
RGX_F_DF_BRACK <- paste0("(", RGX_F, "\\s?", RGX_DF_F_BRACK, ")")
RGX_CHI2_DF_BRACK <- paste0("(", RGX_CHI2, "\\s?", RGX_DF_CHI2_BRACK, ")")

RGX_TEST_DF_BRACK <- paste0("(", RGX_T_DF_BRACK, "|", RGX_R_DF_BRACK, "|", RGX_Q_DF_BRACK, "|", 
                      RGX_F_DF_BRACK, "|", RGX_CHI2_DF_BRACK, ")")

RGX_TEST_DF_VALUE_BRACK <- paste(RGX_TEST_DF_BRACK, RGX_TEST_VALUE, sep = "\\s?")

RGX_NHST_BRACK <- paste(RGX_TEST_DF_VALUE_BRACK, RGX_P_NS, sep = "\\s?")

### Combine all non-apa regexes with the apa regex -----------------------------

# This creates a complete regex that matches everything statcheck recognizes 

RGX_NHST_NONAPA <- paste0("(", RGX_NHST, "|", RGX_F_MSE, "|", RGX_NHST_BRACK, ")")

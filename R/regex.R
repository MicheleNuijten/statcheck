# this script contains the regular expressions that statcheck uses to extract
# NHST results from text

# test types
RGX_T <- "t"
RGX_R <- "r"
RGX_Q <- "Q\\s?-?\\s?(w|W|(w|W)ithin|b|B|(b|B)etween)?"
RGX_F <- "F"
RGX_CHI2 <- "((\\[CHI\\]|\\[DELTA\\]G)\\s?|(\\s[^trFzZQWnD ]\\s?)|([^trFzZQWnD ]2\\s?))2?"
RGX_Z <- "([^a-z](z|Z))"

# degrees of freedom
# the way dfs are reported differs per test type, except for t, r, and Q, where
# they are always in the format "(28)". The regex for these tests can therefore
# be combined
# z-tests do not have dfs
RGX_DF_T_R_Q <- "\\(\\s?\\d*\\.?\\d+\\s?\\)"
RGX_DF_F <- "\\(\\s?\\d*\\.?(I|l|\\d+)\\s?,\\s?\\d*\\.?\\d+\\s?\\)"
RGX_DF_CHI2 <- "\\(\\s?\\d*\\.?\\d+\\s?(,\\s?(N|n)\\s?\\=\\s?\\d*\\,?\\d*\\,?\\d+\\s?)?\\)"

# combine test types with the correct type of df
# put regex between () to create regex groups
RGX_T_DF <- paste0("(", RGX_T, "\\s?", RGX_DF_T_R_Q, ")")
RGX_R_DF <- paste0("(", RGX_R, "\\s?", RGX_DF_T_R_Q, ")")
RGX_Q_DF <- paste0("(", RGX_Q, "\\s?", RGX_DF_T_R_Q, ")")
RGX_F_DF <- paste0("(", RGX_F, "\\s?", RGX_DF_F, ")")
RGX_CHI2_DF <- paste0("(", RGX_CHI2, "\\s?", RGX_DF_CHI2, ")")

RGX_TEST_DF <- paste0("(", RGX_T_DF, "|", RGX_R_DF, "|", RGX_Q_DF, "|", RGX_F_DF, 
                      "|", RGX_CHI2_DF, "|", RGX_Z, ")")

# test value
# this is the same for every type of test
# the part "[^a-zA-Z\\d\\.]{0,3}" is to extract punctuation marks that could 
# signal a weirdly encoded minus sign
# note that impossible values such as r > 1 are excluded at a later stage
RGX_TEST_VALUE <- "[<>=]\\s?[^a-zA-Z\\d\\.]{0,3}\\s?\\d*,?\\d*\\.?\\d+\\s?,"

# p-values
# this is the same for every type of test
RGX_NS <- "([^a-z]n\\.?s\\.?)"
RGX_P <- "(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*)"
RGX_P_NS <- paste0("(", RGX_NS, "|", RGX_P, ")")

# full result
RGX_NHST <- paste(RGX_TEST_DF, RGX_TEST_VALUE, RGX_P_NS, sep = "\\s?")

################################################################################

# regex to recognize test type

# match everything up until the first occurence of a "(" with a positive look
# ahead. A "(" signals the start of the degrees of freedom, so everything before
# that should be the test statistic. Also match the regex for a z-test 
# (because a z-test has no df)

RGX_OPEN_BRACKET <- "(.+?(?=\\())"
RGX_TEST_TYPE <- paste(RGX_Z, RGX_OPEN_BRACKET, sep = "|")

# regex for Q-test

# for the Q-test, we also need to distinguish between Q, Qw, and Qb
# select all raw_nhst results that seem to have a Q-test in them
# it suffices to simply search for the letters "w" and "b"
RGX_QW <- "w"
RGX_QB <- "b"

# regex for degrees of freedom

# combine the separate regexes for the different types of dfs
# in one all-encompassing regex. Group the df-types with parentheses and 
# separate with an OR sign
RGX_DF <- paste0("(", RGX_DF_T_R_Q, ")|(", RGX_DF_F, ")|(", RGX_DF_CHI2, ")")

# regex for comparison symbols
RGX_COMP <- "[<>=]"

# regex for thousands separator
# this regex matches commas flanked by digits on both sides
RGX_1000_SEP <- "(?<=\\d),(?=\\d+)"

# regex for numbers after a point
# used to determine number of decimals
RGX_DEC <- "\\.\\d+"

# regex for weird symbols that should be a minus sign
# match potentially a space, followed by one or more characters that are not a 
# digit, period, or space, followed by a digit or period (using a positive 
# lookahead)
RGX_WEIRD_MINUS <- "\\s?[^\\d\\.\\s]+(?=\\d|\\.)"

# regex for weird df1 in F-tests
# for some reason, typesetting in articles sometimes goes wrong with 
# F-tests and when df1 == 1, it gets typeset as the letter l or I 
RGX_DF1_I_L <- "I|l"

################################################################################
###################### REGEXES FOR WEIRD PDF ENCODING ##########################
################################################################################

# in some JESP articles, a < is translated with a b
# this regex is used in file-to-txt.R to replace it
RGX_B_SMALLER <- "(?<![=<>])b(?=\\s?-?\\s?\\.?\\d)"

# in some JESP articles, a > is translated with a N
# this regex is used in file-to-txt.R to replace it
RGX_N_LARGER <- "(?<![=<>])N(?=\\s?-?\\s?\\.?\\d)"

# in the journal of consumer research, a = is translated with a p
# this regex is used in file-to-txt.R to replace it
RGX_P_EQUAL <- "(?<!(,\\s)|,)p(?=\\s?-?\\s?\\.?\\d)"

# in the Nuijten et al. 2016 article, quotes are translated as B
# this means that tests between quotes are not detected, because
# statcheck can only find tests if there are not preceded by other
# letters. Find upper case B followed by a test statistic
RGX_B_QUOTE <- "B(?=(t|F|r|Q|z|Z))"

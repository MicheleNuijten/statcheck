# this script contains the regular expressions that statcheck uses to extract
# and parse NHST results from text

################################################################################
########## MAIN REGEXES TO EXTRACT APA REPORTED NHST RESULTS FROM TEXT #########
################################################################################

# test types -----------------------------------------

# start of string
# only extract test statistics when they are the start of the "word"
# e.g., do extract t(14) = ..., but not Qt(14) = ... (the latter would be
# wrongly read as a t-test, whereas the t is only a subscript)
# use a negative lookbehind to only match test statistics not directly preceded
# by a letter (but do match test stats preceded by spaces or punctuation signs)
RGX_START <- "(?<![a-zA-Z])"

RGX_T <- paste0(RGX_START, "t")
RGX_R <- paste0(RGX_START, "r")
# (?i) = case insensitive mode
RGX_Q <- paste0(RGX_START, "Q\\s?-?\\s?(?i)(w|within|b|between)?") 
RGX_F <- paste0(RGX_START, "F")
RGX_Z <- paste0(RGX_START, "(?i)z")

# For chi2, the regex is a bit more complicated, because the actual greek letter
# is often not converted correctly
# for the greek letter, match literally typed:
# chi
# x or X
# G 
# or match
# any non-word character that is not a space (captures weird encoding)
# here, we can't use \\w, because that also includes underscores, and those
# we do want to match
# furthermore, don't match <>=
# or match
# a single 2
# followed by maybe a 2, maybe preceded by spaces
RGX_CHI2 <- "(D?(chi|x|X|G)|[^A-Za-z0-9\\s<>=]|^2)\\s?2?"

# degrees of freedom ---------------------------------

# the way dfs are reported differs per test type, except for t, r, and Q, where
# they are always in the format "(28)". The regex for these tests can therefore
# be combined
# z-tests do not have dfs

# regex for parentheses
# make this flexible to allow replacement with other bracket types for non-APA 
RGX_PRTS_1 <- "\\("
RGX_PRTS_2 <- "\\)"

# regex for df themselves
RGX_DF_T_R_Q_NRS <- "\\s?\\d*\\.?\\d+\\s?"

RGX_SEP <- ","

RGX_DF1_F_NRS <- "\\s?\\d*\\.?\\d+\\s?"
RGX_DF2_F_NRS <- "\\s?\\d*\\.?\\d+\\s?"
RGX_DF_F_NRS <- paste0(RGX_DF1_F_NRS, RGX_SEP, RGX_DF2_F_NRS)

RGX_DF1_CHI2_NRS <- "\\s?\\d*\\.?\\d+\\s?"
RGX_DFN_CHI2_NRS <- "\\s?(N|n)\\s?\\=\\s?\\d*\\,?\\d*\\,?\\d+\\s?"
RGX_DF_CHI2_NRS <- paste0(RGX_DF1_CHI2_NRS, "(", RGX_SEP, RGX_DFN_CHI2_NRS, ")?")

# combine into full df regexes with parentheses
RGX_DF_T_R_Q <- 
  paste0(RGX_PRTS_1, RGX_DF_T_R_Q_NRS, RGX_PRTS_2)
RGX_DF_F <- 
  paste0(RGX_PRTS_1,RGX_DF_F_NRS, RGX_PRTS_2)
RGX_DF_CHI2 <- 
  paste0(RGX_PRTS_1, RGX_DF_CHI2_NRS, RGX_PRTS_2)

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
# minus sign. Don't search for <>= again, otherwise cases like <= .1 would be
# extracted as well, and these can't be parsed
RGX_TEST_VALUE <- "[<>=]\\s?[^a-z\\d<>=]{0,3}\\s?\\d*,?\\d*\\.?\\d+\\s?"

# combine test type, df, value ------------------------

RGX_TEST_DF_VALUE <- paste0(RGX_TEST_DF, "\\s?", RGX_TEST_VALUE, RGX_SEP)

# p-values --------------------------------------------

# this is the same for every type of test
RGX_NS <- paste0("(", RGX_START, "(n|N)\\.?(s|S)\\.?)")
RGX_P <- "(p\\s?[<>=]\\s?\\d?\\.\\d+(e-?\\d*)?)"
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
RGX_QW <- "(?i)w"
RGX_QB <- "(?i)b"

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
RGX_MINUS_SPACE <- "-\\s"

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

################################################################################
######################### REGEXES FOR NON-APA STYLE ############################
################################################################################

# General changes:
# 1. allow for multiple spaces (e.g., t(23)  = ...)
# 2. allow for spaces in weird places (e.g., p <. 001)

# 1. Regexes for test types -----------------------------

# No systematic deviations from apa in test stat (type)
# Keep regexes for apa test statistics as defined above:
# RGX_T
# RGX_R
# RGX_Q
# RGX_F
# RGX_Z
# RGX_CHI2

# 2. Regexes for degrees of freedom --------------------

# regex for normal, square, curly, or no brackets
# define curly and square separately, because these need to be replaced when
# cleaning non-apa results
RGX_SQ_CURLY1 <- "\\[|\\{"
RGX_SQ_CURLY2 <- "\\]|\\}"

RGX_BRACK_1 <- paste0("(\\(|", RGX_SQ_CURLY1, ")?")
RGX_BRACK_2 <- paste0("(\\)|", RGX_SQ_CURLY2, ")?")

# define separate brackets for chi2, because these dfs are only matched when
# they're in between some type of brackets. If numbers after the "chi" are
# also matched without brackets (the same as with other tests, when dfs are
# reported as subscripts), the "2" of "chi2" is considered as df when no other
# dfs are reported (e.g., "X2 = 93.3, p < .01")

RGX_BRACK_CHI2_1 <- paste0("(\\(|", RGX_SQ_CURLY1, ")")
RGX_BRACK_CHI2_2 <- paste0("(\\)|", RGX_SQ_CURLY2, ")")

# regex for "DF" in degrees of freedom
# matches cases such as t(DF = 29) = ...
RGX_DF_TXT <- "((df|DF)\\s*=\\s*)?"

# regex for df themselves

# regex for t and Q
# add extra spacing
RGX_DF_T_R_Q_NRS <- "\\s*\\d*\\s*\\.?\\s*\\d+\\s*"

# Allow for sample size instead of df in correlations
# E.g, r(N = 95) = .12, p < .05
RGX_DFN_R_NRS <- "\\(\\s*(N|n)\\s*\\=\\s*\\d+"
RGX_DF_R_NRS <- paste0("(", RGX_DF_T_R_Q_NRS, "|", RGX_DFN_R_NRS, ")")

# For df of F and chi2, also allow for ; to separate dfs
# Also later on, allow for ; as a general separator for test value & p-value
RGX_SEP_NONAPA <- "(,|;)"

# add extra spacing to df for F and chi2
RGX_DF1_F_NRS_NONAPA <- "\\s*\\d*\\s*\\.?\\s*\\d+\\s*"
RGX_DF2_F_NRS_NONAPA <- "\\s*\\d*\\s*\\.?\\s*\\d+\\s*"
RGX_DF_F_NRS_NONAPA <- paste0(RGX_DF1_F_NRS_NONAPA, RGX_SEP_NONAPA, 
                              RGX_DF2_F_NRS_NONAPA)

RGX_DF1_CHI2_NRS_NONAPA <- "\\s*\\d*\\s*\\.?\\s*\\d+\\s*"
RGX_DFN_CHI2_NRS_NONAPA <- "\\s*N\\s*\\=\\s*\\d*\\s*\\,?\\s*\\d*\\s*\\,?\\s*\\d+\\s*"
RGX_DF_CHI2_NRS_NONAPA <- paste0(RGX_DF1_CHI2_NRS_NONAPA, "(", RGX_SEP_NONAPA, 
                                 RGX_DFN_CHI2_NRS_NONAPA, ")?")

# Combine into full df regexes
RGX_DF_R_BRACK <- 
  paste0(RGX_BRACK_1, RGX_DF_TXT, RGX_DF_R_NRS, RGX_BRACK_2)
RGX_DF_T_Q_BRACK <- 
  paste0(RGX_BRACK_1, RGX_DF_TXT, RGX_DF_T_R_Q_NRS, RGX_BRACK_2)
RGX_DF_F_BRACK <- 
  paste0(RGX_BRACK_1, RGX_DF_TXT, RGX_DF_F_NRS_NONAPA, RGX_BRACK_2)
RGX_DF_CHI2_BRACK <- 
  paste0(RGX_BRACK_CHI2_1, RGX_DF_TXT, RGX_DF_CHI2_NRS_NONAPA, RGX_BRACK_CHI2_2)

# Put regex between () to create regex groups
RGX_T_DF_BRACK <- paste0("(", RGX_T, "\\s*", RGX_DF_T_Q_BRACK, ")")
RGX_R_DF_BRACK <- paste0("(", RGX_R, "\\s*", RGX_DF_R_BRACK, ")")
RGX_Q_DF_BRACK <- paste0("(", RGX_Q, "\\s*", RGX_DF_T_Q_BRACK, ")")
RGX_F_DF_BRACK <- paste0("(", RGX_F, "\\s*", RGX_DF_F_BRACK, ")")
RGX_CHI2_DF_BRACK <- paste0("(", RGX_CHI2, "\\s*", RGX_DF_CHI2_BRACK, ")")

# combine all combinations of test type and df into 1 regex
RGX_TEST_DF_BRACK <- paste0("(", 
                            RGX_T_DF_BRACK, "|", RGX_R_DF_BRACK, "|", 
                            RGX_Q_DF_BRACK, "|", RGX_F_DF_BRACK, "|", 
                            RGX_CHI2_DF_BRACK, "|", RGX_Z,
                            ")")

# 3. Regexes for test statistics (values) --------------

# add spaces to test value regex

RGX_TEST_VALUE_NONAPA <- 
  "[<>=]\\s*[^a-z\\d<>=]{0,3}\\s*\\d*\\s*,?\\s*\\d*\\s*\\.?\\s*\\d+\\s*"

RGX_TEST_DF_BRACK_VALUE <- paste(RGX_TEST_DF_BRACK, RGX_TEST_VALUE_NONAPA, 
                                 sep = "\\s*")

# 4. Regexes for p-values ------------------------------

# add more spaces
# this is the same for every type of test
RGX_NS_NONAPA <- paste0("(", RGX_START, "(n|N)\\s*\\.?\\s*(s|S)\\s*\\.?)")
RGX_P_NONAPA <- "(p\\s*[<>=]\\s*\\d?\\s*\\.\\s*\\d+\\s*(e\\s*-?\\s*\\d*)?)"
RGX_P_NS_NONAPA <- paste0("(", RGX_NS_NONAPA, "|", RGX_P_NONAPA, ")")

# 5. Combining full regex -----------------------------

# recognize MSE in between test statistic and p-value. 
# effectively, just search for the letters MSE followed by numbers
# for the numbers we can use the same regex as for the test values
# add a ? at the end: also match stats without MSE in it
RGX_MSE <- paste0("(MSE\\s*", RGX_TEST_VALUE_NONAPA, RGX_SEP_NONAPA, ")?")

RGX_NHST_NONAPA <- paste(RGX_TEST_DF_BRACK_VALUE, RGX_SEP_NONAPA,
                         RGX_MSE, RGX_P_NS_NONAPA, sep = "\\s*")
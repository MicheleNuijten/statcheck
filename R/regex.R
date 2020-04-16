# this script contains the regular expressions that statcheck uses to extract
# NHST results from text

# test types
ttest <- "t"
cor <- "r"
Qtest <- "Q\\s?-?\\s?(w|within|b|between)?"
Ftest <- "F"
chi2 <- "((\\[CHI\\]|\\[DELTA\\]G)\\s?|(\\s[^trFzQWBnD ]\\s?)|([^trFzQWBnD ]2\\s?))2?"
ztest <- "([^a-z]z)"

# degrees of freedom
# the way dfs are reported differs per test type, except for t, r, and Q, where
# they are always in the format "(28)". The regex for these tests can therefore
# be combined
# z-tests do not have dfs
df_t_r_Q <- "\\(\\s?\\d*\\.?\\d+\\s?\\)"
df_F <- "\\(\\s?\\d*\\.?(I|l|\\d+)\\s?,\\s?\\d*\\.?\\d+\\s?\\)"
df_chi2 <- "\\(\\s?\\d*\\.?\\d+\\s?(,\\s?N\\s?\\=\\s?\\d*\\,?\\d*\\,?\\d+\\s?)?\\)"

# combine test types with the correct type of df
# put regex between () to create regex groups
ttest_df <- paste0("(", ttest, "\\s?", df_t_r_Q, ")")
cor_df <- paste0("(", cor, "\\s?", df_t_r_Q, ")")
Qtest_df <- paste0("(", Qtest, "\\s?", df_t_r_Q, ")")
Ftest_df <- paste0("(", Ftest, "\\s?", df_F, ")")
chi2_df <- paste0("(", chi2, "\\s?", df_chi2, ")")

rgx_test_df <- paste0("(", ttest_df, "|", cor_df, "|", Qtest_df, "|", Ftest_df, 
                      "|", chi2_df, "|", ztest, ")")

# test value
# this is the same for every type of test
rgx_test_value <- "[<>=]\\s?[^a-z\\d]{0,3}\\s?\\d*,?\\d*\\.?\\d+\\s?,"

# p-values
# this is the same for every type of test
ns <- "([^a-z]n\\.?s\\.?)"
p_val <- "(p\\s?[<>=]\\s?\\d?\\.\\d+e?-?\\d*)"
rgx_p_val_ns <- paste0("(", ns, "|", p_val, ")")

# full result
nhst <- paste(rgx_test_df, rgx_test_value, rgx_p_val_ns, sep = "\\s?")

################################################################################

# regex to recognize test type

# match everything up until the first occurence of a "(" with a positive look
# ahead. A "(" signals the start of the degrees of freedom, so everything before
# that should be the test statistic. Also match the regex for a z-test 
# (because a z-test has no df)

regex_opening_parenthesis <- "(.+?(?=\\())"
regex_test_type <- paste(ztest, regex_opening_parenthesis, sep = "|")

# regex for Q-test

# for the Q-test, we also need to distinguish between Q, Qw, and Qb
# select all raw_nhst results that seem to have a Q-test in them
# it suffices to simply search for the letters "w" and "b"
rgx_Qw <- "w"
rgx_Qb <- "b"

# regex for degrees of freedom

# combine the separate regexes for the different types of dfs
# in one all-encompassing regex. Group the df-types with parentheses and 
# separate with an OR sign
regex_df <- paste0("(", df_t_r_Q, ")|(", df_F, ")|(", df_chi2, ")")


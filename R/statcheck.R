#' Extract statistics and recompute p-values
#' 
#' \code{statcheck} extracts Null Hypothesis Significance (NHST) results from 
#' strings and returns the extracted values, reported p-values and recomputed 
#' p-values. 
#' 
#' 
#' \code{statcheck} roughly works in three steps.
#' 
#' \strong{1. Scan text for statistical results}
#' 
#' \code{statcheck} uses regular expressions to recognizes statistical results 
#' from t-tests, F-tests, \eqn{\chi2}-tests, Z-tests, Q-tests, and correlations. 
#' statcheck can only recognize these results if the results are reported 
#' exactly according to the APA guidelines:
#' \itemize{
#'     \item \emph{t}(df) = value, \emph{p} = value
#'     \item \emph{F}(df1, df2) = value, \emph{p} = value
#'     \item \emph{r}(df) = value, p = value
#'     \item \emph{\eqn{\chi2}} (df, N = value) = value, \emph{p} = value 
#'     (N is optional)
#'     \item \emph{Z} = value, \emph{p} = value
#'     \item \emph{Q}(df) = value, \emph{p} = value (statcheck can distinguish
#'     between Q, Qw / Q-within, and Qb / Q-between)
#' }
#' \code{statcheck} takes into account that test statistics and p values may be 
#' exactly (=) or inexactly (< or >) reported. Different spacing has also been 
#' taken into account.
#' 
#' \strong{2. Recompute p-value}
#' 
#' \code{statcheck} uses the reported test statistic and degrees of freedom to
#' recompute the p-value. By default, the recomputed p-value is two-sided
#' 
#' \strong{3. Compare reported and recomputed p-value}
#' 
#' This comparison takes into account how the results were reported, e.g., 
#' p < .05 is treated differently than p = .05. Incongruent p values are marked 
#' as an \code{error}. If the reported result is significant and the recomputed 
#' result is not, or vice versa, the result is marked as a 
#' \code{decision_error}.
#' 
#' Correct rounding is taken into account. For instance, a reported t-value of 
#' 2.35 could correspond to an actual value of 2.345 to 2.354 with a range of 
#' p-values that can slightly deviate from the recomputed p-value. 
#' \code{statcheck} will not count cases like this as errors.
#' 
#' Note that when \code{statcheck} flags an \code{error} or 
#' \code{decision_error}, it implicitly assumes that the p-value is the 
#' inconsistent value, but it could just as well be the case that the test 
#' statistic or degrees of freedom contain a reporting error. \code{statcheck}
#' merely detects wether a set of numbers is consistent with each other.
#' 
#' @seealso
#' For more details, see the 
#' \href{https://rpubs.com/michelenuijten/statcheckmanual}{online manual}.
#' 
#' @param texts A vector of strings.
#' @param OneTailedTxt Logical. If TRUE, statcheck searches the text for 
#' "one-sided", "one-tailed", and "directional" to identify the possible use of 
#' one-sided tests. If one or more of these strings is found in the text AND the 
#' result would have been correct if it was a one-sided test, the result is 
#' assumed to be indeed one-sided and is counted as correct.
#' @param apa_style Logical. Should statcheck only extract NHST results reported
#' exactly in APA style? Defaults to TRUE. If FALSE, statcheck will also search 
#' for a non-exhaustive set of variations on APA style (e.g., degrees of freedom
#' reported between square brackets instead of parentheses, semi-colons instead
#' of commas, etc.).
#' @param alpha Assumed level of significance in the scanned texts. Defaults to 
#' .05.
#' @param pEqualAlphaSig Logical. If TRUE, statcheck counts p <= alpha as
#' significant (default), if FALSE, statcheck counts p < alpha as significant.
#' @param pZeroError Logical. If TRUE, statcheck counts p = .000 as an error 
#' (because a p-value is never exactly zero, and should be reported as < .001), 
#' if FALSE, statcheck does not count p = .000 automatically as an error.
#' @param stat Specify which test types you want to extract. "t" to extract 
#' t-values, "F" to extract F-values, "cor" to extract correlations, "chisq"to 
#' extract \eqn{\chi2} values, "Z" to extract Z-values, and "Q" to extract 
#' Q-values. Using \code{c()} you can specify multiple tests. Defaults to all
#' tests.
#' @param OneTailedTests Logical. Do you want to assume that all reported tests 
#' are one-tailed (TRUE) or two-tailed (FALSE, default)? 
#' @param AllPValues Logical. If TRUE, the output will consist of a dataframe 
#' with all detected p values, also the ones that were not part of the full 
#' results in APA format.
#' @param messages Logical. If TRUE, statcheck will print a progress bar while 
#' it's extracting statistics from text.
#' 
#' @return A data frame containing for each extracted statistic:
#' \describe{
#'     \item{source}{Name of the file of which the statistic is extracted}
#'     \item{test_type}{Character indicating the statistic that is extracted}
#'     \item{df1}{First degree of freedom (if applicable)}
#'     \item{df2}{Second degree of freedom}
#'     \item{test_comp}{Reported comparison of the test statistic, when 
#'     importing from pdf this will often not be converted properly}
#'     \item{test_value}{Reported value of the statistic}
#'     \item{p_comp}{Reported comparison, when importing from pdf this might not 
#'     be converted properly}
#'     \item{reported_p}{The reported p-value, or NA if the reported value was 
#'     n.s.}
#'     \item{computed_p}{The recomputed p-value}
#'     \item{raw}{Raw string of the statistical reference that is extracted}
#'     \item{error}{The computed p value is not congruent with the reported 
#'     p-value}
#'     \item{decision_error}{The reported result is significant whereas the 
#'     recomputed result is not, or vice versa.}
#'     \item{one_tailed_in_txt}{Logical. Does the text contain the string 
#'     "sided", "tailed", and/or "directional"?}
#'     \item{apa_factor}{What proportion of all detected p-values was part of a 
#'     fully APA reported result?}
#' }
#' 
#' @examples 
#' txt <- "blablabla the effect was very significant (t(100)=1, p < 0.001)"
#' statcheck(txt)
#' 
#' @export


statcheck <- function(texts,
                      OneTailedTxt = FALSE,
                      apa_style = TRUE,
                      alpha = .05,
                      pEqualAlphaSig = TRUE,
                      pZeroError = TRUE,
                      stat = c("t", "F", "cor", "chisq", "Z", "Q"),
                      OneTailedTests = FALSE,
                      AllPValues = FALSE,
                      messages = TRUE){
  
  # We need empty data frames to store extracted statistics in
  # One for NHST results (Res) and one for p-values (pRes)
  Res <- data.frame(NULL)
  pRes <- data.frame(NULL)
  
  # to indicate where the statistics came from, we need a name for the input
  # texts. In some cases, this is the name of the file the text came from, but
  # if the text has no name, number them
  if (is.null(names(texts))){
    names(texts) <-  seq_along(texts)
  }
  
  
  # start progress bar. If the argument messages == FALSE, don't print this 
  # progress bar. This is mainly useful for the unit tests; otherwise hundreds
  # of progress bars would be printed during testing and that makes the test 
  # results hard to read
  if(messages == TRUE){
    message("Extracting statistics...")
    pb <- utils::txtProgressBar(max = length(text), style = 3)
  }
  
  # for each text in the vector of input texts, extract all p-values and all
  # NHST results
  for (i in seq_along(texts)) {
    txt <- texts[i]
    
    # extract p-values ------------------------------------------
    
    # extract all p values. This is based on a pretty rough regular expression 
    # that will extract anything that resembles p =<> .... We need this info 
    # later on to calculate the APA factor: the ratio (statcheck results)/
    # (total # of p values). It is also possible to let statcheck return this
    # dataframe instead of the data frame with NHST results.
    pvalues <- detect_p_values(txt, apa_style = apa_style)
    
    # append and close:
    # in each repetition of the loop, the extracted p-values are appended 
    # to the existing pRes data frame, so it grows in each step
    if(length(pvalues) > 0){
      pvalues <- cbind(Source = names(txt), pvalues)
      
      pRes <- rbind(pRes, pvalues)
    }
    
    # after appending the pvalues dataframe to the main pRes dataframe,
    # the temporary dataframe pvalues can be removed. 
    rm(pvalues)
    
    # extract NHST results ------------------------------------------
    
    # extract all NHST results. This function scrapes the text for all APA 
    # reported NHST results and parses it so that the separate elements are
    # returned in one large dataframe
    nhst <- extract_stats(txt = txt,
                          apa_style = apa_style,
                          stat = stat)
    
    # append and close: same logic as for the pvalues dataframe above
    if(nrow(nhst) > 0){
      
      nhst$Source <- names(txt)
      nhst$OneTailedInTxt <- extract_1tail(txt)
      
      Res <- rbind(Res, nhst)
      rownames(Res) <- NULL # to avoid having duplicated row names (1, 1, 2 etc.)
    }
    
    rm(nhst)
    
    # update the progress bar
    if(messages == TRUE){
      utils::setTxtProgressBar(pb, i)
    }
    
  }
  
  # close progress bar
  if(messages == TRUE){
    close(pb)
  }
  
  ###---------------------------------------------------------------------
  
  if (nrow(Res) > 0) {
    
    # If the argument OneTailedTests == TRUE, it forces statcheck to treat 
    # every encountered NHST result as a one-tailed test. Note: this is not the
    # same as the automated 1-tailed test detection (switched on with the 
    # argument: OneTailedTxt). The latter works more subtly (see comments in 
    # process_stats()). 
    if (OneTailedTests == TRUE) {
      two_tailed <- FALSE
    } else {
      two_tailed <- TRUE
    }
    
    # create empty variables to fill out during the loop
    Res$Computed <- rep(NA, nrow(Res))
    Res$Error <- rep(NA, nrow(Res))
    Res$DecisionError <- rep(NA, nrow(Res))
    
    # row by row, process the extracted statistics in Res. Specifically,
    # compute the p-value, check if the result is an error and a decision error,
    # and if indicated in the options, check & correct for 1-tailed tests
    for(i in seq_len(nrow(Res))){
      
      result <- process_stats(test_type = Res$Statistic[i],
                              test_stat = Res$Value[i],
                              df1 = Res$df1[i], 
                              df2 = Res$df2[i],
                              reported_p = Res$Reported.P.Value[i],
                              p_comparison = Res$Reported.Comparison[i],
                              test_comparison = Res$Test.Comparison[i],
                              p_dec = Res$dec[i],
                              test_dec = Res$testdec[i],
                              OneTailedInTxt = Res$OneTailedInTxt[i],
                              # options:
                              two_tailed = two_tailed,
                              alpha = alpha,
                              pZeroError = pZeroError,
                              pEqualAlphaSig = pEqualAlphaSig,
                              OneTailedTxt = OneTailedTxt,
                              OneTailedTests = OneTailedTests)
      
      Res$Computed[i] <- result$computed_p
      Res$Error[i] <- result$error
      Res$DecisionError[i] <- result$decision_error
    }
    
    ###---------------------------------------------------------------------
    
    # APAfactor: proportion of APA results (that statcheck reads) 
    # in total number of p values
    # don't calculate if apa_style == FALSE, because then we didn't determine
    # how many results were in APA style. We just scraped everything we could.
    
    if(apa_style == TRUE){
      Res$APAfactor <- calc_APA_factor(pRes, Res)
    } else {
      Res$APAfactor <- rep(NA, nrow(Res))
    }
    
    ###---------------------------------------------------------------------
    
    # select & reorder columns for final data frame
    Res <- Res[ , c("Source", "Statistic", "df1", "df2", "Test.Comparison",
                    "Value", "Reported.Comparison", "Reported.P.Value",
                    "Computed", "Raw", "Error", "DecisionError", 
                    "OneTailedInTxt", "APAfactor")]
    
    # rename columns based on the variable names in the script constants.R
    colnames(Res) <- c(VAR_SOURCE, VAR_TYPE, VAR_DF1, VAR_DF2, 
                       VAR_TEST_COMPARISON, VAR_TEST_VALUE, VAR_P_COMPARISON,
                       VAR_REPORTED_P, VAR_COMPUTED_P, VAR_RAW, VAR_ERROR, 
                       VAR_DEC_ERROR, VAR_1TAILTXT, VAR_APAFACTOR)
    
  }
  
  # Return ------------------------------------------------------------------
  
  if (AllPValues == FALSE) {
    
    # Return message when there are no results
    if (nrow(Res) > 0) {
      class(Res) <- c("statcheck", "data.frame")
      return(Res)
    } else {
      cat("statcheck did not find any results\n")
    }
    
  } else {
    
    if(nrow(pRes) > 0) {
      # first clean then parse p-values using one of the helper functions
      pvalues_clean <- clean_non_apa(pRes$pvalues)
      pvalues_parsed <- extract_p_value(pvalues_clean)
      pvalues_tot <- cbind(Source = pRes$Source, pvalues_parsed)
      
      # rename columns based on the variable names in the script constants.R
      # first make sure that the columns are in the right order before renaming
      pvalues_tot <- pvalues_tot[, c("Source", "p_comp", "p_value", "p_dec")]
      colnames(pvalues_tot) <- c(VAR_SOURCE, VAR_P_COMPARISON, VAR_REPORTED_P, 
                                 VAR_P_DEC)
      
      return(pvalues_tot)
    } else {
      cat("statcheck did not find any p-values\n")
    }
  }
}



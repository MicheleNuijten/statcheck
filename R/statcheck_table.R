#' Check a table of statistical results
#'
#' \code{statcheck_table} checks the consistency of statistical results reported in a 
#' data frame. It applies the same logic as \code{statcheck} but bypasses the text-mining 
#' step, allowing users to provide a structured table of test statistics, degrees of 
#' freedom, and p-values directly.
#'
#' @param stat_table A data frame containing the statistical results to check. 
#' The data frame must contain the following columns:
#' \itemize{
#'   \item \code{test_type}: Character. The type of test (e.g., "t", "F", "chisq", "cor").
#'   \item \code{test_value}: Character or Numeric. The reported test statistic. 
#'   It is recommended to pass this as a character string (e.g., "2.40") to preserve 
#'   trailing zeros, which are used to determine the precision of the calculation.
#'   \item \code{df1}: Numeric. The first degree of freedom.
#'   \item \code{reported_p}: Character or Numeric. The reported p-value. 
#'   Like \code{test_value}, passing this as a character string is recommended to 
#'   preserve precision (e.g., "0.050").
#' }
#' Optional columns include:
#' \itemize{
#'   \item \code{df2}: Numeric. The second degree of freedom (required for F-tests). 
#'   Defaults to NA if missing.
#'   \item \code{test_comparison}: Character. The comparison operator for the test statistic 
#'   ("=", "<", ">"). Defaults to "=".
#'   \item \code{p_comparison}: Character. The comparison operator for the p-value 
#'   ("=", "<", ">"). Defaults to "=".
#' }
#' 
#' @param alpha Numeric. Assumed level of significance in the scanned texts. 
#' Defaults to .05.
#' @param OneTailedTests Logical. Do you want to assume that all reported tests 
#' are one-tailed (TRUE) or two-tailed (FALSE, default)?
#' @param pEqualAlphaSig Logical. If TRUE, statcheck counts p <= alpha as
#' significant (default), if FALSE, statcheck counts p < alpha as significant.
#' @param pZeroError Logical. If TRUE, statcheck counts p = .000 as an error 
#' (because a p-value is never exactly zero, and should be reported as < .001), 
#' if FALSE, statcheck does not count p = .000 automatically as an error.
#' @param OneTailedTxt Logical. If TRUE, statcheck searches the text for 
#' "one-sided", "one-tailed", and "directional" to identify the possible use of 
#' one-sided tests. (Note: This parameter is included for consistency with the 
#' main \code{statcheck} function but is generally set to FALSE for tables 
#' unless a \code{OneTailedInTxt} column is manually added to the input).
#' @param robust_rounding Logical. If TRUE, statcheck uses a robust rounding assumption 
#' (+/- 1 step based on reported decimals) to account for different rounding, flooring, 
#' or ceiling practices. If FALSE (default), it uses standard rounding (+/- 0.5 step).
#' 
#' @return A data frame containing the original input columns plus the following 
#' computed columns:
#' \describe{
#'     \item{computed_p}{The recomputed p-value based on the test statistic and df.}
#'     \item{computed_p_lower}{The lower bound of the recomputed p-value.}
#'     \item{computed_p_upper}{The upper bound of the recomputed p-value.}
#'     \item{error}{Logical. Is the reported p-value incongruent with the recomputed p-value?}
#'     \item{decision_error}{Logical. Is there a decision error (e.g., reported significant but computed non-significant)?}
#' }
#' 
#' @examples
#' # Create a data frame with statistical results
#' # Note the use of strings for values to preserve trailing zeros (precision)
#' my_data <- data.frame(
#'   test_type = c("t", "F"),
#'   test_value = c("2.40", "5.2"), 
#'   df1 = c(20, 2),
#'   df2 = c(NA, 48),
#'   reported_p = c("0.02", "0.005"),
#'   stringsAsFactors = FALSE
#' )
#' 
#' # Check the results
#' statcheck_table(my_data, robust_rounding = TRUE)
#' 
#' @export
statcheck_table <- function(stat_table,
                            alpha = .05,
                            OneTailedTests = FALSE,
                            pEqualAlphaSig = TRUE,
                            pZeroError = TRUE,
                            OneTailedTxt = FALSE,
                            robust_rounding = FALSE) {

  # 1. Input Validation: Ensure necessary columns exist
  required_cols <- c("test_type", "test_value", "df1", "reported_p")
  if (!all(required_cols %in% names(stat_table))) {
    stop("Input must contain columns: ", paste(required_cols, collapse=", "))
  }

  # 2. Handle missing columns (fill defaults)
  if (!"df2" %in% names(stat_table)) stat_table$df2 <- NA
  if (!"test_comparison" %in% names(stat_table)) stat_table$test_comparison <- "="
  if (!"p_comparison" %in% names(stat_table)) stat_table$p_comparison <- "="
  
  # 3. Helper to count decimals (FIXED to handle character input)
  count_decimals <- function(x) {
    # If it's already a number, convert to character (WARNING: this drops trailing zeros)
    # If it's a character (e.g. "2.40"), this preserves it.
    x_char <- as.character(x)
    
    # Remove minus sign if present (handle negative numbers)
    x_clean <- sub("^-", "", x_char)
    
    # Check if there is a decimal point
    if (!grepl("\\.", x_clean)) {
      return(0)
    } else {
      # extract string after the dot and count characters
      return(nchar(sub(".*\\.", "", x_clean)))
    }
  }
  
  # Calculate decimals BEFORE converting to numeric
  # This ensures "2.40" counts as 2 decimals, even though as.numeric("2.40") is 2.4
  stat_table$test_dec <- sapply(stat_table$test_value, count_decimals)
  stat_table$p_dec <- sapply(stat_table$reported_p, count_decimals)

  # 4. Initialize storage
  n <- nrow(stat_table)
  results <- data.frame(
    computed_p = numeric(n),
    computed_p_lower = numeric(n),
    computed_p_upper = numeric(n),
    error = logical(n),
    decision_error = logical(n),
    stringsAsFactors = FALSE
  )

  # 5. Loop and Check
  for (i in 1:n) {
    
    # We now convert to numeric safely just for the calculation steps
    # Note: process_stats expects numeric inputs for test_stat/df/p
    
    res <- process_stats(
      test_type = stat_table$test_type[i],
      test_stat = as.numeric(stat_table$test_value[i]), # Convert to numeric 
      df1 = as.numeric(stat_table$df1[i]),
      df2 = as.numeric(stat_table$df2[i]),
      reported_p = as.numeric(stat_table$reported_p[i]), # Convert to numeric 
      p_comparison = stat_table$p_comparison[i],
      test_comparison = stat_table$test_comparison[i],
      p_dec = stat_table$p_dec[i],       # Pass the pre-calculated decimal count
      test_dec = stat_table$test_dec[i], # Pass the pre-calculated decimal count
      OneTailedInTxt = FALSE, 
      two_tailed = !OneTailedTests,
      alpha = alpha,
      pZeroError = pZeroError,
      pEqualAlphaSig = pEqualAlphaSig,
      OneTailedTxt = OneTailedTxt,
      OneTailedTests = OneTailedTests,
      robust_rounding = robust_rounding
    )
    
    results[i, ] <- res
  }

  # 6. Combine Input and Output
  final_output <- cbind(stat_table, results)
  return(final_output)
}
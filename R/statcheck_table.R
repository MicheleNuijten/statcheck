#' Check a table of statistical results
#' 
#' @param stat_table A data frame with columns: test_type, test_value, df1, df2, reported_p.
#' @param ... arguments passed to process_stats (e.g. alpha, OneTailedTests, robust_rounding)
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
  
  # 3. Helper to count decimals (crucial for bound calculation!)
  # We need to know if "0.05" was reported (2 decimals) or "0.5" (1 decimal)
  count_decimals <- function(x) {
    # Convert to character, remove sign, split by dot
    as.numeric(nchar(sub(".*\\.", "", as.character(abs(x)))))
  }
  
  # We assume the input numeric values in the dataframe reflect the reported precision.
  # If the user passes numeric variables, R might drop trailing zeros (e.g. 0.50 -> 0.5).
  # Ideally, users should pass these as character strings to preserve "0.50".
  # Here we try to detect it from the provided values.
  
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
  
  # 5. Loop and Check (Vectorizing process_stats is hard because of internal logic)
  for (i in 1:n) {
    
    # Process the stats using your modified internal function
    res <- process_stats(
      test_type = stat_table$test_type[i],
      test_stat = as.numeric(stat_table$test_value[i]),
      df1 = as.numeric(stat_table$df1[i]),
      df2 = as.numeric(stat_table$df2[i]),
      reported_p = as.numeric(stat_table$reported_p[i]),
      p_comparison = stat_table$p_comparison[i],
      test_comparison = stat_table$test_comparison[i],
      p_dec = stat_table$p_dec[i],
      test_dec = stat_table$test_dec[i],
      OneTailedInTxt = FALSE, # Not applicable for tables
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

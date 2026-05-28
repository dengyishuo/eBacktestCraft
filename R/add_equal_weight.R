#' Assign equal weights to selected stocks
#'
#' For each trading day, assign equal weights (1/n) to all stocks with signal = 1.
#' Stocks with signal = 0 or NA receive 0 weight.
#'
#' @param df Data frame, must contain 'date' and 'code' columns
#' @param signal_col Signal column name where value = 1 indicates selected stocks
#' @param weight_name Output weight column name. Default is "weight_equal"
#' @param zero_na Whether to treat NA/Inf as 0 in signal column, default TRUE
#' @param output_type Output format: "tibble" (default) or "data.frame"
#'
#' @return Original data frame with appended weight column in specified format
#' @export
#'
#' @importFrom dplyr group_by mutate ungroup select summarise
#' @importFrom rlang .data !! sym :=
#' @importFrom tibble as_tibble
#'
#' @examples
#' \dontrun{
#'
#' }
#' @examples
#' \dontrun{
#' # Create sample data
#' df <- data.frame(
#'   date = as.Date("2024-01-01"),
#'   code = c("A", "B", "C"),
#'   selected = c(1, 1, 0)
#' )
#' df_with_equal_weight <- add_equal_weight(df, signal_col = "selected")
#' }
add_equal_weight <- function(
  df,
  signal_col,
  weight_name = NULL,
  zero_na = TRUE,
  output_type = c("tibble", "data.frame")
) {
  # --------------------------
  # 1. Input validation
  # --------------------------
  if (!all(c("date", "code") %in% colnames(df))) {
    stop("Input data must contain 'date' and 'code' columns!")
  }
  if (!signal_col %in% colnames(df)) {
    stop("Specified signal column not found: ", signal_col)
  }

  output_type <- match.arg(output_type)

  # --------------------------
  # 2. Auto-generate weight column name
  # --------------------------
  if (is.null(weight_name)) {
    weight_name <- paste0("weight_equal_", signal_col)
  }

  # --------------------------
  # 3. Core calculation: equal weight allocation
  # --------------------------
  result_df <- df

  result_df <- result_df %>%
    dplyr::group_by(.data$date) %>%
    dplyr::mutate(
      # Clean signal: zero_na controls whether to treat NA/Inf as 0
      .signal_clean = ifelse(
        zero_na & (is.na(!!sym(signal_col)) | is.infinite(!!sym(signal_col))),
        0,
        !!sym(signal_col)
      ),
      # Mark selected: signal value == 1 indicates selected
      .is_selected = (.data$.signal_clean == 1) & !is.na(.data$.signal_clean),
      # Total selected count for the day
      .n_selected = sum(.data$.is_selected, na.rm = TRUE),
      # Assign equal weights
      !!weight_name := dplyr::case_when(
        .data$.n_selected == 0 ~ 0,
        .data$.is_selected ~ 1 / .data$.n_selected,
        TRUE ~ 0
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$.signal_clean, -.data$.is_selected, -.data$.n_selected)

  # Handle outliers: ensure no NAs
  result_df[[weight_name]] <- ifelse(is.na(result_df[[weight_name]]), 0, result_df[[weight_name]])

  # --------------------------
  # 4. Output format conversion
  # --------------------------
  if (output_type == "tibble") {
    result_df <- tibble::as_tibble(result_df)
  }

  # --------------------------
  # 5. Diagnostic information
  # --------------------------
  daily_summary <- result_df %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(
      total_weight = sum(!!sym(weight_name), na.rm = TRUE),
      n_selected = sum(!!sym(weight_name) > 0, na.rm = TRUE),
      .groups = "drop"
    )

  total_days <- nrow(daily_summary)
  days_with_selection <- sum(daily_summary$n_selected > 0, na.rm = TRUE)
  avg_selected <- mean(daily_summary$n_selected, na.rm = TRUE)
  valid_sum_days <- sum(abs(daily_summary$total_weight - 1) < 1e-6, na.rm = TRUE)

  message(" Generated equal weight column: ", weight_name)
  message(" Total days: ", total_days, ", days with selection: ", days_with_selection)
  message(" Average daily selected stocks: ", round(avg_selected, 2))
  message(
    " Days with weight sum = 1: ", valid_sum_days, "/", total_days,
    " (", round(100 * valid_sum_days / total_days, 1), "%)"
  )

  return(result_df)
}

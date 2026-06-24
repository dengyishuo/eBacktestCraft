#' Assign equal weights to selected stocks
#'
#' For each trading day, assign equal weights (1/n) to all stocks with signal = 1.
#' Stocks with signal = 0 or NA receive 0 weight.
#'
#' @param mkt_data Data frame, must contain 'date' and 'code' columns
#' @param signal_col Signal column name where value = 1 indicates selected stocks
#' @param weight_name Output weight column name. Default is "weight_equal"
#' @param zero_na Whether to treat NA/Inf as 0 in signal column, default TRUE
#' @param output Output format: "tibble" (default) or "data.frame"
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
#' # Example 1: Basic usage, equal weights from a pre-computed signal column
#' mkt_data <- data.frame(
#'   date   = rep(seq(as.Date("2023-01-01"), by = "day", length.out = 60), each = 3),
#'   code   = rep(c("AAPL", "MSFT", "GOOG"), times = 60),
#'   name   = rep(c("Apple", "Microsoft", "Alphabet"), times = 60),
#'   close  = round(runif(180, 100, 300), 2),
#'   open   = round(runif(180, 100, 300), 2),
#'   mom_20 = round(runif(180, -0.2, 0.2), 4),
#'   stringsAsFactors = FALSE
#' )
#' mkt_data$selected <- as.integer(mkt_data$mom_20 > 0)
#' result <- add_equal_weight(mkt_data, signal_col = "selected")
#'
#' # Example 2: Key parameter variant, suppress NA-to-zero coercion, return data.frame
#' result_df <- add_equal_weight(mkt_data,
#'   signal_col  = "selected",
#'   weight_name = "w_eq",
#'   zero_na     = FALSE,
#'   output      = "data.frame"
#' )
#'
#' # Example 3: Backtest workflow, signal to equal weight to run_backtest
#' mkt_data <- add_signal(mkt_data,
#'   indicator_cols = "mom_20", signal_type = "threshold",
#'   threshold = 0, compare_op = ">"
#' )
#' mkt_data <- add_equal_weight(mkt_data, signal_col = "signal_mom_20_gt_0")
#' # bt <- run_backtest(mkt_data, weight_col = "weight_equal_signal_mom_20_gt_0")
#' }
add_equal_weight <- function(
  mkt_data,
  signal_col,
  weight_name = NULL,
  zero_na = TRUE,
  output = c("tibble", "data.frame")
) {
  # ── Input Validation ───────────────────────────────────────────────────────
  # 'date' and 'code' are required to compute weights cross-sectionally per day
  if (!all(c("date", "code") %in% colnames(mkt_data))) {
    stop("mkt_data must contain 'date' and 'code' columns!")
  }
  if (!signal_col %in% colnames(mkt_data)) {
    stop("Specified signal column not found in mkt_data: ", signal_col)
  }

  output <- match.arg(output)

  # ── Auto-Generate Weight Column Name ──────────────────────────────────────
  if (is.null(weight_name)) {
    weight_name <- paste0("weight_equal_", signal_col)
  }

  # ── Equal Weight Allocation ────────────────────────────────────────────────
  result_df <- mkt_data %>%
    dplyr::group_by(.data$date) %>%
    dplyr::mutate(
      # Coerce NA/Inf to 0 so they don't count as selected
      .signal_clean = ifelse(
        zero_na & (is.na(!!sym(signal_col)) | is.infinite(!!sym(signal_col))),
        0,
        !!sym(signal_col)
      ),
      # A stock is selected only when signal equals exactly 1
      .is_selected  = (.data$.signal_clean == 1) & !is.na(.data$.signal_clean),
      # Count selected per day so we can divide equally
      .n_selected   = sum(.data$.is_selected, na.rm = TRUE),
      # 1/n for selected, 0 for unselected; handle the empty-day edge case
      !!weight_name := dplyr::case_when(
        .data$.n_selected == 0 ~ 0,
        .data$.is_selected     ~ 1 / .data$.n_selected,
        TRUE                   ~ 0
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$.signal_clean, -.data$.is_selected, -.data$.n_selected)

  # Ensure no residual NAs leak into the weight column
  result_df[[weight_name]] <- ifelse(is.na(result_df[[weight_name]]), 0, result_df[[weight_name]])

  # ── Output Format Conversion ───────────────────────────────────────────────
  if (output == "tibble") result_df <- tibble::as_tibble(result_df)

  # ── Diagnostics ────────────────────────────────────────────────────────────
  daily_summary <- result_df %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(
      total_weight = sum(!!sym(weight_name), na.rm = TRUE),
      n_selected   = sum(!!sym(weight_name) > 0, na.rm = TRUE),
      .groups      = "drop"
    )

  total_days          <- nrow(daily_summary)
  days_with_selection <- sum(daily_summary$n_selected > 0, na.rm = TRUE)
  avg_selected        <- mean(daily_summary$n_selected, na.rm = TRUE)
  # Count days where weights actually sum to 1 (expected when at least one stock is selected)
  valid_sum_days      <- sum(abs(daily_summary$total_weight - 1) < 1e-6, na.rm = TRUE)

  message(" Generated equal weight column: ", weight_name)
  message(" Total days: ", total_days, ", days with selection: ", days_with_selection)
  message(" Average daily selected stocks: ", round(avg_selected, 2))
  message(
    " Days with weight sum = 1: ", valid_sum_days, "/", total_days,
    " (", round(100 * valid_sum_days / total_days, 1), "%)"
  )

  return(result_df)
}

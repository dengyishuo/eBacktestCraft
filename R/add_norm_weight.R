#' Group-wise normalized weight calculation (linear / Softmax normalization)
#'
#' Normalize indicator values within each trading day to generate weights for stock selection.
#' Supports signal filtering, automatic zero replacement for outliers, and ensures daily
#' weights sum to 1. Fully compatible with quantitative factor workflows.
#'
#' @param mkt_data Data frame in long format, must contain 'date' and 'code' columns
#' @param weight_col Character, column name of the raw indicator used for weight calculation (e.g., "mom_5")
#' @param signal_col Optional character, signal column name. Only rows with signal = 1 receive weights, others get 0
#' @param norm_method Normalization method:
#'   - "linear": Linear normalization (default, weights sum to 1)
#'   - "softmax": Exponential softmax normalization (more extreme concentration on top values)
#' @param weight_name Character, output weight column name. Auto-generated if NULL
#' @param zero_na Logical, whether to automatically replace NA/Inf with 0, default TRUE
#' @param output Output format: "tibble" (default) or "data.frame"
#'
#' @return Data frame with appended weight column, normalized by date, daily weights sum to 1,
#'   in specified output format
#'
#' @importFrom dplyr group_by mutate ungroup select summarise
#' @importFrom rlang .data !! sym :=
#' @importFrom tibble as_tibble
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic usage, linear normalization of momentum factor
#' mkt_data <- data.frame(
#'   date   = rep(seq(as.Date("2023-01-01"), by = "day", length.out = 60), each = 3),
#'   code   = rep(c("AAPL", "MSFT", "GOOG"), times = 60),
#'   name   = rep(c("Apple", "Microsoft", "Alphabet"), times = 60),
#'   close  = round(runif(180, 100, 300), 2),
#'   open   = round(runif(180, 100, 300), 2),
#'   mom_20 = round(runif(180, -0.2, 0.2), 4),
#'   stringsAsFactors = FALSE
#' )
#' result <- add_norm_weight(mkt_data, weight_col = "mom_20")
#'
#' # Example 2: Key parameter variants, softmax normalization with signal filter
#' mkt_data <- add_signal(mkt_data,
#'   indicator_cols = "mom_20", signal_type = "threshold",
#'   threshold = 0, compare_op = ">"
#' )
#' result_softmax <- add_norm_weight(mkt_data,
#'   weight_col  = "mom_20",
#'   signal_col  = "signal_mom_20_gt_0",  # Only positive-momentum stocks get weight
#'   norm_method = "softmax",
#'   output      = "data.frame"
#' )
#'
#' # Example 3: Backtest workflow, threshold signal to norm weight to run_backtest
#' mkt_data <- add_signal(mkt_data,
#'   indicator_cols = "mom_20", signal_type = "threshold",
#'   threshold = 0, compare_op = ">"
#' )
#' mkt_data <- add_norm_weight(mkt_data,
#'   weight_col = "mom_20",
#'   signal_col = "signal_mom_20_gt_0"
#' )
#' # bt <- run_backtest(mkt_data, weight_col = "weight_mom_20_signal_mom_20_gt_0")
#' }
add_norm_weight <- function(
  mkt_data,
  weight_col,
  signal_col = NULL,
  norm_method = "linear",
  weight_name = NULL,
  zero_na = TRUE,
  output = c("tibble", "data.frame")
) {
  # ── Input Validation ───────────────────────────────────────────────────────
  # 'date' and 'code' are required for cross-sectional daily normalization
  if (!all(c("date", "code") %in% colnames(mkt_data))) {
    stop("mkt_data must contain 'date' and 'code' columns!")
  }
  if (!weight_col %in% colnames(mkt_data)) {
    stop("Specified weight column not found in mkt_data: ", weight_col)
  }
  if (!is.null(signal_col) && !signal_col %in% colnames(mkt_data)) {
    stop("Specified signal column not found in mkt_data: ", signal_col)
  }
  # Validate norm_method explicitly so the error is descriptive
  valid_norm_methods <- c("linear", "softmax")
  if (!norm_method %in% valid_norm_methods) {
    stop("norm_method must be one of: ", paste(valid_norm_methods, collapse = ", "))
  }

  output <- match.arg(output)

  # ── Auto-Generate Weight Column Name ──────────────────────────────────────
  if (is.null(weight_name)) {
    weight_name <- paste0("weight_", weight_col)
    if (!is.null(signal_col)) {
      weight_name <- paste0(weight_name, "_", signal_col)
    }
  }

  # ── Normalization Weight Calculation ──────────────────────────────────────
  result_df <- mkt_data %>%
    dplyr::group_by(.data$date) %>%
    dplyr::mutate(
      # Replace NA/Inf with 0 so they contribute nothing to the weight pool
      .weight_value = ifelse(
        zero_na & (is.na(!!sym(weight_col)) | is.infinite(!!sym(weight_col))),
        0,
        !!sym(weight_col)
      ),
      # Zero out rows where the signal filter excludes the stock
      .weight_value = ifelse(
        !is.null(signal_col) & !!sym(signal_col) != 1,
        0,
        .data$.weight_value
      ),
      # Daily total is used as the denominator for linear normalization
      .weight_total = sum(.data$.weight_value, na.rm = TRUE),
      # Compute final weight per normalization method
      !!weight_name := dplyr::case_when(
        .data$.weight_total == 0 ~ 0,                                    # No effective weight on this day
        norm_method == "linear"  ~ .data$.weight_value / .data$.weight_total,
        norm_method == "softmax" ~ exp(.data$.weight_value) /
          sum(exp(.data$.weight_value[.data$.weight_value != 0]), na.rm = TRUE),
        TRUE ~ 0
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$.weight_value, -.data$.weight_total)

  # Ensure no NAs remain (can happen if all softmax denominators are 0)
  result_df[[weight_name]] <- ifelse(is.na(result_df[[weight_name]]), 0, result_df[[weight_name]])

  # ── Output Format Conversion ───────────────────────────────────────────────
  if (output == "tibble") result_df <- tibble::as_tibble(result_df)

  # ── Diagnostics ────────────────────────────────────────────────────────────
  daily_weight_sum <- result_df %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(
      weight_sum = sum(!!sym(weight_name), na.rm = TRUE),
      .groups    = "drop"
    )

  valid_sum_count     <- sum(abs(daily_weight_sum$weight_sum - 1) < 1e-6, na.rm = TRUE)
  total_days          <- nrow(daily_weight_sum)
  # Average fraction of stocks with positive weight, scaled to universe size
  avg_effective_stocks <- mean(result_df[[weight_name]] > 0, na.rm = TRUE) *
    length(unique(result_df$code))

  message(" Generated normalized weight column: ", weight_name)
  message(
    " Total days: ", total_days, ", days with weight sum = 1: ",
    valid_sum_count, " (", round(100 * valid_sum_count / total_days, 1), "%)"
  )
  message(" Average daily effective stocks: ", round(avg_effective_stocks, 2))

  return(result_df)
}

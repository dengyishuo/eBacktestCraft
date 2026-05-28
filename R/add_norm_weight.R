#' Group-wise normalized weight calculation (linear / Softmax normalization)
#'
#' Normalize indicator values within each trading day to generate weights for stock selection.
#' Supports signal filtering, automatic zero replacement for outliers, and ensures daily
#' weights sum to 1. Fully compatible with quantitative factor workflows.
#'
#' @param df Data frame in long format, must contain 'date' and 'code' columns
#' @param weight_col Character, column name of the raw indicator used for weight calculation (e.g., "mom_5")
#' @param signal_col Optional character, signal column name. Only rows with signal = 1 receive weights, others get 0
#' @param norm_method Normalization method:
#'   - "linear": Linear normalization (default, weights sum to 1)
#'   - "softmax": Exponential softmax normalization (more extreme concentration on top values)
#' @param weight_name Character, output weight column name. Auto-generated if NULL
#' @param zero_na Logical, whether to automatically replace NA/Inf with 0, default TRUE
#' @param output_type Output format: "tibble" (default) or "data.frame"
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
#' # Example 1: Simple linear normalization
#' df <- add_norm_weight(df, weight_col = "mom_5")
#'
#' # Example 2: With signal filtering (only assign weights to signal = 1 stocks)
#' df <- add_norm_weight(df, weight_col = "mom_5", signal_col = "signal_mom_5_cross_mom_20")
#'
#' # Example 3: Softmax normalization
#' df <- add_norm_weight(df, weight_col = "ram_20", norm_method = "softmax")
#'
#' # Example 4: Return as base data.frame
#' df <- add_norm_weight(df, weight_col = "mom_5", output_type = "data.frame")
#' }
add_norm_weight <- function(
  df,
  weight_col,
  signal_col = NULL,
  norm_method = "linear",
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
  if (!weight_col %in% colnames(df)) {
    stop("Specified weight column not found: ", weight_col)
  }
  if (!is.null(signal_col) && !signal_col %in% colnames(df)) {
    stop("Specified signal column not found: ", signal_col)
  }

  valid_norm_methods <- c("linear", "softmax")
  if (!norm_method %in% valid_norm_methods) {
    stop("norm_method must be one of: ", paste(valid_norm_methods, collapse = ", "))
  }

  output_type <- match.arg(output_type)

  # --------------------------
  # 2. Auto-generate weight column name
  # --------------------------
  if (is.null(weight_name)) {
    weight_name <- paste0("weight_", weight_col)
    if (!is.null(signal_col)) {
      weight_name <- paste0(weight_name, "_", signal_col)
    }
  }

  # --------------------------
  # 3. Core normalization weight calculation logic
  # --------------------------
  result_df <- df

  result_df <- result_df %>%
    dplyr::group_by(.data$date) %>%
    dplyr::mutate(
      # Auto-zero outliers (NA/Inf)
      .weight_value = ifelse(
        zero_na & (is.na(!!sym(weight_col)) | is.infinite(!!sym(weight_col))),
        0,
        !!sym(weight_col)
      ),
      # Signal filtering: only keep weights where signal == 1
      .weight_value = ifelse(
        !is.null(signal_col) & !!sym(signal_col) != 1,
        0,
        .data$.weight_value
      ),
      # Total effective weight sum for the day
      .weight_total = sum(.data$.weight_value, na.rm = TRUE),
      # Normalization calculation
      !!weight_name := dplyr::case_when(
        .data$.weight_total == 0 ~ 0,
        norm_method == "linear" ~ .data$.weight_value / .data$.weight_total,
        norm_method == "softmax" ~ exp(.data$.weight_value) /
          sum(exp(.data$.weight_value[.data$.weight_value != 0]), na.rm = TRUE),
        TRUE ~ 0
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$.weight_value, -.data$.weight_total)

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
  daily_weight_sum <- result_df %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(
      weight_sum = sum(!!sym(weight_name), na.rm = TRUE),
      .groups = "drop"
    )

  valid_sum_count <- sum(abs(daily_weight_sum$weight_sum - 1) < 1e-6, na.rm = TRUE)
  total_days <- nrow(daily_weight_sum)

  # Calculate average daily effective stocks
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

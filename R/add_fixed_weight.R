#' Assign fixed weights to selected stocks
#'
#' Assign pre-defined fixed weights to selected stocks. Supports multiple input formats
#' for fixed weights and optional daily normalization. Can strictly validate that the
#' selected stock set matches the fixed weight stock set on each trading day.
#'
#' @param mkt_data Data frame, must contain 'date', 'code' columns and signal column
#' @param signal_col Signal column name where value = 1 indicates selected stocks
#' @param fixed_weights Fixed weights, supports three formats:
#'   * Named vector, e.g., c("000001.SZ" = 0.3, "000002.SZ" = 0.7)
#'   * Data frame with 'code' and 'weight' columns
#'   * Numeric vector, must match order of unique(mkt_data$code) after sorting
#' @param weight_name Output weight column name. Default is "weight_fixed"
#' @param normalize_daily Whether to normalize weights to sum to 1 within each day, default FALSE
#' @param zero_na Whether to treat NA/Inf as 0 in signal column, default TRUE
#' @param strict_check Whether to strictly validate that selected stocks match fixed weight stocks on each day, default TRUE
#' @param output Output format: "tibble" (default) or "data.frame"
#'
#' @return Original data frame with appended weight column in specified format
#' @export
#'
#' @importFrom dplyr left_join group_by mutate ungroup select summarise case_when
#' @importFrom rlang .data !! sym :=
#' @importFrom tibble as_tibble
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic usage, named vector of fixed weights
#' mkt_data <- data.frame(
#'   date   = rep(seq(as.Date("2023-01-01"), by = "day", length.out = 60), each = 3),
#'   code   = rep(c("AAPL", "MSFT", "GOOG"), times = 60),
#'   name   = rep(c("Apple", "Microsoft", "Alphabet"), times = 60),
#'   close  = round(runif(180, 100, 300), 2),
#'   open   = round(runif(180, 100, 300), 2),
#'   mom_20 = round(runif(180, -0.2, 0.2), 4),
#'   stringsAsFactors = FALSE
#' )
#' mkt_data$selected <- 1L  # Select all three stocks every day
#' fixed_w <- c("AAPL" = 0.5, "MSFT" = 0.3, "GOOG" = 0.2)
#' result <- add_fixed_weight(mkt_data,
#'   signal_col    = "selected",
#'   fixed_weights = fixed_w,
#'   strict_check  = FALSE   # Skip strict check because all stocks are always selected
#' )
#'
#' # Example 2: Key parameter variant, daily normalization, no strict check
#' result_norm <- add_fixed_weight(mkt_data,
#'   signal_col       = "selected",
#'   fixed_weights    = fixed_w,
#'   normalize_daily  = TRUE,
#'   strict_check     = FALSE,
#'   output           = "data.frame"
#' )
#'
#' # Example 3: Backtest workflow, signal to fixed weight to run_backtest
#' mkt_data <- add_signal(mkt_data,
#'   indicator_cols = "mom_20", signal_type = "threshold",
#'   threshold = 0, compare_op = ">"
#' )
#' mkt_data <- add_fixed_weight(mkt_data,
#'   signal_col    = "signal_mom_20_gt_0",
#'   fixed_weights = fixed_w,
#'   strict_check  = FALSE
#' )
#' # bt <- run_backtest(mkt_data, weight_col = "weight_fixed_signal_mom_20_gt_0")
#' }
add_fixed_weight <- function(
  mkt_data,
  signal_col,
  fixed_weights,
  weight_name = NULL,
  normalize_daily = FALSE,
  zero_na = TRUE,
  strict_check = TRUE,
  output = c("tibble", "data.frame")
) {
  # ── Input Validation ───────────────────────────────────────────────────────
  # 'date' and 'code' are required for daily grouping and code-level weight lookup
  if (!all(c("date", "code") %in% colnames(mkt_data))) {
    stop("mkt_data must contain 'date' and 'code' columns!")
  }
  if (!signal_col %in% colnames(mkt_data)) {
    stop("Specified signal column not found in mkt_data: ", signal_col)
  }
  if (is.null(fixed_weights)) {
    stop("fixed_weights cannot be NULL!")
  }

  output <- match.arg(output)

  # ── Auto-Generate Weight Column Name ──────────────────────────────────────
  if (is.null(weight_name)) {
    weight_name <- paste0("weight_fixed_", signal_col)
  }

  # ── Parse fixed_weights into a Lookup Table ────────────────────────────────
  # Normalise three possible formats into a single (code, fixed_weight) data frame
  all_codes <- unique(mkt_data$code)

  if (is.numeric(fixed_weights) && !is.null(names(fixed_weights))) {
    # Named numeric vector — most common usage
    weight_df <- data.frame(
      code         = names(fixed_weights),
      fixed_weight = as.numeric(fixed_weights),
      stringsAsFactors = FALSE
    )
  } else if (is.data.frame(fixed_weights)) {
    # Data frame input must carry 'code' and 'weight' columns
    if (!all(c("code", "weight") %in% colnames(fixed_weights))) {
      stop("When fixed_weights is a data frame, it must contain 'code' and 'weight' columns!")
    }
    weight_df <- fixed_weights %>%
      dplyr::select(code, fixed_weight = weight)
  } else if (is.numeric(fixed_weights)) {
    # Positional numeric vector — must align with the sorted unique code list
    if (length(fixed_weights) != length(all_codes)) {
      stop(
        "Numeric vector fixed_weights length must equal number of unique codes in mkt_data: ",
        length(all_codes)
      )
    }
    weight_df <- data.frame(
      code         = all_codes,
      fixed_weight = fixed_weights,
      stringsAsFactors = FALSE
    )
  } else {
    stop("fixed_weights must be a named vector, data frame, or numeric vector matching code count!")
  }

  fixed_codes  <- weight_df$code
  n_fixed      <- length(fixed_codes)
  total_fixed  <- sum(weight_df$fixed_weight, na.rm = TRUE)

  # Inform the user if weights don't sum to 1 and normalization is off
  if (abs(total_fixed - 1) > 1e-6 && !normalize_daily) {
    message(
      " Warning: Fixed weight sum is ", round(total_fixed, 4),
      ", not equal to 1. Set normalize_daily = TRUE if daily normalization is needed."
    )
  }

  # ── Optional Strict Validation ─────────────────────────────────────────────
  # Strict check prevents silent mismatches between the signal pool and fixed codes
  if (strict_check) {
    message(" Starting strict validation: checking if selected stocks match fixed weight stocks on each day...")

    df_temp <- mkt_data %>%
      dplyr::mutate(
        .signal_clean = ifelse(
          zero_na & (is.na(!!sym(signal_col)) | is.infinite(!!sym(signal_col))),
          0,
          !!sym(signal_col)
        )
      )

    check_results <- df_temp %>%
      dplyr::group_by(.data$date) %>%
      dplyr::summarise(
        selected_codes = list(.data$code[.data$.signal_clean == 1 & !is.na(.data$.signal_clean)]),
        n_selected     = length(selected_codes[[1]]),
        .groups        = "drop"
      )

    wrong_dates <- check_results %>%
      dplyr::filter(.data$n_selected != n_fixed)

    if (nrow(wrong_dates) > 0) {
      stop(
        "Strict validation failed! Selected stock count on these dates (",
        paste(wrong_dates$n_selected, collapse = ", "),
        ") does not equal fixed weight stock count (", n_fixed, "):\n",
        paste(wrong_dates$date, collapse = ", ")
      )
    }

    # Verify that the exact set of codes matches, not just the count
    for (i in 1:nrow(check_results)) {
      dt           <- check_results$date[i]
      selected_set <- unlist(check_results$selected_codes[i])
      if (!identical(sort(selected_set), sort(fixed_codes))) {
        stop(
          "Strict validation failed! Date ", dt, " selected stock set does not match fixed weight stock set.\n",
          "Selected stocks: ",     paste(selected_set, collapse = ", "), "\n",
          "Fixed weight stocks: ", paste(fixed_codes,  collapse = ", ")
        )
      }
    }

    message(
      " Strict validation passed: Selected stocks match fixed weight stocks on all ",
      nrow(check_results), " trading days"
    )
  }

  # ── Merge Fixed Weights into Panel ────────────────────────────────────────
  # Left join ensures all rows are retained; codes absent from weight_df receive NA -> 0
  result_df <- mkt_data %>%
    dplyr::left_join(weight_df, by = "code")

  result_df$fixed_weight[is.na(result_df$fixed_weight)] <- 0  # Codes not in weight_df get 0

  # ── Calculate Final Weights ────────────────────────────────────────────────
  result_df <- result_df %>%
    dplyr::group_by(.data$date) %>%
    dplyr::mutate(
      .signal_clean = ifelse(
        zero_na & (is.na(!!sym(signal_col)) | is.infinite(!!sym(signal_col))),
        0,
        !!sym(signal_col)
      ),
      .is_selected  = (.data$.signal_clean == 1) & !is.na(.data$.signal_clean),
      # Apply fixed weight only to selected rows; others get 0
      .base_weight  = ifelse(.data$.is_selected, .data$fixed_weight, 0),
      # Optionally rescale within each day so the portfolio is fully invested
      !!weight_name := if (normalize_daily) {
        total_sel_w <- sum(.data$.base_weight, na.rm = TRUE)
        dplyr::case_when(
          total_sel_w == 0 ~ 0,
          TRUE             ~ .data$.base_weight / total_sel_w
        )
      } else {
        .data$.base_weight
      }
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$.signal_clean, -.data$.is_selected, -.data$.base_weight, -fixed_weight)

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
  valid_sum_days      <- sum(abs(daily_summary$total_weight - 1) < 1e-6, na.rm = TRUE)

  message(" Generated fixed weight column: ", weight_name)
  message(" Total days: ", total_days, ", days with selection: ", days_with_selection)
  message(" Average daily selected stocks: ", round(avg_selected, 2))
  if (normalize_daily) {
    message(
      " Daily weights normalized, days with sum = 1: ", valid_sum_days, "/", total_days,
      " (", round(100 * valid_sum_days / total_days, 1), "%)"
    )
  } else {
    message(
      " Fixed weight (unnormalized) average daily sum: ",
      round(mean(daily_summary$total_weight, na.rm = TRUE), 4)
    )
  }

  return(result_df)
}

#' Generate signal for consecutive days meeting a condition
#'
#' Create a 0/1 signal that becomes 1 when a condition has been met for N
#' consecutive trading days (within each asset). Useful for trend confirmation
#' or entry filters like "close > SMA(20) for 3 consecutive days".
#'
#' @param mkt_data Data frame in long format, must contain 'date', 'code', and the
#'   condition column (or a pre-computed signal column)
#' @param condition_col Character string of a column containing 0/1 condition
#'   values (e.g., from \code{add_signal}).
#' @param n_consecutive Integer, number of consecutive days required
#' @param signal_name Character, output signal column name. Auto-generated if NULL.
#' @param output Output format: "tibble" (default) or "data.frame"
#'
#' @return Original data frame with an appended signal column (1 when condition
#'   has been true for n_consecutive days, 0 otherwise)
#'
#' @importFrom dplyr group_by mutate lag ungroup
#' @importFrom rlang .data !! sym :=
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic usage, 2-day consecutive positive momentum signal
#' mkt_data <- data.frame(
#'   date   = rep(seq(as.Date("2023-01-01"), by = "day", length.out = 60), each = 3),
#'   code   = rep(c("AAPL", "MSFT", "GOOG"), times = 60),
#'   name   = rep(c("Apple", "Microsoft", "Alphabet"), times = 60),
#'   close  = round(runif(180, 100, 300), 2),
#'   open   = round(runif(180, 100, 300), 2),
#'   mom_20 = round(runif(180, -0.2, 0.2), 4),
#'   stringsAsFactors = FALSE
#' )
#' mkt_data <- add_signal(mkt_data,
#'   indicator_cols = "mom_20",
#'   signal_type    = "threshold",
#'   threshold      = 0,
#'   compare_op     = ">",
#'   signal_name    = "sig_mom_pos"
#' )
#' result <- add_consecutive_signal(mkt_data,
#'   condition_col  = "sig_mom_pos",
#'   n_consecutive  = 2
#' )
#'
#' # Example 2: Key parameter variant, 5-day consecutive requirement (stricter entry)
#' result_strict <- add_consecutive_signal(mkt_data,
#'   condition_col  = "sig_mom_pos",
#'   n_consecutive  = 5,
#'   signal_name    = "sig_mom_pos_5days",
#'   output         = "data.frame"
#' )
#'
#' # Example 3: Backtest workflow, threshold to consecutive to weight to run_backtest
#' mkt_data <- add_signal(mkt_data,
#'   indicator_cols = "mom_20", signal_type = "threshold",
#'   threshold = 0, compare_op = ">", signal_name = "sig_mom_pos"
#' )
#' mkt_data <- add_consecutive_signal(mkt_data,
#'   condition_col = "sig_mom_pos", n_consecutive = 3
#' )
#' mkt_data <- add_equal_weight(mkt_data,
#'   signal_col = "signal_sig_mom_pos_consecutive3"
#' )
#' # bt <- run_backtest(mkt_data, weight_col = "weight_equal_signal_sig_mom_pos_consecutive3")
#' }
add_consecutive_signal <- function(
  mkt_data,
  condition_col,
  n_consecutive = 2,
  signal_name = NULL,
  output = c("tibble", "data.frame")
) {
  # ── Input Validation ───────────────────────────────────────────────────────
  # 'code' is required to group the time series per asset before counting streaks
  if (!all(c("date", "code") %in% colnames(mkt_data))) {
    stop("mkt_data must contain 'date' and 'code' columns!")
  }
  if (!condition_col %in% colnames(mkt_data)) {
    stop("Condition column not found in mkt_data: ", condition_col)
  }
  if (n_consecutive < 1) stop("n_consecutive must be >= 1")

  output <- match.arg(output)

  # ── Auto-Generate Signal Column Name ───────────────────────────────────────
  if (is.null(signal_name)) {
    signal_name <- paste0("signal_", condition_col, "_consecutive", n_consecutive)
  }

  # ── Consecutive-Day Streak Calculation ────────────────────────────────────
  # Sort within each code group so that lag() and rle() respect time order
  result <- mkt_data %>%
    dplyr::group_by(.data$code) %>%
    dplyr::arrange(.data$date, .by_group = TRUE) %>%
    dplyr::mutate(
      # Binarize the condition: any value > 0 is treated as "condition met"
      cond  = as.integer(!!sym(condition_col) > 0),
      # Use run-length encoding to count consecutive 1s; reset to 0 on any 0
      streak = {
        rle_cond <- rle(.data$cond)
        rep(rle_cond$lengths, rle_cond$lengths) * .data$cond
      },
      # Trigger the signal only when the streak has lasted long enough
      !!signal_name := as.integer(.data$streak >= n_consecutive)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$cond, -.data$streak)

  # ── Diagnostics ────────────────────────────────────────────────────────────
  message(
    " Generated consecutive signal column: ", signal_name,
    " (requires ", n_consecutive, " consecutive days)"
  )

  # ── Output Format Conversion ───────────────────────────────────────────────
  if (output == "tibble") result <- tibble::as_tibble(result)

  return(result)
}

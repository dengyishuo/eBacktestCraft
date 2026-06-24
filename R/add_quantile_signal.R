#' Generate signal by selecting top/bottom quantile of stocks each day
#'
#' Within each trading day, rank the indicator and select stocks that fall
#' above a given quantile (e.g., top 20%) or below a quantile (bottom 20%).
#' This is a flexible alternative to \code{add_rank_signal} when you want
#' to select a variable number of stocks (percentage-based) rather than a fixed count.
#'
#' @param mkt_data Data frame in long format, must contain 'date' and 'code' columns
#' @param rank_col Character string of column name to rank (e.g., "mom_20")
#' @param quantile Numeric between 0 and 1. For top selection, the top `quantile`
#'   fraction of stocks (e.g., 0.2 = top 20\%). For bottom selection, use `select = "bottom"`.
#' @param select Character: "top" (higher values are better) or "bottom"
#' @param signal_name Output signal column name. Auto-generated if NULL.
#' @param output Output format: "tibble" (default) or "data.frame"
#'
#' @return Data frame with an appended 0/1 signal column (1 = selected by quantile)
#'
#' @importFrom dplyr group_by mutate ungroup summarise
#' @importFrom stats quantile
#' @importFrom rlang .data !! sym :=
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic usage, select top 20% stocks by momentum each day
#' mkt_data <- data.frame(
#'   date   = rep(seq(as.Date("2023-01-01"), by = "day", length.out = 60), each = 3),
#'   code   = rep(c("AAPL", "MSFT", "GOOG"), times = 60),
#'   name   = rep(c("Apple", "Microsoft", "Alphabet"), times = 60),
#'   close  = round(runif(180, 100, 300), 2),
#'   open   = round(runif(180, 100, 300), 2),
#'   mom_20 = round(runif(180, -0.2, 0.2), 4),
#'   stringsAsFactors = FALSE
#' )
#' result <- add_quantile_signal(mkt_data, rank_col = "mom_20", quantile = 0.2, select = "top")
#'
#' # Example 2: Key parameter variant, select bottom 30% by momentum (contrarian signal)
#' result_bottom <- add_quantile_signal(mkt_data,
#'   rank_col    = "mom_20",
#'   quantile    = 0.30,
#'   select      = "bottom",
#'   signal_name = "signal_mom20_bottom30pct",
#'   output      = "data.frame"
#' )
#'
#' # Example 3: Backtest workflow, quantile signal to equal weight to run_backtest
#' mkt_data <- add_quantile_signal(mkt_data, rank_col = "mom_20", quantile = 0.2, select = "top")
#' mkt_data <- add_equal_weight(mkt_data, signal_col = "signal_mom_20_top_q20")
#' # bt <- run_backtest(mkt_data, weight_col = "weight_equal_signal_mom_20_top_q20")
#' }
add_quantile_signal <- function(
  mkt_data,
  rank_col,
  quantile = 0.2,
  select = c("top", "bottom"),
  signal_name = NULL,
  output = c("tibble", "data.frame")
) {
  # ── Input Validation ───────────────────────────────────────────────────────
  # Both 'date' and 'code' are needed for cross-sectional daily grouping
  if (!all(c("date", "code") %in% colnames(mkt_data))) {
    stop("mkt_data must contain 'date' and 'code' columns!")
  }
  if (!rank_col %in% colnames(mkt_data)) {
    stop("Rank column not found in mkt_data: ", rank_col)
  }
  # Quantile must be strictly interior to (0,1) so the threshold is well-defined
  if (quantile <= 0 || quantile >= 1) {
    stop("quantile must be between 0 and 1 (exclusive)")
  }

  select <- match.arg(select)
  output <- match.arg(output)

  # ── Auto-Generate Signal Column Name ───────────────────────────────────────
  if (is.null(signal_name)) {
    select_text <- ifelse(select == "top", "top", "bottom")
    signal_name <- paste0("signal_", rank_col, "_", select_text, "_q", quantile * 100)
  }

  # ── Cross-Sectional Quantile Signal ────────────────────────────────────────
  # The quantile threshold is recomputed each day to handle changing universes
  result <- mkt_data %>%
    dplyr::group_by(.data$date) %>%
    dplyr::mutate(
      rank_val = !!sym(rank_col),
      # Compute per-day quantile cutoff (type = 8 is the R default, interpolates)
      q_thresh = if (select == "top") {
        stats::quantile(.data$rank_val, probs = 1 - quantile, na.rm = TRUE, type = 8)
      } else {
        stats::quantile(.data$rank_val, probs = quantile,     na.rm = TRUE, type = 8)
      },
      # Assign 1 when the stock falls in the target quantile tail
      !!signal_name := dplyr::case_when(
        select == "top"    ~ as.integer(.data$rank_val >= .data$q_thresh & !is.na(.data$rank_val)),
        select == "bottom" ~ as.integer(.data$rank_val <= .data$q_thresh & !is.na(.data$rank_val))
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$rank_val, -.data$q_thresh)

  # ── Post-Processing ────────────────────────────────────────────────────────
  result[[signal_name]] <- ifelse(is.na(result[[signal_name]]), 0, result[[signal_name]])

  # ── Diagnostics ────────────────────────────────────────────────────────────
  daily_counts <- result %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(n = sum(!!sym(signal_name), na.rm = TRUE), .groups = "drop")
  avg_selected <- round(mean(daily_counts$n, na.rm = TRUE), 2)
  pct_selected <- quantile * 100

  message(
    " Generated quantile signal column: ", signal_name,
    " (select ", select, " ", pct_selected, "% (", avg_selected, " avg stocks/day))"
  )

  # ── Output Format Conversion ───────────────────────────────────────────────
  if (output == "tibble") result <- tibble::as_tibble(result)

  return(result)
}

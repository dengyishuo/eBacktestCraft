#' Rank-based signal generation function
#'
#' Generate 0/1 signals by ranking specified indicators within each trading day.
#' Select TOP N or BOTTOM N stocks based on ranking order.
#' Optionally filter stocks by a minimum threshold on the ranking column before ranking.
#' Fully compatible with quantitative strategy long-format data structure.
#'
#' @param mkt_data Data frame in long format, must contain 'date' and 'code' columns
#' @param rank_col Character string of column name to rank (e.g., "mom_20", "vol_60")
#' @param top_n Integer, number of stocks to select per day, default 2
#' @param rank_order Ranking order: "desc" for descending (TOP), "asc" for ascending (BOTTOM)
#' @param tie_method Method for handling ties: "min", "max", or "average", default "min"
#' @param rank_threshold Numeric value; only stocks with rank_col > rank_threshold are considered for ranking.
#'   If NULL (default), no threshold filtering is applied.
#' @param signal_name Character, output signal column name. Auto-generated if NULL
#' @param output Output format: "tibble" (default) or "data.frame"
#'
#' @return Data frame with an appended 0/1 signal column (1 = selected, 0 = not selected)
#'   in the specified output format
#'
#' @importFrom dplyr group_by mutate ungroup select summarise
#' @importFrom rlang .data !! sym :=
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic usage, select TOP 2 stocks by 20-day momentum each day
#' mkt_data <- data.frame(
#'   date   = rep(seq(as.Date("2023-01-01"), by = "day", length.out = 60), each = 3),
#'   code   = rep(c("AAPL", "MSFT", "GOOG"), times = 60),
#'   name   = rep(c("Apple", "Microsoft", "Alphabet"), times = 60),
#'   close  = round(runif(180, 100, 300), 2),
#'   open   = round(runif(180, 100, 300), 2),
#'   mom_20 = round(runif(180, -0.2, 0.2), 4),
#'   stringsAsFactors = FALSE
#' )
#' result <- add_rank_signal(mkt_data, rank_col = "mom_20", top_n = 2)
#'
#' # Example 2: Key parameter variants, ascending rank with pre-filter
#' result_bottom <- add_rank_signal(mkt_data,
#'   rank_col        = "mom_20",
#'   top_n           = 1,
#'   rank_order      = "asc",        # Select worst performer
#'   rank_threshold  = -0.05,        # Only consider stocks with mom_20 > -0.05
#'   tie_method      = "max"
#' )
#'
#' # Example 3: Backtest workflow, rank signal -> norm weight -> run_backtest
#' mkt_data <- add_rank_signal(mkt_data, rank_col = "mom_20", top_n = 2)
#' mkt_data <- add_norm_weight(mkt_data,
#'   weight_col = "mom_20",
#'   signal_col = "signal_mom_20_top2"
#' )
#' # bt <- run_backtest(mkt_data, weight_col = "weight_mom_20_signal_mom_20_top2")
#' }
add_rank_signal <- function(
  mkt_data,
  rank_col,
  top_n = 2,
  rank_order = "desc",
  tie_method = "min",
  rank_threshold = NULL,
  signal_name = NULL,
  output = c("tibble", "data.frame")
) {
  # ── Input Validation ───────────────────────────────────────────────────────
  # Panel structure requires both 'date' (time axis) and 'code' (entity axis)
  if (!all(c("date", "code") %in% colnames(mkt_data))) {
    stop("mkt_data must contain 'date' and 'code' columns!")
  }
  if (!rank_col %in% colnames(mkt_data)) {
    stop("Ranking column not found in mkt_data: ", rank_col)
  }

  output <- match.arg(output)

  # ── Auto-Generate Signal Column Name ───────────────────────────────────────
  if (is.null(signal_name)) {
    order_text <- ifelse(rank_order == "desc", "top", "bottom")
    if (!is.null(rank_threshold)) {
      signal_name <- paste0("signal_", rank_col, "_", order_text, top_n, "_gt", rank_threshold)
    } else {
      signal_name <- paste0("signal_", rank_col, "_", order_text, top_n)
    }
  }

  # ── Core Ranking Logic ─────────────────────────────────────────────────────
  # Group by date so rankings are computed cross-sectionally (one day at a time)
  result <- mkt_data %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
      # Coerce NA/Inf to NA so they are pushed to the tail by na.last = TRUE
      rank_val_raw = ifelse(is.na(!!sym(rank_col)) | is.infinite(!!sym(rank_col)), NA_real_, !!sym(rank_col)),

      # Threshold gate: rows failing the filter are excluded from the rank pool
      pass_filter  = if (!is.null(rank_threshold)) rank_val_raw > rank_threshold else TRUE,
      pass_filter  = ifelse(is.na(pass_filter), FALSE, pass_filter),

      # Set filtered-out rows to NA so rank() places them last
      rank_val     = ifelse(pass_filter, rank_val_raw, NA_real_),

      # Rank descending: negate value so rank() gives 1 to the largest
      .rank = if (rank_order == "desc") {
        rank(-rank_val, na.last = TRUE, ties.method = tie_method)
      } else {
        rank(rank_val,  na.last = TRUE, ties.method = tie_method)
      },

      # Signal is 1 only when the row both passes the filter and lands within top_n
      !!signal_name := as.integer(pass_filter & .rank <= top_n)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-rank_val_raw, -rank_val, -.rank, -pass_filter)

  # ── Post-Processing ────────────────────────────────────────────────────────
  # Replace residual NAs (edge case: all values NA on a given day) with 0
  result[[signal_name]] <- ifelse(is.na(result[[signal_name]]), 0, result[[signal_name]])

  # ── Diagnostics ────────────────────────────────────────────────────────────
  daily_count <- result %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(n = sum(!!sym(signal_name), na.rm = TRUE), .groups = "drop")
  avg_selected <- round(mean(daily_count$n, na.rm = TRUE), 2)

  message(
    "Generated signal column: ", signal_name,
    " | Daily average selected: ", avg_selected, " (target top_n = ", top_n, ")"
  )

  # ── Output Format Conversion ───────────────────────────────────────────────
  if (output == "tibble") result <- tibble::as_tibble(result)

  return(result)
}

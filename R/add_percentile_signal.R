#' Add Cross-Sectional Percentile Signal
#'
#' Generates a signal for assets whose indicator value falls within the top or
#' bottom percentile of the cross-section on each date. Unlike
#' \code{\link{add_rank_signal}} which selects a fixed count, this selects a
#' fixed proportion of the pool.
#'
#' @param mkt_data A long-format data frame with columns \code{date} and \code{code}.
#' @param indicator_col Character. Indicator column to rank cross-sectionally.
#' @param pct Numeric in (0, 1). Percentile threshold. Default \code{0.2} (top/bottom 20\%).
#' @param select Character. \code{"top"} (default) = highest values; \code{"bottom"} = lowest values.
#' @param signal_name Output column name. Auto-generated if \code{NULL}.
#' @param output \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return \code{mkt_data} with one appended integer signal column (0/1).
#'
#' @examples
#' data(style, package = "eBacktestCraft")
#' df <- eClassic::add_mom(style, close_col = "adjusted", n = 20)
#' result <- add_percentile_signal(df, indicator_col = "mom_20",
#'                                 pct = 0.3, select = "top")
#'
#' @family signal-cross
#' @importFrom tibble as_tibble
#' @export
add_percentile_signal <- function(mkt_data,
                                  indicator_col,
                                  pct         = 0.2,
                                  select      = c("top", "bottom"),
                                  signal_name = NULL,
                                  output      = c("tibble", "data.frame")) {
  output <- match.arg(output)
  select <- match.arg(select)

  if (!all(c("date", "code") %in% colnames(mkt_data)))
    stop("'mkt_data' must contain 'date' and 'code' columns.")
  if (!indicator_col %in% colnames(mkt_data))
    stop("Column '", indicator_col, "' not found in mkt_data.")
  if (pct <= 0 || pct >= 1)
    stop("'pct' must be in (0, 1).")

  col <- if (is.null(signal_name))
    paste0("signal_pct_", indicator_col, "_", select,
           "_", gsub("\\.", "", as.character(pct)))
  else signal_name

  dates <- unique(mkt_data$date)
  sig   <- integer(nrow(mkt_data))

  for (dt in as.character(dates)) {
    idx <- which(as.character(mkt_data$date) == dt)
    v   <- mkt_data[[indicator_col]][idx]
    valid <- !is.na(v)
    if (sum(valid) == 0) next
    cutoff <- quantile(v[valid],
                       probs = if (select == "top") 1 - pct else pct,
                       na.rm = TRUE)
    cond <- if (select == "top") v >= cutoff else v <= cutoff
    cond[is.na(cond)] <- FALSE
    sig[idx] <- as.integer(cond)
  }

  mkt_data[[col]] <- sig
  message("Generated signal column: ", col,
          ", valid signals: ", sum(mkt_data[[col]], na.rm = TRUE))

  if (output == "tibble") tibble::as_tibble(mkt_data) else mkt_data
}

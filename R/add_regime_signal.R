#' Add Market Regime Signal
#'
#' Identifies bull/bear market regimes based on whether the price (or index) is
#' above or below a long-term moving average. Commonly used as a macro filter to
#' switch between risk-on and risk-off positioning.
#'
#' Signal = 1 when in bull regime (price > MA); 0 when in bear regime.
#'
#' @param mkt_data A long-format data frame with columns \code{date} and \code{code}.
#' @param close_col Character. Price column used to determine regime.
#'   Default \code{"close"}.
#' @param n Integer. Moving average window for regime identification.
#'   Default \code{200L} (classic 200-day MA rule).
#' @param ma_col Character. Pre-computed MA column name. If supplied,
#'   \code{n} and \code{close_col} are ignored.
#' @param signal_name Output column name. Auto-generated if \code{NULL}.
#' @param output \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return \code{mkt_data} with one appended integer signal column (0/1).
#'   1 = bull regime (price above MA), 0 = bear regime.
#'
#' @examples
#' data(style, package = "eBacktestCraft")
#' result <- add_regime_signal(style, close_col = "adjusted", n = 120)
#'
#' @family signal-timeseries
#' @importFrom tibble as_tibble
#' @export
add_regime_signal <- function(mkt_data,
                              close_col   = "close",
                              n           = 200L,
                              ma_col      = NULL,
                              signal_name = NULL,
                              output      = c("tibble", "data.frame")) {
  output <- match.arg(output)

  if (!all(c("date", "code") %in% colnames(mkt_data)))
    stop("'mkt_data' must contain 'date' and 'code' columns.")
  if (is.null(ma_col) && !close_col %in% colnames(mkt_data))
    stop("Column '", close_col, "' not found in mkt_data.")
  if (!is.null(ma_col) && !ma_col %in% colnames(mkt_data))
    stop("Column '", ma_col, "' not found in mkt_data.")

  col <- if (is.null(signal_name))
    paste0("signal_regime_ma", n) else signal_name

  compute_ma <- function(x, w) {
    out <- rep(NA_real_, length(x))
    for (i in seq_len(length(x)))
      if (i >= w) out[i] <- mean(x[(i - w + 1):i], na.rm = TRUE)
    out
  }

  result_list <- lapply(unique(mkt_data$code), function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]
    px  <- sub[[close_col]]
    ma  <- if (!is.null(ma_col)) sub[[ma_col]] else compute_ma(px, n)

    cond   <- px > ma
    cond[is.na(cond)] <- FALSE
    sub[[col]] <- as.integer(cond)
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  message("Generated signal column: ", col,
          ", bull-regime rows: ", sum(res[[col]], na.rm = TRUE))

  if (output == "tibble") tibble::as_tibble(res) else res
}

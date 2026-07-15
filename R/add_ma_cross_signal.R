#' Add Moving Average Crossover Signal
#'
#' Generates a signal when a fast moving average crosses above (golden cross,
#' signal = 1) or below (death cross, signal = -1) a slow moving average.
#' Both MAs are computed internally from a price column, or you can supply
#' pre-computed MA columns via \code{fast_col} / \code{slow_col}.
#'
#' @param mkt_data A long-format data frame with columns \code{date} and \code{code}.
#' @param close_col Character. Price column used to compute MAs. Default \code{"close"}.
#' @param fast_n Integer. Fast MA window. Default \code{5L}.
#' @param slow_n Integer. Slow MA window. Default \code{20L}.
#' @param fast_col Character. Pre-computed fast MA column name. If supplied,
#'   \code{fast_n} and \code{close_col} are ignored for the fast leg.
#' @param slow_col Character. Pre-computed slow MA column name. If supplied,
#'   \code{slow_n} is ignored for the slow leg.
#' @param mode Character. \code{"golden"} (default) = signal 1 on upward cross only;
#'   \code{"death"} = signal 1 on downward cross only;
#'   \code{"both"} = 1 on golden cross, -1 on death cross, 0 otherwise.
#' @param signal_name Output column name. Auto-generated if \code{NULL}.
#' @param output \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return \code{mkt_data} with one appended integer signal column.
#'
#' @examples
#' data(style, package = "eBacktestCraft")
#' result <- add_ma_cross_signal(style, close_col = "adjusted",
#'                               fast_n = 5, slow_n = 20)
#'
#' @family signal-timeseries
#' @importFrom tibble as_tibble
#' @export
add_ma_cross_signal <- function(mkt_data,
                                close_col   = "close",
                                fast_n      = 5L,
                                slow_n      = 20L,
                                fast_col    = NULL,
                                slow_col    = NULL,
                                mode        = c("golden", "death", "both"),
                                signal_name = NULL,
                                output      = c("tibble", "data.frame")) {
  output <- match.arg(output)
  mode   <- match.arg(mode)

  if (!all(c("date", "code") %in% colnames(mkt_data)))
    stop("'mkt_data' must contain 'date' and 'code' columns.")

  compute_ma <- function(x, n) {
    out <- rep(NA_real_, length(x))
    for (i in seq_len(length(x))) {
      if (i >= n) out[i] <- mean(x[(i - n + 1):i], na.rm = TRUE)
    }
    out
  }

  result_list <- lapply(unique(mkt_data$code), function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]
    px  <- sub[[if (!is.null(fast_col)) fast_col else close_col]]

    fast <- if (!is.null(fast_col)) sub[[fast_col]] else compute_ma(px, fast_n)
    slow <- if (!is.null(slow_col)) sub[[slow_col]] else
            compute_ma(sub[[close_col]], slow_n)

    n    <- nrow(sub)
    sig  <- integer(n)
    for (i in 2:n) {
      if (is.na(fast[i]) || is.na(slow[i]) ||
          is.na(fast[i-1]) || is.na(slow[i-1])) next
      golden <- fast[i] > slow[i] && fast[i-1] <= slow[i-1]
      death  <- fast[i] < slow[i] && fast[i-1] >= slow[i-1]
      sig[i] <- switch(mode,
        golden = if (golden)  1L else 0L,
        death  = if (death)   1L else 0L,
        both   = if (golden)  1L else if (death) -1L else 0L
      )
    }
    sub[[if (is.null(signal_name))
           paste0("signal_ma", fast_n, "_cross_ma", slow_n)
         else signal_name]] <- sig
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  col <- if (is.null(signal_name))
    paste0("signal_ma", fast_n, "_cross_ma", slow_n) else signal_name
  message("Generated signal column: ", col,
          ", valid signals: ", sum(abs(res[[col]]), na.rm = TRUE))

  if (output == "tibble") tibble::as_tibble(res) else res
}

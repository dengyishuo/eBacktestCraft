#' Add Breakout Signal
#'
#' Generates a signal when the price breaks above the N-day high (upward breakout,
#' signal = 1) or below the N-day low (downward breakout, signal = 1 or -1).
#' Classic Donchian Channel / Turtle Trading trigger.
#'
#' @param mkt_data A long-format data frame with columns \code{date} and \code{code}.
#' @param close_col Character. Price column to test. Default \code{"close"}.
#' @param n Integer. Lookback window for rolling high/low. Default \code{20L}.
#' @param mode Character.
#'   \code{"up"} (default) = signal 1 when price exceeds rolling high;
#'   \code{"down"} = signal 1 when price falls below rolling low;
#'   \code{"both"} = 1 on upward breakout, -1 on downward breakout.
#' @param signal_name Output column name. Auto-generated if \code{NULL}.
#' @param output \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return \code{mkt_data} with one appended integer signal column.
#'
#' @examples
#' data(style, package = "eBacktestCraft")
#' result <- add_breakout_signal(style, close_col = "adjusted", n = 20)
#'
#' @family signal-timeseries
#' @importFrom tibble as_tibble
#' @export
add_breakout_signal <- function(mkt_data,
                                close_col   = "close",
                                n           = 20L,
                                mode        = c("up", "down", "both"),
                                signal_name = NULL,
                                output      = c("tibble", "data.frame")) {
  output <- match.arg(output)
  mode   <- match.arg(mode)

  if (!all(c("date", "code") %in% colnames(mkt_data)))
    stop("'mkt_data' must contain 'date' and 'code' columns.")
  if (!close_col %in% colnames(mkt_data))
    stop("Column '", close_col, "' not found in mkt_data.")

  col <- if (is.null(signal_name))
    paste0("signal_breakout_", n, "_", mode) else signal_name

  result_list <- lapply(unique(mkt_data$code), function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]
    px  <- sub[[close_col]]
    nr  <- nrow(sub)
    sig <- integer(nr)

    for (i in seq_len(nr)) {
      if (i <= n) next
      window    <- px[(i - n):(i - 1)]   # prior N bars, excludes current
      roll_high <- max(window, na.rm = TRUE)
      roll_low  <- min(window, na.rm = TRUE)
      if (is.na(px[i])) next
      sig[i] <- switch(mode,
        up   = if (px[i] > roll_high)  1L else 0L,
        down = if (px[i] < roll_low)   1L else 0L,
        both = if (px[i] > roll_high)  1L else
               if (px[i] < roll_low)  -1L else 0L
      )
    }

    sub[[col]] <- sig
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  message("Generated signal column: ", col,
          ", valid signals: ", sum(abs(res[[col]]), na.rm = TRUE))

  if (output == "tibble") tibble::as_tibble(res) else res
}

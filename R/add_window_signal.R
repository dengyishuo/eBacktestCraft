#' Extend a Signal Over a Lookforward Window
#'
#' Given an existing 0/1 signal column, keeps the signal active for \code{window}
#' bars after each trigger. Useful when an event (e.g., TD Setup = 9, earnings
#' release) should influence positions for several days after it fires.
#'
#' @param mkt_data A long-format data frame with columns \code{date} and \code{code}.
#' @param signal_col Character. Name of an existing 0/1 (or non-zero) signal column.
#' @param window Integer. Number of bars the signal stays active after the trigger
#'   bar (inclusive). \code{window = 1} means only the trigger bar itself;
#'   \code{window = 5} means the trigger bar plus the 4 following bars.
#' @param carry_value Numeric. Value written during the extended window.
#'   Default \code{1L}. Use \code{-1} if the original signal encodes direction
#'   and you want to preserve it (set \code{carry_value = NA} to copy the
#'   original trigger value instead).
#' @param signal_name Output column name. Auto-generated if \code{NULL}.
#' @param output \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return \code{mkt_data} with one appended integer signal column.
#'
#' @examples
#' data(style, package = "eBacktestCraft")
#' df <- add_breakout_signal(style, close_col = "adjusted", n = 20)
#' result <- add_window_signal(df, signal_col = "signal_breakout_bull", window = 5)
#'
#' @family signal-composite
#' @importFrom tibble as_tibble
#' @export
add_window_signal <- function(mkt_data,
                              signal_col,
                              window      = 5L,
                              carry_value = 1L,
                              signal_name = NULL,
                              output      = c("tibble", "data.frame")) {
  output <- match.arg(output)

  if (!all(c("date", "code") %in% colnames(mkt_data)))
    stop("'mkt_data' must contain 'date' and 'code' columns.")
  if (!signal_col %in% colnames(mkt_data))
    stop("Signal column '", signal_col, "' not found in mkt_data.")
  if (!is.numeric(window) || window < 1L)
    stop("'window' must be a positive integer.")

  window <- as.integer(window)
  col <- if (is.null(signal_name))
    paste0(signal_col, "_win", window) else signal_name

  copy_trigger <- is.na(carry_value)

  result_list <- lapply(unique(mkt_data$code), function(cd) {
    sub <- mkt_data[mkt_data$code == cd, ]
    sub <- sub[order(sub$date), ]
    nr  <- nrow(sub)
    src <- sub[[signal_col]]
    out <- integer(nr)

    for (i in seq_len(nr)) {
      if (!is.na(src[i]) && src[i] != 0L) {
        end_i <- min(nr, i + window - 1L)
        fill  <- if (copy_trigger) src[i] else carry_value
        out[i:end_i] <- as.integer(fill)
      }
    }

    sub[[col]] <- out
    sub
  })

  res <- do.call(rbind, result_list)
  res <- res[order(res$date, res$code), ]

  message("Generated signal column: ", col,
          ", valid signals: ", sum(res[[col]] != 0L, na.rm = TRUE))

  if (output == "tibble") tibble::as_tibble(res) else res
}

#' Add TD Sequential Setup Signal
#'
#' Generates a signal when TD Sequential Setup completes (count = 9), based on
#' the output columns of \code{eTTR::add_td_setup()}. A bullish setup completion
#' (bear exhaustion) triggers a long signal; a bearish setup completion (bull
#' exhaustion) triggers a short signal.
#'
#' This function requires that \code{eTTR::add_td_setup()} has already been
#' called on \code{mkt_data} to produce the setup count columns.
#'
#' @param mkt_data A long-format data frame with columns \code{date} and
#'   \code{code}, already containing TD Setup columns.
#' @param setup_bull_col Character. Bullish setup count column name.
#'   Default \code{"td_setup_bull"} (output of \code{add_td_setup()}).
#' @param setup_bear_col Character. Bearish setup count column name.
#'   Default \code{"td_setup_bear"}.
#' @param mode Character.
#'   \code{"bull"} (default) = signal 1 when bearish setup completes (= 9),
#'   indicating bear exhaustion and potential long entry;
#'   \code{"bear"} = signal 1 when bullish setup completes (= 9),
#'   indicating bull exhaustion and potential short/exit entry;
#'   \code{"both"} = 1 on bear exhaustion, -1 on bull exhaustion.
#' @param window Integer. If > 0, signal stays active for \code{window} bars
#'   after the setup completes (lookback window). Default \code{0L} = signal
#'   only on the exact bar where count = 9.
#' @param signal_name Output column name. Auto-generated if \code{NULL}.
#' @param output \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return \code{mkt_data} with one appended integer signal column.
#'
#' @examples
#' \dontrun{
#' library(eTTR)
#' data(ettr_stocks)
#' df <- add_td_setup(ettr_stocks)
#' result <- add_td_setup_signal(df, mode = "bull", window = 5)
#' }
#'
#' @family signal-timeseries
#' @importFrom tibble as_tibble
#' @export
add_td_setup_signal <- function(mkt_data,
                                setup_bull_col = "td_setup_bull",
                                setup_bear_col = "td_setup_bear",
                                mode           = c("bull", "bear", "both"),
                                window         = 0L,
                                signal_name    = NULL,
                                output         = c("tibble", "data.frame")) {
  output <- match.arg(output)
  mode   <- match.arg(mode)

  if (!all(c("date", "code") %in% colnames(mkt_data)))
    stop("'mkt_data' must contain 'date' and 'code' columns.")
  missing_cols <- setdiff(c(setup_bull_col, setup_bear_col), colnames(mkt_data))
  if (length(missing_cols) > 0)
    stop("Missing TD Setup columns (run eTTR::add_td_setup() first): ",
         paste(missing_cols, collapse = ", "))

  col <- if (is.null(signal_name))
    paste0("signal_td_setup_", mode) else signal_name

  result_list <- lapply(unique(mkt_data$code), function(cd) {
    sub  <- mkt_data[mkt_data$code == cd, ]
    sub  <- sub[order(sub$date), ]
    nr   <- nrow(sub)
    bull <- sub[[setup_bull_col]]
    bear <- sub[[setup_bear_col]]
    sig  <- integer(nr)

    for (i in seq_len(nr)) {
      bear_complete <- !is.na(bear[i]) && bear[i] == 9L
      bull_complete <- !is.na(bull[i]) && bull[i] == 9L

      raw <- switch(mode,
        bull = if (bear_complete)  1L else 0L,
        bear = if (bull_complete)  1L else 0L,
        both = if (bear_complete)  1L else if (bull_complete) -1L else 0L
      )
      sig[i] <- raw
    }

    # Extend signal over window bars after trigger
    if (window > 0L) {
      extended <- sig
      for (i in seq_len(nr)) {
        if (sig[i] != 0L) {
          end_i <- min(nr, i + window - 1L)
          extended[i:end_i] <- sig[i]
        }
      }
      sig <- extended
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

#' Add Voting Signal
#'
#' Combines multiple existing 0/1 signal columns by majority vote: the output
#' signal is 1 when the number of active signals meets or exceeds
#' \code{min_votes}. More robust than AND (which is too strict) or OR (which
#' is too loose) when signals are noisy and partially correlated.
#'
#' @param mkt_data A long-format data frame with columns \code{date} and \code{code}.
#' @param signal_cols Character vector of existing 0/1 signal column names to vote on.
#'   Must be at least 2 columns.
#' @param min_votes Integer. Minimum number of signals that must be active (= 1)
#'   to trigger the output signal. Default \code{NULL} = strict majority
#'   (\code{ceiling(length(signal_cols) / 2)}).
#' @param signal_name Output column name. Auto-generated if \code{NULL}.
#' @param output \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return \code{mkt_data} with one appended integer signal column (0/1).
#'
#' @examples
#' data(style, package = "eBacktestCraft")
#' df <- add_indicator(style, "mom", close_col = "adjusted", n = c(5, 10, 20))
#' df <- add_threshold_signal(df, "mom_5",  threshold = 0, signal_name = "s1")
#' df <- add_threshold_signal(df, "mom_10", threshold = 0, signal_name = "s2")
#' df <- add_threshold_signal(df, "mom_20", threshold = 0, signal_name = "s3")
#' result <- add_vote_signal(df, signal_cols = c("s1", "s2", "s3"), min_votes = 2)
#'
#' @family signal-composite
#' @importFrom tibble as_tibble
#' @export
add_vote_signal <- function(mkt_data,
                            signal_cols,
                            min_votes   = NULL,
                            signal_name = NULL,
                            output      = c("tibble", "data.frame")) {
  output <- match.arg(output)

  if (!all(c("date", "code") %in% colnames(mkt_data)))
    stop("'mkt_data' must contain 'date' and 'code' columns.")
  if (length(signal_cols) < 2)
    stop("'signal_cols' must contain at least 2 column names.")
  missing_cols <- setdiff(signal_cols, colnames(mkt_data))
  if (length(missing_cols) > 0)
    stop("Columns not found in mkt_data: ", paste(missing_cols, collapse = ", "))

  if (is.null(min_votes))
    min_votes <- ceiling(length(signal_cols) / 2)
  if (min_votes < 1 || min_votes > length(signal_cols))
    stop("'min_votes' must be between 1 and length(signal_cols).")

  col <- if (is.null(signal_name))
    paste0("signal_vote_", min_votes, "of", length(signal_cols))
  else signal_name

  mat <- do.call(cbind, lapply(signal_cols, function(s) {
    v <- mkt_data[[s]]
    v[is.na(v)] <- 0L
    as.integer(v)
  }))

  votes <- rowSums(mat, na.rm = TRUE)
  mkt_data[[col]] <- as.integer(votes >= min_votes)

  message("Generated signal column: ", col,
          " (min_votes = ", min_votes, "/", length(signal_cols), ")",
          ", valid signals: ", sum(mkt_data[[col]], na.rm = TRUE))

  if (output == "tibble") tibble::as_tibble(mkt_data) else mkt_data
}

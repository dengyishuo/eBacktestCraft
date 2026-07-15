#' Add OR-Combined Signal
#'
#' Merges multiple existing 0/1 signal columns with OR logic: the output signal
#' is 1 when **any** input signal is 1. Use this to build composite opportunity
#' detectors where any single trigger is sufficient to enter.
#'
#' @param mkt_data A long-format data frame with columns \code{date} and \code{code}.
#' @param signal_cols Character vector of existing 0/1 signal column names to combine.
#'   Must be at least 2 columns.
#' @param signal_name Output column name. Auto-generated if \code{NULL}.
#' @param output \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return \code{mkt_data} with one appended integer signal column (0/1).
#'
#' @examples
#' data(style, package = "eBacktestCraft")
#' df <- eClassic::add_mom(style, close_col = "adjusted", n = c(10, 20))
#' df <- add_threshold_signal(df, indicator_cols = "mom_10",
#'                            threshold = 0, compare_op = ">",
#'                            signal_name = "sig_mom10")
#' df <- add_threshold_signal(df, indicator_cols = "mom_20",
#'                            threshold = 0, compare_op = ">",
#'                            signal_name = "sig_mom20")
#' result <- add_or_signal(df, signal_cols = c("sig_mom10", "sig_mom20"))
#'
#' @family signal-composite
#' @importFrom tibble as_tibble
#' @export
add_or_signal <- function(mkt_data,
                          signal_cols,
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

  col <- if (is.null(signal_name))
    paste0("signal_or_", paste(signal_cols, collapse = "_"))
  else signal_name

  result <- Reduce(`|`, lapply(signal_cols, function(s) {
    v <- mkt_data[[s]]
    v[is.na(v)] <- 0L
    as.logical(v)
  }))

  mkt_data[[col]] <- as.integer(result)
  message("Generated signal column: ", col,
          ", valid signals: ", sum(mkt_data[[col]], na.rm = TRUE))

  if (output == "tibble") tibble::as_tibble(mkt_data) else mkt_data
}

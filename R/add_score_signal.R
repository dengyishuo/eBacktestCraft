#' Add Multi-Factor Score Signal
#'
#' Computes a composite score by taking a weighted sum of multiple indicator
#' columns (after optional cross-sectional Z-score normalisation), then selects
#' the top \code{top_n} assets by score on each date.
#'
#' This is the standard multi-factor stock selection workflow:
#' normalise → weight → score → rank → select.
#'
#' @param mkt_data A long-format data frame with columns \code{date} and \code{code}.
#' @param indicator_cols Character vector. Indicator columns to combine.
#' @param weights Numeric vector. Weights for each indicator. Must be the same
#'   length as \code{indicator_cols}. Need not sum to 1 (auto-normalised).
#'   Default: equal weights.
#' @param top_n Integer. Number of top-scoring assets to select per date.
#'   Default \code{3L}.
#' @param normalize Logical. If \code{TRUE} (default), cross-sectionally
#'   Z-score each indicator before weighting to make scales comparable.
#' @param score_col Character. Name of the intermediate composite score column.
#'   Default \code{"composite_score"}.
#' @param signal_name Output signal column name. Auto-generated if \code{NULL}.
#' @param output \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return \code{mkt_data} with two appended columns: \code{score_col} (numeric
#'   composite score) and the integer signal column (0/1).
#'
#' @examples
#' data(style, package = "eBacktestCraft")
#' df <- add_indicator(style, "mom", close_col = "adjusted", n = c(10, 20))
#' result <- add_score_signal(df,
#'   indicator_cols = c("mom_10", "mom_20"),
#'   weights        = c(0.4, 0.6),
#'   top_n          = 3)
#'
#' @family signal-cross
#' @importFrom tibble as_tibble
#' @export
add_score_signal <- function(mkt_data,
                             indicator_cols,
                             weights     = NULL,
                             top_n       = 3L,
                             normalize   = TRUE,
                             score_col   = "composite_score",
                             signal_name = NULL,
                             output      = c("tibble", "data.frame")) {
  output <- match.arg(output)

  if (!all(c("date", "code") %in% colnames(mkt_data)))
    stop("'mkt_data' must contain 'date' and 'code' columns.")
  missing_cols <- setdiff(indicator_cols, colnames(mkt_data))
  if (length(missing_cols) > 0)
    stop("Columns not found in mkt_data: ", paste(missing_cols, collapse = ", "))

  k <- length(indicator_cols)
  if (is.null(weights)) weights <- rep(1 / k, k)
  if (length(weights) != k)
    stop("'weights' must have the same length as 'indicator_cols'.")
  weights <- weights / sum(weights)   # normalise to sum = 1

  col <- if (is.null(signal_name))
    paste0("signal_score_top", top_n) else signal_name

  dates <- unique(mkt_data$date)
  score_vec <- rep(NA_real_, nrow(mkt_data))
  sig_vec   <- integer(nrow(mkt_data))

  for (dt in as.character(dates)) {
    idx <- which(as.character(mkt_data$date) == dt)
    mat <- matrix(NA_real_, nrow = length(idx), ncol = k)

    for (j in seq_len(k)) {
      v <- mkt_data[[indicator_cols[j]]][idx]
      if (normalize) {
        mu  <- mean(v, na.rm = TRUE)
        sd_ <- sd(v,   na.rm = TRUE)
        v   <- if (!is.na(sd_) && sd_ > 0) (v - mu) / sd_ else v - mu
      }
      mat[, j] <- v
    }

    score <- as.numeric(mat %*% weights)
    score_vec[idx] <- score

    valid <- which(!is.na(score))
    if (length(valid) == 0) next
    ranked   <- order(score[valid], decreasing = TRUE)
    selected <- valid[ranked[seq_len(min(top_n, length(valid)))]]
    sig_vec[idx[selected]] <- 1L
  }

  mkt_data[[score_col]] <- score_vec
  mkt_data[[col]]       <- sig_vec

  message("Generated signal column: ", col,
          ", valid signals: ", sum(mkt_data[[col]], na.rm = TRUE))

  if (output == "tibble") tibble::as_tibble(mkt_data) else mkt_data
}

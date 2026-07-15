#' Add Z-Score Signal
#'
#' Computes the cross-sectional Z-score of an indicator on each date and
#' generates a signal for rows whose Z-score exceeds a threshold.
#' Useful for standardised factor selection where raw values are not comparable
#' across assets.
#'
#' @param mkt_data A long-format data frame with columns \code{date} and \code{code}.
#' @param indicator_col Character. Indicator column to standardise.
#' @param threshold Numeric. Z-score threshold. Default \code{1.0}.
#' @param compare_op Character. Comparison operator applied to the Z-score.
#'   Default \code{">"} (select assets with Z-score above threshold).
#' @param signal_name Output column name. Auto-generated if \code{NULL}.
#' @param output \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return \code{mkt_data} with one appended integer signal column (0/1).
#'
#' @examples
#' data(style, package = "eBacktestCraft")
#' df <- eClassic::add_mom(style, close_col = "adjusted", n = 20)
#' result <- add_zscore_signal(df, indicator_col = "mom_20", threshold = 0.5)
#'
#' @family signal-cross
#' @importFrom tibble as_tibble
#' @export
add_zscore_signal <- function(mkt_data,
                              indicator_col,
                              threshold   = 1.0,
                              compare_op  = ">",
                              signal_name = NULL,
                              output      = c("tibble", "data.frame")) {
  output <- match.arg(output)

  if (!all(c("date", "code") %in% colnames(mkt_data)))
    stop("'mkt_data' must contain 'date' and 'code' columns.")
  if (!indicator_col %in% colnames(mkt_data))
    stop("Column '", indicator_col, "' not found in mkt_data.")

  op_map <- c(">" = "gt", "<" = "lt", ">=" = "gte", "<=" = "lte",
              "==" = "eq", "!=" = "neq")
  col <- if (is.null(signal_name))
    paste0("signal_zscore_", indicator_col, "_", op_map[compare_op],
           "_", gsub("\\.", "", as.character(threshold)))
  else signal_name

  dates <- unique(mkt_data$date)
  sig   <- numeric(nrow(mkt_data))

  for (dt in as.character(dates)) {
    idx <- which(as.character(mkt_data$date) == dt)
    v   <- mkt_data[[indicator_col]][idx]
    mu  <- mean(v, na.rm = TRUE)
    sd_ <- sd(v,   na.rm = TRUE)
    if (is.na(sd_) || sd_ == 0) next
    z <- (v - mu) / sd_
    cond <- switch(compare_op,
      ">"  = z > threshold,
      "<"  = z < threshold,
      ">=" = z >= threshold,
      "<=" = z <= threshold,
      "==" = z == threshold,
      "!=" = z != threshold,
      stop("Unsupported operator: ", compare_op)
    )
    cond[is.na(cond)] <- FALSE
    sig[idx] <- as.integer(cond)
  }

  mkt_data[[col]] <- as.integer(sig)
  message("Generated signal column: ", col,
          ", valid signals: ", sum(mkt_data[[col]], na.rm = TRUE))

  if (output == "tibble") tibble::as_tibble(mkt_data) else mkt_data
}

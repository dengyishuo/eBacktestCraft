#' Add Macro Indicator Signal
#'
#' Generates a signal based on an external macroeconomic indicator (e.g.,
#' interest rates, PMI, CPI, credit spreads). The macro series is joined to
#' \code{mkt_data} by date and forward-filled, then compared against a
#' threshold or its own trend.
#'
#' @param mkt_data A long-format data frame with columns \code{date} and \code{code}.
#' @param macro_data A data frame with columns \code{date} and \code{value}
#'   containing the macro indicator time series. Dates need not align with
#'   trading days — values are forward-filled to all trading dates.
#' @param method Character. How to generate the signal:
#'   \describe{
#'     \item{\code{"threshold"} (default)}{Signal 1 when macro value satisfies
#'       \code{compare_op} against \code{threshold}.}
#'     \item{\code{"trend"}}{Signal 1 when macro value is above its own
#'       \code{ma_n}-day moving average (uptrend = risk-on).}
#'     \item{\code{"change"}}{Signal 1 when the N-period change in the macro
#'       value satisfies \code{compare_op} against \code{threshold}.}
#'   }
#' @param threshold Numeric. Threshold value for \code{method = "threshold"} or
#'   \code{"change"}. Default \code{0}.
#' @param compare_op Character. Comparison operator for threshold/change methods.
#'   Default \code{">"}.
#' @param ma_n Integer. MA window for \code{method = "trend"}. Default \code{12L}.
#' @param change_n Integer. Lookback for \code{method = "change"}. Default \code{1L}
#'   (period-over-period change).
#' @param macro_col Character. Column name of the macro value in \code{macro_data}.
#'   Default \code{"value"}.
#' @param signal_name Output column name. Auto-generated if \code{NULL}.
#' @param output \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return \code{mkt_data} with one appended integer signal column (0/1).
#'   The signal is identical for all assets on any given date (macro filter).
#'
#' @examples
#' \dontrun{
#' data(style, package = "eBacktestCraft")
#'
#' # Simulated 10-year yield data (monthly)
#' macro <- data.frame(
#'   date  = seq(as.Date("2020-01-01"), as.Date("2024-12-01"), by = "month"),
#'   value = c(1.8, 1.5, 0.7, 0.6, 0.6, 0.7, 0.9, 1.3, 1.8, 2.3,
#'             2.8, 3.5, 3.8, 4.0, 4.2, 4.3, 4.2, 4.0, 3.8, 3.5,
#'             3.3, 3.1, 3.0, 2.9, 2.8, 2.7, 2.6, 2.5, 2.4, 2.3,
#'             2.2, 2.1, 2.0, 1.9, 1.8, 1.7, 1.6, 1.5, 1.4, 1.3,
#'             1.2, 1.1, 1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3,
#'             0.2, 0.1, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)
#' )
#' # Signal: 10Y yield < 3% (low-rate environment, risk-on)
#' result <- add_macro_signal(style, macro_data = macro,
#'                            method = "threshold",
#'                            threshold = 3.0, compare_op = "<")
#' }
#'
#' @family signal-event
#' @importFrom tibble as_tibble
#' @export
add_macro_signal <- function(mkt_data,
                             macro_data,
                             method      = c("threshold", "trend", "change"),
                             threshold   = 0,
                             compare_op  = ">",
                             ma_n        = 12L,
                             change_n    = 1L,
                             macro_col   = "value",
                             signal_name = NULL,
                             output      = c("tibble", "data.frame")) {
  output <- match.arg(output)
  method <- match.arg(method)

  if (!all(c("date", "code") %in% colnames(mkt_data)))
    stop("'mkt_data' must contain 'date' and 'code' columns.")
  if (!all(c("date", macro_col) %in% colnames(macro_data)))
    stop("'macro_data' must contain 'date' and '", macro_col, "' columns.")

  col <- if (is.null(signal_name))
    paste0("signal_macro_", method) else signal_name

  macro_data$date <- as.Date(macro_data$date)
  mkt_data$date   <- as.Date(mkt_data$date)

  # Forward-fill macro onto all trading dates
  all_dates  <- sort(unique(mkt_data$date))
  macro_vals <- rep(NA_real_, length(all_dates))

  for (i in seq_along(all_dates)) {
    past <- macro_data[[macro_col]][macro_data$date <= all_dates[i]]
    if (length(past) > 0) macro_vals[i] <- tail(past, 1)
  }

  # Compute signal on the macro series
  n_dates <- length(all_dates)
  date_sig <- integer(n_dates)

  for (i in seq_len(n_dates)) {
    v <- macro_vals[i]
    if (is.na(v)) next

    active <- switch(method,
      threshold = {
        switch(compare_op,
          ">"  = v > threshold, "<"  = v < threshold,
          ">=" = v >= threshold, "<=" = v <= threshold,
          "==" = v == threshold, "!=" = v != threshold,
          stop("Unsupported operator: ", compare_op)
        )
      },
      trend = {
        if (i <= ma_n) FALSE else {
          ma <- mean(macro_vals[max(1, i - ma_n + 1):i], na.rm = TRUE)
          !is.na(ma) && v > ma
        }
      },
      change = {
        if (i <= change_n) FALSE else {
          prev <- macro_vals[i - change_n]
          if (is.na(prev)) FALSE else {
            delta <- v - prev
            switch(compare_op,
              ">"  = delta > threshold, "<"  = delta < threshold,
              ">=" = delta >= threshold, "<=" = delta <= threshold,
              "==" = delta == threshold, "!=" = delta != threshold,
              stop("Unsupported operator: ", compare_op)
            )
          }
        }
      }
    )
    date_sig[i] <- as.integer(isTRUE(active))
  }

  # Map date-level signal back to all rows
  date_map <- setNames(date_sig, as.character(all_dates))
  mkt_data[[col]] <- as.integer(date_map[as.character(mkt_data$date)])
  mkt_data[[col]][is.na(mkt_data[[col]])] <- 0L

  message("Generated signal column: ", col,
          ", valid signals: ", sum(mkt_data[[col]], na.rm = TRUE))

  if (output == "tibble") tibble::as_tibble(mkt_data) else mkt_data
}

#' Add Between-Range Signal
#'
#' Generates a 0/1 signal where the indicator value falls within a closed interval
#' \code{[lower, upper]}. Useful for mean-reversion zone detection or neutral-band
#' filtering.
#'
#' @param mkt_data A long-format data frame with columns \code{date} and \code{code}.
#' @param indicator_col Character. The indicator column to test.
#' @param lower Numeric. Lower bound (inclusive).
#' @param upper Numeric. Upper bound (inclusive). Must be >= \code{lower}.
#' @param signal_name Output column name. Auto-generated if \code{NULL}.
#' @param output \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return \code{mkt_data} with one appended integer signal column (0/1).
#'
#' @examples
#' data(style, package = "eBacktestCraft")
#' df <- eClassic::add_mom(style, close_col = "adjusted", n = 20)
#' result <- add_between_signal(df, indicator_col = "mom_20",
#'                              lower = -0.05, upper = 0.05)
#'
#' @family signal-timeseries
#' @importFrom tibble as_tibble
#' @export
add_between_signal <- function(mkt_data,
                               indicator_col,
                               lower,
                               upper,
                               signal_name = NULL,
                               output      = c("tibble", "data.frame")) {
  output <- match.arg(output)

  if (!all(c("date", "code") %in% colnames(mkt_data)))
    stop("'mkt_data' must contain 'date' and 'code' columns.")
  if (!indicator_col %in% colnames(mkt_data))
    stop("Column '", indicator_col, "' not found in mkt_data.")
  if (!is.numeric(lower) || !is.numeric(upper))
    stop("'lower' and 'upper' must be numeric.")
  if (lower > upper)
    stop("'lower' must be <= 'upper'.")

  if (is.null(signal_name))
    signal_name <- paste0("signal_", indicator_col,
                          "_between_", gsub("\\.", "", lower),
                          "_", gsub("\\.", "", upper))

  v    <- mkt_data[[indicator_col]]
  v[is.na(v) | is.infinite(v)] <- NA_real_
  cond <- (v >= lower) & (v <= upper)
  cond[is.na(cond)] <- FALSE

  mkt_data[[signal_name]] <- as.integer(cond)
  message("Generated signal column: ", signal_name,
          ", valid signals: ", sum(mkt_data[[signal_name]], na.rm = TRUE))

  if (output == "tibble") tibble::as_tibble(mkt_data) else mkt_data
}

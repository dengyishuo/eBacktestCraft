#' Add Crossover Signal
#'
#' Detects when an indicator crosses above (golden cross) or below (death cross)
#' a band boundary. The band can be a fixed numeric value or another column.
#'
#' @param mkt_data A long-format data frame with columns \code{date} and \code{code}.
#' @param indicator_col Character. The column to watch for a crossover.
#' @param cross_upper Numeric or column name. Upper band — triggers a golden-cross
#'   signal (1) when \code{indicator_col} crosses above it. Set \code{NULL} to
#'   skip golden-cross detection.
#' @param cross_lower Numeric or column name. Lower band — triggers a death-cross
#'   signal (1) when \code{indicator_col} crosses below it. Set \code{NULL} to
#'   skip death-cross detection.
#' @param signal_name Output column name. Auto-generated if \code{NULL}.
#' @param output \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return \code{mkt_data} with one appended integer signal column (0/1).
#'
#' @examples
#' data(style, package = "eBacktestCraft")
#' df <- eClassic::add_ma(style, close_col = "adjusted", n = c(5, 20))
#' result <- add_crossover_signal(df, indicator_col = "ma_5", cross_upper = "ma_20")
#'
#' @family signal-timeseries
#' @importFrom dplyr lag
#' @importFrom tibble as_tibble
#' @export
add_crossover_signal <- function(mkt_data,
                                 indicator_col,
                                 cross_upper = NULL,
                                 cross_lower = NULL,
                                 signal_name = NULL,
                                 output = c("tibble", "data.frame")) {
  output <- match.arg(output)

  if (!all(c("date", "code") %in% colnames(mkt_data)))
    stop("'mkt_data' must contain 'date' and 'code' columns.")
  if (!indicator_col %in% colnames(mkt_data))
    stop("Column '", indicator_col, "' not found in mkt_data.")
  if (is.null(cross_upper) && is.null(cross_lower))
    stop("At least one of 'cross_upper' or 'cross_lower' must be specified.")

  if (is.null(signal_name)) {
    suffix <- if (!is.null(cross_upper)) "cross_up" else "cross_down"
    signal_name <- paste0("signal_", indicator_col, "_", suffix)
  }

  v <- mkt_data[[indicator_col]]
  v[is.na(v) | is.infinite(v)] <- 0

  resolve_band <- function(band) {
    if (is.character(band) && band %in% colnames(mkt_data)) {
      b <- mkt_data[[band]]
    } else {
      b <- rep(as.numeric(band), nrow(mkt_data))
    }
    b[is.na(b)] <- 0
    b
  }

  if (!is.null(cross_upper)) {
    upper <- resolve_band(cross_upper)
    cross <- (v > upper) & (dplyr::lag(v, 1) <= dplyr::lag(upper, 1))
  } else {
    lower <- resolve_band(cross_lower)
    cross <- (v < lower) & (dplyr::lag(v, 1) >= dplyr::lag(lower, 1))
  }
  cross[is.na(cross)] <- FALSE

  mkt_data[[signal_name]] <- as.integer(cross)
  message("Generated signal column: ", signal_name,
          ", valid signals: ", sum(mkt_data[[signal_name]], na.rm = TRUE))

  if (output == "tibble") tibble::as_tibble(mkt_data) else mkt_data
}

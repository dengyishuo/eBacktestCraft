#' Add Constant Signal
#'
#' Assigns a fixed integer value to every row. Useful as a baseline "always-on"
#' signal for equal-weight buy-and-hold benchmarks.
#'
#' @param mkt_data A long-format data frame with columns \code{date} and \code{code}.
#' @param value Integer. The constant value to assign. Default \code{1L}.
#' @param signal_name Output column name. Auto-generated if \code{NULL}.
#' @param output \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return \code{mkt_data} with one appended integer signal column.
#'
#' @examples
#' data(style, package = "eBacktestCraft")
#' result <- add_constant_signal(style)
#'
#' @family signal-composite
#' @importFrom tibble as_tibble
#' @export
add_constant_signal <- function(mkt_data,
                                value       = 1L,
                                signal_name = NULL,
                                output      = c("tibble", "data.frame")) {
  output <- match.arg(output)

  if (!all(c("date", "code") %in% colnames(mkt_data)))
    stop("'mkt_data' must contain 'date' and 'code' columns.")

  if (is.null(signal_name))
    signal_name <- paste0("signal_constant_", value)

  mkt_data[[signal_name]] <- as.integer(value)
  message("Generated constant signal column: ", signal_name,
          " (all rows = ", value, ")")

  if (output == "tibble") tibble::as_tibble(mkt_data) else mkt_data
}

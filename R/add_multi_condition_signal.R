#' Add Multi-Condition Signal
#'
#' Combines multiple indicator columns into a single signal using AND (\code{"&"})
#' or OR (\code{"|"}) logic. Each column is treated as truthy when its value is
#' greater than zero; \code{NA}/\code{Inf} values are treated as falsy.
#'
#' @param mkt_data A long-format data frame with columns \code{date} and \code{code}.
#' @param indicator_cols Character vector of at least two indicator column names.
#' @param logic_op \code{"&"} (AND, default) or \code{"|"} (OR).
#' @param signal_name Output column name. Auto-generated if \code{NULL}.
#' @param output \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return \code{mkt_data} with one appended integer signal column (0/1).
#'
#' @examples
#' data(style, package = "eBacktestCraft")
#' df <- eClassic::add_mom(style, close_col = "adjusted", n = c(10, 20))
#' result <- add_multi_condition_signal(df,
#'   indicator_cols = c("mom_10", "mom_20"), logic_op = "&")
#'
#' @family signal-composite
#' @importFrom tibble as_tibble
#' @export
add_multi_condition_signal <- function(mkt_data,
                                       indicator_cols,
                                       logic_op    = "&",
                                       signal_name = NULL,
                                       output      = c("tibble", "data.frame")) {
  output <- match.arg(output)

  if (!all(c("date", "code") %in% colnames(mkt_data)))
    stop("'mkt_data' must contain 'date' and 'code' columns.")
  if (length(indicator_cols) < 2)
    stop("'indicator_cols' must contain at least 2 columns.")
  missing_cols <- setdiff(indicator_cols, colnames(mkt_data))
  if (length(missing_cols) > 0)
    stop("Columns not found in mkt_data: ", paste(missing_cols, collapse = ", "))
  if (!logic_op %in% c("&", "|"))
    stop("'logic_op' must be '&' or '|'.")

  if (is.null(signal_name)) {
    label <- if (logic_op == "&") "and" else "or"
    signal_name <- paste0("signal_", paste(indicator_cols, collapse = "_"), "_", label)
  }

  result <- NULL
  for (col in indicator_cols) {
    v    <- mkt_data[[col]]
    v[is.na(v) | is.infinite(v)] <- 0
    cond <- v > 0
    cond[is.na(cond)] <- FALSE
    result <- if (is.null(result)) cond else
              if (logic_op == "&") result & cond else result | cond
  }

  mkt_data[[signal_name]] <- as.integer(result)
  message("Generated signal column: ", signal_name,
          ", valid signals: ", sum(mkt_data[[signal_name]], na.rm = TRUE))

  if (output == "tibble") tibble::as_tibble(mkt_data) else mkt_data
}

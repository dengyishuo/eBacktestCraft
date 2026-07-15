#' Add Threshold Signal
#'
#' Generates a 0/1 signal for each row where the indicator value satisfies a
#' comparison against a fixed threshold. Multiple indicator columns are combined
#' with AND logic (all conditions must hold).
#'
#' @param mkt_data A long-format data frame with columns \code{date} and \code{code}.
#' @param indicator_cols Character vector of indicator column names.
#' @param threshold Numeric threshold(s). Length 1 (recycled) or same length as
#'   \code{indicator_cols}.
#' @param compare_op Comparison operator(s): \code{">"}, \code{"<"}, \code{">="},
#'   \code{"<="}, \code{"=="}, \code{"!="}. Length 1 (recycled) or same length as
#'   \code{indicator_cols}.
#' @param signal_name Output column name. Auto-generated from first indicator and
#'   operator if \code{NULL}.
#' @param output \code{"tibble"} (default) or \code{"data.frame"}.
#'
#' @return \code{mkt_data} with one appended integer signal column (0/1).
#'
#' @examples
#' data(style, package = "eBacktestCraft")
#' df <- eClassic::add_mom(style, close_col = "adjusted", n = 20)
#' result <- add_threshold_signal(df, indicator_cols = "mom_20",
#'                                threshold = 0, compare_op = ">")
#' @family signal-timeseries
#' @importFrom tibble as_tibble
#' @export
add_threshold_signal <- function(mkt_data,
                                 indicator_cols,
                                 threshold  = 0,
                                 compare_op = ">",
                                 signal_name = NULL,
                                 output = c("tibble", "data.frame")) {
  output <- match.arg(output)

  if (!all(c("date", "code") %in% colnames(mkt_data)))
    stop("'mkt_data' must contain 'date' and 'code' columns.")
  if (is.null(indicator_cols) || length(indicator_cols) == 0)
    stop("'indicator_cols' must be specified.")
  missing_cols <- setdiff(indicator_cols, colnames(mkt_data))
  if (length(missing_cols) > 0)
    stop("Columns not found in mkt_data: ", paste(missing_cols, collapse = ", "))
  if (length(threshold)  != 1 && length(threshold)  != length(indicator_cols))
    stop("'threshold' must be length 1 or match length of 'indicator_cols'.")
  if (length(compare_op) != 1 && length(compare_op) != length(indicator_cols))
    stop("'compare_op' must be length 1 or match length of 'indicator_cols'.")

  if (is.null(signal_name)) {
    op_map <- c(">" = "gt", "<" = "lt", ">=" = "gte", "<=" = "lte",
                "==" = "eq", "!=" = "neq")
    signal_name <- paste0("signal_", indicator_cols[1], "_",
                          op_map[compare_op[1]], "_",
                          gsub("\\.", "", as.character(threshold[1])))
  }

  result <- TRUE
  for (i in seq_along(indicator_cols)) {
    col    <- indicator_cols[i]
    thresh <- if (length(threshold)  == 1) threshold  else threshold[i]
    op     <- if (length(compare_op) == 1) compare_op else compare_op[i]
    v      <- mkt_data[[col]]
    v[is.na(v) | is.infinite(v)] <- NA
    cond <- switch(op,
      ">"  = v > thresh,
      "<"  = v < thresh,
      ">=" = v >= thresh,
      "<=" = v <= thresh,
      "==" = v == thresh,
      "!=" = v != thresh,
      stop("Unsupported operator: ", op)
    )
    cond[is.na(cond)] <- FALSE
    result <- if (i == 1) cond else result & cond
  }

  mkt_data[[signal_name]] <- as.integer(result)
  message("Generated signal column: ", signal_name,
          ", valid signals: ", sum(mkt_data[[signal_name]], na.rm = TRUE))

  if (output == "tibble") tibble::as_tibble(mkt_data) else mkt_data
}

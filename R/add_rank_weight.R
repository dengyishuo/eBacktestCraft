#' Rank-proportional weights
#'
#' For each trading day, ranks selected stocks by \code{factor_col} and
#' assigns weights \eqn{w_i = r_i / \sum r_j} where \eqn{r_i} is the rank.
#' Higher factor → higher rank → higher weight when \code{ascending = FALSE}.
#'
#' @param mkt_data Data frame with \code{date} and \code{code} columns.
#' @param signal_col Column where value = 1 marks selected stocks.
#' @param factor_col Column used to rank stocks.
#' @param ascending If TRUE, lower factor → higher weight. Default FALSE.
#' @param weight_name Output column name. Auto-generated if NULL.
#' @param output \code{"tibble"} (default) or \code{"data.frame"}.
#' @return \code{mkt_data} with one appended weight column.
#' @family weight
#' @importFrom dplyr group_by mutate ungroup select if_else
#' @importFrom rlang .data !! sym :=
#' @importFrom tibble as_tibble
#' @export
add_rank_weight <- function(
  mkt_data,
  signal_col,
  factor_col,
  ascending   = FALSE,
  weight_name = NULL,
  output      = c("tibble", "data.frame")
) {
  if (!all(c("date", "code") %in% colnames(mkt_data)))
    stop("mkt_data must contain 'date' and 'code' columns")
  for (col in c(signal_col, factor_col))
    if (!col %in% colnames(mkt_data)) stop("Column not found: ", col)
  output <- match.arg(output)
  wname  <- if (is.null(weight_name))
    paste0("weight_rank_", factor_col, "_", signal_col) else weight_name

  result <- mkt_data %>%
    dplyr::group_by(.data$date) %>%
    dplyr::mutate(
      .sel   = (!!sym(signal_col) == 1) & !is.na(!!sym(signal_col)),
      .v     = ifelse(.data$.sel, !!sym(factor_col), NA_real_),
      .r     = rank(.data$.v, ties.method = "average", na.last = "keep"),
      .r     = if (!ascending) .data$.r else {
        n_sel <- sum(.data$.sel, na.rm = TRUE)
        n_sel + 1 - .data$.r
      },
      .r     = ifelse(.data$.sel, .data$.r, 0),
      .rsum  = sum(.data$.r, na.rm = TRUE),
      !!wname := ifelse(.data$.rsum > 0, .data$.r / .data$.rsum, 0)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$.sel, -.data$.v, -.data$.r, -.data$.rsum)

  result[[wname]][is.na(result[[wname]])] <- 0
  .diag_weight(result, wname, "rank")
  if (output == "tibble") tibble::as_tibble(result) else result
}

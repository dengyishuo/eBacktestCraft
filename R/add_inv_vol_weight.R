#' Inverse-volatility weights
#'
#' Weights inversely proportional to each stock's rolling realised volatility:
#' \eqn{w_i = (1/\sigma_i) / \sum_j (1/\sigma_j)}.
#' Lower volatility → higher weight.
#'
#' @param mkt_data Data frame with \code{date} and \code{code} columns.
#' @param signal_col Column where value = 1 marks selected stocks.
#' @param return_col Per-asset return column for rolling vol estimation.
#' @param window Rolling lookback periods. Default 60.
#' @param annual_factor Annualisation factor (252 / 52 / 12). Default 252.
#' @param weight_name Output column name. Auto-generated if NULL.
#' @param output \code{"tibble"} (default) or \code{"data.frame"}.
#' @return \code{mkt_data} with one appended weight column.
#' @family weight
#' @importFrom dplyr group_by mutate ungroup arrange select
#' @importFrom rlang .data !! sym :=
#' @importFrom zoo rollapply
#' @importFrom tibble as_tibble
#' @export
add_inv_vol_weight <- function(
  mkt_data,
  signal_col,
  return_col,
  window       = 60L,
  annual_factor = 252L,
  weight_name  = NULL,
  output       = c("tibble", "data.frame")
) {
  if (!all(c("date", "code") %in% colnames(mkt_data)))
    stop("mkt_data must contain 'date' and 'code' columns")
  for (col in c(signal_col, return_col))
    if (!col %in% colnames(mkt_data)) stop("Column not found: ", col)
  output <- match.arg(output)
  wname  <- if (is.null(weight_name))
    paste0("weight_inv_vol_", signal_col) else weight_name

  # rolling vol per stock (time-series grouping)
  result <- mkt_data %>%
    dplyr::group_by(.data$code) %>%
    dplyr::arrange(.data$date, .by_group = TRUE) %>%
    dplyr::mutate(
      .vol = zoo::rollapply(!!sym(return_col), window,
                            FUN = sd, fill = NA, align = "right",
                            na.rm = TRUE) * sqrt(annual_factor)
    ) %>%
    dplyr::ungroup()

  # cross-sectional inv-vol normalisation
  result <- result %>%
    dplyr::group_by(.data$date) %>%
    dplyr::mutate(
      .sel      = (!!sym(signal_col) == 1) & !is.na(.data$.vol) & (.data$.vol > 0),
      .inv      = ifelse(.data$.sel, 1 / .data$.vol, 0),
      .inv_sum  = sum(.data$.inv, na.rm = TRUE),
      !!wname  := ifelse(.data$.inv_sum > 0, .data$.inv / .data$.inv_sum, 0)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$.vol, -.data$.sel, -.data$.inv, -.data$.inv_sum) %>%
    dplyr::arrange(.data$date, .data$code)

  result[[wname]][is.na(result[[wname]])] <- 0
  .diag_weight(result, wname, "inv_vol")
  if (output == "tibble") tibble::as_tibble(result) else result
}

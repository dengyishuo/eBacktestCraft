#' Target-volatility weight scaling
#'
#' Rescales an existing weight column each day so the implied portfolio
#' volatility matches \code{target_vol}.
#' leverage = target_vol / realized_vol, capped at \code{max_leverage}.
#'
#' @param mkt_data Data frame with \code{date} and \code{code} columns.
#' @param weight_col Existing weight column to rescale.
#' @param return_col Per-asset return column for rolling vol estimation.
#' @param target_vol Annualised target volatility (e.g. 0.10 = 10\%). Default 0.10.
#' @param window Rolling lookback periods. Default 60.
#' @param annual_factor Annualisation factor. Default 252.
#' @param max_leverage Maximum leverage multiplier. Default 2.
#' @param weight_name Output column name. Auto-generated if NULL.
#' @param output \code{"tibble"} (default) or \code{"data.frame"}.
#' @return \code{mkt_data} with one appended weight column.
#' @family weight
#' @importFrom dplyr group_by mutate ungroup arrange select
#' @importFrom rlang .data !! sym :=
#' @importFrom tibble as_tibble
#' @export
add_target_vol_weight <- function(
  mkt_data,
  weight_col,
  return_col,
  target_vol    = 0.10,
  window        = 60L,
  annual_factor = 252L,
  max_leverage  = 2.0,
  weight_name   = NULL,
  output        = c("tibble", "data.frame")
) {
  if (!all(c("date", "code") %in% colnames(mkt_data)))
    stop("mkt_data must contain 'date' and 'code' columns")
  for (col in c(weight_col, return_col))
    if (!col %in% colnames(mkt_data)) stop("Column not found: ", col)
  output <- match.arg(output)
  wname  <- if (is.null(weight_name))
    paste0("weight_tvol_", weight_col) else weight_name

  dates <- sort(unique(mkt_data$date))
  result <- mkt_data %>%
    dplyr::arrange(.data$date, .data$code)
  result[[wname]] <- 0

  for (i in seq_along(dates)) {
    dt       <- dates[i]
    day_rows <- which(result$date == dt)
    w_today  <- result[[weight_col]][day_rows]

    if (i <= window || sum(w_today, na.rm = TRUE) < 1e-10) {
      result[[wname]][day_rows] <- w_today
      next
    }

    past_dates <- dates[max(1L, i - window):(i - 1L)]
    past_rows  <- which(result$date %in% past_dates)
    past_sub   <- result[past_rows, c("date", "code", return_col)]
    ret_wide   <- tryCatch(
      stats::reshape(past_sub, idvar = "date", timevar = "code",
                     direction = "wide"),
      error = function(e) NULL
    )
    if (is.null(ret_wide) || nrow(ret_wide) < 5) {
      result[[wname]][day_rows] <- w_today
      next
    }
    ret_mat    <- as.matrix(ret_wide[, -1, drop = FALSE])
    ret_mat[is.na(ret_mat)] <- 0

    today_codes <- result$code[day_rows]
    col_names   <- sub(paste0(return_col, "."), "", colnames(ret_mat), fixed = TRUE)
    w_aligned   <- sapply(col_names, function(cd) {
      idx <- which(today_codes == cd)
      if (length(idx)) w_today[idx[1]] else 0
    })

    port_ret    <- as.numeric(ret_mat %*% w_aligned)
    realized_vol <- sd(port_ret, na.rm = TRUE) * sqrt(annual_factor)
    leverage     <- if (realized_vol > 1e-10) target_vol / realized_vol else 1
    leverage     <- min(leverage, max_leverage)
    result[[wname]][day_rows] <- w_today * leverage
  }

  result[[wname]][is.na(result[[wname]])] <- 0
  .diag_weight(result, wname, paste0("target_vol(", round(target_vol * 100, 1), "%)"))
  if (output == "tibble") tibble::as_tibble(result) else result
}

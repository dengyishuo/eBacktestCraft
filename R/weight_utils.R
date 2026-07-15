# Internal helpers shared across all add_*_weight functions.
# Not exported.

.diag_weight <- function(df, col, label) {
  daily   <- tapply(df[[col]], df$date,
                    function(x) c(sum(x, na.rm = TRUE), sum(x > 0, na.rm = TRUE)))
  totals  <- sapply(daily, `[[`, 1)
  n_sels  <- sapply(daily, `[[`, 2)
  total_d <- length(daily)
  days_sel <- sum(n_sels > 0)
  avg_sel  <- mean(n_sels)
  valid    <- sum(abs(totals - 1) < 1e-6)
  message(" Generated ", label, " weight column: ", col)
  message(" Total days: ", total_d, ", days with selection: ", days_sel)
  message(" Average daily selected stocks: ", round(avg_sel, 2))
  message(" Days with weight sum = 1: ", valid, "/", total_d,
          " (", round(100 * valid / total_d, 1), "%)")
}

.fill_fallback <- function(result, wname, sel_rows, fallback) {
  n <- length(sel_rows)
  result[[wname]][sel_rows] <<- if (fallback == "equal" && n > 0) 1 / n else 0
}

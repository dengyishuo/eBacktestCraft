#' Parameter Grid Scanning
#'
#' Build a parameter grid from candidate value lists, then run one backtest per
#' row and aggregate performance metrics into a single ranked table.
#'
#' Workflow (mirrors quantstrat's \code{add.distribution} / \code{apply.paramset}
#' pattern, but simplified for weight-based strategies):
#'
#' \enumerate{
#'   \item \code{param_grid()} — create the full Cartesian product of candidate
#'         values.
#'   \item \code{run_param_scan()} — iterate over every row of the grid, merge
#'         each row into the base config, run the backtest, extract metrics.
#'   \item \code{rank_param_scan()} — rank / filter / sort the results table.
#' }
#'
#' @name param_scan
NULL


# ─────────────────────────────────────────────────────────────────────────────
# 1.  param_grid()
# ─────────────────────────────────────────────────────────────────────────────

#' Build a Cartesian Parameter Grid
#'
#' Takes a named list of candidate vectors and expands them into a data frame
#' where every row is one unique parameter combination (full Cartesian product).
#'
#' @param ... Named arguments, each a vector of candidate values for that
#'   parameter.  Parameter names must match fields in the backtest config
#'   list (e.g. \code{rebalance_cycle}, \code{fixed_component_sl_ratio}).
#'
#' @return A \code{data.frame} with one column per parameter and one row per
#'   combination.  Use as the \code{grid} argument to \code{run_param_scan()}.
#'
#' @examples
#' grid <- param_grid(
#'   rebalance_cycle          = c("monthly", "quarterly"),
#'   fixed_component_sl_ratio = c(0.05, 0.10, 0.15),
#'   fee_rate                 = c(0.0001, 0.0003)
#' )
#' nrow(grid)  # 2 * 3 * 2 = 12
#'
#' @export
param_grid <- function(...) {
  args <- list(...)
  if (length(args) == 0L)
    stop("Provide at least one named parameter vector.")
  if (is.null(names(args)) || any(names(args) == ""))
    stop("All arguments must be named (parameter name = candidate values).")
  expand.grid(args, stringsAsFactors = FALSE)
}


# ─────────────────────────────────────────────────────────────────────────────
# 2.  run_param_scan()
# ─────────────────────────────────────────────────────────────────────────────

#' Run a Parameter Scan
#'
#' Iterates over every row of \code{grid}, merges the row into
#' \code{base_config}, runs the backtest, and computes performance metrics.
#' Returns a tidy data frame with one row per parameter combination.
#'
#' @param df Long-format market data panel (same format as \code{run_backtest()}).
#' @param grid Parameter grid produced by \code{param_grid()}, or any
#'   \code{data.frame} with named columns matching config fields.
#' @param base_config Base backtest configuration list from
#'   \code{default_backtest_config()}.  Each grid row's values are overlaid
#'   on top of this base.  Defaults to \code{default_backtest_config()}.
#' @param metrics Character vector of metrics to extract from
#'   \code{performance_analysis()}.  Must be column names in
#'   \code{performance_analysis(result)$metrics}.
#'   Default: \code{c("total_return_pct", "annual_return_pct",
#'   "annual_volatility_pct", "sharpe_ratio", "max_drawdown_pct",
#'   "calmar_ratio", "win_rate_pct")}.
#' @param risk_free_rate Annual risk-free rate forwarded to
#'   \code{performance_analysis()}.  Default \code{0.02}.
#' @param parallel Logical.  Use \code{parallel::mclapply()} for multi-core
#'   execution (Unix/macOS only).  Ignored on Windows.  Default \code{FALSE}.
#' @param n_cores Number of cores when \code{parallel = TRUE}.
#'   Default \code{parallel::detectCores() - 1}.
#' @param verbose Logical.  Print progress.  Default \code{TRUE}.
#'
#' @return A \code{tibble} with all grid columns plus a \code{.scan_id} column
#'   and one column per requested metric.  Rows with errors have \code{NA}
#'   metrics and a non-empty \code{.error} column.
#'
#' @examples
#' \dontrun{
#' grid <- param_grid(
#'   rebalance_cycle          = c("monthly", "quarterly"),
#'   fixed_component_sl_ratio = c(0.05, 0.10)
#' )
#' base <- default_backtest_config() |>
#'   set_component_stop_loss(enable = TRUE, type = "fixed")
#'
#' scan_result <- run_param_scan(df, grid, base_config = base)
#' head(scan_result)
#' }
#'
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble tibble
#' @export
run_param_scan <- function(
  df,
  grid,
  base_config    = default_backtest_config(),
  metrics        = c("total_return_pct", "annual_return_pct",
                     "annual_volatility_pct", "sharpe_ratio",
                     "max_drawdown_pct", "calmar_ratio", "win_rate_pct"),
  risk_free_rate = 0.02,
  parallel       = FALSE,
  n_cores        = max(1L, parallel::detectCores() - 1L),
  verbose        = TRUE
) {
  if (!is.data.frame(grid) || nrow(grid) == 0L)
    stop("`grid` must be a non-empty data.frame. Use param_grid() to build one.")

  n_combos <- nrow(grid)
  if (verbose)
    message("Starting parameter scan: ", n_combos, " combination(s)...")

  # Worker function: run one combination, return a one-row data.frame
  .run_one <- function(i) {
    row     <- grid[i, , drop = FALSE]
    cfg     <- base_config
    for (nm in names(row))
      cfg[[nm]] <- row[[nm]]

    result  <- tryCatch(
      run_backtest(cfg, df),
      error = function(e) e
    )

    if (inherits(result, "error")) {
      row$.scan_id <- i
      row$.error   <- conditionMessage(result)
      for (m in metrics) row[[m]] <- NA_real_
      return(row)
    }

    perf <- tryCatch(
      performance_analysis(result, risk_free_rate = risk_free_rate,
                           what = "metrics"),
      error = function(e) NULL
    )

    row$.scan_id <- i
    row$.error   <- ""
    for (m in metrics) {
      row[[m]] <- if (!is.null(perf) && m %in% colnames(perf))
        perf[[m]][1L]
      else
        NA_real_
    }

    if (verbose)
      message(sprintf("  [%d/%d] done — sharpe=%.3f  mdd=%.1f%%",
                      i, n_combos,
                      ifelse(is.na(row$sharpe_ratio), NA, row$sharpe_ratio),
                      ifelse(is.na(row$max_drawdown_pct), NA, row$max_drawdown_pct)))
    row
  }

  if (parallel && .Platform$OS.type != "windows") {
    rows <- parallel::mclapply(seq_len(n_combos), .run_one, mc.cores = n_cores)
  } else {
    rows <- lapply(seq_len(n_combos), .run_one)
  }

  out <- dplyr::bind_rows(rows)
  # Reorder: scan_id first, then grid columns, then metrics, then .error
  grid_cols   <- names(grid)
  metric_cols <- metrics[metrics %in% names(out)]
  col_order   <- c(".scan_id", grid_cols, metric_cols, ".error")
  col_order   <- col_order[col_order %in% names(out)]
  out         <- out[, col_order, drop = FALSE]

  if (verbose)
    message("Scan complete: ", sum(out$.error == ""), "/", n_combos,
            " succeeded.")
  tibble::as_tibble(out)
}


# ─────────────────────────────────────────────────────────────────────────────
# 3.  rank_param_scan()
# ─────────────────────────────────────────────────────────────────────────────

#' Rank and Filter Parameter Scan Results
#'
#' Sorts the scan result table by one or more metrics, optionally filters by
#' minimum thresholds, and adds a \code{.rank} column.
#'
#' @param scan_result Output from \code{run_param_scan()}.
#' @param by Primary metric to rank by.  Default \code{"sharpe_ratio"}.
#' @param descending Sort direction for \code{by}: \code{TRUE} = higher is
#'   better (Sharpe, return); \code{FALSE} = lower is better (drawdown).
#'   Default \code{TRUE}.
#' @param min_sharpe Minimum acceptable Sharpe ratio.  Rows below this are
#'   dropped.  Default \code{NULL} (no filter).
#' @param max_drawdown Maximum acceptable drawdown (\%, negative value).
#'   E.g. \code{-20} drops rows where \code{max_drawdown_pct < -20}.
#'   Default \code{NULL}.
#' @param top_n Keep only the top \code{n} rows after filtering.
#'   Default \code{NULL} (keep all).
#'
#' @return A \code{tibble} with a \code{.rank} column prepended, sorted and
#'   filtered as requested.
#'
#' @examples
#' \dontrun{
#' ranked <- rank_param_scan(scan_result,
#'                           by           = "sharpe_ratio",
#'                           min_sharpe   = 0.5,
#'                           max_drawdown = -20,
#'                           top_n        = 10)
#' }
#'
#' @importFrom dplyr filter arrange desc mutate select
#' @importFrom tibble as_tibble
#' @export
rank_param_scan <- function(
  scan_result,
  by           = "sharpe_ratio",
  descending   = TRUE,
  min_sharpe   = NULL,
  max_drawdown = NULL,
  top_n        = NULL
) {
  if (!inherits(scan_result, "data.frame"))
    stop("`scan_result` must be the output of run_param_scan().")
  if (!by %in% names(scan_result))
    stop("Column '", by, "' not found in scan_result.")

  out <- scan_result[!is.na(scan_result[[by]]) & scan_result$.error == "", ]

  if (!is.null(min_sharpe) && "sharpe_ratio" %in% names(out))
    out <- out[!is.na(out$sharpe_ratio) & out$sharpe_ratio >= min_sharpe, ]

  if (!is.null(max_drawdown) && "max_drawdown_pct" %in% names(out))
    out <- out[!is.na(out$max_drawdown_pct) & out$max_drawdown_pct >= max_drawdown, ]

  if (descending) {
    out <- out[order(-out[[by]]), ]
  } else {
    out <- out[order(out[[by]]), ]
  }

  if (!is.null(top_n))
    out <- utils::head(out, top_n)

  out$.rank <- seq_len(nrow(out))
  col_order <- c(".rank", setdiff(names(out), ".rank"))
  tibble::as_tibble(out[, col_order, drop = FALSE])
}


# ─────────────────────────────────────────────────────────────────────────────
# 4.  sensitivity_table() — one-way sensitivity around a best point
# ─────────────────────────────────────────────────────────────────────────────

#' One-Way Sensitivity Analysis
#'
#' Holds all parameters fixed at their \code{base_config} values and varies
#' one parameter at a time.  Returns a tidy long-format table showing how each
#' metric responds to each parameter sweep.  Useful for understanding which
#' parameters matter most.
#'
#' @param df Market data panel.
#' @param param_ranges Named list, where each element is a numeric vector of
#'   candidate values for that parameter. E.g.:
#'   \code{list(fee_rate = c(0.0001, 0.0003, 0.001), lot_size = c(100, 200))}.
#' @param base_config Base configuration.  All non-swept parameters take their
#'   value from here.
#' @param metrics Metrics to extract.  Same default as \code{run_param_scan()}.
#' @param risk_free_rate Forwarded to \code{performance_analysis()}.
#' @param verbose Logical.  Default \code{TRUE}.
#'
#' @return A \code{tibble} with columns \code{param_name}, \code{param_value},
#'   and one column per metric.
#'
#' @examples
#' \dontrun{
#' sens <- sensitivity_table(
#'   df,
#'   param_ranges = list(
#'     fee_rate                 = c(0.0001, 0.0003, 0.0010),
#'     fixed_component_sl_ratio = c(0.05, 0.10, 0.15, 0.20)
#'   ),
#'   base_config = default_backtest_config()
#' )
#' }
#'
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble as_tibble
#' @export
sensitivity_table <- function(
  df,
  param_ranges,
  base_config    = default_backtest_config(),
  metrics        = c("total_return_pct", "annual_return_pct",
                     "annual_volatility_pct", "sharpe_ratio",
                     "max_drawdown_pct", "calmar_ratio", "win_rate_pct"),
  risk_free_rate = 0.02,
  verbose        = TRUE
) {
  if (!is.list(param_ranges) || length(param_ranges) == 0L)
    stop("`param_ranges` must be a non-empty named list.")

  rows <- list()
  for (nm in names(param_ranges)) {
    candidates <- param_ranges[[nm]]
    if (verbose) message("Sweeping: ", nm, " (", length(candidates), " values)")

    for (v in candidates) {
      cfg      <- base_config
      cfg[[nm]] <- v

      result <- tryCatch(run_backtest(cfg, df), error = function(e) e)
      row    <- tibble::tibble(param_name = nm, param_value = as.character(v))

      if (inherits(result, "error")) {
        for (m in metrics) row[[m]] <- NA_real_
        row$.error <- conditionMessage(result)
      } else {
        perf <- tryCatch(
          performance_analysis(result, risk_free_rate = risk_free_rate,
                               what = "metrics"),
          error = function(e) NULL
        )
        for (m in metrics)
          row[[m]] <- if (!is.null(perf) && m %in% colnames(perf))
            perf[[m]][1L] else NA_real_
        row$.error <- ""
      }
      rows[[length(rows) + 1L]] <- row
    }
  }

  tibble::as_tibble(dplyr::bind_rows(rows))
}

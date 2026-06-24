# =============================================================================
# Benchmark Strategies
# Three passive reference portfolios, all powered by run_backtest().
#
# How the engine reads weights:
#   - Weight column must be present on every row so the engine can read it at
#     any rebalance trigger date.
#   - On NON-rebalance days the engine ignores the weight column entirely;
#     positions drift with prices.
#   - run_benchmark_ew : weight = 1/N on every row + normal rebalance_cycle
#                        → engine reads 1/N at each trigger and resets to EW
#   - run_benchmark_bh : weight = 1/N on every row + rebalance_cycle = 99999
#                        → engine fires only once at start, then never again
#                        → portfolio drifts freely (true buy-and-hold)
# =============================================================================


# ── 1. Equal-weight rebalanced ────────────────────────────────────────────────

#' Run Equal-Weight Rebalanced Benchmark
#'
#' Constructs a periodically rebalanced equal-weight portfolio. On each
#' rebalance date the engine reads the weight column (1/N for each constituent)
#' and resets all positions back to equal weight. Between rebalance dates
#' positions drift freely.
#'
#' @param mkt_data Long-format panel data (same data used for the main
#'   strategy). Must contain \code{date}, \code{code}, and OHLC columns.
#' @param config A config list from \code{default_backtest_config()} already
#'   tuned for fees, capital, and date range. If \code{NULL}, defaults are used.
#'   The \code{weight_col} field is always overridden internally.
#' @param date_col Character. Name of the date column. Default \code{"date"}.
#' @param code_col Character. Name of the asset-code column. Default \code{"code"}.
#' @param ... Additional arguments forwarded to \code{run_backtest()}.
#'
#' @return Same list structure as \code{run_backtest()}: equity_curve,
#'   transactions, config.
#' @export
#' @importFrom dplyr group_by mutate ungroup n
#'
#' @examples
#' \dontrun{
#' library(eBacktestCraft)
#' library(eFactorCraft)
#'
#' universe <- data.frame(
#'   code = c("510300.SS", "510050.SS", "510500.SS"),
#'   name = c("CSI 300 ETF", "SSE 50 ETF", "CSI 500 ETF")
#' )
#' mkt_data <- eFactorCraft::get_data(universe, "2020-01-01", "2024-12-31")
#'
#' cfg <- default_backtest_config() |>
#'   set_init_capital(1000000) |>
#'   set_rebalance_cycle("monthly")
#'
#' # Monthly equal-weight rebalanced benchmark
#' bt_ew <- run_benchmark_ew(mkt_data, config = cfg)
#' performance_analysis(bt_ew)
#' }
run_benchmark_ew <- function(mkt_data,
                             config = NULL,
                             date_col = "date",
                             code_col = "code",
                             ...) {
  if (is.null(config)) config <- default_backtest_config()

  # Assign weight 1/N to every row to ensure valid weight available on each rebalance trigger
  # N equals the count of valid assets in each cross-section date
  mkt_data <- mkt_data %>%
    dplyr::group_by(.data[[date_col]]) %>%
    dplyr::mutate(
      .weight_ew = ifelse(!is.na(.data[[code_col]]), 1 / dplyr::n(), NA_real_)
    ) %>%
    dplyr::ungroup()

  config$weight_col <- ".weight_ew"
  run_backtest(config, mkt_data)
}


# ── 2. Constituent Buy-and-Hold ───────────────────────────────────────────────

#' Run Constituent Buy-and-Hold Benchmark
#'
#' Buys all constituents at equal weight on the first rebalance date and never
#' rebalances again. The weight column is populated with 1/N on every row so
#' the engine always has something to read, but \code{rebalance_cycle = 99999}
#' ensures the engine fires only once at the start. After that, positions drift
#' freely with market prices (true buy-and-hold).
#'
#' @inheritParams run_benchmark_ew
#'
#' @return Same list structure as \code{run_backtest()}.
#' @export
#' @importFrom dplyr group_by mutate ungroup n
#'
#' @examples
#' \dontrun{
#' library(eBacktestCraft)
#' library(eFactorCraft)
#'
#' universe <- data.frame(
#'   code = c("510300.SS", "510050.SS", "510500.SS"),
#'   name = c("CSI 300 ETF", "SSE 50 ETF", "CSI 500 ETF")
#' )
#' mkt_data <- eFactorCraft::get_data(universe, "2020-01-01", "2024-12-31")
#'
#' cfg <- default_backtest_config() |>
#'   set_init_capital(1000000)
#'
#' # Constituent buy-and-hold benchmark:
#' # equal-weight entry at first date, then price-driven drift
#' bt_bh <- run_benchmark_bh(mkt_data, config = cfg)
#' performance_analysis(bt_bh)
#' }
run_benchmark_bh <- function(mkt_data,
                             config = NULL,
                             date_col = "date",
                             code_col = "code",
                             ...) {
  if (is.null(config)) config <- default_backtest_config()

  # Same equal-weight assignment as EW benchmark across all rows
  # Core difference: extremely long rebalance cycle ensures only one initial position setup
  mkt_data <- mkt_data %>%
    dplyr::group_by(.data[[date_col]]) %>%
    dplyr::mutate(
      .weight_bh = ifelse(!is.na(.data[[code_col]]), 1 / dplyr::n(), NA_real_)
    ) %>%
    dplyr::ungroup()

  config$weight_col <- ".weight_bh"
  config$rebalance_mode <- "calendar"
  config$rebalance_cycle <- 99999L # Disable subsequent rebalancing after initial entry

  run_backtest(config, mkt_data)
}


# ── 3. Index Buy-and-Hold ─────────────────────────────────────────────────────

#' Run Index Buy-and-Hold Benchmark
#'
#' Buys a market index (or any single ETF) on the first date and holds it to
#' the end. The index data must be pre-downloaded in the same long format as
#' \code{mkt_data} — use \code{eFactorCraft::get_data()} with the index code.
#'
#' The weight column is set to 1 on every row (100\% allocation to the single
#' asset). \code{rebalance_cycle = 99999} means the engine buys once and
#' never rebalances.
#'
#' @param index_data Long-format data for a single index/ETF. Must contain
#'   \code{date}, \code{code}, \code{name}, \code{open}, \code{high},
#'   \code{low}, \code{close}, \code{adjusted}, \code{volume}. Obtain via
#'   \code{eFactorCraft::get_data(data.frame(code = "000300.SS",
#'   name = "CSI300"), start_date, end_date)}.
#' @param config A config list from \code{default_backtest_config()}. If
#'   \code{NULL}, defaults are used. \code{weight_col},
#'   \code{rebalance_mode}, \code{rebalance_cycle}, and \code{lot_size} are
#'   always overridden internally.
#' @param lot_size Numeric. Lot size for the index instrument. Use \code{1}
#'   for index futures or continuous instruments; use \code{100} for
#'   standard A-share ETFs. Default \code{1}.
#' @param ... Additional arguments forwarded to \code{run_backtest()}.
#'
#' @return Same list structure as \code{run_backtest()}.
#' @export
#'
#' @examples
#' \dontrun{
#' library(eBacktestCraft)
#' library(eFactorCraft)
#'
#' # Download CSI 300 index market data
#' idx_data <- eFactorCraft::get_data(
#'   data.frame(code = "000300.SS", name = "CSI300"),
#'   start_date = "2020-01-01",
#'   end_date = "2024-12-31"
#' )
#'
#' cfg <- default_backtest_config() |>
#'   set_init_capital(1000000)
#'
#' # CSI 300 buy-and-hold benchmark
#' bt_idx <- run_benchmark_index(idx_data, config = cfg, lot_size = 100)
#' performance_analysis(bt_idx)
#' }
run_benchmark_index <- function(index_data,
                                config = NULL,
                                lot_size = 1,
                                ...) {
  if (is.null(config)) config <- default_backtest_config()

  # Full single-asset allocation; one-time opening position only
  index_data$.weight_idx <- 1

  config$weight_col <- ".weight_idx"
  config$rebalance_mode <- "calendar"
  config$rebalance_cycle <- 99999L
  config$lot_size <- lot_size

  run_backtest(config, index_data)
}


# ── 4. Combine results ────────────────────────────────────────────────────────

#' Combine Strategy and Benchmark Equity Curves
#'
#' Extracts and aligns equity curves from one or more \code{run_backtest()}
#' results. Each curve is normalised to a base NAV of 1 at the first date,
#' then stacked into a long-format tibble ready for plotting.
#'
#' @param ... Two or more \code{run_backtest()} result lists (strategy first,
#'   then benchmarks in any order).
#' @param names Character vector of display labels, one per result. Defaults
#'   to \code{c("Strategy", "BM1", "BM2", ...)}.
#' @param base_capital Numeric. NAV base value at start. Default \code{1}.
#'
#' @return A tibble with columns \code{date}, \code{series} (label),
#'   \code{nav}.
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#'
#' @examples
#' \dontrun{
#' cmp <- compare_benchmarks(
#'   bt_strategy, bt_ew, bt_bh, bt_idx,
#'   names = c("Strategy", "Equal-Weight Rebalance", "Constituent B&H", "CSI300 B&H")
#' )
#' head(cmp)
#' }
compare_benchmarks <- function(..., names = NULL, base_capital = 1) {
  results <- list(...)
  n <- length(results)

  if (n < 2) stop("Provide at least two run_backtest() results to compare.")

  if (is.null(names)) {
    names <- c("Strategy", paste0("BM", seq_len(n - 1)))
  }
  if (length(names) != n) {
    stop("'names' must have the same length as the number of results provided.")
  }

  rows <- vector("list", n)
  for (i in seq_len(n)) {
    ec <- results[[i]]$equity_curve
    if (is.null(ec)) stop(paste0("Result ", i, " has no $equity_curve component."))

    rows[[i]] <- tibble::tibble(
      date   = ec$date,
      series = names[i],
      nav    = ec$total_asset / ec$total_asset[1] * base_capital
    )
  }

  dplyr::bind_rows(rows)
}


# ── 5. Plot comparison ────────────────────────────────────────────────────────

#' Plot Strategy vs Benchmark NAV Comparison
#'
#' Two-panel chart: upper panel shows normalised NAV curves for the strategy
#' and all benchmarks; lower panel (optional) shows the excess NAV of the
#' strategy relative to one chosen benchmark.
#'
#' Requires the \pkg{patchwork} package for the two-panel layout. If
#' \pkg{patchwork} is not installed, only the NAV panel is returned with a
#' message.
#'
#' @param compare_result Tibble returned by \code{compare_benchmarks()}.
#' @param show_excess Logical. Show excess-NAV sub-panel. Default \code{TRUE}.
#' @param base_series Character. Name of the benchmark to compute excess
#'   against (must match a value in \code{compare_result$series}). Defaults
#'   to the second unique series (first benchmark).
#' @param colors Named character vector mapping series labels to hex colors.
#'   If \code{NULL}, a built-in palette is used.
#' @param title Character. Plot title. Default \code{"Strategy vs Benchmarks"}.
#' @param height_ratio Numeric vector of length 2 controlling the relative
#'   height of the two panels. Default \code{c(3, 1)}.
#'
#' @return A ggplot object, or a patchwork composite when
#'   \code{show_excess = TRUE} and \pkg{patchwork} is available.
#' @export
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual labs theme_minimal theme element_text geom_hline
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' \dontrun{
#' cmp <- compare_benchmarks(
#'   bt_strategy, bt_ew, bt_bh, bt_idx,
#'   names = c("Strategy", "Equal-Weight Rebalance", "Constituent B&H", "CSI300 B&H")
#' )
#'
#' # Two-panel visualization (patchwork package required)
#' plot_benchmark_compare(
#'   cmp,
#'   show_excess  = TRUE,
#'   base_series  = "CSI300 B&H",
#'   title        = "Strategy vs Benchmark NAV Performance"
#' )
#' }
plot_benchmark_compare <- function(compare_result,
                                   show_excess = TRUE,
                                   base_series = NULL,
                                   colors = NULL,
                                   title = "Strategy vs Benchmarks",
                                   height_ratio = c(3, 1)) {
  series_levels <- unique(compare_result$series)

  if (is.null(base_series)) base_series <- series_levels[2]
  if (!base_series %in% series_levels) {
    stop("'base_series' not found in compare_result$series.")
  }

  if (is.null(colors)) {
    palette <- c(
      "#E63946", "#457B9D", "#2A9D8F", "#E9C46A",
      "#F4A261", "#264653", "#A8DADC"
    )
    colors <- stats::setNames(
      palette[seq_along(series_levels)],
      series_levels
    )
  }

  # Upper panel: Normalized NAV performance curves
  p_nav <- ggplot2::ggplot(
    compare_result,
    ggplot2::aes(x = date, y = nav, color = series)
  ) +
    ggplot2::geom_line(linewidth = 0.8, alpha = 0.9) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(
      title = title, x = NULL, y = "NAV (Base = 1)",
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", hjust = 0.5),
      legend.position = "top"
    )

  if (!show_excess) {
    return(p_nav)
  }

  # Lower panel: Strategy excess return against specified benchmark
  strategy_series <- series_levels[1]

  wide <- compare_result %>%
    tidyr::pivot_wider(names_from = series, values_from = nav)

  excess_df <- wide %>%
    dplyr::mutate(
      excess = .data[[strategy_series]] - .data[[base_series]]
    ) %>%
    dplyr::select(date, excess)

  p_excess <- ggplot2::ggplot(
    excess_df,
    ggplot2::aes(x = date, y = excess)
  ) +
    ggplot2::geom_line(color = colors[[strategy_series]], linewidth = 0.6) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::labs(
      x = "Date",
      y = paste0("Excess NAV vs ", base_series)
    ) +
    ggplot2::theme_minimal(base_size = 12)

  # Combine two panels if patchwork is installed; return NAV plot only otherwise
  if (requireNamespace("patchwork", quietly = TRUE)) {
    p_nav / p_excess + patchwork::plot_layout(heights = height_ratio)
  } else {
    message("Install 'patchwork' package to enable two-panel layout. Only NAV curve will be returned currently.")
    p_nav
  }
}

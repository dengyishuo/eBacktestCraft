#' Run backtest using a configuration list
#'
#' This is the main user-facing function. It accepts a configuration list
#' (typically created by \code{default_backtest_config()} and modified by
#' setter functions) and executes the backtest.
#'
#' @param config A configuration list as returned by \code{default_backtest_config()}.
#' @param df Data frame with OHLC data (long format).
#' @param ... Additional parameters to override the configuration.
#'
#' @return A list containing daily_positions, equity_curve, transactions, and config.
#' @export
#'
#' @examples
#' \dontrun{
#' cfg <- default_backtest_config() %>%
#'   set_lot_size(200) %>%
#'   set_rebalance_cycle("monthly")
#' result <- run_backtest(cfg, my_data)
#' }
run_backtest <- function(config, df, ...) {
  if (!is.list(config)) {
    stop("First argument must be a configuration list from default_backtest_config()")
  }
  args <- utils::modifyList(config, list(...))
  args$df <- df
  do.call(.run_backtest, args)
}

#' Comprehensive performance analysis for quantitative strategies
#'
#' Automatically analyze backtest results, calculate core metrics including returns,
#' risk, risk-adjusted returns, and trade win rate.
#' Outputs standardized metrics table, daily returns and drawdown details, and transaction records.
#'
#' @param result List output from the backtest() function
#' @param risk_free_rate Annual risk-free rate, default 0.02 (2\%)
#' @param initial_capital Initial capital, automatically read from backtest result if NULL
#' @param output_type Output format: "tibble" (default) or "data.frame"
#'
#' @return List containing 3 core data tables:
#'   \item{metrics}{Summary table of all performance metrics}
#'   \item{daily_details}{Daily returns, cumulative returns, and drawdowns}
#'   \item{transactions}{Complete transaction records}
#'
#' @importFrom dplyr group_by summarise mutate lag
#' @importFrom tibble as_tibble
#' @export
#'
#' @examples
#' \dontrun{
#' # Analyze backtest results
#' bt_result <- backtest(df)
#' perf <- performance_analysis(bt_result)
#'
#' # View performance metrics
#' print(perf$metrics)
#' }
performance_analysis <- function(
  result,
  risk_free_rate = 0.02,
  initial_capital = NULL,
  output_type = c("tibble", "data.frame")
) {
  # --------------------------
  # 1. Extract data from result
  # --------------------------
  # Support both new and old output formats for backward compatibility
  if ("equity_curve" %in% names(result)) {
    # New format from backtest()
    total_table <- result$equity_curve
    trades_table <- result$transactions
    config <- result$config
  } else if ("total_asset" %in% names(result)) {
    # Alternative new format
    total_table <- result
    trades_table <- NULL
    config <- NULL
  } else {
    stop("Invalid result format. Expected output from backtest() function")
  }

  # Ensure trades_table exists
  if (is.null(trades_table)) {
    trades_table <- data.frame()
  }

  output_type <- match.arg(output_type)

  # --------------------------
  # 2. Validate data
  # --------------------------
  if (is.null(total_table) || nrow(total_table) == 0) {
    stop("No equity curve data found")
  }

  if (is.null(initial_capital)) {
    initial_capital <- total_table$total_asset[1]
  }

  # --------------------------
  # 3. Date range calculations
  # --------------------------
  start_date <- min(total_table$date)
  end_date <- max(total_table$date)
  total_calendar_days <- as.numeric(end_date - start_date)
  total_trade_days <- nrow(total_table) - 1

  # --------------------------
  # 4. Return calculations
  # --------------------------
  daily_returns <- na.omit(total_table$daily_return[-1])
  if (length(daily_returns) == 0) {
    stop("No valid daily returns found")
  }

  final_capital <- tail(total_table$total_asset, 1)
  total_return <- (final_capital / initial_capital) - 1
  annual_return <- (1 + total_return)^(252 / length(daily_returns)) - 1

  # --------------------------
  # 5. Risk calculations
  # --------------------------
  annual_volatility <- stats::sd(daily_returns, na.rm = TRUE) * sqrt(252)

  negative_returns <- daily_returns[daily_returns < 0]
  downside_volatility <- if (length(negative_returns) > 0) {
    stats::sd(negative_returns) * sqrt(252)
  } else {
    0
  }

  # --------------------------
  # 6. Cumulative returns and drawdown
  # --------------------------
  total_table <- total_table %>%
    dplyr::mutate(
      cum_return = .data$total_asset / initial_capital,
      cum_return_high = cummax(.data$cum_return),
      drawdown = (.data$cum_return - .data$cum_return_high) / .data$cum_return_high
    )

  max_drawdown <- min(total_table$drawdown, na.rm = TRUE)
  max_drawdown_abs <- abs(max_drawdown)

  # --------------------------
  # 7. Risk-adjusted returns
  # --------------------------
  sharpe <- if (annual_volatility == 0) 0 else (annual_return - risk_free_rate) / annual_volatility
  sortino <- if (downside_volatility == 0) 0 else (annual_return - risk_free_rate) / downside_volatility
  calmar <- if (max_drawdown_abs == 0) 0 else annual_return / max_drawdown_abs

  # --------------------------
  # 8. Win rate calculations
  # --------------------------
  win_days <- sum(daily_returns > 0)
  day_win_rate <- win_days / length(daily_returns)

  # Get rebalance times from config
  rebalance_times <- 0
  if (!is.null(config)) {
    if ("total_rebalance_days" %in% names(config)) {
      rebalance_times <- config$total_rebalance_days
    }
  }

  # --------------------------
  # 9. Trade statistics
  # --------------------------
  total_trades <- nrow(trades_table)
  buy_trades <- if (total_trades > 0) {
    sum(trades_table$direction == "BUY", na.rm = TRUE)
  } else {
    0
  }
  sell_trades <- if (total_trades > 0) {
    sum(trades_table$direction == "SELL", na.rm = TRUE)
  } else {
    0
  }

  # Trade win rate and profit/loss ratio
  trade_win_rate <- 0
  profit_loss_ratio <- 0

  if (total_trades > 0 && "trade_date" %in% colnames(trades_table) && "cash_after" %in% colnames(trades_table)) {
    # Group by trade date to calculate P&L per trading day
    trade_profit <- trades_table %>%
      dplyr::group_by(.data$trade_date) %>%
      dplyr::summarise(
        pnl = dplyr::last(.data$cash_after) - dplyr::first(.data$cash_after),
        .groups = "drop"
      )

    win_trades <- sum(trade_profit$pnl > 0, na.rm = TRUE)
    total_valid <- nrow(trade_profit)

    if (total_valid > 0) {
      trade_win_rate <- win_trades / total_valid

      avg_profit <- mean(trade_profit$pnl[trade_profit$pnl > 0], na.rm = TRUE)
      avg_loss <- mean(abs(trade_profit$pnl[trade_profit$pnl < 0]), na.rm = TRUE)

      if (!is.na(avg_profit) && !is.na(avg_loss) && avg_loss > 0) {
        profit_loss_ratio <- avg_profit / avg_loss
      }
    }
  }

  # --------------------------
  # 10. Build metrics table
  # --------------------------
  metrics_table <- data.frame(
    category = c(
      rep("Basic Info", 7), rep("Returns", 3), rep("Risk", 3),
      rep("Risk-Adjusted Returns", 3), rep("Trading", 6)
    ),
    metric = c(
      "Start Date", "End Date", "Calendar Days", "Trading Days",
      "Initial Capital", "Final Capital", "Rebalance Times",
      "Total Return", "Annual Return", "Risk-Free Rate",
      "Annual Volatility", "Downside Volatility", "Max Drawdown",
      "Sharpe Ratio", "Sortino Ratio", "Calmar Ratio",
      "Daily Win Rate", "Trade Win Rate", "Total Trades",
      "Buy Trades", "Sell Trades", "Profit/Loss Ratio"
    ),
    value = c(
      as.character(start_date), as.character(end_date),
      total_calendar_days, total_trade_days,
      round(initial_capital, 2), round(final_capital, 2), rebalance_times,
      paste0(round(total_return * 100, 2), "%"),
      paste0(round(annual_return * 100, 2), "%"),
      paste0(round(risk_free_rate * 100, 2), "%"),
      paste0(round(annual_volatility * 100, 2), "%"),
      paste0(round(downside_volatility * 100, 2), "%"),
      paste0(round(max_drawdown * 100, 2), "%"),
      round(sharpe, 2), round(sortino, 2), round(calmar, 2),
      paste0(round(day_win_rate * 100, 2), "%"),
      paste0(round(trade_win_rate * 100, 2), "%"),
      total_trades, buy_trades, sell_trades, round(profit_loss_ratio, 2)
    ),
    stringsAsFactors = FALSE
  )

  # --------------------------
  # 11. Prepare daily details
  # --------------------------
  daily_details <- total_table %>%
    dplyr::select(
      .data$date, .data$total_asset, .data$daily_return,
      .data$cum_return, .data$cum_return_high, .data$drawdown
    )

  # --------------------------
  # 12. Output format conversion
  # --------------------------
  if (output_type == "tibble") {
    metrics_table <- tibble::as_tibble(metrics_table)
    daily_details <- tibble::as_tibble(daily_details)
    if (nrow(trades_table) > 0) {
      trades_table <- tibble::as_tibble(trades_table)
    }
  }

  # --------------------------
  # 13. Print summary
  # --------------------------
  message("\n==============================================")
  message("Performance Analysis Summary")
  message("==============================================")
  message("Period: ", start_date, " to ", end_date)
  message("Total Return: ", round(total_return * 100, 2), "%")
  message("Annual Return: ", round(annual_return * 100, 2), "%")
  message("Max Drawdown: ", round(max_drawdown * 100, 2), "%")
  message("Sharpe Ratio: ", round(sharpe, 2))
  message("Daily Win Rate: ", round(day_win_rate * 100, 2), "%")
  message("Total Trades: ", total_trades)
  message("==============================================\n")

  # --------------------------
  # 14. Return results
  # --------------------------
  return(list(
    metrics = metrics_table,
    daily_details = daily_details,
    transactions = trades_table
  ))
}

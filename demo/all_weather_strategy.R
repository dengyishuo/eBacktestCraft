# File: demo/all_weather_strategy.R
# Purpose: All-weather portfolio backtest using built-in data

library(eBacktestCraft)

data("all_weather")

# 1. Signal & Weight Calculation
cat("\n=== Preparing Data ===\n")
dat_with_signal <- add_signal(all_weather, type = "constant")
dat_with_weight <- add_fixed_weight(
  dat_with_signal,
  signal_col    = "signal_constant_1",
  fixed_weights = c(0.15, 0.08, 0.07, 0.2, 0.2, 0.15, 0.075, 0.075),
  strict_check  = FALSE
)

# 2. Configuration & Backtest Execution
cat("\n=== Running Backtest ===\n")
cfg <- default_backtest_config() |>
  set_weight_col("weight_fixed_signal_constant_1") |>
  set_init_capital(100000) |>
  set_fee_rate(0.0003) |>
  set_stamp_tax(0.0005) |>
  set_slippage_rate(0.001) |>
  set_lot_size(100) |>
  set_exec_price_col("close") |>
  set_eval_price_col("adjusted") |>
  set_rebalance_cycle("quarterly")

res <- run_backtest(cfg, dat_with_weight)

# 3. Backtest Results
cat("\n=== Performance Summary ===\n")
print(performance_analysis(res)$metrics)

final_nav <- tail(res$equity_curve$total_asset, 1)
ret <- (final_nav / 100000 - 1) * 100
cat(sprintf("\nFinal Net Asset Value: %.2f, Total Return: %.2f%%\n", final_nav, ret))
cat(sprintf("Total Number of Transactions: %d\n", nrow(res$transactions)))

# File: demo/style_strategy.R
# Purpose: Style ETF momentum rotation using built-in dataset

library(eClassic)
library(eBacktestCraft)

data("style", package = "eBacktestCraft")
dat_style <- style

# 1. Risk-Adjusted Momentum (RAM-20)
dat_style_with_indicator <- eClassic::add_ram(dat_style, close_col = "adjusted", n = 20)

# 2. Cross-Sectional Ranking Signal: Select top 3 assets by RAM-20 daily
dat_style_with_signal <- add_rank_signal(
  dat_style_with_indicator,
  rank_col = "ram_20",
  top_n    = 3
)

# 3. Softmax Weight Allocation
dat_style_with_weight <- add_norm_weight(
  mkt_data    = dat_style_with_signal,
  weight_col  = "ram_20",
  signal_col  = "signal_ram_20_top3",
  norm_method = "softmax"
)

# 4. Configuration & Backtest (Monthly + 5% Drift Hybrid Rebalancing)
cfg <- default_backtest_config() |>
  set_weight_col("weight_ram_20_signal_ram_20_top3") |>
  set_init_capital(100000) |>
  set_fee_rate(0.0003) |>
  set_stamp_tax(0.0005) |>
  set_slippage_rate(0.001) |>
  set_lot_size(100) |>
  set_exec_price_col("close") |>
  set_eval_price_col("adjusted") |>
  set_rebalance_mode("hybrid") |>
  set_rebalance_cycle("monthly") |>
  set_weight_change_threshold(0.05)

res_style <- run_backtest(cfg, dat_style_with_weight)

# 5. Backtest Results
cat("\n=== Performance Summary ===\n")
print(performance_analysis(res_style)$metrics)

cat("\nEquity Curve (Last 5 Rows):\n")
print(tail(res_style$equity_curve))

cat("\nTransaction Records (First 5 Rows):\n")
print(head(res_style$transactions))

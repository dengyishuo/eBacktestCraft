# ============================================================
# eBacktestCraft Manual Integration Test
# add_indicator -> add_signal -> add_weight -> run_backtest
# ============================================================
library(eBacktestCraft)
library(dplyr)

data(style, package = "eBacktestCraft")

cat("--- 1. add_indicator ---\n")
df <- add_indicator(style, "mom", close_col = "adjusted", n = 20)
df <- add_indicator(df, "eClassic.volatility", close_col = "adjusted", n = 20)
df <- add_indicator(df, "rsi", close_col = "close", n = 14)
cat(sprintf("  新增列: %s\n", paste(setdiff(colnames(df), colnames(style)), collapse = ", ")))

cat("\n--- 2. add_signal ---\n")
df <- add_signal(df, type = "percentile", indicator_col = "mom_20", pct = 0.3)
sig_cols <- grep("^signal_", colnames(df), value = TRUE)
cat(sprintf("  信号列: %s\n", paste(sig_cols, collapse = ", ")))

cat("\n--- 3. add_weight ---\n")
df <- add_weight(df, type = "equal", signal_col = sig_cols[1])
wt_cols <- grep("^weight_", colnames(df), value = TRUE)
cat(sprintf("  权重列: %s\n", paste(wt_cols, collapse = ", ")))

cat("\n--- 4. config ---\n")
config <- default_backtest_config() |>
  set_weight_col(wt_cols[1]) |>
  set_init_capital(1000000) |>
  set_date_range("2023-01-01", "2024-12-31") |>
  set_rebalancing(mode = "calendar", cycle = "monthly")
cat(sprintf("  weight_col: %s\n", config$weight_col))

cat("\n--- 5. run_backtest ---\n")
result <- run_backtest(config, df)

cat("\n--- 6. performance_analysis ---\n")
perf <- performance_analysis(result, what = "metrics")
print(perf)

cat("\n--- 7. plots ---\n")
perf_daily <- performance_analysis(result, what = "daily_details")
print(plot_equity_curve(perf_daily))
cat("equity curve OK\n")
print(plot_drawdown(perf_daily))
cat("drawdown OK\n")
print(plot_return_dist(perf_daily))
cat("return dist OK\n")

cat("\n=== ALL PASS ===\n")

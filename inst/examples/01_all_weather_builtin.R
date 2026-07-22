# ============================================================
# 全天候组合（内置数据集）
# All-Weather Portfolio — built-in dataset
# ============================================================
#
# 策略：固定权重配置 8 类资产（股票、债券、黄金、商品）
# 数据：内置 all_weather 数据集，无需网络连接
# ============================================================

library(eBacktestCraft)

# ── 1. 加载内置数据 ──────────────────────────────────────────
data("all_weather")

# ── 2. 生成常数信号 + 固定权重 ──────────────────────────────
dat_with_signal <- add_signal(all_weather, type = "constant")

dat_with_weight <- add_fixed_weight(
  dat_with_signal,
  signal_col    = "signal_constant_1",
  fixed_weights = c(0.15, 0.08, 0.07, 0.2, 0.2, 0.15, 0.075, 0.075),
  strict_check  = FALSE
)

# ── 3. 回测 ───────────────────────────────────────────────────
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

# ── 4. 结果 ───────────────────────────────────────────────────
cat("\n=== 绩效摘要 ===\n")
print(performance_analysis(res)$metrics)

cat("\n净值曲线（最后5行）：\n")
print(tail(res$equity_curve))

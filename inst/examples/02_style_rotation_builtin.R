# ============================================================
# 风格 ETF 动量轮动（内置数据集）
# Style ETF Momentum Rotation — built-in dataset
# ============================================================
#
# 策略：基于风险调整动量（RAM-20）每日选出排名前 3 的 ETF，
#        按 softmax 权重分配仓位，月度 + 5% 漂移混合调仓
# 数据：内置 style 数据集，无需网络连接
# ============================================================

library(eClassic)
library(eBacktestCraft)

# ── 1. 加载内置数据 ──────────────────────────────────────────
data("style", package = "eBacktestCraft")
dat_style <- style

# ── 2. 计算风险调整动量（RAM-20）────────────────────────────
dat_style_with_indicator <- eClassic::add_ram(dat_style, close_col = "adjusted", n = 20)

# ── 3. 截面排名信号：每日选出 RAM-20 前 3 ─────────────────────
dat_style_with_signal <- add_rank_signal(
  dat_style_with_indicator,
  rank_col = "ram_20",
  top_n    = 3
)

# ── 4. Softmax 权重分配（按 RAM-20 大小正比分配）──────────────
dat_style_with_weight <- add_norm_weight(
  mkt_data    = dat_style_with_signal,
  weight_col  = "ram_20",
  signal_col  = "signal_ram_20_top3",
  norm_method = "softmax"
)

# ── 5. 回测（混合调仓：月度 + 权重漂移 > 5% 触发）────────────
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

# ── 6. 查看结果 ───────────────────────────────────────────────
cat("\n=== 绩效摘要 ===\n")
print(performance_analysis(res_style)$metrics)

cat("\n=== 净值曲线（最后5行）===\n")
print(tail(res_style$equity_curve))

cat("\n=== 交易记录（前5行）===\n")
print(head(res_style$transactions))

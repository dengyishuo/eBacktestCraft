# ============================================================
# 全天候组合（实时数据）
# All-Weather Portfolio — live data via eFactorCraft
# ============================================================
#
# 策略：固定权重配置 8 类资产（股票、债券、黄金、商品）
# 数据：通过 eFactorCraft::get_data() 实时下载，需要网络连接
# ============================================================

library(eFactorCraft)
library(eBacktestCraft)

# ── 1. 定义资产池 ─────────────────────────────────────────────
universe <- data.frame(
  code = c(
    "510300.SS",  # 沪深300ETF
    "512100.SS",  # 中证1000
    "512890.SS",  # 红利低波
    "511130.SS",  # 30年期国债
    "511260.SS",  # 10年期国债
    "511010.SS",  # 5年期国债
    "518880.SS",  # 黄金ETF
    "510170.SS"   # 商品ETF
  ),
  name = c(
    "沪深300ETF", "中证1000", "红利低波",
    "30年期国债", "10年期国债", "5年期国债",
    "黄金ETF", "商品ETF"
  ),
  stringsAsFactors = FALSE
)

# ── 2. 下载历史行情 ───────────────────────────────────────────
cat("下载行情数据...\n")
dat <- eFactorCraft::get_data(
  universe   = universe,
  start_date = "2020-01-01",
  end_date   = as.character(Sys.Date())
)

# ── 3. 生成常数信号 + 固定权重 ──────────────────────────────
dat_with_signal <- add_signal(dat, type = "constant")

dat_with_weight <- add_fixed_weight(
  dat_with_signal,
  signal_col    = "signal_constant_1",
  fixed_weights = c(0.15, 0.08, 0.07, 0.2, 0.2, 0.15, 0.075, 0.075),
  strict_check  = FALSE
)

# ── 4. 回测 ──────────────────────────────────────────────────
cat("运行回测...\n")

cfg <- default_backtest_config() |>
  set_init_capital(100000) |>
  set_fee_rate(0.0003) |>
  set_stamp_tax(0.0005) |>
  set_slippage_rate(0.001) |>
  set_exec_price_col("open") |>   # eFactorCraft::get_data 返回小写列名
  set_rebalance_cycle("quarterly")

cfg <- set_weight_col(cfg, "weight_fixed_signal_constant_1")

res <- run_backtest(cfg, dat_with_weight)

# ── 5. 绩效摘要 ───────────────────────────────────────────────
cat("\n=== 全天候组合绩效 ===\n")
print(performance_analysis(res)$metrics)

cat("\n净值曲线（最后5行）：\n")
print(tail(res$equity_curve))

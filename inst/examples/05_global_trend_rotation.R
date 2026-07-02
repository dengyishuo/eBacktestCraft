# ============================================================
# 全球资产趋势轮动
# Global Asset Trend Rotation — Nasdaq 100 / CSI 300 / Gold
# ============================================================
#
# 策略：计算 20 日动量，动量 > 0 时做多，按动量大小线性加权；
#        每 10 个交易日或权重漂移 > 3% 时触发调仓
# 数据：通过 eFactorCraft::get_data() 实时下载，需要网络连接
# ============================================================

library(eFactorCraft)
library(eClassic)
library(eBacktestCraft)

# ── 1. 定义资产池 ─────────────────────────────────────────────
universe <- data.frame(
  code = c("513100.SS", "510300.SS", "518880.SS"),
  name = c("Nasdaq 100 ETF", "CSI 300 ETF", "Gold ETF"),
  stringsAsFactors = FALSE
)

# ── 2. 下载历史行情 ───────────────────────────────────────────
cat("下载行情数据...\n")
global_price_dat <- eFactorCraft::get_data(
  universe   = universe,
  start_date = "2020-01-01",
  end_date   = as.character(Sys.Date())
)

# ── 3. 计算 20 日价格动量 ─────────────────────────────────────
global_price_dat <- eClassic::add_mom(
  mkt_data  = global_price_dat,
  close_col = "adjusted",
  n         = 20
)

# ── 4. 生成交易信号（动量 > 0 时做多）──────────────────────────
global_price_dat <- add_signal(
  mkt_data       = global_price_dat,
  indicator_cols = "mom_20",
  signal_type    = "threshold",
  threshold      = 0,
  compare_op     = ">",
  signal_name    = "signal_mom20_gt_0"
)

# ── 5. 按动量大小线性加权（只对有效信号赋权）─────────────────
global_price_dat <- add_norm_weight(
  mkt_data    = global_price_dat,
  weight_col  = "mom_20",
  signal_col  = "signal_mom20_gt_0",
  norm_method = "linear",
  weight_name = "weight_final",
  zero_na     = TRUE,
  output      = "tibble"
)

# ── 6. 回测 ──────────────────────────────────────────────────
cat("运行回测...\n")
cfg <- default_backtest_config() |>
  set_weight_col("weight_final") |>
  set_start_date("2020-01-01") |>
  set_end_date(as.character(Sys.Date())) |>
  set_exec_price_col("open") |> # eFactorCraft::get_data 返回小写列名
  set_eval_price_col("adjusted") |>
  set_init_capital(100000) |>
  set_rebalance_mode("hybrid") |>
  set_rebalance_cycle(10) |>
  set_weight_change_threshold(0.03) |>
  set_fee_rate(0.0003) |>
  set_stamp_tax(0.001) |>
  set_slippage_rate(0.001) |>
  set_lot_size(100)

bt_result <- run_backtest(cfg, global_price_dat)

# ── 7. 绩效摘要 ───────────────────────────────────────────────
cat("\n=== 全球趋势轮动绩效 ===\n")
print(performance_analysis(bt_result)$metrics)

cat("\n净值曲线（最后5行）：\n")
print(tail(bt_result$equity_curve))

cat("\n交易记录（前5行）：\n")
print(head(bt_result$transactions))

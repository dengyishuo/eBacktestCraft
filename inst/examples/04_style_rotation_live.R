# ============================================================
# 风格 ETF 动量轮动（实时数据，13只ETF）
# Style ETF Momentum Rotation — live data via eFactorCraft
# ============================================================
#
# 策略：基于风险调整动量（RAM-20）每日选出排名前 3 的 ETF，
#        按 softmax 权重分配仓位，月度 + 5% 漂移混合调仓
# 数据：通过 eFactorCraft::get_data() 实时下载，需要网络连接
# ============================================================

library(eFactorCraft)
library(eClassic)
library(eBacktestCraft)

# ── 1. 定义资产池（13只风格/红利/成长ETF）─────────────────────
universe <- data.frame(
  code = c(
    "563020.SS",  # 红利低波ETF易方达
    "515180.SS",  # 红利ETF易方达
    "512100.SS",  # 中证1000ETF南方
    "159531.SZ",  # 中证2000ETF南方
    "159259.SZ",  # 成长ETF易方达
    "159967.SZ",  # 创业板成长ETF华夏
    "588020.SS",  # 科创成长ETF易方达
    "562310.SS",  # 沪深300成长ETF银华
    "562520.SS",  # 中证1000成长ETF华夏
    "159606.SZ",  # 中证500成长ETF易方达
    "159209.SZ",  # 红利质量ETF招商
    "515960.SS",  # 质量ETF中金
    "560500.SS"   # 500质量成长ETF鹏扬
  ),
  name = c(
    "红利低波ETF易方达", "红利ETF易方达", "中证1000ETF南方",
    "中证2000ETF南方", "成长ETF易方达", "创业板成长ETF华夏",
    "科创成长ETF易方达", "沪深300成长ETF银华", "中证1000成长ETF华夏",
    "中证500成长ETF易方达", "红利质量ETF招商", "质量ETF中金",
    "500质量成长ETF鹏扬"
  ),
  stringsAsFactors = FALSE
)

# ── 2. 下载历史行情 ───────────────────────────────────────────
cat("下载行情数据（", nrow(universe), "只ETF）...\n")
dat_style <- eFactorCraft::get_data(
  universe   = universe,
  start_date = "2020-01-01",
  end_date   = as.character(Sys.Date())
)
cat("下载完成，共", nrow(dat_style), "行\n")

# ── 3. 计算风险调整动量（RAM-20）────────────────────────────
dat_style_with_indicator <- eClassic::add_ram(dat_style, close_col = "adjusted", n = 20)

# ── 4. 截面排名信号：每日选出 RAM-20 前 3 ─────────────────────
dat_style_with_signal <- add_rank_signal(
  dat_style_with_indicator,
  rank_col = "ram_20",
  top_n    = 3
)

# ── 5. Softmax 权重分配 ───────────────────────────────────────
dat_style_with_weight <- add_norm_weight(
  mkt_data    = dat_style_with_signal,
  weight_col  = "ram_20",
  signal_col  = "signal_ram_20_top3",
  norm_method = "softmax"
)

# ── 6. 回测（混合调仓：月度 + 权重漂移 > 5% 触发）────────────
cat("运行回测...\n")

cfg <- default_backtest_config() |>
  set_init_capital(100000) |>
  set_fee_rate(0.0003) |>
  set_stamp_tax(0.0005) |>
  set_slippage_rate(0.001) |>
  set_exec_price_col("open") |>   # eFactorCraft::get_data 返回小写列名
  set_rebalance_mode("hybrid") |>
  set_rebalance_cycle("monthly")

cfg <- set_weight_col(cfg, "weight_ram_20_signal_ram_20_top3") |>
  set_weight_change_threshold(0.05)

res_style <- run_backtest(cfg, dat_style_with_weight)

# ── 7. 查看结果 ───────────────────────────────────────────────
cat("\n=== 风格轮动绩效 ===\n")
print(performance_analysis(res_style)$metrics)

cat("\n净值曲线（最后5行）：\n")
print(tail(res_style$equity_curve))

cat("\n交易记录（前5行）：\n")
print(head(res_style$transactions))

# ============================================================
# 双 EMA 交叉策略
# Dual EMA Crossover Strategy — Golden Cross / Death Cross
# ============================================================
#
# 策略：短期 EMA(12) 上穿长期 EMA(26) 时买入（金叉）
#       短期 EMA(12) 下穿长期 EMA(26) 时卖出（死叉）
# 标的：AAPL（苹果公司）
# ============================================================

library(eBacktestCraft)
library(quantmod)

# ── 1. 获取行情数据 ──────────────────────────────────────────
cat("获取 AAPL 数据...\n")
getSymbols("AAPL", from = "2019-01-01", to = "2025-08-20", adjust = TRUE,
           auto.assign = TRUE)

dat <- AAPL
df <- data.frame(
  date     = as.Date(index(dat)),
  code     = "AAPL",
  name     = "Apple Inc.",
  open     = as.numeric(quantmod::Op(dat)),
  high     = as.numeric(quantmod::Hi(dat)),
  low      = as.numeric(quantmod::Lo(dat)),
  close    = as.numeric(quantmod::Cl(dat)),
  volume   = as.numeric(quantmod::Vo(dat)),
  adjusted = as.numeric(quantmod::Ad(dat)),
  stringsAsFactors = FALSE
)
df <- df[df$date >= as.Date("2020-01-01"), ]
cat(sprintf("数据范围: %s ~ %s, %d 行\n", min(df$date), max(df$date), nrow(df)))

# ── 2. EMA 交叉信号 ───────────────────────────────────────
cat("生成 EMA 交叉信号...\n")
# mode="both": 金叉=1, 死叉=-1, 无交叉=0
df <- add_ma_cross_signal(df,
  close_col   = "adjusted",
  fast_n      = 12,
  slow_n      = 26,
  mode        = "both",
  signal_name = "ema_cross_12_26")

# ── 3. 持仓状态持久化 ─────────────────────────────────────
# 金叉后一直持有，直到死叉信号才清仓
df$hold <- 0
for (i in seq_len(nrow(df))) {
  if (i > 1) df$hold[i] <- df$hold[i - 1]
  if (!is.na(df$ema_cross_12_26[i])) {
    if (df$ema_cross_12_26[i] == 1)  df$hold[i] <- 1
    if (df$ema_cross_12_26[i] == -1) df$hold[i] <- 0
  }
}
df$signal_ema_cross <- as.integer(df$hold)
cat(sprintf("金叉: %d, 死叉: %d, 持仓天数: %d / %d\n",
  sum(!is.na(df$ema_cross_12_26) & df$ema_cross_12_26 == 1),
  sum(!is.na(df$ema_cross_12_26) & df$ema_cross_12_26 == -1),
  sum(df$hold), nrow(df)))

# ── 4. 等权配置 (信号=1 时满仓) ──────────────────────────────
df <- add_equal_weight(df,
  signal_col  = "signal_ema_cross",
  weight_name = "weight_ema_cross")

# ── 5. 回测配置与执行 ────────────────────────────────────────
cfg <- default_backtest_config() |>
  set_weight_col("weight_ema_cross") |>
  set_init_capital(100000) |>
  set_start_date("2020-01-01") |>
  set_end_date("2025-08-20") |>
  set_exec_price_col("close") |>
  set_eval_price_col("adjusted") |>
  set_lot_size(1) |>
  set_rebalance_cycle("daily")

cat("\n执行回测...\n")
res <- run_backtest(cfg, df)

# ── 6. 绩效摘要 ───────────────────────────────────────────────
cat("\n=== EMA 交叉策略绩效 (AAPL) ===\n")
perf <- performance_analysis(res)
print(perf$metrics)

cat(sprintf("\n总交易次数: %d\n", nrow(res$transactions)))

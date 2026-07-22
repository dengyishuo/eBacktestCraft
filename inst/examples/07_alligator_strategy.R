# ============================================================
# 鳄鱼线趋势策略
# Alligator Trend Strategy — EMA alignment on single asset
# ============================================================
#
# 策略：当 EMA13 > EMA8 > EMA5 时做多（鳄鱼张口向上），否则空仓
#       三条 EMA 分别代表 jaw（下颚）、teeth（牙齿）、lips（嘴唇）
# 数据：通过 quantmod 获取上证指数 000001.SS 实时行情
# ============================================================

library(eBacktestCraft)
library(eTTR)
library(quantmod)

# ── 1. 获取行情数据 ──────────────────────────────────────────
cat("获取上证指数数据...\n")
getSymbols("000001.SS", from = "2019-01-01", auto.assign = TRUE)

dat <- `000001.SS`
df <- data.frame(
  date     = as.Date(index(dat)),
  code     = "000001.SS",
  name     = "Shanghai Composite",
  open     = as.numeric(quantmod::Op(dat)),
  high     = as.numeric(quantmod::Hi(dat)),
  low      = as.numeric(quantmod::Lo(dat)),
  close    = as.numeric(quantmod::Cl(dat)),
  volume   = as.numeric(quantmod::Vo(dat)),
  adjusted = as.numeric(quantmod::Ad(dat)),
  stringsAsFactors = FALSE
)

df <- df[df$date >= as.Date("2019-12-01"), ]
cat(sprintf("数据范围: %s ~ %s, %d 行\n", min(df$date), max(df$date), nrow(df)))

# ── 2. 计算鳄鱼线 (EMA) ─────────────────────────────────────
cat("计算鳄鱼线指标...\n")
df <- eTTR::add_ema(df, n = 13)  # jaw   (下颚)
df <- eTTR::add_ema(df, n = 8)   # teeth (牙齿)
df <- eTTR::add_ema(df, n = 5)   # lips  (嘴唇)

# ── 3. 构造比较列 ──────────────────────────────────────────
df$jaw_above_teeth  <- df$EMA_13 - df$EMA_8
df$teeth_above_lips <- df$EMA_8  - df$EMA_5

# ── 4. 生成信号 ────────────────────────────────────────────
df <- add_threshold_signal(df,
  indicator_cols = "jaw_above_teeth",
  threshold = 0, compare_op = ">",
  signal_name = "sig_jaw_gt_teeth")

df <- add_threshold_signal(df,
  indicator_cols = "teeth_above_lips",
  threshold = 0, compare_op = ">",
  signal_name = "sig_teeth_gt_lips")

df <- add_multi_condition_signal(df,
  indicator_cols = c("sig_jaw_gt_teeth", "sig_teeth_gt_lips"),
  logic_op = "&",
  signal_name = "alligator_uptrend")

# ── 5. 等权配置 (信号=1 时满仓) ───────────────────────────────
df <- add_equal_weight(df,
  signal_col  = "alligator_uptrend",
  weight_name = "weight_alligator")

# ── 6. 回测配置与执行 ─────────────────────────────────────────
cfg <- default_backtest_config() |>
  set_weight_col("weight_alligator") |>
  set_init_capital(100000) |>
  set_start_date("2019-12-01") |>
  set_exec_price_col("close") |>
  set_eval_price_col("adjusted") |>
  set_fee_rate(0) |>
  set_stamp_tax(0) |>
  set_slippage_rate(0) |>
  set_lot_size(1) |>
  set_rebalance_cycle("daily")

cat("\n执行回测...\n")
res <- run_backtest(cfg, df)

# ── 7. 绩效摘要 ───────────────────────────────────────────────
cat("\n=== 鳄鱼线策略绩效 ===\n")
perf <- performance_analysis(res)
print(perf$metrics)

cat("\n净值曲线（最后 5 行）：\n")
print(tail(res$equity_curve))

cat(sprintf("\n总交易次数: %d\n", nrow(res$transactions)))

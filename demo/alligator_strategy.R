# File: demo/alligator_strategy.R
# Purpose: Alligator trend strategy — EMA13 > EMA8 > EMA5 alignment

library(eBacktestCraft)
library(eTTR)
library(quantmod)

# 1. Data
cat("Fetching data...\n")
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

# 2. Alligator EMAs
df <- eTTR::add_ema(df, n = 13)  # jaw
df <- eTTR::add_ema(df, n = 8)   # teeth
df <- eTTR::add_ema(df, n = 5)   # lips

# 3. Signal construction
df$jaw_above_teeth  <- df$EMA_13 - df$EMA_8
df$teeth_above_lips <- df$EMA_8  - df$EMA_5
df <- add_threshold_signal(df, indicator_cols = "jaw_above_teeth",  threshold = 0, compare_op = ">", signal_name = "sig_jaw_gt_teeth")
df <- add_threshold_signal(df, indicator_cols = "teeth_above_lips", threshold = 0, compare_op = ">", signal_name = "sig_teeth_gt_lips")
df <- add_multi_condition_signal(df, indicator_cols = c("sig_jaw_gt_teeth", "sig_teeth_gt_lips"), logic_op = "&", signal_name = "alligator_uptrend")
df <- add_equal_weight(df, signal_col = "alligator_uptrend", weight_name = "weight_alligator")

# 4. Backtest
cfg <- default_backtest_config() |>
  set_weight_col("weight_alligator") |>
  set_init_capital(100000)     |>
  set_start_date("2019-12-01") |>
  set_exec_price_col("close")  |>
  set_eval_price_col("adjusted") |>
  set_lot_size(1)              |>
  set_rebalance_cycle("daily")

res <- run_backtest(cfg, df)

# 5. Results
cat("\n=== Alligator Strategy Performance ===\n")
print(performance_analysis(res)$metrics)
cat(sprintf("\nTotal trades: %d\n", nrow(res$transactions)))

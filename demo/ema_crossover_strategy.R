# File: demo/ema_crossover_strategy.R
# Purpose: Dual EMA crossover strategy — Golden Cross / Death Cross on AAPL

library(eBacktestCraft)
library(quantmod)

# 1. Data
cat("Fetching AAPL data...\n")
getSymbols("AAPL", from = "2019-01-01", to = "2025-08-20", adjust = TRUE, auto.assign = TRUE)
dat <- AAPL
df <- data.frame(
  date = as.Date(index(dat)), code = "AAPL", name = "Apple Inc.",
  open = as.numeric(Op(dat)), high = as.numeric(Hi(dat)),
  low = as.numeric(Lo(dat)), close = as.numeric(Cl(dat)),
  volume = as.numeric(Vo(dat)), adjusted = as.numeric(Ad(dat)),
  stringsAsFactors = FALSE
)
df <- df[df$date >= as.Date("2020-01-01"), ]

# 2. EMA crossover signal (golden=1, death=-1, else=0)
df <- add_ma_cross_signal(df, close_col = "adjusted",
                          fast_n = 12, slow_n = 26, mode = "both",
                          signal_name = "ema_cross_12_26")

# 3. Persistent position (hold from golden cross until death cross)
df$hold <- 0
for (i in seq_len(nrow(df))) {
  if (i > 1) df$hold[i] <- df$hold[i - 1]
  if (!is.na(df$ema_cross_12_26[i])) {
    if (df$ema_cross_12_26[i] == 1)  df$hold[i] <- 1
    if (df$ema_cross_12_26[i] == -1) df$hold[i] <- 0
  }
}
df$signal_ema_cross <- as.integer(df$hold)

# 4. Equal weight and backtest
df <- add_equal_weight(df, signal_col = "signal_ema_cross",
                       weight_name = "weight_ema_cross")
cfg <- default_backtest_config() |>
  set_weight_col("weight_ema_cross") |>
  set_init_capital(100000) |>
  set_start_date("2020-01-01") |>
  set_end_date("2025-08-20") |>
  set_exec_price_col("close") |>
  set_eval_price_col("adjusted") |>
  set_lot_size(1) |>
  set_rebalance_cycle("daily")

res <- run_backtest(cfg, df)

# 5. Results
cat("\n=== EMA Crossover Performance (AAPL) ===\n")
print(performance_analysis(res)$metrics)
cat(sprintf("\nTotal trades: %d\n", nrow(res$transactions)))

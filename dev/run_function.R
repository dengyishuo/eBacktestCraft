# ETF 代码与名称对应表
stock_df <- data.frame(
  code = c(
    "510300.SS",
    "512100.SS",
    "512890.SS",
    "511130.SS",
    "511260.SS",
    "511010.SS",
    "518880.SS",
    "510170.SS"
  ),
  name = c(
    "沪深300ETF",
    "中证1000",
    "红利低波",
    "30年期国债",
    "10年期国债",
    "5年期国债",
    "黄金ETF",
    "商品ETF"
  ),
  stringsAsFactors = FALSE
)

library(FactorCraft)
dat <- get_data(stock_df, start_date = "2020-01-01", end_date = "2026-05-01")

dat_with_signal <- add_signal(dat, signal_type = "constant")

dat_with_weight <- add_fixed_weight(dat_with_signal,
  signal_col = "signal_constant_1",
  fixed_weights = c(0.15, 0.08, 0.07, 0.2, 0.2, 0.15, 0.075, 0.075),
  strict_check = FALSE
)


# ==============================================
# 1. backtest 函数
# ==============================================
bt_result <- backtest(
  # 核心基础参数
  df = dat_with_weight,
  weight_col = "weight_fixed_signal_constant_1",
  signal_col = "signal_constant_1",
  init_capital = 100000,
  fee_rate = 0.0003,
  stamp_tax = 0.0005,
  slippage_rate = 0.001,
  lot_size = 100,
  min_weight = 1e-6,
  single_max_weight = 0.95,
  global_max_hold_pct = 1.0,

  # 调仓参数
  rebalance_mode = "calendar",
  rebalance_cycle = "quarterly",

  # 价格参数
  exec_price_col = "close",
  eval_price_col = "adjusted",

  # 风险控制
  enable_stop_loss = FALSE,

  # 输出格式
  output_type = "tibble"
)

# ==============================================
# 3. run_backtest_final 函数
# ==============================================
res <- run_backtest_final(
  # 核心基础参数
  df = dat_with_weight,
  weight_col = "weight_fixed_signal_constant_1",
  init_capital = 100000,
  fee_rate = 0.0003,
  stamp_tax = 0.0005,
  slippage_rate = 0.001,
  lot_size = 100,
  min_weight = 1e-6,
  single_max_weight = 0.95,
  global_max_hold_pct = 1.0,
  skip_suspended = TRUE,

  # 调仓参数
  rebalance_mode = "calendar",
  rebalance_cycle = "quarterly",
  weight_change_threshold = 0.01,

  # 价格参数
  exec_price_col = "close",
  eval_price_col = "adjusted",

  # 风险控制
  enable_component_stop_loss = FALSE,
  enable_portfolio_stop_loss = FALSE,
  enable_component_take_profit = FALSE,
  enable_portfolio_take_profit = FALSE,

  # 输出格式
  output_type = "tibble"
)

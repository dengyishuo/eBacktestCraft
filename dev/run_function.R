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

library(eFactorCraft)

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


##########################################################
################ 风格轮动ETF组合
##########################################################


# ETF 代码与名称对应表
stock_df <- data.frame(
  code = c(
    "563020.SS",
    "515180.SS",
    "512100.SS",
    "159531.SZ",
    "159259.SZ",
    "159967.SZ",
    "588020.SS",
    "562310.SS",
    "562520.SS",
    "159606.SZ",
    "159209.SZ",
    "515960.SS",
    "560500.SS"
  ),
  name = c(
    "红利低波ETF易方达",
    "红利ETF易方达",
    "中证1000ETF南方",
    "中证2000ETF南方",
    "成长ETF易方达",
    "创业板成长ETF华夏",
    "科创成长ETF易方达",
    "沪深300成长ETF银华",
    "中证1000成长ETF华夏",
    "中证500成长ETF易方达",
    "红利质量ETF招商",
    "质量ETF中金",
    "500质量成长ETF鹏扬"
  ),
  stringsAsFactors = FALSE
)

library(eFactorCraft)
dat_style <- get_data(stock_df, start_date = "2020-01-01", end_date = "2026-05-31")

dat_style_with_indicator <- add_risk_adj_mom(dat_style, close_col = "adjusted")

dat_style_with_signal <- add_rank_signal(dat_style_with_indicator, rank_col = "ram_20", top_n = 3)

# 生成权重
dat_style_with_weight <- add_norm_weight(
  df = dat_style_with_signal, # 你的输入数据
  weight_col = "ram_20", # 用什么指标加权（RAM动量）
  signal_col = "signal_ram_20_top3", # 只给选中的3只ETF加权
  norm_method = "softmax" # linear = 等权重（推荐）
)


# ==============================================
# 3. run_backtest_final 函数
# ==============================================
res_style <- run_backtest_final(
  df = dat_style_with_weight,
  weight_col = "weight_ram_20_signal_ram_20_top3",
  init_capital = 100000,
  fee_rate = 0.0003,
  stamp_tax = 0.0005,
  slippage_rate = 0.001,
  lot_size = 100,
  min_weight = 1e-6,
  single_max_weight = 0.95,
  global_max_hold_pct = 1.0,
  skip_suspended = TRUE,
  rebalance_mode = "hybrid",
  rebalance_cycle = "monthly",
  weight_change_threshold = 0.05,
  exec_price_col = "close",
  eval_price_col = "adjusted",
  enable_component_stop_loss = FALSE,
  enable_portfolio_stop_loss = FALSE,
  enable_component_take_profit = FALSE,
  enable_portfolio_take_profit = FALSE,
  output_type = "tibble"
)



##########################################################
####  全球资产趋势轮动 | Global Asset Trend Rotation  ####
##########################################################

# ----------------------------------------------------
# 1. 定义标的池 | Define Universe
# ----------------------------------------------------
# 构建策略标的池：纳斯达克100 / 沪深300 / 黄金ETF
# Define the investment universe: Nasdaq 100, CSI 300, Gold ETF
universe_df <- data.frame(
  code = c("513100.SS", "510300.SS", "518880.SS"),
  name = c("Nasdaq 100 ETF", "CSI 300 ETF", "Gold ETF"),
  stringsAsFactors = FALSE
)

# ----------------------------------------------------
# 2. 下载历史数据 | Download Historical Data
# ----------------------------------------------------
library(eFactorCraft)
# 获取复权行情数据 | Get adjusted price data
global_price_dat <- get_data(
  stock_df    = universe_df,
  start_date  = "2020-01-01",
  end_date    = "2026-05-31"
)

# ----------------------------------------------------
# 3. 计算动量指标 | Calculate Momentum Indicator
# ----------------------------------------------------
library(eClassic)
# 计算20日价格动量 | Compute 20-day price momentum
global_price_dat <- add_mom(
  data       = global_price_dat,
  close_col  = "adjusted",
  n          = c(20)
)

# ----------------------------------------------------
# 4. 生成交易信号
# 4. Generate Trading Signals
# ----------------------------------------------------
library(eBacktestCraft)
# 信号规则：动量大于0时做多
# Rule: long when momentum > 0
global_price_dat <- add_signal(
  df             = global_price_dat,
  indicator_cols = "mom_20",
  signal_type    = "threshold",
  threshold      = 0,
  compare_op     = ">",
  signal_name    = "signal_mom20_gt_0"
)

# ----------------------------------------------------
# 5. 计算标准化权重 | Compute Normalized Weights
# ----------------------------------------------------
# 根据动量大小线性加权，只对有效信号赋值权重
# Linear-weight assets by momentum, only for valid signals
global_price_dat <- add_norm_weight(
  df          = global_price_dat,
  weight_col  = "mom_20",
  signal_col  = "signal_mom20_gt_0",
  norm_method = "linear",
  weight_name = "weight_final",
  zero_na     = TRUE,
  output_type = "tibble"
)

# ----------------------------------------------------
# 6. 运行回测 Run Backtest
# ----------------------------------------------------
bt_result <- backtest(
  df = global_price_dat,
  weight_col = "weight_final", # 目标权重列 | Target weight column
  start_date = "2020-01-01", # 回测开始日 | Backtest start date
  end_date = "2026-05-31", # 回测结束日 | Backtest end date
  exec_price_col = "open", # 成交价格：开盘价 | Execution price: open
  eval_price_col = "adjusted", # 估值价格：复权价 | Valuation price: adjusted
  init_capital = 100000, # 初始资金 | Initial capital

  # 调仓规则 | Rebalancing Rules
  rebalance_mode = "hybrid", # 混合模式：日历+漂移 | Hybrid: calendar + drift
  rebalance_cycle = 10, # 每10个交易日调仓 | Rebalance every 10 trading days
  weight_change_threshold = 0.03, # 权重漂移>3%强制调仓 | Rebalance if drift >3%

  # 交易成本 | Transaction Costs
  fee_rate = 0.0003, # 手续费 | Commission fee
  stamp_tax = 0.001, # 印花税 | Stamp duty
  slippage_rate = 0.001, # 滑点 | Slippage
  lot_size = 100, # 交易单位 | Trading lot size

  # 止损止盈（关闭）| Stop-loss / Take-profit (disabled)
  enable_component_stop_loss = FALSE,
  enable_portfolio_stop_loss = FALSE,
  enable_component_take_profit = FALSE,
  enable_portfolio_take_profit = FALSE,

  # 风险约束 | Risk Constraints
  single_max_weight = 0.5, # 单个标的最大权重 | Max weight per asset
  global_max_hold_pct = 1.0, # 整体最大仓位 | Max total exposure
  output_type = "tibble" # 输出格式 | Output format
)




usethis::use_r("engine") # 生成 R/engine.R
usethis::use_r("config") # 生成 R/config.R
usethis::use_r("run") # 生成 R/run.R

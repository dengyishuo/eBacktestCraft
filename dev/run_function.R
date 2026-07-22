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

dat_with_signal <- add_signal(dat, type = "constant")

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
# 1. 定义ETF标的池 | Define ETF Universe
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
bt_result <- run_backtest(
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





# ============================================================
# 沪深300 相关 ETF：120 日动量因子 IC 分析
# 检验 A股 ETF 截面是否存在动量效应或反转效应
# ============================================================
library(eFactorCraft)
library(eClassic)
library(dplyr)

# ── 0. 定义 ETF 截面 ──────────────────────────────────────────
universe <- data.frame(
  code = c(
    "510300.SS", "510050.SS", "510500.SS", "159915.SZ",
    "512010.SS", "512880.SS", "512200.SS", "515050.SS",
    "512400.SS", "512660.SS", "515000.SS", "159869.SZ",
    "516160.SS", "516950.SS", "512800.SS", "512690.SS",
    "159741.SZ", "512170.SS", "516110.SS", "159745.SZ",
    "515220.SS", "562500.SS"
  ),
  name = c(
    "沪深300ETF", "上证50ETF", "中证500ETF", "创业板ETF",
    "医疗ETF", "证券ETF", "房地产ETF", "消费ETF",
    "有色金属ETF", "国防军工ETF", "科技ETF", "粮食ETF",
    "新能源车ETF", "光伏ETF", "银行ETF", "酒ETF",
    "半导体ETF", "医疗器械ETF", "碳中和ETF", "创新药ETF",
    "煤炭ETF", "中证A50ETF"
  ),
  stringsAsFactors = FALSE
)

# ── 1. 拉取数据 ──────────────────────────────────────────────
cat("正在下载", nrow(universe), "只ETF数据...\n")
raw <- get_data(
  universe   = universe,
  start_date = "2018-01-01",
  end_date   = as.character(Sys.Date()),
  output     = "tibble"
)
cat("下载完成，共", nrow(raw), "行，", length(unique(raw$code)), "只ETF\n")

# ── 2. 计算日收益率 ──────────────────────────────────────────
df <- raw |>
  arrange(code, date) |>
  group_by(code) |>
  mutate(ret = adjusted / lag(adjusted) - 1) |>
  ungroup()

# ── 3. 计算 120 日动量（改用 eClassic::add_mom） ─────────────
# [变更1] 替换手动 mutate，使用 add_mom；
#   close_col = "adjusted"（get_data 输出复权价列名）
#   type = "discrete" 与原脚本 adjusted/lag(adjusted,121)-1 对齐
df <- df |>
  eClassic::add_mom(close_col = "adjusted", n = 120, type = "discrete")

cat("因子有效行数：", sum(!is.na(df$mom_120)), "\n")

# ── 4. 构造未来收益列（命名为 forward_ 前缀，供 plot_quantile 直接使用）
df <- df |>
  add_next_return(close_col = "adjusted", n = c(5, 20, 60), new_col = "forward")

# ── 5. 因子预处理 ────────────────────────────────────────────
df <- df |>
  add_winsorize(factor_cols = "mom_120", probs = c(0.05, 0.95)) |>
  add_standardize(factor_cols = "win_mom_120")

# ── 6. IC 分析 ───────────────────────────────────────────────
cat("\n======= IC 分析（截面 Pearson 相关）=======\n")
ic <- ic_analysis(
  df,
  factor_cols  = "win_mom_120",
  forward_cols = c("forward_5", "forward_20", "forward_60")
)
ir <- ir_analysis(ic)
print(ir)

# ── 7. IC 时序统计与折线图 ───────────────────────────────────
cat("\n======= IC 时序统计 =======\n")
for (fwd in c("forward_5", "forward_20", "forward_60")) {
  ic_ts <- ic$win_mom_120 |> filter(forward == fwd)
  cat(sprintf(
    "%-12s | 均值 IC = %+.4f | 胜率 = %.1f%% | |IC| > 0.1 比例 = %.1f%%\n",
    fwd,
    mean(ic_ts$ic, na.rm = TRUE),
    mean(ic_ts$ic > 0, na.rm = TRUE) * 100,
    mean(abs(ic_ts$ic) > 0.1, na.rm = TRUE) * 100
  ))
}

if (requireNamespace("ggplot2", quietly = TRUE)) {
  p_ic <- ggplot2::ggplot(
    ic$win_mom_120,
    ggplot2::aes(x = date, y = ic, color = forward)
  ) +
    ggplot2::geom_line(alpha = 0.6, linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    ggplot2::geom_smooth(method = "loess", span = 0.15, se = FALSE, linewidth = 1) +
    ggplot2::facet_wrap(~forward, ncol = 1, scales = "free_y") +
    ggplot2::scale_color_manual(
      values = c("forward_5" = "#E63946", "forward_20" = "#457B9D", "forward_60" = "#2A9D8F")
    ) +
    ggplot2::labs(
      title = "120日动量因子截面 IC 时序",
      subtitle = "正值：动量效应；负值：反转效应",
      x = "日期", y = "IC（Pearson相关系数）", color = "预测期"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "none")
  print(p_ic)
}

# ── 8. Q3 分层回测（ETF 截面小，n_groups=3） ─────────────────
# [变更2] 三处优化：
#   a) 一次传入全部三个预测期，无需多次调用
#   b) 删去手动 filter(！is.na)——quantile_analysis 内部已按列过滤
#   c) forward_ 前缀与 plot_quantile 直接兼容，可调用内置出图函数
cat("\n======= Q3 分层回测 =======\n")
qa <- quantile_analysis(
  df,
  factor_cols  = "win_mom_120",
  forward_cols = c("forward_5", "forward_20", "forward_60"),
  n_groups     = 3
)

print(qa$win_mom_120$quantile_returns)
cat("多空价差（Q3 - Q1）：\n")
print(qa$win_mom_120$long_short_spread)

if (requireNamespace("ggplot2", quietly = TRUE)) {
  p_q <- plot_quantile(
    qa$win_mom_120$quantile_returns,
    plot_type = "heatmap",
    title     = "120日动量 Q3 分层收益热力图"
  )
  print(p_q)
}

# ── 9. 效应判断 ──────────────────────────────────────────────
cat("\n======= 效应判断 =======\n")
judge <- function(ic_val, period) {
  if (abs(ic_val) < 0.02) {
    cat(sprintf("%s：无显著效应（IC ≈ 0）\n", period))
  } else if (ic_val > 0) {
    cat(sprintf("%s：存在动量效应（IC = %+.4f）\n", period, ic_val))
  } else {
    cat(sprintf("%s：存在反转效应（IC = %+.4f）\n", period, ic_val))
  }
}

ic_tbl <- ic$win_mom_120
judge(mean(ic_tbl$ic[ic_tbl$forward == "forward_5"], na.rm = TRUE), "短期（forward_5） ")
judge(mean(ic_tbl$ic[ic_tbl$forward == "forward_20"], na.rm = TRUE), "中期（forward_20）")
judge(mean(ic_tbl$ic[ic_tbl$forward == "forward_60"], na.rm = TRUE), "长期（forward_60）")

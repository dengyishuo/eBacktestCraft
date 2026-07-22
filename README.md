# eBacktestCraft

> eQuant-R 策略层 — 专业量化策略回测框架

eBacktestCraft is a professional backtesting framework for quantitative strategies, designed to work seamlessly with the eQuant-R ecosystem. It supports multiple rebalance modes, advanced stop-loss mechanisms, portfolio constraints, transaction costs, and comprehensive performance analytics with professional visualization tools.

---

## 在 eQuant 生态中的角色

| | |
|---|---|
| **层级** | 策略层（回测引擎 + 绩效分析） |
| **上游依赖** | eTTR, eClassic, eAlpha101（因子信号源，可选）；eFactorCraft（因子工程输出，推荐） |
| **下游调用** | 无（终端用户直接使用） |
| **生态定位** | 全链路的终端环节。消费因子层和因子工程层的输出，提供事件驱动回测引擎，支持日历/阈值调仓、固定/移动/混合止损、组合约束、交易成本模型，输出绩效评估（年化收益、Sharpe、最大回撤）和专业图表 |

```
eBacktestCraft (策略层: 回测引擎)
  ├── 上游输入: eFactorCraft (处理后因子) / eTTR + eClassic + eAlpha101 (原始因子)
  ├── 核心引擎:
  │   ├── add_signal()  → 信号生成 (quantile / threshold)
  │   ├── add_weight()  → 权重分配 (equal / marketcap / custom)
  │   ├── run()         → 事件驱动回测
  │   └── param_scan()  → 参数网格扫描
  ├── 绩效分析:
  │   ├── benchmark()   → 基准对比
  │   └── plot_*()      → 可视化报告
  └── 终端用户: 策略研究 / 参数优化 / 绩效归因
```

---

## Installation

```r
# GitHub 安装
devtools::install_github("dengyishuo/eBacktestCraft")

# 推荐使用 pak
if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
pak::pak("dengyishuo/eBacktestCraft")
```

## Quick Start

```r
library(eBacktestCraft)
library(eClassic)
library(dplyr)

# 加载数据 + 使用统一路由添加因子
data(style, package = "eBacktestCraft")
df <- style |>
  add_indicator("mom", close_col = "adjusted", n = c(20, 60)) |>
  add_indicator("volatility", close_col = "adjusted", n = 20)

# 配置回测（使用 set_* 链式调用）
config <- default_backtest_config() |>
  set_init_capital(1000000) |>
  set_date_range("2023-01-01", "2024-12-31") |>
  set_rebalancing(mode = "calendar", cycle = "monthly")

# 生成信号和权重（umbrella 函数，type 分发）
sig <- add_signal(df, type = "quantile",
                  indicator_cols = "mom_20", top_n = 10)
wt  <- add_weight(sig, type = "equal")

# 运行回测
result <- run_backtest(df, config, wt)
```

## 统一指标路由 — `add_indicator()`

`add_indicator` 是跨包的统一指标路由函数，根据 short name 自动分发到正确的底层包：

```r
# 统一接口，一行调用任意指标
df <- add_indicator(df, "rsi",          close_col = "close", n = 14)    # → eTTR::add_rsi
df <- add_indicator(df, "macd",         close_col = "close")            # → eTTR::add_macd
df <- add_indicator(df, "mom",          close_col = "close", n = 20)    # → eClassic::add_mom
df <- add_indicator(df, "alpha001",     close_col = "close")            # → eAlpha101::add_alpha001
df <- add_indicator(df, "csp_doji",     output = "tibble")              # → eCandleSticks::add_csp_doji

# 浏览全部 230+ 可路由指标
list_indicators()           # 全部
list_indicators("eClassic") # 只看经典因子
```

| 来源包 | 指标数 | short name 示例 |
|--------|--------|-----------------|
| eTTR | 58 | sma, ema, rsi, macd, bbands, atr, kdj, obv, sar, stoch... |
| eClassic | 13 | mom, beta, ram, size, value, return, rps, slope... |
| eAlpha101 | 100 | alpha001 ~ alpha101 |
| eCandleSticks | 63 | csp_doji, csp_engulfing, csp_hammer, csp_star... |

**名称冲突处理**：`sma` / `volatility` 默认路由到 eTTR。使用 `"eClassic.sma"` 显式路由到 eClassic。

## 核心功能

| 功能 | 函数 | 说明 |
|------|------|------|
| 信号生成 | `add_signal()` | 分位数/阈值/排名信号 |
| 权重分配 | `add_weight()` | 等权/市值加权/自定义 |
| 回测引擎 | `run()` | 事件驱动，支持多调仓模式 |
| 参数扫描 | `param_scan()` | 网格搜索最优参数组合 |
| 基准对比 | `benchmark()` | 与基准指数绩效对比 |
| 绩效分析 | 内置于 `run()` | 年化收益 / Sharpe / 最大回撤 / 胜率 |

## 策略配置 — `set_*` 函数

eBacktestCraft 提供了一套链式 `set_*` 函数来配置回测参数。工作流为：
`default_backtest_config()` → `set_*` 链式调用 → 传入 `run()`。

### 使用示例

```r
library(eBacktestCraft)

# 获取默认配置 + 链式覆盖
config <- default_backtest_config() |>
  set_init_capital(1000000) |>
  set_date_range("2023-01-01", "2024-12-31") |>
  set_rebalancing(mode = "calendar", cycle = "monthly") |>
  set_transaction_costs(fee_rate = 0.0003, stamp_tax = 0.001, slippage_rate = 0.001) |>
  set_weight_constraints(single_max_weight = 0.1, global_max_hold_pct = 0.95) |>
  set_component_stop_loss(enable = TRUE, type = "trailing_fixed", trailing_fixed_ratio = 0.10) |>
  set_oco_component(sl_type = "trailing_fixed", tp_type = "fixed", sl_ratio = 0.10, tp_ratio = 0.15)

# 传入回测引擎
result <- run_backtest(df, config, wt)
```

### 复合 setter（推荐，一次设置一组相关参数）

| 函数 | 用途 | 主要参数 |
|------|------|---------|
| `set_date_range()` | 回测日期范围 | `start_date`, `end_date` |
| `set_transaction_costs()` | 交易成本 | `fee_rate`, `stamp_tax`, `slippage_rate`, `lot_size` |
| `set_weight_constraints()` | 权重约束 | `single_max_weight`, `global_max_hold_pct`, `min_weight` |
| `set_rebalancing()` | 调仓参数 | `mode` (`"calendar"`/`"weight_shift"`/`"hybrid"`), `cycle`, `weight_threshold` |
| `set_component_stop_loss()` | 个股止损 | `enable`, `type`, `fixed_ratio`, `trailing_fixed_ratio`, `atr_n`, `atr_k`, `vol_n`, `vol_sigma` |
| `set_portfolio_stop_loss()` | 组合止损 | 同上（portfolio 级别） |
| `set_component_take_profit()` | 个股止盈 | `enable`, `type`, `fixed_ratio`, `trailing_fixed_ratio`, `atr_k`, `vol_sigma` |
| `set_portfolio_take_profit()` | 组合止盈 | 同上（portfolio 级别） |
| `set_stop_limit_gap()` | 限价成交缺口 | `component_sl_gap`, `component_tp_gap`, `portfolio_sl_gap`, `portfolio_tp_gap` |
| `set_oco_component()` | 个股 OCO 双向挂单 | `sl_type`, `tp_type`, `sl_ratio`, `tp_ratio`, `sl_gap`, `tp_gap` |
| `set_oco_portfolio()` | 组合 OCO 双向挂单 | 同上（portfolio 级别） |

### 单参数 setter（精细控制）

| 类别 | 函数 | 默认值 | 说明 |
|------|------|--------|------|
| **基础** | `set_init_capital()` | `100000` | 初始资金 |
| | `set_weight_col()` | `"weight"` | 权重列名 |
| | `set_output_type()` | `"tibble"` | 输出格式 |
| **价格列** | `set_exec_price_col()` | `"open"` | 成交价格列 |
| | `set_eval_price_col()` | `"adjusted"` | 估值价格列 |
| **交易成本** | `set_fee_rate()` | `0.0003` | 佣金费率 |
| | `set_stamp_tax()` | `0.001` | 印花税（卖出单向） |
| | `set_slippage_rate()` | `0.001` | 滑点率 |
| | `set_lot_size()` | `100` | 最小交易手数 |
| **权重约束** | `set_single_max_weight()` | `0.95` | 单资产最大权重 |
| | `set_global_max_hold_pct()` | `1.0` | 全局最大持仓比例 |
| | `set_min_weight()` | `1e-6` | 最小有效权重 |
| **调仓** | `set_rebalance_mode()` | `"calendar"` | 调仓模式 |
| | `set_rebalance_cycle()` | `"quarterly"` | 调仓周期 |
| | `set_weight_change_threshold()` | `0.01` | 权重偏离阈值 |
| | `set_skip_suspended()` | `TRUE` | 跳过停牌股票 |
| **个股止损** | `set_enable_component_stop_loss()` | `FALSE` | 启用开关 |
| | `set_component_stop_loss_type()` | `"fixed"` | 止损类型：`"fixed"`, `"trailing_fixed"`, `"trailing_atr"`, `"trailing_vol"`, `"trailing_log"`, `"stop_limit"` |
| | `set_fixed_component_sl_ratio()` | `0.1` | 固定止损比例 |
| | `set_trailing_fixed_component_sl_ratio()` | `0.1` | 移动止损回撤比例 |
| | `set_atr_n_component()` / `set_atr_k_component()` | `14` / `2.0` | ATR 止损参数 |
| | `set_vol_n_component()` / `set_vol_sigma_component()` | `20` / `2.0` | 波动率止损参数 |
| | `set_log_vol_n_component()` / `set_log_vol_sigma_component()` | `20` / `2.0` | 对数波动率止损参数 |
| **组合止损** | `set_enable_portfolio_stop_loss()` | `FALSE` | 启用开关 |
| | `set_portfolio_stop_loss_type()` | `"fixed"` | 类型（同上） |
| | `set_fixed_portfolio_sl_ratio()` / `set_trailing_fixed_portfolio_sl_ratio()` | `0.1` | 比例参数 |
| | `set_atr_n_portfolio()` / `set_atr_k_portfolio()` | `14` / `2.0` | ATR 参数 |
| | `set_vol_n_portfolio()` / `set_vol_sigma_portfolio()` | `20` / `2.0` | 波动率参数 |
| | `set_log_vol_n_portfolio()` / `set_log_vol_sigma_portfolio()` | `20` / `2.0` | 对数波动率参数 |
| **个股止盈** | `set_enable_component_take_profit()` | `FALSE` | 启用开关 |
| | `set_component_take_profit_type()` | `"fixed"` | 止盈类型 |
| | `set_fixed_component_tp_ratio()` / `set_trailing_fixed_component_tp_ratio()` | `0.1` | 止盈比例 |
| | `set_atr_k_component_tp()` | `2.0` | ATR 止盈 K |
| | `set_vol_sigma_component_tp()` / `set_log_vol_sigma_component_tp()` | `2.0` | 波动率止盈 sigma |
| **组合止盈** | `set_enable_portfolio_take_profit()` | `FALSE` | 启用开关 |
| | `set_portfolio_take_profit_type()` | `"fixed"` | 类型 |
| | `set_fixed_portfolio_tp_ratio()` / `set_trailing_fixed_portfolio_tp_ratio()` | `0.1` | 比例 |
| | `set_atr_k_portfolio_tp()` | `2.0` | ATR 止盈 K |
| | `set_vol_sigma_portfolio_tp()` / `set_log_vol_sigma_portfolio_tp()` | `2.0` | 波动率止盈 sigma |

### 止损/止盈类型说明

| 类型 | 描述 |
|------|------|
| `"fixed"` | 固定比例：价格跌破入场价 × (1 - ratio) 时触发 |
| `"trailing_fixed"` | 移动固定比例：从持仓以来的最高价回撤 ratio 时触发 |
| `"trailing_atr"` | 移动 ATR：最高价 - k × ATR(N) 时触发 |
| `"trailing_vol"` | 移动波动率：最高价 - sigma × vol(N) 时触发 |
| `"trailing_log"` | 移动对数波动率：最高价 - sigma × log_vol(N) 时触发 |
| `"stop_limit"` | 限价止损：触发价下方 gap 范围内下单，防止击穿成交 |

> **OCO (One-Cancels-Other)**：止损与止盈配对挂单，先触发者生效，另一单自动撤销。使用 `set_oco_component()` / `set_oco_portfolio()` 一键配置。

## 相关包

- [eFactorCraft](../eFactorCraft/) — 上游因子工程，提供经预处理和评价的因子
- [eTTR](../eTTR/) — 技术指标因子源
- [eClassic](../eClassic/) — 经典因子源
- [eAlpha101](../eAlpha101/) — Alpha 因子源

## 作者

邓一硕 · GitHub: [dengyishuo](https://github.com/dengyishuo)

---

## 联系我们

| | |
|---|---|
| 公司官网 | [xquant.shop](https://xquant.shop) |
| 公司公众号 | xquant-shop |
| 个人公众号 | i锐角 |

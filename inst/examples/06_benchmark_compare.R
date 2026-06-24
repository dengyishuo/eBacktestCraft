# ============================================================
# 策略 vs 三类基准 对比示例
# ============================================================
#
# 三类基准说明：
#
#   run_benchmark_ew()    等权再平衡基准
#     - weight 列每行写 1/N（引擎只在再平衡触发日读取该列）
#     - 按 config 中的 rebalance_cycle 定期将持仓重置回等权
#     - 用途：衡量因子选股是否胜过"无脑等权"
#
#   run_benchmark_bh()    成分股买入持有基准
#     - weight 列同样写 1/N，但 rebalance_cycle = 99999
#     - 引擎只在第一天触发，之后永不再平衡，持仓随价格自然漂移
#     - 用途：衡量主动再平衡是否创造价值（相对纯持有）
#
#   run_benchmark_index() 大盘指数买入持有基准
#     - 单资产，weight = 1，rebalance_cycle = 99999
#     - 用途：衡量组合相对市场基准的超额收益（Alpha）
#
# ============================================================

library(eBacktestCraft)
library(eFactorCraft)
library(dplyr)

# ── 0. 数据准备 ───────────────────────────────────────────────────────────────

universe <- data.frame(
  code = c(
    "510300.SS", "510050.SS", "510500.SS", "159915.SZ",
    "512010.SS", "512880.SS", "512200.SS", "515050.SS",
    "512400.SS", "512660.SS"
  ),
  name = c(
    "沪深300ETF", "上证50ETF", "中证500ETF", "创业板ETF",
    "医疗ETF", "证券ETF", "房地产ETF", "消费ETF",
    "有色金属ETF", "国防军工ETF"
  ),
  stringsAsFactors = FALSE
)

cat("下载成分股数据...\n")
mkt_data <- eFactorCraft::get_data(
  universe   = universe,
  start_date = "2020-01-01",
  end_date   = as.character(Sys.Date())
)

cat("下载指数数据（沪深300）...\n")
idx_data <- eFactorCraft::get_data(
  universe   = data.frame(code = "000300.SS", name = "沪深300", stringsAsFactors = FALSE),
  start_date = "2020-01-01",
  end_date   = as.character(Sys.Date())
)

# ── 1. 构造主策略权重（示例：120日动量 → 按因子值正比分配权重） ──────────────

mkt_data <- mkt_data |>
  eClassic::add_mom(close_col = "adjusted", n = 120, type = "discrete") |>
  arrange(code, date)

# 截面正则化：正动量资产按比例分配，负动量或 NA 清零
mkt_data <- mkt_data |>
  group_by(date) |>
  mutate(
    mom_pos   = pmax(mom_120, 0),
    weight    = ifelse(sum(mom_pos, na.rm = TRUE) > 0,
                       mom_pos / sum(mom_pos, na.rm = TRUE),
                       0)
  ) |>
  ungroup()

# ── 2. 公共 config（费率、资金与日期与各基准保持一致） ───────────────────────

cfg <- default_backtest_config() |>
  set_init_capital(1000000) |>
  set_fee_rate(0.0003) |>
  set_stamp_tax(0.001) |>
  set_slippage_rate(0.001) |>
  set_exec_price_col("open") |>   # eFactorCraft::get_data 返回小写列名
  set_rebalance_cycle("monthly")   # 主策略和 EW 基准均月度再平衡

# ── 3. 回测主策略 ─────────────────────────────────────────────────────────────

cat("\n回测主策略（动量加权）...\n")
bt_strategy <- run_backtest(cfg, mkt_data)

# ── 4. 三类基准回测 ───────────────────────────────────────────────────────────

cat("回测基准 1：等权月度再平衡...\n")
bt_ew <- run_benchmark_ew(mkt_data, config = cfg)

cat("回测基准 2：成分股买入持有...\n")
bt_bh <- run_benchmark_bh(mkt_data, config = cfg)
# 注：run_benchmark_bh 内部会将 rebalance_cycle 覆盖为 99999，
#     所以 cfg 中的 "monthly" 对 B&H 无效。

cat("回测基准 3：沪深300 买入持有...\n")
bt_idx <- run_benchmark_index(idx_data, config = cfg, lot_size = 100)

# ── 5. 绩效汇总 ───────────────────────────────────────────────────────────────

cat("\n======= 策略绩效 =======\n")
print(performance_analysis(bt_strategy)$metrics)

cat("\n======= 等权再平衡基准 =======\n")
print(performance_analysis(bt_ew)$metrics)

cat("\n======= 成分股B&H基准 =======\n")
print(performance_analysis(bt_bh)$metrics)

cat("\n======= 沪深300 B&H基准 =======\n")
print(performance_analysis(bt_idx)$metrics)

# ── 6. 合并净值曲线 ───────────────────────────────────────────────────────────

cmp <- compare_benchmarks(
  bt_strategy, bt_ew, bt_bh, bt_idx,
  names = c("动量加权策略", "等权再平衡", "成分股B&H", "沪深300 B&H")
)

cat("\n净值对比（前5行）：\n")
print(head(cmp))

# ── 7. 对比图 ─────────────────────────────────────────────────────────────────

# 上图：四条净值曲线
# 下图：策略相对沪深300的超额净值
p <- plot_benchmark_compare(
  cmp,
  show_excess = TRUE,
  base_series = "沪深300 B&H",
  title       = "动量加权策略 vs 三类基准"
)
print(p)

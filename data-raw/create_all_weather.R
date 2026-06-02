# 文件：data-raw/create_all_weather.R
# 目的：从 FactorCraft 获取原始 OHLC 数据，保存为包内置数据集 all_weather
# 运行方式：在包根目录下执行 Rscript data-raw/create_all_weather.R
# 或 source("data-raw/create_all_weather.R")

library(FactorCraft)

# ==============================================
# 1. 定义要获取的资产代码（8只ETF）及名称
# ==============================================
etf_data <- data.frame(
  code = c(
    "510300.SS", "512100.SS", "512890.SS",
    "511130.SS", "511260.SS", "511010.SS",
    "518880.SS", "510170.SS"
  ),
  name = c(
    "沪深300ETF", "中证1000", "红利低波",
    "30年期国债", "10年期国债", "5年期国债",
    "黄金ETF", "商品ETF"
  ),
  stringsAsFactors = FALSE
)

# ==============================================
# 2. 获取原始 OHLC 数据（不添加任何信号和权重）
# ==============================================
cat("正在从 FactorCraft 获取原始 OHLC 数据...\n")
all_weather <- get_data(etf_data, start_date = "2020-01-01", end_date = "2026-05-01")
cat("数据获取完成，共", nrow(all_weather), "行。\n")

# ==============================================
# 3. 保存为内置数据
# ==============================================
# 确保 data/ 目录存在
if (!dir.exists("data")) dir.create("data")

# 保存数据（使用 xz 压缩以减小体积）
usethis::use_data(all_weather, overwrite = TRUE, compress = "xz")

cat("内置数据已保存至 data/all_weather.rda\n")

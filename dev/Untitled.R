# =====================================================================
# Advanced Multi-Mode Quantitative Backtesting System
# Configuration-driven interface + original optimized engine
# =====================================================================

# ------------------------------ 依赖包 ---------------------------------
# 确保已安装：dplyr, lubridate, zoo, tibble, rlang
# 若未安装，取消注释以下行：
# install.packages(c("dplyr", "lubridate", "zoo", "tibble", "rlang"))

# ------------------------------ 1. 默认配置 ----------------------------
#' Create default backtest configuration
#'
#' @return A list of default parameters for \code{run_backtest}.
#' @export
default_backtest_config <- function() {
  list(
    weight_col = "weight",
    start_date = NULL,
    end_date = NULL,
    exec_price_col = "Open",
    eval_price_col = "adjusted",
    init_capital = 100000,
    lot_size = 100,
    fee_rate = 0.0003,
    stamp_tax = 0.001,
    slippage_rate = 0.001,
    min_weight = 1e-6,
    enable_component_stop_loss = FALSE,
    component_stop_loss_type = "fixed",
    fixed_component_sl_ratio = 0.1,
    trailing_fixed_component_sl_ratio = 0.1,
    atr_n_component = 14,
    atr_k_component = 2.0,
    vol_n_component = 20,
    vol_sigma_component = 2.0,
    log_vol_n_component = 20,
    log_vol_sigma_component = 2.0,
    enable_portfolio_stop_loss = FALSE,
    portfolio_stop_loss_type = "fixed",
    fixed_portfolio_sl_ratio = 0.1,
    trailing_fixed_portfolio_sl_ratio = 0.1,
    atr_n_portfolio = 14,
    atr_k_portfolio = 2.0,
    vol_n_portfolio = 20,
    vol_sigma_portfolio = 2.0,
    log_vol_n_portfolio = 20,
    log_vol_sigma_portfolio = 2.0,
    enable_component_take_profit = FALSE,
    component_take_profit_type = "fixed",
    fixed_component_tp_ratio = 0.1,
    trailing_fixed_component_tp_ratio = 0.1,
    atr_k_component_tp = 2.0,
    vol_sigma_component_tp = 2.0,
    log_vol_sigma_component_tp = 2.0,
    enable_portfolio_take_profit = FALSE,
    portfolio_take_profit_type = "fixed",
    fixed_portfolio_tp_ratio = 0.1,
    trailing_fixed_portfolio_tp_ratio = 0.1,
    atr_k_portfolio_tp = 2.0,
    vol_sigma_portfolio_tp = 2.0,
    log_vol_sigma_portfolio_tp = 2.0,
    single_max_weight = 0.95,
    global_max_hold_pct = 1.0,
    rebalance_mode = "calendar",
    rebalance_cycle = "quarterly",
    weight_change_threshold = 0.01,
    skip_suspended = TRUE,
    output_type = "tibble"
  )
}

# ------------------------------ 2. 细粒度 setter（每个参数一个） --------------------
set_weight_col <- function(config, value) {
  config$weight_col <- value
  config
}
set_start_date <- function(config, value) {
  config$start_date <- value
  config
}
set_end_date <- function(config, value) {
  config$end_date <- value
  config
}
set_exec_price_col <- function(config, value) {
  config$exec_price_col <- value
  config
}
set_eval_price_col <- function(config, value) {
  config$eval_price_col <- value
  config
}
set_init_capital <- function(config, value) {
  config$init_capital <- value
  config
}
set_lot_size <- function(config, value) {
  config$lot_size <- value
  config
}
set_fee_rate <- function(config, value) {
  config$fee_rate <- value
  config
}
set_stamp_tax <- function(config, value) {
  config$stamp_tax <- value
  config
}
set_slippage_rate <- function(config, value) {
  config$slippage_rate <- value
  config
}
set_min_weight <- function(config, value) {
  config$min_weight <- value
  config
}

# component stop-loss
set_enable_component_stop_loss <- function(config, value) {
  config$enable_component_stop_loss <- value
  config
}
set_component_stop_loss_type <- function(config, value) {
  config$component_stop_loss_type <- value
  config
}
set_fixed_component_sl_ratio <- function(config, value) {
  config$fixed_component_sl_ratio <- value
  config
}
set_trailing_fixed_component_sl_ratio <- function(config, value) {
  config$trailing_fixed_component_sl_ratio <- value
  config
}
set_atr_n_component <- function(config, value) {
  config$atr_n_component <- value
  config
}
set_atr_k_component <- function(config, value) {
  config$atr_k_component <- value
  config
}
set_vol_n_component <- function(config, value) {
  config$vol_n_component <- value
  config
}
set_vol_sigma_component <- function(config, value) {
  config$vol_sigma_component <- value
  config
}
set_log_vol_n_component <- function(config, value) {
  config$log_vol_n_component <- value
  config
}
set_log_vol_sigma_component <- function(config, value) {
  config$log_vol_sigma_component <- value
  config
}

# portfolio stop-loss
set_enable_portfolio_stop_loss <- function(config, value) {
  config$enable_portfolio_stop_loss <- value
  config
}
set_portfolio_stop_loss_type <- function(config, value) {
  config$portfolio_stop_loss_type <- value
  config
}
set_fixed_portfolio_sl_ratio <- function(config, value) {
  config$fixed_portfolio_sl_ratio <- value
  config
}
set_trailing_fixed_portfolio_sl_ratio <- function(config, value) {
  config$trailing_fixed_portfolio_sl_ratio <- value
  config
}
set_atr_n_portfolio <- function(config, value) {
  config$atr_n_portfolio <- value
  config
}
set_atr_k_portfolio <- function(config, value) {
  config$atr_k_portfolio <- value
  config
}
set_vol_n_portfolio <- function(config, value) {
  config$vol_n_portfolio <- value
  config
}
set_vol_sigma_portfolio <- function(config, value) {
  config$vol_sigma_portfolio <- value
  config
}
set_log_vol_n_portfolio <- function(config, value) {
  config$log_vol_n_portfolio <- value
  config
}
set_log_vol_sigma_portfolio <- function(config, value) {
  config$log_vol_sigma_portfolio <- value
  config
}

# component take-profit
set_enable_component_take_profit <- function(config, value) {
  config$enable_component_take_profit <- value
  config
}
set_component_take_profit_type <- function(config, value) {
  config$component_take_profit_type <- value
  config
}
set_fixed_component_tp_ratio <- function(config, value) {
  config$fixed_component_tp_ratio <- value
  config
}
set_trailing_fixed_component_tp_ratio <- function(config, value) {
  config$trailing_fixed_component_tp_ratio <- value
  config
}
set_atr_k_component_tp <- function(config, value) {
  config$atr_k_component_tp <- value
  config
}
set_vol_sigma_component_tp <- function(config, value) {
  config$vol_sigma_component_tp <- value
  config
}
set_log_vol_sigma_component_tp <- function(config, value) {
  config$log_vol_sigma_component_tp <- value
  config
}

# portfolio take-profit
set_enable_portfolio_take_profit <- function(config, value) {
  config$enable_portfolio_take_profit <- value
  config
}
set_portfolio_take_profit_type <- function(config, value) {
  config$portfolio_take_profit_type <- value
  config
}
set_fixed_portfolio_tp_ratio <- function(config, value) {
  config$fixed_portfolio_tp_ratio <- value
  config
}
set_trailing_fixed_portfolio_tp_ratio <- function(config, value) {
  config$trailing_fixed_portfolio_tp_ratio <- value
  config
}
set_atr_k_portfolio_tp <- function(config, value) {
  config$atr_k_portfolio_tp <- value
  config
}
set_vol_sigma_portfolio_tp <- function(config, value) {
  config$vol_sigma_portfolio_tp <- value
  config
}
set_log_vol_sigma_portfolio_tp <- function(config, value) {
  config$log_vol_sigma_portfolio_tp <- value
  config
}

# constraints
set_single_max_weight <- function(config, value) {
  config$single_max_weight <- value
  config
}
set_global_max_hold_pct <- function(config, value) {
  config$global_max_hold_pct <- value
  config
}

# rebalancing
set_rebalance_mode <- function(config, value) {
  config$rebalance_mode <- value
  config
}
set_rebalance_cycle <- function(config, value) {
  config$rebalance_cycle <- value
  config
}
set_weight_change_threshold <- function(config, value) {
  config$weight_change_threshold <- value
  config
}

# others
set_skip_suspended <- function(config, value) {
  config$skip_suspended <- value
  config
}
set_output_type <- function(config, value) {
  config$output_type <- value
  config
}

# ------------------------------ 3. 复合 setter（一次设置一组参数） ----------------

#' Set component stop-loss parameters
set_component_stop_loss <- function(config,
                                    enable = TRUE,
                                    type = "fixed",
                                    fixed_ratio = 0.1,
                                    trailing_fixed_ratio = 0.1,
                                    atr_n = 14,
                                    atr_k = 2.0,
                                    vol_n = 20,
                                    vol_sigma = 2.0,
                                    log_vol_n = 20,
                                    log_vol_sigma = 2.0) {
  config$enable_component_stop_loss <- enable
  if (enable) {
    config$component_stop_loss_type <- type
    config$fixed_component_sl_ratio <- fixed_ratio
    config$trailing_fixed_component_sl_ratio <- trailing_fixed_ratio
    config$atr_n_component <- atr_n
    config$atr_k_component <- atr_k
    config$vol_n_component <- vol_n
    config$vol_sigma_component <- vol_sigma
    config$log_vol_n_component <- log_vol_n
    config$log_vol_sigma_component <- log_vol_sigma
  }
  config
}

#' Set portfolio stop-loss parameters
set_portfolio_stop_loss <- function(config,
                                    enable = TRUE,
                                    type = "fixed",
                                    fixed_ratio = 0.1,
                                    trailing_fixed_ratio = 0.1,
                                    atr_n = 14,
                                    atr_k = 2.0,
                                    vol_n = 20,
                                    vol_sigma = 2.0,
                                    log_vol_n = 20,
                                    log_vol_sigma = 2.0) {
  config$enable_portfolio_stop_loss <- enable
  if (enable) {
    config$portfolio_stop_loss_type <- type
    config$fixed_portfolio_sl_ratio <- fixed_ratio
    config$trailing_fixed_portfolio_sl_ratio <- trailing_fixed_ratio
    config$atr_n_portfolio <- atr_n
    config$atr_k_portfolio <- atr_k
    config$vol_n_portfolio <- vol_n
    config$vol_sigma_portfolio <- vol_sigma
    config$log_vol_n_portfolio <- log_vol_n
    config$log_vol_sigma_portfolio <- log_vol_sigma
  }
  config
}

#' Set component take-profit parameters
set_component_take_profit <- function(config,
                                      enable = TRUE,
                                      type = "fixed",
                                      fixed_ratio = 0.1,
                                      trailing_fixed_ratio = 0.1,
                                      atr_k = 2.0,
                                      vol_sigma = 2.0,
                                      log_vol_sigma = 2.0) {
  config$enable_component_take_profit <- enable
  if (enable) {
    config$component_take_profit_type <- type
    config$fixed_component_tp_ratio <- fixed_ratio
    config$trailing_fixed_component_tp_ratio <- trailing_fixed_ratio
    config$atr_k_component_tp <- atr_k
    config$vol_sigma_component_tp <- vol_sigma
    config$log_vol_sigma_component_tp <- log_vol_sigma
  }
  config
}

#' Set portfolio take-profit parameters
set_portfolio_take_profit <- function(config,
                                      enable = TRUE,
                                      type = "fixed",
                                      fixed_ratio = 0.1,
                                      trailing_fixed_ratio = 0.1,
                                      atr_k = 2.0,
                                      vol_sigma = 2.0,
                                      log_vol_sigma = 2.0) {
  config$enable_portfolio_take_profit <- enable
  if (enable) {
    config$portfolio_take_profit_type <- type
    config$fixed_portfolio_tp_ratio <- fixed_ratio
    config$trailing_fixed_portfolio_tp_ratio <- trailing_fixed_ratio
    config$atr_k_portfolio_tp <- atr_k
    config$vol_sigma_portfolio_tp <- vol_sigma
    config$log_vol_sigma_portfolio_tp <- log_vol_sigma
  }
  config
}

#' Set transaction costs (commission, stamp tax, slippage, lot size)
set_transaction_costs <- function(config,
                                  fee_rate = 0.0003,
                                  stamp_tax = 0.001,
                                  slippage_rate = 0.001,
                                  lot_size = 100) {
  config$fee_rate <- fee_rate
  config$stamp_tax <- stamp_tax
  config$slippage_rate <- slippage_rate
  config$lot_size <- lot_size
  config
}

#' Set weight constraints (single asset cap, global exposure, minimum weight)
set_weight_constraints <- function(config,
                                   single_max_weight = 0.95,
                                   global_max_hold_pct = 1.0,
                                   min_weight = 1e-6) {
  config$single_max_weight <- single_max_weight
  config$global_max_hold_pct <- global_max_hold_pct
  config$min_weight <- min_weight
  config
}

#' Set rebalancing parameters (mode, cycle, weight change threshold)
set_rebalancing <- function(config,
                            mode = "calendar",
                            cycle = "quarterly",
                            weight_threshold = 0.01) {
  config$rebalance_mode <- mode
  config$rebalance_cycle <- cycle
  config$weight_change_threshold <- weight_threshold
  config
}

#' Set date range for backtest
set_date_range <- function(config, start_date = NULL, end_date = NULL) {
  if (!is.null(start_date)) config$start_date <- start_date
  if (!is.null(end_date)) config$end_date <- end_date
  config
}

# ------------------------------ 4. 配置驱动运行函数 -----------------------------

#' Run backtest using a configuration list
#'
#' @param config List of parameters (as returned by \code{default_backtest_config}).
#' @param df Data frame with OHLC data.
#' @param ... Additional parameters to override config.
#'
#' @return Same as \code{run_backtest}.
#' @export
run_backtest_from_config <- function(config, df, ...) {
  # Override config with any extra arguments
  args <- modifyList(config, list(...))
  args$df <- df
  do.call(run_backtest, args)
}

# ------------------------------ 5. 原始高性能回测引擎（最终优化版）-----------------
# 注意：下面函数与您提供的完全一致，已包含数字交易日周期支持。
# 确保已加载所需包：dplyr, lubridate, zoo, tibble, rlang
run_backtest <- function(
  df,
  weight_col = "weight",
  start_date = NULL,
  end_date = NULL,
  exec_price_col = "Open",
  eval_price_col = "adjusted",
  init_capital = 100000,
  lot_size = 100,
  fee_rate = 0.0003,
  stamp_tax = 0.001,
  slippage_rate = 0.001,
  min_weight = 1e-6,
  enable_component_stop_loss = FALSE,
  component_stop_loss_type = "fixed",
  fixed_component_sl_ratio = 0.1,
  trailing_fixed_component_sl_ratio = 0.1,
  atr_n_component = 14,
  atr_k_component = 2.0,
  vol_n_component = 20,
  vol_sigma_component = 2.0,
  log_vol_n_component = 20,
  log_vol_sigma_component = 2.0,
  enable_portfolio_stop_loss = FALSE,
  portfolio_stop_loss_type = "fixed",
  fixed_portfolio_sl_ratio = 0.1,
  trailing_fixed_portfolio_sl_ratio = 0.1,
  atr_n_portfolio = 14,
  atr_k_portfolio = 2.0,
  vol_n_portfolio = 20,
  vol_sigma_portfolio = 2.0,
  log_vol_n_portfolio = 20,
  log_vol_sigma_portfolio = 2.0,
  enable_component_take_profit = FALSE,
  component_take_profit_type = "fixed",
  fixed_component_tp_ratio = 0.1,
  trailing_fixed_component_tp_ratio = 0.1,
  atr_k_component_tp = 2.0,
  vol_sigma_component_tp = 2.0,
  log_vol_sigma_component_tp = 2.0,
  enable_portfolio_take_profit = FALSE,
  portfolio_take_profit_type = "fixed",
  fixed_portfolio_tp_ratio = 0.1,
  trailing_fixed_portfolio_tp_ratio = 0.1,
  atr_k_portfolio_tp = 2.0,
  vol_sigma_portfolio_tp = 2.0,
  log_vol_sigma_portfolio_tp = 2.0,
  single_max_weight = 0.95,
  global_max_hold_pct = 1.0,
  rebalance_mode = "calendar",
  rebalance_cycle = "quarterly",
  weight_change_threshold = 0.01,
  skip_suspended = TRUE,
  output_type = "tibble"
) {
  # ==============================================
  # 1. Standardize column names
  # ==============================================
  col_map <- list(
    date = c("Date", "date"),
    code = c("Code", "code"),
    open = c("Open", "open"),
    high = c("High", "high"),
    low = c("Low", "low"),
    close = c("Close", "close"),
    adjusted = c("Adj.Close", "adjusted"),
    volume = c("Volume", "volume")
  )

  data_raw <- df
  for (std_name in names(col_map)) {
    for (alias in col_map[[std_name]]) {
      if (alias %in% colnames(data_raw)) {
        data_raw[[std_name]] <- data_raw[[alias]]
        break
      }
    }
  }

  # ==============================================
  # 2. Validate required columns
  # ==============================================
  required_cols <- c("date", "code", "open", "close", "adjusted", weight_col, exec_price_col)
  missing_cols <- setdiff(required_cols, colnames(data_raw))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # ==============================================
  # 3. Process date range
  # ==============================================
  data_raw$date <- as.Date(data_raw$date)
  if (!is.null(start_date)) {
    start_date <- as.Date(start_date)
    data_raw <- data_raw[data_raw$date >= start_date, ]
  } else {
    start_date <- min(data_raw$date, na.rm = TRUE)
  }
  if (!is.null(end_date)) {
    end_date <- as.Date(end_date)
    data_raw <- data_raw[data_raw$date <= end_date, ]
  } else {
    end_date <- max(data_raw$date, na.rm = TRUE)
  }

  if (nrow(data_raw) == 0) {
    stop("No valid data in the specified date range.")
  }

  # ==============================================
  # 4. Validate and match parameters
  # ==============================================
  output_type <- match.arg(output_type)
  rebalance_mode <- match.arg(rebalance_mode, c("calendar", "weight_shift", "hybrid"))
  component_stop_loss_type <- match.arg(component_stop_loss_type, c("fixed", "trailing_fixed", "trailing_atr", "trailing_vol", "trailing_log"))
  portfolio_stop_loss_type <- match.arg(portfolio_stop_loss_type, c("fixed", "trailing_fixed", "trailing_atr", "trailing_vol", "trailing_log"))
  component_take_profit_type <- match.arg(component_take_profit_type, c("fixed", "trailing_fixed", "trailing_atr", "trailing_vol", "trailing_log"))
  portfolio_take_profit_type <- match.arg(portfolio_take_profit_type, c("fixed", "trailing_fixed", "trailing_atr", "trailing_vol", "trailing_log"))

  # --------------------------
  # 【升级点】支持数字交易日周期
  # --------------------------
  is_numeric_cycle <- is.numeric(rebalance_cycle) && length(rebalance_cycle) == 1 && rebalance_cycle >= 1
  valid_str_cycles <- c("daily", "weekly", "monthly", "quarterly", "semiannual", "annual")
  if (!is_numeric_cycle && !rebalance_cycle %in% valid_str_cycles) {
    stop("rebalance_cycle must be a positive integer or one of: ", paste(valid_str_cycles, collapse = ", "))
  }

  # ==============================================
  # 5. Technical indicator functions
  # ==============================================
  calc_atr <- function(high, low, close, n) {
    tr <- pmax(high - low, abs(high - dplyr::lag(close)), abs(low - dplyr::lag(close)))
    zoo::rollapply(tr, width = n, FUN = mean, fill = NA, align = "right")
  }

  calc_vol <- function(p, n) {
    r <- p / dplyr::lag(p) - 1
    zoo::rollapply(r, n, sd, fill = NA, align = "right")
  }

  calc_log_vol <- function(p, n) {
    r <- log(p / dplyr::lag(p))
    zoo::rollapply(r, n, sd, fill = NA, align = "right")
  }

  # ==============================================
  # 6. Preprocess data and compute indicators
  # ==============================================
  data_processed <- data_raw %>%
    dplyr::arrange(code, date) %>%
    dplyr::group_by(code) %>%
    dplyr::mutate(
      comp_atr    = calc_atr(high, low, close, atr_n_component),
      comp_vol    = calc_vol(close, vol_n_component),
      comp_logvol = calc_log_vol(close, log_vol_n_component)
    ) %>%
    dplyr::mutate(dplyr::across(c("comp_atr", "comp_vol", "comp_logvol"), ~ zoo::na.locf(., na.rm = FALSE))) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(date, code)

  # ==============================================
  # 7. Identify trading dates and rebalance days
  # ==============================================
  trade_dates <- sort(unique(data_processed$date))
  tickers <- unique(data_processed$code)
  nt <- length(tickers)
  nd <- length(trade_dates)

  is_calendar <- rep(FALSE, nd)
  is_weight_shift <- rep(FALSE, nd)
  is_rebalance <- rep(FALSE, nd)

  # Calendar rebalance days
  if (rebalance_mode %in% c("calendar", "hybrid")) {
    if (is_numeric_cycle) {
      # --------------------------
      # 【升级点】数字周期：每 N 个交易日
      # --------------------------
      n_cycle <- as.integer(rebalance_cycle)
      is_calendar <- (seq_along(trade_dates) - 1) %% n_cycle == 0
    } else {
      # --------------------------
      # 原有字符串周期
      # --------------------------
      p <- switch(rebalance_cycle,
        daily = "day",
        weekly = "week",
        monthly = "month",
        quarterly = "quarter",
        semiannual = "halfyear",
        annual = "year"
      )
      d <- tibble::tibble(date = trade_dates) %>%
        dplyr::mutate(g = lubridate::floor_date(date, p)) %>%
        dplyr::group_by(g) %>%
        dplyr::slice(1) %>%
        dplyr::pull(date)
      is_calendar <- trade_dates %in% d
    }
  }

  # Weight shift rebalance days
  if (rebalance_mode %in% c("weight_shift", "hybrid")) {
    wdf <- data_processed %>%
      dplyr::select(date, code, w = dplyr::all_of(weight_col)) %>%
      dplyr::arrange(code, date) %>%
      dplyr::group_by(code) %>%
      dplyr::mutate(
        w_prev = dplyr::lag(w, default = -999),
        drift = abs(w - w_prev),
        trigger = drift > weight_change_threshold
      ) %>%
      dplyr::ungroup()

    wdays <- wdf %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(any_trigger = any(trigger, na.rm = TRUE), .groups = "drop")

    is_weight_shift <- trade_dates %in% wdays$date[wdays$any_trigger]
    is_weight_shift[1] <- TRUE
  }

  # Final rebalance flag
  if (rebalance_mode == "calendar") {
    is_rebalance <- is_calendar
  } else if (rebalance_mode == "weight_shift") {
    is_rebalance <- is_weight_shift
  } else if (rebalance_mode == "hybrid") {
    is_rebalance <- is_calendar | is_weight_shift
  }
  is_rebalance[1] <- TRUE
  total_rebalance_days <- sum(is_rebalance)

  # ==============================================
  # 8. Initialize portfolio state variables
  # ==============================================
  cash <- as.numeric(init_capital)
  positions <- setNames(rep(0, nt), tickers)
  hold_cost <- setNames(rep(0, nt), tickers)
  hold_high_water <- setNames(rep(0, nt), tickers)
  portfolio_high_water <- init_capital
  portfolio_high_water_history <- numeric(nd)

  # Flag for assets stopped in current rebalancing cycle
  stopped_in_cycle <- logical(nt)
  names(stopped_in_cycle) <- tickers

  equity_series <- numeric(nd)
  equity_series[1] <- init_capital
  trade_list <- list()
  position_daily_list <- list()
  account_cash_list <- list()

  day_data_list <- split(data_processed, data_processed$date)

  # ==============================================
  # 9. Main backtesting loop
  # ==============================================
  for (i in seq_along(trade_dates)) {
    today <- trade_dates[i]
    day_data <- day_data_list[[as.character(today)]]
    today_is_rebalance <- is_rebalance[i]

    # Get current market data
    current_exec_price <- setNames(rep(0, nt), tickers)
    current_eval_price <- setNames(rep(0, nt), tickers)
    current_atr <- setNames(rep(0, nt), tickers)
    current_vol <- setNames(rep(0, nt), tickers)
    current_logvol <- setNames(rep(0, nt), tickers)

    if (nrow(day_data) > 0) {
      idx <- match(day_data$code, tickers)
      current_exec_price[idx] <- day_data[[exec_price_col]]
      current_eval_price[idx] <- day_data[[eval_price_col]]
      current_atr[idx] <- day_data$comp_atr
      current_vol[idx] <- day_data$comp_vol
      current_logvol[idx] <- day_data$comp_logvol
    }
    current_exec_price[is.na(current_exec_price)] <- 0
    current_eval_price[is.na(current_eval_price)] <- 0

    # Calculate portfolio value
    hold_mkt_val <- sum(positions * current_eval_price, na.rm = TRUE)
    total_asset <- cash + hold_mkt_val
    equity_series[i] <- total_asset
    portfolio_high_water <- max(portfolio_high_water, total_asset)
    portfolio_high_water_history[i] <- portfolio_high_water

    # ==============================================
    # 9.1 Check component stop-loss
    # ==============================================
    component_stop_sell <- rep(FALSE, nt)
    pf_sl <- FALSE

    if (enable_component_stop_loss) {
      for (j in 1:nt) {
        if (positions[j] <= 0 || current_eval_price[j] <= 0 || hold_cost[j] <= 0) next
        cp <- current_eval_price[j]
        cst <- hold_cost[j]
        ch <- hold_high_water[j]
        hold_high_water[j] <- max(ch, cp)
        trig <- FALSE

        if (component_stop_loss_type == "fixed") {
          line <- cst * (1 - fixed_component_sl_ratio)
          trig <- cp <= line
        } else if (component_stop_loss_type == "trailing_fixed") {
          line <- ch * (1 - trailing_fixed_component_sl_ratio)
          trig <- cp <= line
        } else if (component_stop_loss_type == "trailing_atr") {
          line <- ch - atr_k_component * current_atr[j]
          trig <- !is.na(line) && cp <= line
        } else if (component_stop_loss_type == "trailing_vol") {
          line <- ch - vol_sigma_component * current_vol[j]
          trig <- !is.na(line) && cp <= line
        } else if (component_stop_loss_type == "trailing_log") {
          line <- ch * exp(-log_vol_sigma_component * current_logvol[j])
          trig <- !is.na(line) && cp <= line
        }
        if (trig) component_stop_sell[j] <- TRUE
      }
    }

    # ==============================================
    # 9.2 Check portfolio stop-loss
    # ==============================================
    if (enable_portfolio_stop_loss) {
      v <- total_asset
      hw <- portfolio_high_water
      if (portfolio_stop_loss_type == "fixed") {
        pf_sl <- v <= init_capital * (1 - fixed_portfolio_sl_ratio)
      } else if (portfolio_stop_loss_type == "trailing_fixed") {
        pf_sl <- v <= hw * (1 - trailing_fixed_portfolio_sl_ratio)
      } else if (portfolio_stop_loss_type == "trailing_atr") {
        wnd <- max(1, i - atr_n_portfolio):i
        eq <- equity_series[wnd]
        atrw <- mean(pmax(diff(eq), 0), na.rm = TRUE)
        pf_sl <- !is.na(atrw) && v <= hw - atr_k_portfolio * atrw
      } else if (portfolio_stop_loss_type == "trailing_vol") {
        wnd <- max(1, i - vol_n_portfolio):i
        s <- sd(equity_series[wnd], na.rm = TRUE)
        pf_sl <- !is.na(s) && v <= hw - vol_sigma_portfolio * s
      } else if (portfolio_stop_loss_type == "trailing_log") {
        wnd <- max(1, i - log_vol_n_portfolio):i
        r <- diff(log(equity_series[wnd]))
        lv <- sd(r, na.rm = TRUE)
        pf_sl <- !is.na(lv) && v <= hw * exp(-log_vol_sigma_portfolio * lv)
      }
    }
    if (pf_sl) component_stop_sell[] <- TRUE

    # ==============================================
    # 9.3 Check component take-profit
    # ==============================================
    tp_sell <- rep(FALSE, nt)
    pf_tp <- FALSE

    if (enable_component_take_profit) {
      for (j in 1:nt) {
        if (positions[j] <= 0 || current_eval_price[j] <= 0 || hold_cost[j] <= 0) next
        cp <- current_eval_price[j]
        ch <- hold_high_water[j]
        trig <- FALSE

        if (component_take_profit_type == "fixed") {
          trig <- cp >= hold_cost[j] * (1 + fixed_component_tp_ratio)
        } else if (component_take_profit_type == "trailing_fixed") {
          trig <- cp <= ch * (1 - trailing_fixed_component_tp_ratio)
        } else if (component_take_profit_type == "trailing_atr") {
          trig <- !is.na(current_atr[j]) && cp <= ch - atr_k_component_tp * current_atr[j]
        } else if (component_take_profit_type == "trailing_vol") {
          trig <- !is.na(current_vol[j]) && cp <= ch - vol_sigma_component_tp * current_vol[j]
        } else if (component_take_profit_type == "trailing_log") {
          trig <- !is.na(current_logvol[j]) && cp <= ch * exp(-log_vol_sigma_component_tp * current_logvol[j])
        }
        if (trig) tp_sell[j] <- TRUE
      }
    }

    # ==============================================
    # 9.4 Check portfolio take-profit
    # ==============================================
    if (enable_portfolio_take_profit) {
      v <- total_asset
      hw <- portfolio_high_water
      if (portfolio_take_profit_type == "fixed") {
        pf_tp <- v >= init_capital * (1 + fixed_portfolio_tp_ratio)
      } else if (portfolio_take_profit_type == "trailing_fixed") {
        pf_tp <- v <= hw * (1 - trailing_fixed_portfolio_tp_ratio)
      } else if (portfolio_take_profit_type == "trailing_atr") {
        wnd <- max(1, i - atr_n_portfolio):i
        eq <- equity_series[wnd]
        atrw <- mean(pmax(diff(eq), 0), na.rm = TRUE)
        pf_tp <- !is.na(atrw) && v <= hw - atr_k_portfolio_tp * atrw
      } else if (portfolio_take_profit_type == "trailing_vol") {
        wnd <- max(1, i - vol_n_portfolio):i
        s <- sd(equity_series[wnd], na.rm = TRUE)
        pf_tp <- !is.na(s) && v <= hw - vol_sigma_portfolio_tp * s
      } else if (portfolio_take_profit_type == "trailing_log") {
        wnd <- max(1, i - log_vol_n_portfolio):i
        r <- diff(log(equity_series[wnd]))
        lv <- sd(r, na.rm = TRUE)
        pf_tp <- !is.na(lv) && v <= hw * exp(-log_vol_sigma_portfolio_tp * lv)
      }
    }
    if (pf_tp) {
      component_stop_sell[] <- TRUE
      tp_sell[] <- TRUE
    }

    # ==============================================
    # 9.5 Execute stop-loss / take-profit sells
    # ==============================================
    sell_idx <- which(component_stop_sell | tp_sell)
    for (j in sell_idx) {
      if (positions[j] == 0 || current_exec_price[j] <= 0) next
      q <- positions[j]
      ep <- current_exec_price[j] * (1 - slippage_rate)
      rev <- q * ep
      f <- rev * fee_rate
      stx <- rev * stamp_tax
      cash <- cash + rev - f - stx

      trade_list[[length(trade_list) + 1]] <- data.frame(
        trade_date = today,
        code = tickers[j],
        direction = "SELL",
        price = round(current_exec_price[j], 3),
        exec_price = round(ep, 3),
        quantity = q,
        commission = round(f, 2),
        stamp_tax = round(stx, 2),
        cash_after = round(cash, 2),
        trade_type = dplyr::case_when(
          pf_sl ~ "PORTFOLIO_STOP",
          component_stop_sell[j] ~ "COMPONENT_STOP",
          pf_tp ~ "PORTFOLIO_TP",
          TRUE ~ "COMPONENT_TP"
        )
      )
      positions[j] <- 0
      hold_cost[j] <- 0
      hold_high_water[j] <- 0

      # Mark as stopped in current cycle
      if (component_stop_sell[j] || pf_sl) {
        stopped_in_cycle[j] <- TRUE
      }
    }

    # ==============================================
    # 9.6 Execute rebalancing
    # ==============================================
    if (today_is_rebalance && !pf_sl && !pf_tp) {
      # Reset stop flags for new cycle
      stopped_in_cycle[] <- FALSE

      ta <- cash + sum(positions * current_eval_price, na.rm = TRUE)
      w <- rep(0, nt)
      if (nrow(day_data) > 0) {
        idx <- match(day_data$code, tickers)
        raw <- day_data[[weight_col]]
        raw[is.na(raw)] <- 0
        raw <- pmin(raw, single_max_weight)
        s <- sum(raw)
        if (s > 0) raw <- raw / s * global_max_hold_pct
        raw[raw < min_weight] <- 0
        w[idx] <- raw
      }

      # Exclude assets stopped in current cycle
      w[stopped_in_cycle] <- 0

      target <- floor(ta * w / current_exec_price / lot_size) * lot_size
      target[is.na(target) | current_exec_price <= 0] <- 0

      # Sell excess positions
      for (j in 1:nt) {
        if (positions[j] > target[j]) {
          q <- positions[j] - target[j]
          ep <- current_exec_price[j] * (1 - slippage_rate)
          rev <- q * ep
          cash <- cash + rev - rev * fee_rate - rev * stamp_tax
          positions[j] <- target[j]
          if (positions[j] == 0) {
            hold_cost[j] <- 0
            hold_high_water[j] <- 0
          }
          trade_list[[length(trade_list) + 1]] <- data.frame(
            trade_date = today,
            code = tickers[j],
            direction = "SELL",
            price = round(current_exec_price[j], 3),
            exec_price = round(ep, 3),
            quantity = q,
            commission = round(rev * fee_rate, 2),
            stamp_tax = round(rev * stamp_tax, 2),
            cash_after = round(cash, 2),
            trade_type = "REBALANCE"
          )
        }
      }

      # Buy target positions
      for (j in 1:nt) {
        if (positions[j] < target[j]) {
          q <- target[j] - positions[j]
          ep <- current_exec_price[j] * (1 + slippage_rate)
          cost_buy <- q * ep
          fee_buy <- cost_buy * fee_rate
          if (cash < cost_buy + fee_buy) next
          cash <- cash - cost_buy - fee_buy
          old <- positions[j]
          positions[j] <- target[j]

          if (old == 0) {
            hold_cost[j] <- ep
            hold_high_water[j] <- current_exec_price[j]
          } else {
            hold_cost[j] <- (hold_cost[j] * old + ep * q) / positions[j]
          }

          trade_list[[length(trade_list) + 1]] <- data.frame(
            trade_date = today,
            code = tickers[j],
            direction = "BUY",
            price = round(current_exec_price[j], 3),
            exec_price = round(ep, 3),
            quantity = q,
            commission = round(fee_buy, 2),
            stamp_tax = 0,
            cash_after = round(cash, 2),
            trade_type = "REBALANCE"
          )
        }
      }
    }

    # ==============================================
    # 9.7 Save daily records
    # ==============================================
    pos_df <- data.frame(
      date = today,
      code = tickers,
      quantity = as.numeric(positions),
      exec_price = as.numeric(current_exec_price),
      eval_price = as.numeric(current_eval_price),
      component_high_water = as.numeric(hold_high_water)
    )
    pos_df$market_value <- pos_df$quantity * pos_df$eval_price
    pos_df$is_rebalance_day <- today_is_rebalance
    pos_df$is_stop_loss_day <- any(component_stop_sell) || pf_sl
    position_daily_list[[i]] <- pos_df

    mv <- sum(pos_df$market_value, na.rm = TRUE)
    account_cash_list[[i]] <- data.frame(
      date = today,
      cash = round(cash, 2),
      market_value = round(mv, 2),
      total_asset = round(cash + mv, 2),
      is_rebalance_day = today_is_rebalance,
      is_stop_loss_day = any(component_stop_sell) || pf_sl
    )
  }

  # ==============================================
  # 10. Combine results
  # ==============================================
  trades_df <- dplyr::bind_rows(trade_list)
  positions_df <- dplyr::bind_rows(position_daily_list)
  equity_df <- dplyr::bind_rows(account_cash_list)

  equity_df <- equity_df %>%
    dplyr::mutate(
      daily_return = round(total_asset / dplyr::lag(total_asset) - 1, 6),
      daily_return = ifelse(is.na(daily_return), 0, daily_return)
    )
  equity_df$daily_return[1] <- 0
  equity_df$portfolio_high_water <- portfolio_high_water_history

  if (output_type == "tibble") {
    positions_df <- tibble::as_tibble(positions_df)
    equity_df <- tibble::as_tibble(equity_df)
    if (nrow(trades_df) > 0) trades_df <- tibble::as_tibble(trades_df)
  }

  # ==============================================
  # 11. Print summary
  # ==============================================
  final <- tail(equity_df$total_asset, 1)
  ret <- (final / init_capital - 1) * 100
  total_trades <- nrow(trades_df)

  message("==============================================")
  message(" Backtest completed!")
  message(" Period: ", start_date, " to ", end_date)
  message("Rebalance mode: ", rebalance_mode, " (", rebalance_cycle, ")")
  message(" Initial capital: ", init_capital)
  message(" Final asset: ", round(final, 2))
  message(" Total return: ", round(ret, 2), "%")
  message("Total rebalance days: ", total_rebalance_days)
  message("Total trades: ", total_trades)
  message("==============================================")

  # ==============================================
  # 12. Return results
  # ==============================================
  return(list(
    daily_positions = positions_df,
    equity_curve = equity_df,
    transactions = trades_df,
    config = list(
      start_date = as.character(start_date),
      end_date = as.character(end_date),
      init_capital = init_capital,
      final_asset = final,
      total_return_pct = ret,
      total_rebalance_days = total_rebalance_days,
      total_trades = total_trades,
      exec_price_col = exec_price_col,
      eval_price_col = eval_price_col,
      fee_rate = fee_rate,
      stamp_tax = stamp_tax,
      slippage_rate = slippage_rate,
      lot_size = lot_size,
      rebalance_mode = rebalance_mode,
      rebalance_cycle = rebalance_cycle,
      weight_change_threshold = weight_change_threshold,
      enable_component_stop_loss = enable_component_stop_loss,
      component_stop_loss_type = component_stop_loss_type,
      enable_portfolio_stop_loss = enable_portfolio_stop_loss,
      portfolio_stop_loss_type = portfolio_stop_loss_type,
      enable_component_take_profit = enable_component_take_profit,
      component_take_profit_type = component_take_profit_type,
      enable_portfolio_take_profit = enable_portfolio_take_profit,
      portfolio_take_profit_type = portfolio_take_profit_type,
      single_max_weight = single_max_weight,
      global_max_hold_pct = global_max_hold_pct,
      min_weight = min_weight
    )
  ))
}

# ------------------------------ 6. 使用示例（注释）--------------------------------
# 以下为典型用法演示，不需要执行时可删除
#
# # 加载数据（假设 my_data 是符合格式的 data.frame）
# cfg <- default_backtest_config() %>%
#   set_lot_size(200) %>%
#   set_fee_rate(0.0002) %>%
#   set_rebalance_cycle("monthly") %>%
#   set_component_stop_loss(enable = TRUE, type = "trailing_fixed", trailing_fixed_ratio = 0.08)
#
# result <- run_backtest_from_config(cfg, df = my_data)
#
# # 也可以临时覆盖某个参数
# result2 <- run_backtest_from_config(cfg, df = my_data, init_capital = 50000)

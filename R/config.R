#' Default backtest configuration
#'
#' Returns a list of all default parameters used by the backtesting engine.
#'
#' @return A list of default parameters.
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

# ==============================================
# Simple setter functions (single parameter)
# ==============================================

#' @title Set backtest parameters
#' @name set_parameter
#' @description Modify a single field in the backtest configuration list.
#' @param config A configuration list from `default_backtest_config()`.
#' @param value New value for the parameter.
#' @return Modified configuration list.
NULL

#' @rdname set_parameter
#' @export
set_weight_col <- function(config, value) {
  config$weight_col <- value
  config
}

#' @rdname set_parameter
#' @export
set_start_date <- function(config, value) {
  config$start_date <- value
  config
}

#' @rdname set_parameter
#' @export
set_end_date <- function(config, value) {
  config$end_date <- value
  config
}

#' @rdname set_parameter
#' @export
set_exec_price_col <- function(config, value) {
  config$exec_price_col <- value
  config
}

#' @rdname set_parameter
#' @export
set_eval_price_col <- function(config, value) {
  config$eval_price_col <- value
  config
}

#' @rdname set_parameter
#' @export
set_init_capital <- function(config, value) {
  config$init_capital <- value
  config
}

#' @rdname set_parameter
#' @export
set_lot_size <- function(config, value) {
  config$lot_size <- value
  config
}

#' @rdname set_parameter
#' @export
set_fee_rate <- function(config, value) {
  config$fee_rate <- value
  config
}

#' @rdname set_parameter
#' @export
set_stamp_tax <- function(config, value) {
  config$stamp_tax <- value
  config
}

#' @rdname set_parameter
#' @export
set_slippage_rate <- function(config, value) {
  config$slippage_rate <- value
  config
}

#' @rdname set_parameter
#' @export
set_min_weight <- function(config, value) {
  config$min_weight <- value
  config
}

#' @rdname set_parameter
#' @export
set_enable_component_stop_loss <- function(config, value) {
  config$enable_component_stop_loss <- value
  config
}

#' @rdname set_parameter
#' @export
set_component_stop_loss_type <- function(config, value) {
  config$component_stop_loss_type <- value
  config
}

#' @rdname set_parameter
#' @export
set_fixed_component_sl_ratio <- function(config, value) {
  config$fixed_component_sl_ratio <- value
  config
}

#' @rdname set_parameter
#' @export
set_trailing_fixed_component_sl_ratio <- function(config, value) {
  config$trailing_fixed_component_sl_ratio <- value
  config
}

#' @rdname set_parameter
#' @export
set_atr_n_component <- function(config, value) {
  config$atr_n_component <- value
  config
}

#' @rdname set_parameter
#' @export
set_atr_k_component <- function(config, value) {
  config$atr_k_component <- value
  config
}

#' @rdname set_parameter
#' @export
set_vol_n_component <- function(config, value) {
  config$vol_n_component <- value
  config
}

#' @rdname set_parameter
#' @export
set_vol_sigma_component <- function(config, value) {
  config$vol_sigma_component <- value
  config
}

#' @rdname set_parameter
#' @export
set_log_vol_n_component <- function(config, value) {
  config$log_vol_n_component <- value
  config
}

#' @rdname set_parameter
#' @export
set_log_vol_sigma_component <- function(config, value) {
  config$log_vol_sigma_component <- value
  config
}

#' @rdname set_parameter
#' @export
set_enable_portfolio_stop_loss <- function(config, value) {
  config$enable_portfolio_stop_loss <- value
  config
}

#' @rdname set_parameter
#' @export
set_portfolio_stop_loss_type <- function(config, value) {
  config$portfolio_stop_loss_type <- value
  config
}

#' @rdname set_parameter
#' @export
set_fixed_portfolio_sl_ratio <- function(config, value) {
  config$fixed_portfolio_sl_ratio <- value
  config
}

#' @rdname set_parameter
#' @export
set_trailing_fixed_portfolio_sl_ratio <- function(config, value) {
  config$trailing_fixed_portfolio_sl_ratio <- value
  config
}

#' @rdname set_parameter
#' @export
set_atr_n_portfolio <- function(config, value) {
  config$atr_n_portfolio <- value
  config
}

#' @rdname set_parameter
#' @export
set_atr_k_portfolio <- function(config, value) {
  config$atr_k_portfolio <- value
  config
}

#' @rdname set_parameter
#' @export
set_vol_n_portfolio <- function(config, value) {
  config$vol_n_portfolio <- value
  config
}

#' @rdname set_parameter
#' @export
set_vol_sigma_portfolio <- function(config, value) {
  config$vol_sigma_portfolio <- value
  config
}

#' @rdname set_parameter
#' @export
set_log_vol_n_portfolio <- function(config, value) {
  config$log_vol_n_portfolio <- value
  config
}

#' @rdname set_parameter
#' @export
set_log_vol_sigma_portfolio <- function(config, value) {
  config$log_vol_sigma_portfolio <- value
  config
}

#' @rdname set_parameter
#' @export
set_enable_component_take_profit <- function(config, value) {
  config$enable_component_take_profit <- value
  config
}

#' @rdname set_parameter
#' @export
set_component_take_profit_type <- function(config, value) {
  config$component_take_profit_type <- value
  config
}

#' @rdname set_parameter
#' @export
set_fixed_component_tp_ratio <- function(config, value) {
  config$fixed_component_tp_ratio <- value
  config
}

#' @rdname set_parameter
#' @export
set_trailing_fixed_component_tp_ratio <- function(config, value) {
  config$trailing_fixed_component_tp_ratio <- value
  config
}

#' @rdname set_parameter
#' @export
set_atr_k_component_tp <- function(config, value) {
  config$atr_k_component_tp <- value
  config
}

#' @rdname set_parameter
#' @export
set_vol_sigma_component_tp <- function(config, value) {
  config$vol_sigma_component_tp <- value
  config
}

#' @rdname set_parameter
#' @export
set_log_vol_sigma_component_tp <- function(config, value) {
  config$log_vol_sigma_component_tp <- value
  config
}

#' @rdname set_parameter
#' @export
set_enable_portfolio_take_profit <- function(config, value) {
  config$enable_portfolio_take_profit <- value
  config
}

#' @rdname set_parameter
#' @export
set_portfolio_take_profit_type <- function(config, value) {
  config$portfolio_take_profit_type <- value
  config
}

#' @rdname set_parameter
#' @export
set_fixed_portfolio_tp_ratio <- function(config, value) {
  config$fixed_portfolio_tp_ratio <- value
  config
}

#' @rdname set_parameter
#' @export
set_trailing_fixed_portfolio_tp_ratio <- function(config, value) {
  config$trailing_fixed_portfolio_tp_ratio <- value
  config
}

#' @rdname set_parameter
#' @export
set_atr_k_portfolio_tp <- function(config, value) {
  config$atr_k_portfolio_tp <- value
  config
}

#' @rdname set_parameter
#' @export
set_vol_sigma_portfolio_tp <- function(config, value) {
  config$vol_sigma_portfolio_tp <- value
  config
}

#' @rdname set_parameter
#' @export
set_log_vol_sigma_portfolio_tp <- function(config, value) {
  config$log_vol_sigma_portfolio_tp <- value
  config
}

#' @rdname set_parameter
#' @export
set_single_max_weight <- function(config, value) {
  config$single_max_weight <- value
  config
}

#' @rdname set_parameter
#' @export
set_global_max_hold_pct <- function(config, value) {
  config$global_max_hold_pct <- value
  config
}

#' @rdname set_parameter
#' @export
set_rebalance_mode <- function(config, value) {
  config$rebalance_mode <- value
  config
}

#' @rdname set_parameter
#' @export
set_rebalance_cycle <- function(config, value) {
  config$rebalance_cycle <- value
  config
}

#' @rdname set_parameter
#' @export
set_weight_change_threshold <- function(config, value) {
  config$weight_change_threshold <- value
  config
}

#' @rdname set_parameter
#' @export
set_skip_suspended <- function(config, value) {
  config$skip_suspended <- value
  config
}

#' @rdname set_parameter
#' @export
set_output_type <- function(config, value) {
  config$output_type <- value
  config
}

# ==============================================
# Composite setters
# ==============================================

#' Set component stop-loss parameters
#'
#' @param config Configuration list.
#' @param enable Logical, enable component stop-loss.
#' @param type Stop-loss type.
#' @param fixed_ratio Fixed ratio.
#' @param trailing_fixed_ratio Trailing fixed ratio.
#' @param atr_n ATR lookback.
#' @param atr_k ATR multiplier.
#' @param vol_n Volatility lookback.
#' @param vol_sigma Volatility sigma.
#' @param log_vol_n Log volatility lookback.
#' @param log_vol_sigma Log volatility sigma.
#' @return Modified configuration list.
#' @export
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
#'
#' @param config Configuration list.
#' @param enable Logical.
#' @param type Stop-loss type.
#' @param fixed_ratio Fixed ratio.
#' @param trailing_fixed_ratio Trailing fixed ratio.
#' @param atr_n ATR lookback.
#' @param atr_k ATR multiplier.
#' @param vol_n Volatility lookback.
#' @param vol_sigma Volatility sigma.
#' @param log_vol_n Log volatility lookback.
#' @param log_vol_sigma Log volatility sigma.
#' @return Modified configuration list.
#' @export
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
#'
#' @param config Configuration list.
#' @param enable Logical.
#' @param type Take-profit type.
#' @param fixed_ratio Fixed ratio.
#' @param trailing_fixed_ratio Trailing fixed ratio.
#' @param atr_k ATR multiplier.
#' @param vol_sigma Volatility sigma.
#' @param log_vol_sigma Log volatility sigma.
#' @return Modified configuration list.
#' @export
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
#'
#' @param config Configuration list.
#' @param enable Logical.
#' @param type Take-profit type.
#' @param fixed_ratio Fixed ratio.
#' @param trailing_fixed_ratio Trailing fixed ratio.
#' @param atr_k ATR multiplier.
#' @param vol_sigma Volatility sigma.
#' @param log_vol_sigma Log volatility sigma.
#' @return Modified configuration list.
#' @export
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
#'
#' @param config Configuration list.
#' @param fee_rate Commission rate.
#' @param stamp_tax Stamp tax rate (sell only).
#' @param slippage_rate Slippage rate.
#' @param lot_size Minimum lot size.
#' @return Modified configuration list.
#' @export
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

#' Set weight constraints
#'
#' @param config Configuration list.
#' @param single_max_weight Maximum weight per asset.
#' @param global_max_hold_pct Maximum portfolio exposure.
#' @param min_weight Minimum effective weight.
#' @return Modified configuration list.
#' @export
set_weight_constraints <- function(config,
                                   single_max_weight = 0.95,
                                   global_max_hold_pct = 1.0,
                                   min_weight = 1e-6) {
  config$single_max_weight <- single_max_weight
  config$global_max_hold_pct <- global_max_hold_pct
  config$min_weight <- min_weight
  config
}

#' Set rebalancing parameters
#'
#' @param config Configuration list.
#' @param mode Rebalancing mode: "calendar", "weight_shift", "hybrid".
#' @param cycle Rebalancing cycle (string or positive integer).
#' @param weight_threshold Weight drift threshold.
#' @return Modified configuration list.
#' @export
set_rebalancing <- function(config,
                            mode = "calendar",
                            cycle = "quarterly",
                            weight_threshold = 0.01) {
  config$rebalance_mode <- mode
  config$rebalance_cycle <- cycle
  config$weight_change_threshold <- weight_threshold
  config
}

#' Set date range
#'
#' @param config Configuration list.
#' @param start_date Start date (Date or character).
#' @param end_date End date (Date or character).
#' @return Modified configuration list.
#' @export
set_date_range <- function(config, start_date = NULL, end_date = NULL) {
  if (!is.null(start_date)) config$start_date <- start_date
  if (!is.null(end_date)) config$end_date <- end_date
  config
}

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
    exec_price_col = "open",
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
    # Stop-Limit execution gap ------------------------------------------------
    # SL stop_limit: limit floor = trigger * (1 - gap).  No fill if open gaps
    #   below floor (price skipped the limit level entirely).
    # TP stop_limit: limit ceiling = trigger * (1 + gap).  No fill if open gaps
    #   above ceiling (price already past the bracket window).
    stop_limit_component_sl_gap = 0.005,
    stop_limit_component_tp_gap = 0.005,
    stop_limit_portfolio_sl_gap = 0.005,
    stop_limit_portfolio_tp_gap = 0.005,
    # OCO (One-Cancels-Other) bracket ----------------------------------------
    # When TRUE, SL and TP are treated as a linked pair.  Whichever fires first
    # closes the position; the other is automatically cancelled.
    enable_oco_component = FALSE,
    enable_oco_portfolio  = FALSE,
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

# ── Stop-Limit setters ───────────────────────────────────────────────────────

#' Set stop-limit execution gap
#'
#' Controls how far below (SL) or above (TP) the trigger price the fill floor/
#' ceiling sits.  If the open price gaps past this level, the stop-limit order
#' does NOT fill — protecting against extreme gap-through fills.
#'
#' @param config Configuration list.
#' @param component_sl_gap Gap fraction below SL trigger for component stop-limit.
#'   E.g. 0.005 = 0.5% below trigger. Default 0.005.
#' @param component_tp_gap Gap fraction above TP trigger for component stop-limit.
#'   Default 0.005.
#' @param portfolio_sl_gap Gap for portfolio stop-limit SL. Default 0.005.
#' @param portfolio_tp_gap Gap for portfolio stop-limit TP. Default 0.005.
#' @return Modified configuration list.
#' @export
set_stop_limit_gap <- function(config,
                               component_sl_gap = 0.005,
                               component_tp_gap = 0.005,
                               portfolio_sl_gap = 0.005,
                               portfolio_tp_gap = 0.005) {
  config$stop_limit_component_sl_gap <- component_sl_gap
  config$stop_limit_component_tp_gap <- component_tp_gap
  config$stop_limit_portfolio_sl_gap <- portfolio_sl_gap
  config$stop_limit_portfolio_tp_gap <- portfolio_tp_gap
  config
}


# ── OCO setters ──────────────────────────────────────────────────────────────

#' Set OCO (One-Cancels-Other) bracket for component stop-loss and take-profit
#'
#' When OCO is enabled, stop-loss and take-profit are treated as a linked pair:
#' whichever fires first closes the position; the other is automatically
#' cancelled.  This is equivalent to enabling both
#' \code{enable_component_stop_loss} and \code{enable_component_take_profit}
#' simultaneously, but documents the intent explicitly.
#'
#' @param config Configuration list.
#' @param sl_type Stop-loss type for the OCO pair.  One of
#'   \code{"fixed"}, \code{"trailing_fixed"}, \code{"trailing_atr"},
#'   \code{"trailing_vol"}, \code{"trailing_log"}, \code{"stop_limit"}.
#' @param tp_type Take-profit type for the OCO pair.  Same choices as
#'   \code{sl_type}.
#' @param sl_ratio Fixed or trailing ratio for the stop-loss leg.
#' @param tp_ratio Fixed ratio for the take-profit leg.
#' @param sl_gap Stop-limit gap below SL trigger (only used when
#'   \code{sl_type = "stop_limit"}).
#' @param tp_gap Stop-limit gap above TP trigger (only used when
#'   \code{tp_type = "stop_limit"}).
#' @return Modified configuration list.
#' @export
set_oco_component <- function(config,
                              sl_type  = "trailing_fixed",
                              tp_type  = "fixed",
                              sl_ratio = 0.10,
                              tp_ratio = 0.10,
                              sl_gap   = 0.005,
                              tp_gap   = 0.005) {
  config$enable_oco_component               <- TRUE
  config$enable_component_stop_loss         <- TRUE
  config$component_stop_loss_type           <- sl_type
  config$trailing_fixed_component_sl_ratio  <- sl_ratio
  config$fixed_component_sl_ratio           <- sl_ratio
  config$stop_limit_component_sl_gap        <- sl_gap

  config$enable_component_take_profit       <- TRUE
  config$component_take_profit_type         <- tp_type
  config$fixed_component_tp_ratio           <- tp_ratio
  config$stop_limit_component_tp_gap        <- tp_gap
  config
}

#' Set OCO bracket for portfolio-level stop-loss and take-profit
#'
#' Portfolio-level equivalent of \code{\link{set_oco_component}}.
#'
#' @param config Configuration list.
#' @param sl_type Portfolio stop-loss type.
#' @param tp_type Portfolio take-profit type.
#' @param sl_ratio Stop-loss ratio.
#' @param tp_ratio Take-profit ratio.
#' @param sl_gap Stop-limit gap for SL.
#' @param tp_gap Stop-limit gap for TP.
#' @return Modified configuration list.
#' @export
set_oco_portfolio <- function(config,
                              sl_type  = "trailing_fixed",
                              tp_type  = "fixed",
                              sl_ratio = 0.10,
                              tp_ratio = 0.10,
                              sl_gap   = 0.005,
                              tp_gap   = 0.005) {
  config$enable_oco_portfolio                <- TRUE
  config$enable_portfolio_stop_loss          <- TRUE
  config$portfolio_stop_loss_type            <- sl_type
  config$trailing_fixed_portfolio_sl_ratio   <- sl_ratio
  config$fixed_portfolio_sl_ratio            <- sl_ratio
  config$stop_limit_portfolio_sl_gap         <- sl_gap

  config$enable_portfolio_take_profit        <- TRUE
  config$portfolio_take_profit_type          <- tp_type
  config$fixed_portfolio_tp_ratio            <- tp_ratio
  config$stop_limit_portfolio_tp_gap         <- tp_gap
  config
}

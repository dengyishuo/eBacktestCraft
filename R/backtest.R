#' Advanced multi-mode quantitative backtesting system
#'
#' Supports three rebalancing modes: daily rebalancing, calendar-based periodic rebalancing
#' (weekly/monthly/quarterly), and signal-triggered rebalancing.
#' Built-in comprehensive risk controls: stop-loss (fixed/trailing), single position cap,
#' global exposure cap, slippage, commission, and stamp tax.
#' Outputs standardized results: daily holdings, equity curve, transaction details.
#'
#' @param df Data frame in long format with OHLC data
#' @param weight_col Weight column name, default "weight"
#' @param signal_col Signal column name (1 = hold, 0 = no position), default "signal"
#' @param start_date Backtest start date, default earliest date in data
#' @param end_date Backtest end date, default latest date in data
#' @param exec_price_type Execution price type (reserved for compatibility)
#' @param exec_price_col Execution price column name (reserved for compatibility)
#' @param eval_price_col Valuation price column, default "Adj.Close"
#' @param init_capital Initial capital, default 100000
#' @param lot_size Lot size (number of shares), default 100
#' @param fee_rate Commission fee rate, default 0.0003
#' @param stamp_tax Stamp tax rate, default 0.0005
#' @param slippage_rate Slippage rate, default 0.001
#' @param min_weight Minimum effective weight, default 1e-6
#' @param enable_stop_loss Whether to enable stop-loss, default TRUE
#' @param stop_loss_type Stop-loss type: "trailing" or "fixed"
#' @param stop_loss_ratio Stop-loss ratio, default 0.1 (10\%)
#' @param single_max_weight Maximum weight for single stock, default 0.95
#' @param global_max_hold_pct Maximum global position percentage, default 1.0
#' @param rebalance_mode Rebalancing mode: "daily", "calendar", or "signal"
#' @param rebalance_cycle Calendar rebalancing cycle: "monthly"/"weekly"/"quarterly" or numeric (days)
#' @param output_type Output format: "tibble" (default) or "data.frame"
#'
#' @return List containing 3 core tables plus config:
#'   \item{daily_positions}{Daily position details for all stocks}
#'   \item{equity_curve}{Account equity curve}
#'   \item{transactions}{Complete transaction records}
#'   \item{config}{Backtest parameter summary}
#'
#' @importFrom dplyr bind_rows
#' @importFrom lubridate floor_date
#' @importFrom zoo na.locf
#' @export
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' df <- data.frame(
#'   date = rep(as.Date(c("2024-01-01", "2024-01-02")), each = 2),
#'   code = rep(c("000001.SS", "000002.SZ"), 2),
#'   open = c(10, 20, 11, 21),
#'   high = c(11, 21, 12, 22),
#'   low = c(9, 19, 10, 20),
#'   close = c(10.5, 20.5, 11.5, 21.5),
#'   adjusted = c(10.5, 20.5, 11.5, 21.5),
#'   volume = c(1000, 2000, 1100, 2100),
#'   weight = c(0.6, 0.4, 0.6, 0.4),
#'   signal = c(1, 1, 1, 1)
#' )
#'
#' result <- backtest(df, weight_col = "weight", signal_col = "signal")
#' }
backtest <- function(
  # Core data parameters
  df,
  weight_col = "weight",
  signal_col = "signal",
  # Backtest time range
  start_date = NULL,
  end_date = NULL,
  # Execution price configuration
  exec_price_type = c("close", "open", "custom"),
  exec_price_col = NULL,
  eval_price_col = "Adj.Close",
  # Capital and transaction costs
  init_capital = 100000,
  lot_size = 100,
  fee_rate = 0.0003,
  stamp_tax = 0.0005,
  slippage_rate = 0.001,
  min_weight = 1e-6,
  # Stop-loss configuration
  enable_stop_loss = TRUE,
  stop_loss_type = "trailing",
  stop_loss_ratio = 0.1,
  # Position limit configuration
  single_max_weight = 0.95,
  global_max_hold_pct = 1.0,
  # Rebalancing mode core parameters
  rebalance_mode = c("daily", "calendar", "signal"),
  rebalance_cycle = c("monthly", "weekly", "quarterly", 1),
  # Output format
  output_type = c("tibble", "data.frame")
) {
  # ==============================================
  # 1. Column name standardization (compatible with Chinese/English)
  # ==============================================
  col_map <- list(
    date = c("Date", "date"),
    code = c("Code", "code"),
    open = c("Open", "open"),
    high = c("High", "high"),
    low = c("Low", "low"),
    close = c("Close", "close"),
    adjusted = c("Adj.Close", "adjusted", "eval_price"),
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

  # Validate required columns
  required_cols <- c("date", "code", "open", "close", "adjusted", weight_col, signal_col)
  missing_cols <- setdiff(required_cols, colnames(data_raw))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ", paste(missing_cols, collapse = ", "),
      "\nPlease ensure column names match or specify weight_col/signal_col parameters"
    )
  }

  # ==============================================
  # 2. Date parameter processing
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

  # Validate filtered data
  if (nrow(data_raw) == 0) {
    stop("No valid data within specified date range! Please check start_date/end_date")
  }

  output_type <- match.arg(output_type)

  # ==============================================
  # 3. Missing value imputation
  # ==============================================
  data_processed <- data_raw %>%
    dplyr::arrange(.data$code, .data$date) %>%
    dplyr::group_by(.data$code) %>%
    dplyr::mutate(dplyr::across(
      c(open, high, low, close, adjusted, volume),
      ~ zoo::na.locf(., na.rm = FALSE, fromLast = FALSE)
    )) %>%
    dplyr::mutate(dplyr::across(
      c(open, high, low, close, adjusted, volume),
      ~ ifelse(is.na(.) | is.infinite(.), 0, .)
    )) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$date, .data$code)

  # Extract trading dates and tickers
  trade_dates <- sort(unique(data_processed$date))
  tickers <- sort(unique(data_processed$code))
  n_days <- length(trade_dates)
  n_tickers <- length(tickers)

  # ==============================================
  # 4. Rebalancing mode initialization
  # ==============================================
  rebalance_mode <- match.arg(rebalance_mode)
  is_rebalance_day <- rep(TRUE, n_days)

  # Calendar rebalancing mode
  if (rebalance_mode == "calendar") {
    if (is.numeric(rebalance_cycle)) {
      rebalance_cycle <- as.integer(rebalance_cycle)
      if (rebalance_cycle < 1) rebalance_cycle <- 1
      is_rebalance_day <- (seq_along(trade_dates) - 1) %% rebalance_cycle == 0
    } else {
      rebalance_cycle <- match.arg(rebalance_cycle, c("monthly", "weekly", "quarterly"))
      for (i in seq_along(trade_dates)) {
        today <- trade_dates[i]
        if (rebalance_cycle == "monthly") {
          first_day_of_month <- lubridate::floor_date(today, "month")
          is_rebalance_day[i] <- today == first_day_of_month
        } else if (rebalance_cycle == "weekly") {
          first_day_of_week <- lubridate::floor_date(today, "week", week_start = 1)
          is_rebalance_day[i] <- today == first_day_of_week
        } else if (rebalance_cycle == "quarterly") {
          first_day_of_quarter <- lubridate::floor_date(today, "quarter")
          is_rebalance_day[i] <- today == first_day_of_quarter
        }
      }
    }
  }

  # Signal-triggered rebalancing mode
  if (rebalance_mode == "signal") {
    signal_df <- data_processed %>%
      dplyr::select(.data$date, .data$code, signal = dplyr::all_of(signal_col)) %>%
      dplyr::arrange(.data$code, .data$date) %>%
      dplyr::group_by(.data$code) %>%
      dplyr::mutate(signal_change = .data$signal != dplyr::lag(.data$signal, default = 0)) %>%
      dplyr::ungroup()

    signal_change_df <- signal_df %>%
      dplyr::group_by(.data$date) %>%
      dplyr::summarise(has_signal_change = any(.data$signal_change), .groups = "drop")

    for (i in seq_along(trade_dates)) {
      today <- trade_dates[i]
      has_change <- signal_change_df$has_signal_change[signal_change_df$date == today]
      is_rebalance_day[i] <- ifelse(length(has_change) > 0 && has_change, TRUE, FALSE)
    }
    is_rebalance_day[1] <- TRUE
  }

  # ==============================================
  # 5. Initialize variables
  # ==============================================
  cash <- as.numeric(init_capital)
  positions <- setNames(rep(0, n_tickers), tickers)
  last_close <- setNames(rep(0, n_tickers), tickers)
  hold_cost <- setNames(rep(0, n_tickers), tickers)
  hold_high_water <- setNames(rep(0, n_tickers), tickers)

  trade_list <- list()
  position_daily_list <- list()
  account_cash_list <- list()

  # ==============================================
  # 6. Initial equity row
  # ==============================================
  initial_account_row <- data.frame(
    date = start_date,
    cash = round(init_capital, 2),
    market_value = 0,
    total_asset = round(init_capital, 2),
    daily_return = 0
  )
  account_cash_list[[1]] <- initial_account_row

  # ==============================================
  # 7. Main backtest loop
  # ==============================================
  for (i in seq_along(trade_dates)) {
    today <- trade_dates[i]
    day_data <- data_processed[data_processed$date == today, ]
    today_is_rebalance <- is_rebalance_day[i]

    # --------------------------
    # 7.1 Current price handling
    # --------------------------
    current_close <- last_close
    for (tic in tickers) {
      p <- day_data$close[day_data$code == tic]
      if (length(p) > 0 && !is.na(p) && !is.infinite(p)) {
        current_close[tic] <- p
      }
    }
    current_close[is.na(current_close) | is.infinite(current_close)] <- 0

    # --------------------------
    # 7.2 Current asset calculation
    # --------------------------
    hold_mkt_val <- sum(positions * current_close, na.rm = TRUE)
    hold_mkt_val <- ifelse(is.na(hold_mkt_val) | is.infinite(hold_mkt_val), 0, hold_mkt_val)
    total_asset <- cash + hold_mkt_val
    total_asset <- ifelse(is.na(total_asset) | is.infinite(total_asset), cash, total_asset)

    # --------------------------
    # 7.3 Stop-loss judgment
    # --------------------------
    stop_trigger_today <- FALSE
    if (enable_stop_loss) {
      for (j in seq_along(tickers)) {
        tic <- tickers[j]
        curr_hold <- positions[j]
        curr_close <- current_close[j]
        curr_cost <- hold_cost[j]
        curr_high <- hold_high_water[j]

        if (curr_hold <= 0 || curr_close <= 0 || curr_cost <= 0 || is.na(curr_high)) {
          next
        }

        new_high <- max(curr_high, curr_close)
        if (!is.na(new_high)) {
          hold_high_water[j] <- new_high
        }

        if (stop_loss_type == "trailing") {
          drawdown <- ifelse(hold_high_water[j] == 0, 0,
            (hold_high_water[j] - curr_close) / hold_high_water[j]
          )
          if (!is.na(drawdown) && drawdown >= stop_loss_ratio) {
            stop_trigger_today <- TRUE
            break
          }
        } else if (stop_loss_type == "fixed") {
          drop_ratio <- ifelse(curr_cost == 0, 0,
            (curr_cost - curr_close) / curr_cost
          )
          if (!is.na(drop_ratio) && drop_ratio >= stop_loss_ratio) {
            stop_trigger_today <- TRUE
            break
          }
        }
      }
    }

    # --------------------------
    # 7.4 Rebalancing logic
    # --------------------------
    if (stop_trigger_today) {
      day_data$alloc_capital <- 0
      day_data$target_shares <- 0
      today_is_rebalance <- TRUE
    } else if (today_is_rebalance) {
      # Calculate target weights and shares
      weight_adj <- pmin(day_data[[weight_col]], single_max_weight)
      weight_adj[is.na(weight_adj) | is.infinite(weight_adj)] <- 0
      weight_sum <- sum(weight_adj, na.rm = TRUE)
      if (weight_sum == 0) weight_sum <- 1
      weight_adj <- weight_adj / weight_sum * global_max_hold_pct

      alloc_capital <- total_asset * weight_adj
      alloc_capital[is.na(alloc_capital) | is.infinite(alloc_capital)] <- 0

      theo_shares <- floor(alloc_capital / day_data$close / lot_size) * lot_size
      theo_shares[is.na(theo_shares) | is.infinite(theo_shares)] <- 0

      target_shares <- theo_shares
      target_shares[day_data[[signal_col]] != 1] <- 0
      target_shares[weight_adj < min_weight] <- 0
      target_shares[is.na(target_shares) | is.infinite(target_shares)] <- 0

      day_data$target_shares <- target_shares
    } else {
      day_data$target_shares <- 0
      for (j in seq_along(tickers)) {
        day_data$target_shares[day_data$code == tickers[j]] <- positions[j]
      }
    }

    # --------------------------
    # 7.5 Trade execution: sell first, then buy
    # --------------------------
    if (today_is_rebalance) {
      # Sell orders
      for (j in seq_along(tickers)) {
        tic <- tickers[j]
        curr_hold <- as.numeric(positions[j])
        target <- day_data$target_shares[day_data$code == tic]
        if (length(target) == 0) target <- 0
        target <- as.numeric(target)
        if (is.na(target)) target <- 0

        if (!is.na(curr_hold) && !is.na(target) && curr_hold > target) {
          sell_num <- curr_hold - target
          close_price <- current_close[j]
          if (is.na(close_price) || close_price <= 0 || sell_num <= 0) next

          exec_price <- close_price * (1 - slippage_rate)
          if (is.na(exec_price) | is.infinite(exec_price)) exec_price <- close_price
          trade_amount <- sell_num * exec_price
          fee <- trade_amount * fee_rate
          stamp <- trade_amount * stamp_tax
          cash <- cash + trade_amount - fee - stamp

          trade_list[[length(trade_list) + 1]] <- data.frame(
            trade_date = today,
            code = tic,
            direction = "SELL",
            close_price = round(close_price, 3),
            exec_price = round(exec_price, 3),
            quantity = sell_num,
            commission = round(fee, 2),
            stamp_tax = round(stamp, 2),
            cash_after = round(cash, 2),
            trade_type = ifelse(stop_trigger_today, "STOP_LOSS", "REBALANCE")
          )
          positions[j] <- target
          if (positions[j] == 0) {
            hold_cost[j] <- 0
            hold_high_water[j] <- 0
          }
        }
      }

      # Buy orders
      buy_idx <- which(day_data$target_shares > 0)
      for (idx in buy_idx) {
        tic <- day_data$code[idx]
        j <- which(tickers == tic)
        if (length(j) == 0) next

        curr_hold <- as.numeric(positions[j])
        target <- as.numeric(day_data$target_shares[idx])
        if (is.na(target)) target <- 0
        buy_num <- target - curr_hold
        if (is.na(buy_num) || buy_num <= 0) next

        close_price <- current_close[j]
        if (is.na(close_price) || close_price <= 0) next

        exec_price <- close_price * (1 + slippage_rate)
        trade_amount <- buy_num * exec_price
        fee <- trade_amount * fee_rate
        total_cost <- trade_amount + fee

        if (!is.na(cash) && cash >= total_cost) {
          cash <- cash - total_cost
          positions[j] <- target

          if (hold_cost[j] == 0) {
            hold_cost[j] <- exec_price
          } else {
            hold_cost[j] <- (hold_cost[j] * curr_hold + exec_price * buy_num) / target
          }
          if (hold_high_water[j] == 0) {
            hold_high_water[j] <- close_price
          } else {
            hold_high_water[j] <- max(hold_high_water[j], close_price)
          }

          trade_list[[length(trade_list) + 1]] <- data.frame(
            trade_date = today,
            code = tic,
            direction = "BUY",
            close_price = round(close_price, 3),
            exec_price = round(exec_price, 3),
            quantity = buy_num,
            commission = round(fee, 2),
            stamp_tax = 0,
            cash_after = round(cash, 2),
            trade_type = "REBALANCE"
          )
        }
      }
    }

    # --------------------------
    # 7.6 Daily position record
    # --------------------------
    pos_df <- data.frame(
      date = today,
      code = tickers,
      quantity = as.numeric(positions),
      close_price = as.numeric(current_close)
    )
    pos_df$quantity[is.na(pos_df$quantity) | is.infinite(pos_df$quantity)] <- 0
    pos_df$close_price[is.na(pos_df$close_price) | is.infinite(pos_df$close_price)] <- 0
    pos_df$market_value <- pos_df$quantity * pos_df$close_price
    pos_df$market_value[is.na(pos_df$market_value) | is.infinite(pos_df$market_value)] <- 0

    daily_total_asset <- sum(pos_df$market_value) + cash
    pos_df$position_pct <- round(pos_df$market_value / daily_total_asset, 4)
    pos_df$position_pct[is.na(pos_df$position_pct) | is.infinite(pos_df$position_pct)] <- 0
    pos_df$is_rebalance_day <- today_is_rebalance
    pos_df$is_stop_loss_day <- stop_trigger_today

    position_daily_list[[length(position_daily_list) + 1]] <- pos_df

    # --------------------------
    # 7.7 Daily asset record
    # --------------------------
    final_hold_val <- sum(pos_df$market_value, na.rm = TRUE)
    final_hold_val <- ifelse(is.na(final_hold_val) | is.infinite(final_hold_val), 0, final_hold_val)
    account_cash_list[[length(account_cash_list) + 1]] <- data.frame(
      date = today,
      cash = round(cash, 2),
      market_value = round(final_hold_val, 2),
      total_asset = round(cash + final_hold_val, 2),
      is_rebalance_day = today_is_rebalance,
      is_stop_loss_day = stop_trigger_today
    )

    # --------------------------
    # 7.8 Update state
    # --------------------------
    last_close <- current_close
  }

  # ==============================================
  # 8. Combine results
  # ==============================================
  trades_df <- dplyr::bind_rows(trade_list)
  positions_daily <- dplyr::bind_rows(position_daily_list)
  total_daily <- dplyr::bind_rows(account_cash_list)

  total_daily <- total_daily %>%
    dplyr::mutate(
      daily_return = round(.data$total_asset / dplyr::lag(.data$total_asset) - 1, 6),
      daily_return = ifelse(is.na(.data$daily_return) | is.infinite(.data$daily_return), 0, .data$daily_return)
    )
  total_daily$daily_return[1] <- 0

  rownames(trades_df) <- NULL
  rownames(positions_daily) <- NULL
  rownames(total_daily) <- NULL

  # ==============================================
  # 9. Output format conversion
  # ==============================================
  if (output_type == "tibble") {
    positions_daily <- tibble::as_tibble(positions_daily)
    total_daily <- tibble::as_tibble(total_daily)
    if (nrow(trades_df) > 0) {
      trades_df <- tibble::as_tibble(trades_df)
    }
  }

  # ==============================================
  # 10. Output log
  # ==============================================
  final_asset <- tail(total_daily$total_asset, 1)
  total_return <- (final_asset / init_capital - 1) * 100

  message("==============================================")
  message(" Backtest completed!")
  message(" Period: ", start_date, " to ", end_date)
  message(
    "Rebalance mode: ", rebalance_mode,
    ifelse(rebalance_mode == "calendar", paste0(" (", rebalance_cycle, ")"), "")
  )
  message(" Initial capital: ", round(init_capital, 2))
  message(" Final asset: ", round(final_asset, 2))
  message(" Total return: ", round(total_return, 2), "%")
  message("Total trades: ", nrow(trades_df))
  message("==============================================")

  # ==============================================
  # 11. Return results
  # ==============================================
  return(list(
    daily_positions = positions_daily,
    equity_curve = total_daily,
    transactions = trades_df,
    config = list(
      rebalance_mode = rebalance_mode,
      rebalance_cycle = rebalance_cycle,
      total_rebalance_days = sum(is_rebalance_day)
    )
  ))
}

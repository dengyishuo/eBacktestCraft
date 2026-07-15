#' Generate signal based on rolling window statistics
#'
#' Create a 0/1 signal when the indicator value exceeds a rolling mean plus/minus
#' a multiple of rolling standard deviation (e.g., Bollinger Bands breakout) or
#' crosses a moving average. The rolling calculation is performed per asset.
#'
#' @param mkt_data Data frame in long format, must contain 'date', 'code', and the
#'   indicator column.
#' @param indicator_col Character string of column name to analyze (e.g., "close").
#' @param window Integer, rolling window length (e.g., 20 for 20-day).
#' @param n_sd Numeric, number of standard deviations for the band. Use 0 for
#'   simple moving average only.
#' @param direction Character: "above" (signal when indicator > center + n_sd * sd),
#'   "below" (indicator < center - n_sd * sd), or "cross_above" (crossover above
#'   the upper band, requires previous day condition).
#' @param center_type Character: "mean" (default) or "median" for the central tendency.
#' @param signal_name Character, output signal column name. Auto-generated if NULL.
#' @param output Output format: "tibble" (default) or "data.frame"
#'
#' @return Original data frame with an appended signal column.
#'
#' @family signal-timeseries
#' @importFrom dplyr group_by mutate lag ungroup
#' @importFrom zoo rollapply
#' @importFrom rlang .data !! sym :=
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic usage, Bollinger Band breakout signal
#' mkt_data <- data.frame(
#'   date   = rep(seq(as.Date("2023-01-01"), by = "day", length.out = 60), each = 3),
#'   code   = rep(c("AAPL", "MSFT", "GOOG"), times = 60),
#'   name   = rep(c("Apple", "Microsoft", "Alphabet"), times = 60),
#'   close  = round(runif(180, 100, 300), 2),
#'   open   = round(runif(180, 100, 300), 2),
#'   mom_20 = round(runif(180, -0.2, 0.2), 4),
#'   stringsAsFactors = FALSE
#' )
#' result <- add_rolling_signal(mkt_data,
#'   indicator_col = "close",
#'   window        = 20,
#'   n_sd          = 2,
#'   direction     = "above"
#' )
#'
#' # Example 2: Key parameter variant, median-based crossover with 50-day window
#' result_cross <- add_rolling_signal(mkt_data,
#'   indicator_col = "close",
#'   window        = 50,
#'   n_sd          = 0,
#'   direction     = "cross_above",
#'   center_type   = "median",
#'   output        = "data.frame"
#' )
#'
#' # Example 3: Backtest workflow, rolling signal -> equal weight -> run_backtest
#' mkt_data <- add_rolling_signal(mkt_data,
#'   indicator_col = "close", window = 20, n_sd = 2, direction = "above"
#' )
#' mkt_data <- add_equal_weight(mkt_data,
#'   signal_col = "signal_close_SMA20_sd2_above_upper"
#' )
#' # bt <- run_backtest(mkt_data, weight_col = "weight_equal_signal_close_SMA20_sd2_above_upper")
#' }
add_rolling_signal <- function(
  mkt_data,
  indicator_col,
  window = 20,
  n_sd = 2,
  direction = c("above", "below", "cross_above", "cross_below"),
  center_type = c("mean", "median"),
  signal_name = NULL,
  output = c("tibble", "data.frame")
) {
  # ── Input Validation ───────────────────────────────────────────────────────
  # 'date' and 'code' are required; indicator_col must exist
  if (!all(c("date", "code") %in% colnames(mkt_data))) {
    stop("mkt_data must contain 'date' and 'code' columns!")
  }
  if (!indicator_col %in% colnames(mkt_data)) {
    stop("Indicator column not found in mkt_data: ", indicator_col)
  }
  if (window < 2)  stop("window must be at least 2")   # Need at least 2 points for sd
  if (n_sd   < 0)  stop("n_sd must be non-negative")

  direction   <- match.arg(direction)
  center_type <- match.arg(center_type)
  output      <- match.arg(output)

  # ── Rolling Statistics Helper ──────────────────────────────────────────────
  # Compute rolling center and rolling std dev per code group
  roll_func <- function(x) {
    if (center_type == "mean") {
      mu    <- zoo::rollapply(x, window, mean,   fill = NA, align = "right")
      sigma <- zoo::rollapply(x, window, sd,     fill = NA, align = "right")
    } else {
      mu    <- zoo::rollapply(x, window, median, fill = NA, align = "right")
      sigma <- zoo::rollapply(x, window, sd,     fill = NA, align = "right")  # sd still used for band width
    }
    list(mu = mu, sigma = sigma)
  }

  # ── Per-Asset Rolling Signal Computation ──────────────────────────────────
  # Group by 'code' (not 'date') so the rolling window runs along the time series
  result <- mkt_data %>%
    dplyr::group_by(.data$code) %>%
    dplyr::arrange(.data$date, .by_group = TRUE) %>%
    dplyr::mutate(
      value    = !!sym(indicator_col),
      roll     = list(roll_func(.data$value)),
      center   = .data$roll[[1]]$mu,
      sd_roll  = .data$roll[[1]]$sigma,
      upper_band = .data$center + n_sd * .data$sd_roll,
      lower_band = .data$center - n_sd * .data$sd_roll
    ) %>%
    dplyr::select(-.data$roll) %>%
    dplyr::mutate(
      signal_int = dplyr::case_when(
        direction == "above"      ~ as.integer(.data$value > .data$upper_band),
        direction == "below"      ~ as.integer(.data$value < .data$lower_band),
        # cross_above: current bar above upper, prior bar was not — single-bar event
        direction == "cross_above" ~ as.integer(
          .data$value > .data$upper_band &
            dplyr::lag(.data$value, 1) <= dplyr::lag(.data$upper_band, 1)
        ),
        direction == "cross_below" ~ as.integer(
          .data$value < .data$lower_band &
            dplyr::lag(.data$value, 1) >= dplyr::lag(.data$lower_band, 1)
        )
      )
    ) %>%
    dplyr::ungroup()

  # ── Post-Processing ────────────────────────────────────────────────────────
  # NA at the start of each window is expected; map to 0 for downstream use
  result$signal_int[is.na(result$signal_int)] <- 0

  # ── Auto-Generate Signal Column Name ───────────────────────────────────────
  if (is.null(signal_name)) {
    dir_short   <- switch(direction,
      above       = "above_upper",
      below       = "below_lower",
      cross_above = "cross_above",
      cross_below = "cross_below"
    )
    center_short <- ifelse(center_type == "mean", "SMA", "MED")
    signal_name  <- paste0(
      "signal_", indicator_col, "_", center_short, window,
      if (n_sd > 0) paste0("_sd", n_sd), "_", dir_short
    )
  }

  # ── Write Results and Clean Temp Columns ──────────────────────────────────
  result[[signal_name]] <- result$signal_int
  result <- result %>% dplyr::select(
    -.data$value, -.data$center, -.data$sd_roll,
    -.data$upper_band, -.data$lower_band, -.data$signal_int
  )

  message(
    " Generated rolling signal column: ", signal_name,
    " (window=", window, ", n_sd=", n_sd, ", direction=", direction, ")"
  )

  # ── Output Format Conversion ───────────────────────────────────────────────
  if (output == "tibble") result <- tibble::as_tibble(result)

  return(result)
}

#' Standardized signal generation function for trading strategies
#'
#' Generate 0/1 trading signals based on technical indicators or factor values.
#' Supports threshold judgment, crossover detection, multi-condition logic,
#' constant signal assignment, and between-range detection.
#'
#' @param mkt_data Data frame in long format, must contain 'date' and 'code' columns
#' @param indicator_cols Character vector of indicator column names used for signal calculation,
#'   e.g., c("ram_20", "mom_5")
#' @param signal_type Signal type:
#'   - "threshold": Threshold judgment (default)
#'   - "crossover": Crossover detection (golden cross / death cross)
#'   - "multi_condition": Multi-condition logic (AND/OR)
#'   - "constant": Constant signal (all 1s, all 0s, or any fixed value)
#'   - "between": Check if indicator value is within a range [lower, upper] (inclusive)
#' @param threshold Numeric threshold value(s), only used for "threshold" type
#' @param compare_op Comparison operator: ">", "<", ">=", "<=", "==", "!="
#' @param cross_upper Upper band (numeric or column name), only used for "crossover" type
#' @param cross_lower Lower band (numeric or column name), only used for "crossover" type
#' @param logic_op Multi-condition logic: "&" for AND, "|" for OR
#' @param between_lower Numeric lower bound (inclusive), only used for "between" type
#' @param between_upper Numeric upper bound (inclusive), only used for "between" type
#' @param signal_name Output signal column name. Auto-generated if NULL
#' @param constant_value Constant value to assign, only used for "constant" type (e.g., 1 or 0)
#' @param output Output format: "tibble" (default) or "data.frame"
#'
#' @return Original data frame with an appended signal column in specified format
#'
#' @importFrom dplyr lag
#' @importFrom rlang .data !! sym :=
#' @importFrom tibble as_tibble
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic threshold signal, momentum above zero triggers a long signal
#' mkt_data <- data.frame(
#'   date   = rep(seq(as.Date("2023-01-01"), by = "day", length.out = 60), each = 3),
#'   code   = rep(c("AAPL", "MSFT", "GOOG"), times = 60),
#'   name   = rep(c("Apple", "Microsoft", "Alphabet"), times = 60),
#'   close  = round(runif(180, 100, 300), 2),
#'   open   = round(runif(180, 100, 300), 2),
#'   mom_20 = round(runif(180, -0.2, 0.2), 4),
#'   stringsAsFactors = FALSE
#' )
#' result <- add_signal(mkt_data,
#'   indicator_cols = "mom_20",
#'   signal_type    = "threshold",
#'   threshold      = 0,
#'   compare_op     = ">"
#' )
#'
#' # Example 2: Between-range signal, mom_20 between -0.05 and 0.1 (mean-reversion zone)
#' result_between <- add_signal(mkt_data,
#'   indicator_cols = "mom_20",
#'   signal_type    = "between",
#'   between_lower  = -0.05,
#'   between_upper  =  0.10
#' )
#'
#' # Example 3: Backtest workflow, threshold signal -> equal weight -> run_backtest
#' mkt_data <- add_signal(mkt_data,
#'   indicator_cols = "mom_20",
#'   signal_type    = "threshold",
#'   threshold      = 0,
#'   compare_op     = ">",
#'   output         = "data.frame"
#' )
#' mkt_data <- add_equal_weight(mkt_data, signal_col = "signal_mom_20_gt_0")
#' # bt <- run_backtest(mkt_data, weight_col = "weight_equal_signal_mom_20_gt_0")
#' }
add_signal <- function(
  mkt_data,
  indicator_cols = NULL,
  signal_type = c("threshold", "crossover", "multi_condition", "constant", "between"),
  threshold = 0,
  compare_op = ">",
  cross_upper = NULL,
  cross_lower = NULL,
  logic_op = "&",
  between_lower = NULL,
  between_upper = NULL,
  signal_name = NULL,
  constant_value = 1,
  output = c("tibble", "data.frame")
) {
  # ── Input Validation ───────────────────────────────────────────────────────
  # Enforce minimum schema: both 'date' and 'code' are required for panel operations
  if (!all(c("date", "code") %in% colnames(mkt_data))) {
    stop("mkt_data must contain 'date' and 'code' columns!")
  }

  signal_type <- match.arg(signal_type)
  output      <- match.arg(output)

  # Constant signal does not require any indicator column
  if (signal_type != "constant") {
    if (is.null(indicator_cols) || length(indicator_cols) == 0) {
      stop("Non-constant signal types must specify indicator_cols!")
    }
    if (!all(indicator_cols %in% colnames(mkt_data))) {
      missing_cols <- setdiff(indicator_cols, colnames(mkt_data))
      stop("Specified indicator column(s) not found in mkt_data: ", paste(missing_cols, collapse = ", "))
    }
  }

  # Between type needs exactly one column and valid numeric bounds
  if (signal_type == "between") {
    if (length(indicator_cols) != 1) {
      stop("Between signal type requires exactly one indicator column!")
    }
    if (is.null(between_lower) || is.null(between_upper)) {
      stop("Between signal type requires both between_lower and between_upper parameters!")
    }
    if (!is.numeric(between_lower) || !is.numeric(between_upper)) {
      stop("between_lower and between_upper must be numeric!")
    }
    if (between_lower > between_upper) {
      stop("between_lower must be less than or equal to between_upper!")
    }
  }

  # Threshold with multiple columns: threshold/compare_op must be scalar or match length
  if (signal_type == "threshold" && length(indicator_cols) > 1) {
    if (length(threshold) != 1 && length(threshold) != length(indicator_cols)) {
      stop("For multiple columns, threshold must be length 1 or match number of indicator columns!")
    }
    if (length(compare_op) != 1 && length(compare_op) != length(indicator_cols)) {
      stop("For multiple columns, compare_op must be length 1 or match number of indicator columns!")
    }
  }

  if (signal_type == "multi_condition" && length(indicator_cols) < 2) {
    stop("multi_condition type requires at least 2 indicator columns!")
  }

  # ── Auto-Generate Signal Column Name ───────────────────────────────────────
  if (is.null(signal_name)) {
    if (signal_type == "threshold") {
      op_map   <- c(">" = "gt", "<" = "lt", ">=" = "gte", "<=" = "lte", "==" = "eq", "!=" = "neq")
      op_short <- op_map[compare_op[1]]
      signal_name <- paste0("signal_", indicator_cols[1], "_", op_short, "_", gsub("\\.", "", as.character(threshold[1])))
    } else if (signal_type == "crossover") {
      suffix      <- ifelse(is.null(cross_lower), "cross_up", "cross_down")
      signal_name <- paste0("signal_", indicator_cols[1], "_", suffix)
    } else if (signal_type == "multi_condition") {
      logic       <- ifelse(logic_op == "&", "and", "or")
      signal_name <- paste0("signal_", paste(indicator_cols, collapse = "_"), "_", logic)
    } else if (signal_type == "constant") {
      signal_name <- paste0("signal_constant_", constant_value)
    } else if (signal_type == "between") {
      signal_name <- paste0("signal_", indicator_cols[1], "_between_", between_lower, "_", between_upper)
    }
  }

  # ── Signal Calculation ─────────────────────────────────────────────────────
  result_df <- mkt_data

  # Constant signal: assign a fixed integer to every row without reading any indicator
  if (signal_type == "constant") {
    signal_result              <- rep(as.integer(constant_value), nrow(result_df))
    signal_result[is.na(signal_result)] <- 0L
    result_df[[signal_name]]   <- signal_result
    message(" Generated constant signal column: ", signal_name, " (all marked as ", constant_value, ")")

    if (output == "tibble") result_df <- tibble::as_tibble(result_df)
    return(result_df)
  }

  # Threshold signal: iterate over indicator columns, combining with AND logic
  if (signal_type == "threshold") {
    signal_result <- TRUE
    for (i in seq_along(indicator_cols)) {
      col   <- indicator_cols[i]
      thresh <- if (length(threshold)  == 1) threshold  else threshold[i]
      op     <- if (length(compare_op) == 1) compare_op else compare_op[i]

      v              <- result_df[[col]]
      v[is.na(v) | is.infinite(v)] <- NA  # Neutralize bad values so they never pass the condition

      cond <- switch(op,
        ">"  = v > thresh,
        "<"  = v < thresh,
        ">=" = v >= thresh,
        "<=" = v <= thresh,
        "==" = v == thresh,
        "!=" = v != thresh,
        stop("Unsupported comparison operator")
      )
      cond[is.na(cond)] <- FALSE          # NA comparisons must yield no signal
      signal_result <- if (i == 1) cond else signal_result & cond
    }
    signal_result <- as.integer(signal_result)
  }

  # Crossover signal: detect price crossing a band boundary
  else if (signal_type == "crossover") {
    col <- indicator_cols[1]
    v   <- result_df[[col]]
    v[is.na(v) | is.infinite(v)] <- 0    # Default to 0 so lag arithmetic stays valid

    if (is.character(cross_upper) && cross_upper %in% colnames(result_df)) {
      upper <- result_df[[cross_upper]]
    } else {
      upper <- rep(as.numeric(cross_upper), nrow(result_df))
    }
    upper[is.na(upper)] <- 0

    if (!is.null(cross_lower)) {
      if (is.character(cross_lower) && cross_lower %in% colnames(result_df)) {
        lower <- result_df[[cross_lower]]
      } else {
        lower <- rep(as.numeric(cross_lower), nrow(result_df))
      }
      lower[is.na(lower)] <- 0
      # Death cross: current price falls below lower band while prior day was above
      cross <- (v < lower) & (dplyr::lag(v, 1) > dplyr::lag(lower, 1))
    } else {
      # Golden cross: current price rises above upper band while prior day was below
      cross <- (v > upper) & (dplyr::lag(v, 1) < dplyr::lag(upper, 1))
    }
    cross[is.na(cross)] <- FALSE
    signal_result <- as.integer(cross)
  }

  # Multi-condition logic: combine multiple boolean conditions via AND or OR
  else if (signal_type == "multi_condition") {
    signal_result <- NULL
    for (col in indicator_cols) {
      v              <- result_df[[col]]
      v[is.na(v) | is.infinite(v)] <- 0
      cond           <- v > 0
      cond[is.na(cond)] <- FALSE

      if (is.null(signal_result)) {
        signal_result <- cond
      } else {
        signal_result <- if (logic_op == "&") signal_result & cond else signal_result | cond
      }
    }
    signal_result <- as.integer(signal_result)
  }

  # Between-range signal: value must lie within [lower, upper] inclusive
  else if (signal_type == "between") {
    col <- indicator_cols[1]
    v   <- result_df[[col]]
    v[is.na(v) | is.infinite(v)] <- NA_real_  # Treat NA/Inf as failing the range test
    cond              <- (v >= between_lower) & (v <= between_upper)
    cond[is.na(cond)] <- FALSE
    signal_result     <- as.integer(cond)
  }

  # ── Write Results ──────────────────────────────────────────────────────────
  result_df[[signal_name]] <- signal_result
  message(" Generated signal column: ", signal_name, ", valid signals: ", sum(signal_result, na.rm = TRUE))

  # ── Output Format Conversion ───────────────────────────────────────────────
  if (output == "tibble") result_df <- tibble::as_tibble(result_df)

  return(result_df)
}

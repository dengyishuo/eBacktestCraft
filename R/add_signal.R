#' Standardized signal generation function for trading strategies
#'
#' Generate 0/1 trading signals based on technical indicators or factor values.
#' Supports threshold judgment, crossover detection, multi-condition logic,
#' and constant signal assignment.
#'
#' @param df Data frame in long format, must contain 'date' and 'code' columns
#' @param indicator_cols Character vector of indicator column names used for signal calculation,
#'   e.g., c("ram_20", "mom_5")
#' @param signal_type Signal type:
#'   - "threshold": Threshold judgment (default)
#'   - "crossover": Crossover detection (golden cross / death cross)
#'   - "multi_condition": Multi-condition logic (AND/OR)
#'   - "constant": Constant signal (all 1s, all 0s, or any fixed value)
#' @param threshold Numeric threshold value(s), only used for "threshold" type
#' @param compare_op Comparison operator: ">", "<", ">=", "<=", "==", "!="
#' @param cross_upper Upper band (numeric or column name), only used for "crossover" type
#' @param cross_lower Lower band (numeric or column name), only used for "crossover" type
#' @param logic_op Multi-condition logic: "&" for AND, "|" for OR
#' @param signal_name Output signal column name. Auto-generated if NULL
#' @param constant_value Constant value to assign, only used for "constant" type (e.g., 1 or 0)
#' @param output_type Output format: "tibble" (default) or "data.frame"
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
#' # Threshold signal returning tibble (default)
#' df_tibble <- add_signal(df,
#'   indicator_cols = "ram_20",
#'   signal_type = "threshold",
#'   threshold = 0, compare_op = ">"
#' )
#'
#' # Threshold signal returning data.frame
#' df_frame <- add_signal(df,
#'   indicator_cols = "ram_20",
#'   signal_type = "threshold",
#'   threshold = 0, compare_op = ">",
#'   output_type = "data.frame"
#' )
#' }
add_signal <- function(
  df,
  indicator_cols = NULL,
  signal_type = c("threshold", "crossover", "multi_condition", "constant"),
  threshold = 0,
  compare_op = ">",
  cross_upper = NULL,
  cross_lower = NULL,
  logic_op = "&",
  signal_name = NULL,
  constant_value = 1,
  output_type = c("tibble", "data.frame")
) {
  # --------------------------
  # 1. Input validation
  # --------------------------
  if (!all(c("date", "code") %in% colnames(df))) {
    stop("Input data must contain 'date' and 'code' columns!")
  }

  signal_type <- match.arg(signal_type)
  output_type <- match.arg(output_type)

  # Constant signal doesn't require indicator columns
  if (signal_type != "constant") {
    if (is.null(indicator_cols) || length(indicator_cols) == 0) {
      stop("Non-constant signal types must specify indicator_cols!")
    }
    if (!all(indicator_cols %in% colnames(df))) {
      missing_cols <- setdiff(indicator_cols, colnames(df))
      stop("Specified indicator column(s) not found: ", paste(missing_cols, collapse = ", "))
    }
  }

  # Multi-column parameter validation
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

  # --------------------------
  # 2. Auto-generate column name
  # --------------------------
  if (is.null(signal_name)) {
    if (signal_type == "threshold") {
      op_map <- c(">" = "gt", "<" = "lt", ">=" = "gte", "<=" = "lte", "==" = "eq", "!=" = "neq")
      op_short <- op_map[compare_op[1]]
      signal_name <- paste0("signal_", indicator_cols[1], "_", op_short, "_", gsub("\\.", "", as.character(threshold[1])))
    } else if (signal_type == "crossover") {
      suffix <- ifelse(is.null(cross_lower), "cross_up", "cross_down")
      signal_name <- paste0("signal_", indicator_cols[1], "_", suffix)
    } else if (signal_type == "multi_condition") {
      logic <- ifelse(logic_op == "&", "and", "or")
      signal_name <- paste0("signal_", paste(indicator_cols, collapse = "_"), "_", logic)
    } else if (signal_type == "constant") {
      signal_name <- paste0("signal_constant_", constant_value)
    }
  }

  # --------------------------
  # 3. Signal calculation
  # --------------------------
  result_df <- df

  # Constant signal (all 1s / all 0s)
  if (signal_type == "constant") {
    signal_result <- rep(as.integer(constant_value), nrow(result_df))
    signal_result[is.na(signal_result)] <- 0L

    result_df[[signal_name]] <- signal_result
    message(" Generated constant signal column: ", signal_name, " (all marked as ", constant_value, ")")

    # Convert output format
    if (output_type == "tibble") {
      result_df <- tibble::as_tibble(result_df)
    }
    return(result_df)
  }

  # Threshold signal
  if (signal_type == "threshold") {
    signal_result <- TRUE
    for (i in seq_along(indicator_cols)) {
      col <- indicator_cols[i]
      thresh <- if (length(threshold) == 1) threshold else threshold[i]
      op <- if (length(compare_op) == 1) compare_op else compare_op[i]

      v <- result_df[[col]]
      v[is.na(v) | is.infinite(v)] <- NA

      cond <- switch(op,
        ">" = v > thresh,
        "<" = v < thresh,
        ">=" = v >= thresh,
        "<=" = v <= thresh,
        "==" = v == thresh,
        "!=" = v != thresh,
        stop("Unsupported comparison operator")
      )
      cond[is.na(cond)] <- FALSE
      signal_result <- if (i == 1) cond else signal_result & cond
    }
    signal_result <- as.integer(signal_result)
  }

  # Crossover signal
  else if (signal_type == "crossover") {
    col <- indicator_cols[1]
    v <- result_df[[col]]
    v[is.na(v) | is.infinite(v)] <- 0

    # Upper band
    if (is.character(cross_upper) && cross_upper %in% colnames(result_df)) {
      upper <- result_df[[cross_upper]]
    } else {
      upper <- rep(as.numeric(cross_upper), nrow(result_df))
    }
    upper[is.na(upper)] <- 0

    # Lower band (for death cross)
    if (!is.null(cross_lower)) {
      if (is.character(cross_lower) && cross_lower %in% colnames(result_df)) {
        lower <- result_df[[cross_lower]]
      } else {
        lower <- rep(as.numeric(cross_lower), nrow(result_df))
      }
      lower[is.na(lower)] <- 0
      cross <- (v < lower) & (dplyr::lag(v, 1) > dplyr::lag(lower, 1))
    } else {
      cross <- (v > upper) & (dplyr::lag(v, 1) < dplyr::lag(upper, 1))
    }
    cross[is.na(cross)] <- FALSE
    signal_result <- as.integer(cross)
  }

  # Multi-condition logic
  else if (signal_type == "multi_condition") {
    signal_result <- NULL
    for (col in indicator_cols) {
      v <- result_df[[col]]
      v[is.na(v) | is.infinite(v)] <- 0
      cond <- v > 0
      cond[is.na(cond)] <- FALSE

      if (is.null(signal_result)) {
        signal_result <- cond
      } else {
        signal_result <-
          if (logic_op == "&") {
            signal_result & cond
          } else if (logic_op == "|") signal_result | cond
      }
    }
    signal_result <- as.integer(signal_result)
  }

  # --------------------------
  # 4. Write results
  # --------------------------
  result_df[[signal_name]] <- signal_result
  message(" Generated signal column: ", signal_name, ", valid signals: ", sum(signal_result, na.rm = TRUE))

  # --------------------------
  # 5. Convert output format
  # --------------------------
  if (output_type == "tibble") {
    result_df <- tibble::as_tibble(result_df)
  }

  return(result_df)
}

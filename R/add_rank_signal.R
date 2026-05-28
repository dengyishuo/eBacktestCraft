#' Rank-based signal generation function
#'
#' Generate 0/1 signals by ranking specified indicators within each trading day.
#' Select TOP N or BOTTOM N stocks based on ranking order.
#' Fully compatible with quantitative strategy long-format data structure.
#'
#' @param df Data frame in long format, must contain 'date' and 'code' columns
#' @param rank_col Character string of column name to rank (e.g., "mom_20", "vol_60")
#' @param top_n Integer, number of stocks to select per day, default 2
#' @param rank_order Ranking order: "desc" for descending (TOP), "asc" for ascending (BOTTOM)
#' @param tie_method Method for handling ties: "min", "max", or "average", default "min"
#' @param signal_name Character, output signal column name. Auto-generated if NULL
#' @param output_type Output format: "tibble" (default) or "data.frame"
#'
#' @return Data frame with an appended 0/1 signal column (1 = selected, 0 = not selected)
#'   in the specified output format
#'
#' @importFrom dplyr group_by mutate ungroup select
#' @importFrom rlang .data !! sym :=
#' @export
#'
#' @examples
#' \dontrun{
#' # Select TOP 2 stocks by mom_20 (descending order)
#' df <- add_rank_signal(df, rank_col = "mom_20", top_n = 2)
#'
#' # Select BOTTOM 3 stocks by vol_60 (ascending order)
#' df <- add_rank_signal(df, rank_col = "vol_60", top_n = 3, rank_order = "asc")
#'
#' # Return as base data.frame instead of tibble
#' df <- add_rank_signal(df, rank_col = "mom_20", top_n = 5, output_type = "data.frame")
#' }
add_rank_signal <- function(
  df,
  rank_col,
  top_n = 2,
  rank_order = "desc",
  tie_method = "min",
  signal_name = NULL,
  output_type = c("tibble", "data.frame")
) {
  # --------------------------
  # 1. Input validation
  # --------------------------
  # Check required columns
  if (!all(c("date", "code") %in% colnames(df))) {
    stop("Input data must contain 'date' and 'code' columns!")
  }

  # Check rank column exists
  if (!rank_col %in% colnames(df)) {
    stop("Specified rank column not found: ", rank_col)
  }

  # Validate parameters
  if (!is.numeric(top_n) || top_n <= 0 || top_n != as.integer(top_n)) {
    stop("top_n must be a positive integer!")
  }

  valid_rank_orders <- c("desc", "asc")
  if (!rank_order %in% valid_rank_orders) {
    stop("rank_order must be one of: ", paste(valid_rank_orders, collapse = ", "))
  }

  valid_tie_methods <- c("min", "max", "average")
  if (!tie_method %in% valid_tie_methods) {
    stop("tie_method must be one of: ", paste(valid_tie_methods, collapse = ", "))
  }

  output_type <- match.arg(output_type)

  # --------------------------
  # 2. Auto-generate signal column name
  # --------------------------
  if (is.null(signal_name)) {
    order_short <- ifelse(rank_order == "desc", "top", "bottom")
    signal_name <- paste0("signal_", rank_col, "_", order_short, top_n)
  }

  # --------------------------
  # 3. Core ranking and signal calculation logic
  # --------------------------
  result_df <- df

  # Group by date, calculate ranks, generate signals
  result_df <- result_df %>%
    dplyr::group_by(.data$date) %>%
    dplyr::mutate(
      # Handle outliers: replace NA/Inf with -Inf (for desc) or Inf (for asc)
      .rank_value = ifelse(
        is.na(!!sym(rank_col)) | is.infinite(!!sym(rank_col)),
        ifelse(rank_order == "desc", -Inf, Inf),
        !!sym(rank_col)
      ),
      # Calculate rank according to order
      .rank = rank(
        x = ifelse(rank_order == "desc", -.data$.rank_value, .data$.rank_value),
        ties.method = tie_method,
        na.last = TRUE
      ),
      # Generate signal: 1 if rank <= top_n, otherwise 0
      !!signal_name := as.integer(.data$.rank <= top_n)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$.rank_value, -.data$.rank)

  # Handle remaining NAs: convert to 0
  result_df[[signal_name]] <- ifelse(is.na(result_df[[signal_name]]), 0, result_df[[signal_name]])

  # --------------------------
  # 4. Output format conversion
  # --------------------------
  if (output_type == "tibble") {
    result_df <- tibble::as_tibble(result_df)
  }

  # --------------------------
  # 5. Diagnostic message
  # --------------------------
  avg_selected <- mean(result_df[[signal_name]], na.rm = TRUE) * length(unique(result_df$code))
  message(
    " Generated rank signal column: ", signal_name,
    ", average daily selected stocks: ", round(avg_selected, 2)
  )

  return(result_df)
}

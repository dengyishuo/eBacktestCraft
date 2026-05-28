#' Plot position ratio over time
#'
#' @param perf_result List, output from performance_analysis() function
#' @return ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' perf <- performance_analysis(result)
#' plot_position_ratio(perf)
#' }
plot_position_ratio <- function(perf_result) {
  df <- perf_result$daily_details

  # Check if market_value exists
  if (!"market_value" %in% colnames(df)) {
    warning("market_value column not found in daily_details")
    return(NULL)
  }

  df$date <- as.Date(df$date)
  df$position_ratio <- df$market_value / df$total_asset
  df$position_ratio[is.na(df$position_ratio) | is.infinite(df$position_ratio)] <- 0

  ggplot2::ggplot(df, ggplot2::aes(x = .data$date, y = .data$position_ratio)) +
    ggplot2::geom_line(color = "#F77F00", linewidth = 1) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    ggplot2::labs(title = "Position Ratio", x = "Date", y = "Position Ratio") +
    theme_quant()
}

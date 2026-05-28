#' Plot equity curve (total asset over time)
#'
#' @param perf_result List, output from performance_analysis() function
#' @return ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' perf <- performance_analysis(result)
#' plot_equity_curve(perf)
#' }
plot_equity_curve <- function(perf_result) {
  df <- perf_result$daily_details
  df$date <- as.Date(df$date)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$date, y = .data$total_asset)) +
    ggplot2::geom_line(color = "#E63946", linewidth = 1) +
    ggplot2::labs(title = "Equity Curve", x = "", y = "Total Asset") +
    theme_quant()
}

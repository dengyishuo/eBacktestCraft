#' Combined plot: Cumulative return + Drawdown in one chart
#'
#' @param perf_result List, output from performance_analysis() function
#' @return ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' perf <- performance_analysis(result)
#' plot_return_drawdown_combined(perf)
#' }
plot_return_drawdown_combined <- function(perf_result) {
  df <- perf_result$daily_details
  df$date <- as.Date(df$date)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$date)) +
    ggplot2::geom_area(
      ggplot2::aes(y = .data$drawdown),
      fill = "#2A9D8F",
      alpha = 0.7
    ) +
    ggplot2::geom_area(
      ggplot2::aes(y = .data$cum_return - 1),
      fill = "#E63946",
      alpha = 0.7
    ) +
    ggplot2::labs(
      title = "Cumulative Return vs Drawdown",
      x = "Date",
      y = "Return / Drawdown"
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    theme_quant()
}

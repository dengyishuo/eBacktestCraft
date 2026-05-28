#' Plot drawdown area chart
#'
#' @param perf_result List, output from performance_analysis() function
#' @return ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' perf <- performance_analysis(result)
#' plot_drawdown(perf)
#' }
plot_drawdown <- function(perf_result) {
  df <- perf_result$daily_details
  df$date <- as.Date(df$date)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$date, y = .data$drawdown)) +
    ggplot2::geom_area(fill = "#2A9D8F", alpha = 0.7) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::labs(title = "Drawdown Curve", x = "Date", y = "Drawdown") +
    theme_quant()
}

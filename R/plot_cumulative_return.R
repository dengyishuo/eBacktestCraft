#' Plot cumulative return area chart
#'
#' @param perf_result List, output from performance_analysis() function
#' @return ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' perf <- performance_analysis(result)
#' plot_cumulative_return(perf)
#' }
plot_cumulative_return <- function(perf_result) {
  df <- perf_result$daily_details
  df$date <- as.Date(df$date)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$date, y = .data$cum_return - 1)) +
    ggplot2::geom_area(fill = "#E63946", alpha = 0.7) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::labs(title = "Cumulative Return", x = "Date", y = "Return") +
    theme_quant()
}

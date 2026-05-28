#' Plot daily return bar chart
#'
#' @param perf_result List, output from performance_analysis() function
#' @return ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' perf <- performance_analysis(result)
#' plot_daily_return(perf)
#' }
plot_daily_return <- function(perf_result) {
  df <- perf_result$daily_details
  df$date <- as.Date(df$date)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$date, y = .data$daily_return)) +
    ggplot2::geom_col(
      ggplot2::aes(fill = .data$daily_return >= 0),
      alpha = 0.8,
      position = "identity"
    ) +
    ggplot2::scale_fill_manual(values = c("TRUE" = "#E63946", "FALSE" = "#2A9D8F")) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    ggplot2::labs(title = "Daily Returns", x = "Date", y = "Daily Return") +
    theme_quant() +
    ggplot2::theme(legend.position = "none")
}

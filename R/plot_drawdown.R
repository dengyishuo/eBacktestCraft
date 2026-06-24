#' Plot Drawdown Curve
#'
#' Area chart of the strategy's rolling drawdown from the high-water mark.
#' The deepest trough is annotated with the maximum drawdown value.
#'
#' @param perf_result List returned by \code{performance_analysis()}, or a
#'   data frame containing columns \code{date} and \code{drawdown}.
#' @param color Character. Fill colour. Default \code{"#2A9D8F"}.
#' @param annotate_max Logical. If \code{TRUE} (default), annotate the maximum
#'   drawdown point with its value.
#' @param title Character. Plot title. Default \code{"Drawdown"}.
#'
#' @return A ggplot object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_area geom_hline scale_y_continuous labs annotate
#' @importFrom scales percent_format
#'
#' @examples
#' \dontrun{
#' perf <- performance_analysis(res)
#' plot_drawdown(perf)
#' plot_drawdown(perf, annotate_max = FALSE)
#' }
plot_drawdown <- function(perf_result,
                          color = "#2A9D8F",
                          annotate_max = TRUE,
                          title = "Drawdown") {
  df <- if (is.data.frame(perf_result)) {
    perf_result
  } else {
    perf_result$daily_details
  }
  df$date <- as.Date(df$date)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$date, y = .data$drawdown)) +
    ggplot2::geom_area(fill = color, alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::labs(title = title, x = NULL, y = "Drawdown") +
    theme_quant()

  if (annotate_max) {
    idx <- which.min(df$drawdown)
    max_dd <- df$drawdown[idx]
    max_date <- df$date[idx]
    p <- p +
      ggplot2::annotate(
        "point",
        x = max_date, y = max_dd,
        color = "red", size = 2.5
      ) +
      ggplot2::annotate(
        "text",
        x = max_date, y = max_dd,
        label = scales::percent(max_dd, accuracy = 0.1),
        vjust = 1.6, hjust = 0.5, size = 3, color = "red"
      )
  }

  p
}

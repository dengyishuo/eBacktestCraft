#' Plot Daily Return Bar Chart
#'
#' Bar chart of daily returns, coloured green for positive and red for negative
#' days. A horizontal zero line is drawn for reference.
#'
#' @param perf_result List returned by \code{performance_analysis()}, or a
#'   data frame containing columns \code{date} and \code{daily_return}.
#' @param color_up Character. Bar colour for positive returns.
#'   Default \code{"#E63946"}.
#' @param color_down Character. Bar colour for negative returns.
#'   Default \code{"#2A9D8F"}.
#' @param title Character. Plot title. Default \code{"Daily Returns"}.
#'
#' @return A ggplot object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_col geom_hline scale_fill_manual scale_y_continuous labs theme
#' @importFrom scales percent_format
#'
#' @examples
#' \dontrun{
#' perf <- performance_analysis(res)
#' plot_daily_return(perf)
#' }
plot_daily_return <- function(perf_result,
                              color_up = "#E63946",
                              color_down = "#2A9D8F",
                              title = "Daily Returns") {
  df <- if (is.data.frame(perf_result)) {
    perf_result
  } else {
    perf_result$daily_details
  }
  df$date <- as.Date(df$date)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$date, y = .data$daily_return)) +
    ggplot2::geom_col(
      ggplot2::aes(fill = .data$daily_return >= 0),
      alpha = 0.8,
      position = "identity"
    ) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = color_up, "FALSE" = color_down)
    ) +
    ggplot2::geom_hline(yintercept = 0, color = "grey30", linewidth = 0.4) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::labs(title = title, x = NULL, y = "Daily Return") +
    theme_quant() +
    ggplot2::theme(legend.position = "none")
}

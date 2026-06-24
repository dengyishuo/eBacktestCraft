#' Plot Daily Return Distribution
#'
#' Histogram of daily returns overlaid with a normal-distribution reference
#' curve. Vertical lines mark the mean and ±1 standard deviation. The
#' left tail (losses) is shaded in a contrasting colour.
#'
#' @param perf_result List returned by \code{performance_analysis()}, or a
#'   data frame containing column \code{daily_return}.
#' @param bins Integer. Number of histogram bins. Default \code{60}.
#' @param color_pos Character. Fill colour for positive-return bars.
#'   Default \code{"#E63946"}.
#' @param color_neg Character. Fill colour for negative-return bars.
#'   Default \code{"#2A9D8F"}.
#' @param show_normal Logical. Overlay a fitted normal density curve.
#'   Default \code{TRUE}.
#' @param title Character. Plot title. Default \code{"Daily Return Distribution"}.
#'
#' @return A ggplot object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_histogram geom_vline geom_function scale_x_continuous scale_y_continuous labs theme after_stat
#' @importFrom scales percent_format
#' @importFrom stats dnorm sd
#'
#' @examples
#' \dontrun{
#' perf <- performance_analysis(res)
#' plot_return_dist(perf)
#' plot_return_dist(perf, bins = 40, show_normal = FALSE)
#' }
plot_return_dist <- function(perf_result,
                             bins = 60L,
                             color_pos = "#E63946",
                             color_neg = "#2A9D8F",
                             show_normal = TRUE,
                             title = "Daily Return Distribution") {
  df <- if (is.data.frame(perf_result)) {
    perf_result
  } else {
    perf_result$daily_details
  }

  ret <- stats::na.omit(df$daily_return)
  mu <- mean(ret)
  sig <- stats::sd(ret)

  plot_df <- data.frame(daily_return = ret)

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$daily_return)) +
    ggplot2::geom_histogram(
      ggplot2::aes(
        fill  = ggplot2::after_stat(.data$x >= 0),
        y     = ggplot2::after_stat(.data$density)
      ),
      bins = bins,
      alpha = 0.75,
      color = "white",
      linewidth = 0.2
    ) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = color_pos, "FALSE" = color_neg),
      guide  = "none"
    ) +
    ggplot2::geom_vline(
      xintercept = mu, color = "grey20",
      linetype = "dashed", linewidth = 0.7
    ) +
    ggplot2::geom_vline(
      xintercept = c(mu - sig, mu + sig), color = "grey50",
      linetype = "dotted", linewidth = 0.5
    ) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::labs(
      title = title,
      subtitle = sprintf(
        "Mean = %+.3f%%   Std Dev = %.3f%%   Skewness = %.2f   Excess Kurtosis = %.2f",
        mu * 100, sig * 100,
        mean(((ret - mu) / sig)^3),
        mean(((ret - mu) / sig)^4) - 3
      ),
      x = "Daily Return",
      y = "Density"
    ) +
    theme_quant()

  if (show_normal) {
    p <- p +
      ggplot2::geom_function(
        fun = stats::dnorm,
        args = list(mean = mu, sd = sig),
        color = "grey20", linewidth = 0.9
      )
  }

  p
}
